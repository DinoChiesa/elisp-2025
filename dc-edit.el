;;; dc-edit.el --- editing and buffer utilities for Dino   -*- coding: utf-8; lexical-binding: t;  -*-

;; Author: Dino Chiesa
;; Version: 2026.10.24
;; Keywords: utility, editing
;; License: New BSD

;;; Commentary:
;;
;; General text, code, buffer, and editing utilities.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 's)
(require 'url-util)

;;; Filename insertion and replacement

(defconst dc-edit--short-filename-regex "[A-Za-z]\\S+\\.[-A-Za-z0-9_]+"
  "Regexp for finding a filename.")

(defconst dc-edit--short-filename-no-extension-regex "[A-Za-z][-A-Za-z0-9_-]+"
  "Regexp for finding a filename without an extension.")

(defconst dc-edit--filename-edge-regex "[ \\!\\?\\\"\\.'#$%&*+/;<=>@^`|~]"
  "Regexp for finding the edge of a filename.")

(defun dc-edit/fixup-linefeeds ()
  "Replace DOS CR-LF (\\xd\\xa) with Unix LF (\\xa)."
  (interactive)
  (save-excursion
    (while (search-forward "\xd" nil t)
      (replace-match "" nil t))))

(defun dc-edit/sort-lines-nocase ()
  "Sort lines in region case-insensitively."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun dc-edit/insert-filename (&optional arg)
  "Insert the name of the file behind the buffer at point.
When invoked with prefix ARG (C-u), sets the full directory path into
the kill ring instead of inserting. Also works in Dired buffers."
  (interactive "P")
  (let* ((full-path
          (or (buffer-file-name)
              (and (derived-mode-p 'dired-mode)
                   (or (and (fboundp 'dired-current-directory)
                            (dired-current-directory))
                       default-directory))))
         (fname
          (when full-path
            (if arg
                full-path
              (file-name-nondirectory (directory-file-name full-path))))))
    (if (not fname)
        (message "No file or directory associated with buffer")
      (kill-new fname)
      (if (and (not arg)
               (not buffer-read-only))
          (insert fname)
        (message fname)))))

(defun dc-edit--maybe-delete-file-name-looking-forward (no-extension)
  "If point is on a filename, delete it."
  (let ((found
         (if no-extension
             (looking-at dc-edit--short-filename-no-extension-regex)
           (looking-at dc-edit--short-filename-regex))))
    (when found
      (setq found (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
      (delete-region (match-beginning 0) (match-end 0)))
    found))

(defun dc-edit/replace-filename (&optional arg)
  "Insert the name of the file behind the buffer at point, replacing existing filename.
Optional prefix ARG replaces base filename with no extension."
  (interactive "P")
  (let* ((no-extension arg)
         (full-fname (file-name-nondirectory (buffer-file-name)))
         (fname (if no-extension
                    (file-name-sans-extension full-fname)
                  full-fname)))
    (kill-new fname)
    (and (re-search-backward dc-edit--filename-edge-regex (line-beginning-position) t)
         (progn (forward-char)
                (dc-edit--maybe-delete-file-name-looking-forward no-extension)))
    (insert fname)))

(defun dc-edit/replace-filename-no-extension ()
  "Insert the base name of the file behind the buffer at point, replacing existing."
  (interactive)
  (dc-edit/replace-filename t))

;;; Indentation & Untabify

(defun dc-edit/indent-buffer ()
  "Re-indent an entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun dc-edit/toggle-buffer-modified ()
  "Toggle the `buffer-modified-p' value for the current buffer."
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

(defun dc-edit/do-markdown ()
  "Remove `delete-trailing-whitespace' from `before-save-hook'.
Helpful when editing markdown files requiring trailing whitespace for linebreaks."
  (interactive)
  (remove-hook 'write-contents-functions 'delete-trailing-whitespace)
  (remove-hook 'before-save-hook-functions 'delete-trailing-whitespace))

(defvar dc-edit-no-untabify-modes '(makefile-mode BSDmakefile salted-file-mode)
  "List of modes for which untabify is not desired on save.")

(defun dc-edit/untabify-maybe ()
  "Untabify current buffer unless major-mode is in `dc-edit-no-untabify-modes'."
  (interactive)
  (when (and (not indent-tabs-mode)
             (or (not dc-edit-no-untabify-modes)
                 (cl-every (lambda (m) (not (derived-mode-p m)))
                           dc-edit-no-untabify-modes)))
    (untabify (point-min) (point-max))))

(defun dc-edit/untabify-unconditionally ()
  "Untabify the current buffer completely and unconditionally."
  (interactive)
  (untabify (point-min) (point-max)))

(defun dc-edit/revert-buffer-unconditionally ()
  "Revert the current buffer unconditionally."
  (interactive)
  (revert-buffer t t))

(defun dc-edit/toggle-truncation ()
  "Toggle the state of `truncate-lines'."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (redraw-display))

;;; Base64

(defvar dc-edit-base64-prog
  (if (eq system-type 'windows-nt)
      "c:/dev/dotnet/base64.exe"
    "openssl base64 < ")
  "Command to generate base64 encoding for a given file, emitting to stdout.")

(defun dc-edit/base64-encode-file (filename)
  "Return base64 encoding of FILENAME."
  (let ((command (concat dc-edit-base64-prog " " filename)))
    (shell-command-to-string command)))

(defun dc-edit/base64-insert-file (filename)
  "Insert the base64 encoding of FILENAME at point."
  (interactive "*fFile? ")
  (save-excursion
    (let* ((beg (point))
           (fname-quoted (concat "\"" filename "\""))
           (b64 (replace-regexp-in-string (char-to-string 13) ""
                                          (dc-edit/base64-encode-file fname-quoted))))
      (forward-char 1)
      (forward-char -1)
      (insert b64)
      (kill-region beg (point))
      (yank)
      (exchange-point-and-mark))))

;;; UUIDs

(defconst dc-edit-uuid-regex "[a-fA-F0-9]\\{8\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{4\\}-[a-fA-F0-9]\\{12\\}"
  "Regexp for UUID.")

(defun dc-edit/maybe-delete-uuid-looking-forward ()
  "If point is looking forward at a UUID, delete it and return t."
  (interactive)
  (save-match-data
    (when (looking-at dc-edit-uuid-regex)
      (delete-region (match-beginning 0) (match-end 0))
      t)))

(defconst dc-edit--punctuation-regex "[\\!\\?\\\"\\.'#$%&*+/;<=>@^`|~]"
  "Regexp for punctuation.")

(defun dc-edit/maybe-delete-uuid-under-point ()
  "If point is on a UUID string, delete it and return t."
  (interactive)
  (save-excursion
    (save-match-data
      (or (dc-edit/maybe-delete-uuid-looking-forward)
          (and (re-search-backward dc-edit--punctuation-regex (line-beginning-position) t)
               (progn (forward-char)
                      (dc-edit/maybe-delete-uuid-looking-forward)))
          (and (re-search-backward "[[:space:]]" (line-beginning-position) t)
               (progn (forward-char)
                      (dc-edit/maybe-delete-uuid-looking-forward)))
          (progn
            (beginning-of-line)
            (dc-edit/maybe-delete-uuid-looking-forward))))))

(defvar dc-edit-uuidgen-prog
  (if (eq system-type 'windows-nt)
      "pwsh.exe -command \"& {[guid]::NewGuid().ToString()}\""
    "/usr/bin/uuidgen")
  "Program to generate one UUID and emit it to stdout.")

(defun dc-edit/uuid-gen ()
  "Generate a new UUID string and return it."
  (let ((uuid (shell-command-to-string dc-edit-uuidgen-prog)))
    (substring uuid 0 -1)))

(defun dc-edit/insert-uuid ()
  "Insert a new UUID at point, replacing any existing UUID under point."
  (interactive)
  (let ((uuid (dc-edit/uuid-gen)))
    (kill-new uuid)
    (dc-edit/maybe-delete-uuid-under-point)
    (insert uuid)))

(defun dc-edit/csharp-snippet ()
  "Narrow to enclosing curly brace block."
  (interactive)
  (save-excursion
    (re-search-backward "{")
    (forward-char 1)
    (set-mark (point))
    (re-search-forward "}")
    (forward-char -1)
    (narrow-to-region (point) (mark))))

(defun dc-edit/file-contents-as-string (filename)
  "Get the contents of FILENAME as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun dc-edit/log (label text &rest args)
  "Log message control string TEXT with ARGS prefixed by LABEL."
  (let ((msg (apply #'format text args)))
    (message "%s %s %s" label (format-time-string "%H:%M:%S") msg)))

(defun dc-edit/urlencode-region (start end)
  "URL-encode the string in region from START to END."
  (interactive "r")
  (save-excursion
    (let ((encoded (url-hexify-string (buffer-substring-no-properties start end))))
      (delete-region start end)
      (goto-char start)
      (insert encoded))))

(defun dc-edit/urldecode-region (start end)
  "URL-decode the string in region from START to END."
  (interactive "r")
  (save-excursion
    (let ((decoded (url-unhex-string (buffer-substring-no-properties start end))))
      (delete-region start end)
      (goto-char start)
      (insert decoded))))

(defun dc-edit/sum-column (start end)
  "Sum a column of numbers in rectangular region between START and END."
  (interactive "r")
  (copy-rectangle-to-register 9 start end)
  (set-buffer (get-buffer-create "*calc-sum*"))
  (erase-buffer)
  (insert-register 9)
  (let ((sum 0))
    (while (re-search-forward "[0-9]*\\.?[0-9]+" nil t)
      (setq sum (+ sum (string-to-number (match-string 0)))))
    (if (fboundp 'paste-to-osx)
        (paste-to-osx (format "%f" sum)))
    (message "Sum: %f" sum)))

(defun dc-edit/insert-as-self (char n)
  "Insert CHAR N times simulating `self-insert-command'."
  (let ((last-command-event char)
        (repeat (or n 1)))
    (self-insert-command repeat)))

(defun dc-edit/insert-paired-quotes (arg)
  "Skip the char if typeover of matching ending quote; otherwise insert pair."
  (interactive "*p")
  (let ((char last-command-event))
    (if (and (or (eq char ?\") (eq char ?\'))
             (eq char (following-char)))
        (forward-char)
      (self-insert-command (prefix-numeric-value arg))
      (self-insert-command (prefix-numeric-value arg))
      (forward-char -1))))

(defvar dc-edit-skeleton-pair-alist
  '((?\) . ?\()
    (?\> . ?\<)
    (?\] . ?\[)
    (?\" . ?\"))
  "Alist of close/open brackets for typeover handling.")

(defun dc-edit/skeleton-pair-end (arg)
  "Skip the char if it is an ending bracket, otherwise insert it."
  (interactive "*p")
  (let ((char last-command-event))
    (if (and (assq char dc-edit-skeleton-pair-alist)
             (eq char (following-char)))
        (forward-char)
      (self-insert-command (prefix-numeric-value arg)))))

(defun dc-edit/escape-braces-in-regex (str)
  "Replace { with \\{ and } with \\} in STR."
  (replace-regexp-in-string "\\(\u007b\\|\u007d\\)" "\\\\\\1" str nil nil))

;;; Casing & Word Sorting

(defun dc-edit/camel-to-snakecase-word-at-point ()
  "Convert camelCase to snake_case for the word at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (start (car bounds))
           (end (cdr bounds)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil start end)
      (goto-char (1- end))
      (downcase-region start (cdr (bounds-of-thing-at-point 'word))))))

(defun dc-edit/snake-to-camelcase-word-at-point ()
  "Convert snake_case to camelCase for the word at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (start (car bounds))
           (end (cdr bounds)))
      (goto-char start)
      (let ((case-fold-search nil))
        (while (re-search-forward "_\\([a-z]\\)" end t)
          (replace-match (upcase (match-string 1)) t t))))))

(defun dc-edit/sort-words-in-region (begin end &optional case-insensitive)
  "Sort words in region between BEGIN and END.
With prefix argument CASE-INSENSITIVE (C-u), sort is case-insensitive."
  (interactive "r\nP")
  (let* ((case-fold (if case-insensitive t nil))
         (region-string (buffer-substring-no-properties begin end))
         (words (split-string region-string "[[:space:]]+" t))
         (sorted-words (if case-fold
                           (sort words #'string-lessp)
                         (sort words #'string<)))
         (new-string (string-join sorted-words " ")))
    (delete-region begin end)
    (insert new-string)))

(defun dc-edit/indent-line-to-current-column ()
  "Indent the current line so its first non-whitespace character begins at point's column."
  (interactive)
  (indent-line-to (current-column)))

(defun dc-edit/shell-command-on-buffer (command)
  "Run COMMAND on entire buffer, replacing buffer contents."
  (save-excursion
    (let ((output-goes-to-current-buffer t)
          (output-replaces-current-content t))
      (shell-command-on-region (point-min)
                               (point-max)
                               command
                               output-goes-to-current-buffer
                               output-replaces-current-content))))

(defun dc-edit/shfmt-buffer ()
  "Run shfmt on current buffer."
  (interactive)
  (let ((shfmt-cmd "~/go/bin/shfmt"))
    (when (file-exists-p shfmt-cmd)
      (let ((orig-point (point))
            (command
             (mapconcat 'identity
                        `(,shfmt-cmd
                          "--filename" ,(buffer-file-name) "-ln"
                          ,(cl-case (bound-and-true-p sh-shell)
                             (sh "posix")
                             (bash "bash")
                             (t "bash"))
                          "-i"
                          ,(number-to-string (if (boundp 'sh-basic-offset) sh-basic-offset 4))
                          "-")
                        " ")))
        (message "shfmt-buffer command: %s" command)
        (dc-edit/shell-command-on-buffer command)
        (goto-char orig-point)))))

(defun dc-edit/nclfmt-buffer ()
  "Run nclfmt on the current buffer."
  (interactive)
  (if-let* ((fname (buffer-file-name))
            (_ (or (s-ends-with? ".ncl" fname)
                   (s-ends-with? ".blueprint" fname))))
      (if-let* ((nclfmt-cmd (executable-find "nclfmt"))
                (_ (file-exists-p nclfmt-cmd)))
          (let* ((command (format "%s --in_place %s" nclfmt-cmd fname))
                 (saved-point (point)))
            (message "nclfmt-buffer command: %s" command)
            (dc-edit/shell-command-on-buffer command)
            (goto-char saved-point))
        (message "Cannot find nclfmt"))
    (message "Not a .ncl file")))

(defun dc-edit/gclfmt-buffer ()
  "Run gclfmt on the current buffer."
  (interactive)
  (if-let* ((fname (buffer-file-name))
            (_ (s-ends-with? ".pp" fname)))
      (if-let* ((gclfmt-cmd (executable-find "gclfmt"))
                (_ (file-exists-p gclfmt-cmd)))
          (let* ((command (format "%s -w %s" gclfmt-cmd fname))
                 (saved-point (point)))
            (message "gclfmt-buffer command: %s" command)
            (dc-edit/shell-command-on-buffer command)
            (goto-char saved-point))
        (message "Cannot find gclfmt"))
    (message "Not a .pp file")))

(defun dc-edit/generate-random-string (len)
  "Return a random string of length LEN."
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~")
         (l (length chars))
         (res ""))
    (dotimes (_ len res)
      (setq res (concat res (char-to-string (elt chars (random l))))))))

;;; Backward compatibility aliases for existing templates/callers

(provide 'dc-edit)

;;; dc-edit.el ends here