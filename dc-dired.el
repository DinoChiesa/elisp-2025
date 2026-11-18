;;; -*- coding: utf-8; lexical-binding: t;  -*-

;;; dc-dired.el --- fixups for dired mode
;;
;; Author: Dino Chiesa
;; Created: Sat, 31 Mar 2012  10:31
;; Package-Requires: ()
;; Version: 2025.05.26
;; URL: https://github.com/DinoChiesa/dpchiesa-elisp/blob/master/dc-dired.el
;; License: Public Domain
;; Keywords: dired

;;; Commentary:

;; This module extends the basic dired to do sorting on extension and
;; size, in addition to name and timestamp. Use the s key to cycle through
;; sort modes.

;; To use it, place something like this in your .emacs file:
;;
;; (use-package dired
;;   :ensure nil
;;   :config
;;   (require 'dc-dired)
;;   ;; eliminate the gid in dired when using ls-lisp (eg, on windows)
;;   (setq ls-lisp-verbosity '(links uid)))


(require 'autorevert)
(require 'dired)
(require 'dired-aux)
(require 'ls-lisp)
(require 's)


(defvar dc-dired-time-format-switch "--time-style=long-iso"
  "Switch to use for time format. Always prefixed after rotating the sort switches.")

;; (defun ls-lisp-format-time (file-attr time-index now)
;;   "################")

(defun dc-dired-available-switches ()
  ;; On MacOS, the builtin ls program does not do the -X option. (lame)
  ;; The MacPorts or brew versions of GNU ls does. If it exists, use it.
  ;; the -X is used by dired-fixups for sorting by extension.
  (cond
   ((eq system-type 'darwin)
    (let ((candidate-dirs (list "/opt/local/bin" "/usr/local/opt/coreutils/bin"))
          (switches-to-use '("t" "U" "S" "")))
      (while candidate-dirs
        (let ((candidate (concat (file-name-as-directory (car candidate-dirs)) "gls")))
          (if (file-exists-p candidate)

              (setq ls-lisp-use-insert-directory-program t ;; unsure I need this
                    switches-to-use '("t" "U" "S" "X" "")
                    insert-directory-program candidate)))
        (setq candidate-dirs (cdr candidate-dirs)))
      switches-to-use))
   ((eq system-type 'gnu/linux)
    '("t" "U" "S" "X" ""))
   (t
    '("t" "U" "S" ""))))

(defvar dc-dired-sort-switches-to-cycle (dc-dired-available-switches) ;; eg '("t" "U" "S" "")
  "The one-character switches to cycle through for dired with `dired-sort-toggle'.
The toggle scheme: cycle through these switches:  -t  -U -S, and (maybe) -X,
to sort by:
  -t = last mod time
  -U = time of file creation
  -S = size
  -X = extension (not always available)
  default = name")

(setf (symbol-function 'dc-dired-available-switches) nil)

(defun ls-lisp-format-file-size (f-size human-readable)
  "This is a redefinition of the function from `dired.el'. This
fixes the formatting of file sizes in dired mode, to support very
large files. Without this change, dired supports 8 digits max,
which is up to 10gb.  Some files are larger than that.
"
  (if (or (not human-readable) (< f-size 1024))
      (format (if (floatp f-size) " %11.0f" " %11d") f-size)
    (let (post-fixes)
      (cl-do ((f-size (/ f-size 1024.0) (/ f-size 1024.0))
              ;; kilo, mega, giga, tera, peta, exa
              (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
          ((< f-size 1024) (format " %10.0f%s"  f-size (car post-fixes)))))))


(defun dc-dired-next-sorting-switch (old)
  "returns the next sorting switch, a one-character string, after OLD"
  (let ((found (member old dc-dired-sort-switches-to-cycle)))
    (or (and found
             (or (cadr found) "t"))
        "t")))

(defun dc-dired/sort-cycle (&optional arg)
  "This is intended as a replacement for, or alternative to,
the `dired-sort-toggle' fn from dired.el. Normally, dired sorts on
either name or time, and you can swap between those two (eg, toggle)
with the s key. This function allows sorting on name, size, mod time,
create time, and (maybe) extension. Cycling works the same, with the s
key. So it's no longer toggling, but rotating.

With optional ARG, do not cycle. Instead just use that arg as the
new switch. It should be one of [tUXS] . X is only legal on a
platform with gls. Invoking with an optional arg can be useful
when called from a dired-mode hook fn, to set the default /
initial sort."
  (interactive)
  (setq dired-actual-switches
        (let (case-fold-search)
          (cond
           ;; set new switch to passed-in value
           ((and arg (member arg dc-dired-sort-switches-to-cycle))
            (dc-dired-generate-new-sorting-switch-string arg))

           ;; invalid arg; just sort by name
           (arg
            (dc-dired-generate-new-sorting-switch-string ""))

           ;; Rotate among the available sort switches.
           (t
            ;; Strip off the time-format switch. The logic in this module
            ;; currently forces it to always be suffixed to any dired switches.
            (let ((switches-without-time-format
                   (s-trim
                    (if (not (s-blank? dc-dired-time-format-switch))
                        (s-chop-prefix dc-dired-time-format-switch
                                       (s-chop-suffix dc-dired-time-format-switch dired-actual-switches))
                      dired-actual-switches))))

              ;; The following will rotate the sorting switches, and then re-apply the time
              ;; format switch if it is available.
              ;; There are two cases. (a) The existing switches have a space, (b) the existing
              ;; switches do not use a space.
              (cond
               ;; This case handles the situation in which the options to ls are not concatted.
               ((string-match " " switches-without-time-format)
                (let ((n 0)
                      (L (1- (length dc-dired-sort-switches-to-cycle)))
                      result)
                  (while (and (< n L) (not result))
                    (let ((cur-switch (nth n dc-dired-sort-switches-to-cycle)))
                      (if (and (not (string= cur-switch ""))
                               (string-match (concat " -" cur-switch "\\'") switches-without-time-format))
                          (let ((next-switch (dc-dired-next-sorting-switch cur-switch))
                                (prefix (substring switches-without-time-format 0 (match-beginning 0))))
                            (setq result
                                  (if (string= next-switch "")
                                      prefix
                                    (concat prefix " -" next-switch))))))
                    (setq n (1+ n)))
                  (or result
                      (concat switches-without-time-format " -t"))))

               ;; the options to ls are collapsed, no intervening spaces
               (t
                (let* ((switches-regex
                        (concat "[" (mapconcat 'identity dc-dired-sort-switches-to-cycle "") "]"))
                       (old-sorting-switch
                        (if (string-match switches-regex switches-without-time-format)
                            (substring switches-without-time-format (match-beginning 0) (match-end 0))
                          "ZZ")))
                  (dc-dired-generate-new-sorting-switch-string
                   (dc-dired-next-sorting-switch old-sorting-switch)
                   switches-without-time-format)))))))))

  (dc-dired-sort-set-modeline dired-actual-switches)
  (if (not (s-blank? dc-dired-time-format-switch))
      (setq dired-actual-switches
            ;; 20260228-1401 - On windows anyway (maybe because of ls-lisp?) if
            ;; the time-format switch appears before the other "-la?" switches,
            ;; then the 'a' is apparently ignored, which means the dot and
            ;; double-dot (current and parent dir) are not shown in dired. So
            ;; this logic appends the time-format switch.
            (concat dired-actual-switches " " dc-dired-time-format-switch)))

  (revert-buffer))


(defun dc-dired-generate-new-sorting-switch-string (new &optional old)
  (concat
   "-la"
   ;; strip -l and any other sorting switches
   (replace-regexp-in-string
    (concat "[-l" (mapconcat 'identity dc-dired-sort-switches-to-cycle "") "]")
    ""
    (or old ""))
   new))


(defun dc-dired-sort-set-modeline (switches)
  "This is cribbed from the dired-sort-set-modeline fn in `dired.el'.
This one supports the new search modes defined in the new
`dc-dired/sort-cycle'.
"
  ;; Set modeline display according to dired-actual-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (when (eq major-mode 'dired-mode)
    (setq mode-name
          (let (case-fold-search)
            (cond ((string-match "-[^t]*t[^t]*$" switches)
                   "Dired/mtime")
                  ((string-match "\\b-[^U]*U[^U]*$" switches)
                   "Dired/ctime")
                  ((string-match "\\b-[^X]*X[^X]*$" switches)
                   "Dired/ext")
                  ((string-match "\\b-[^S]*S[^S]*$" switches)
                   "Dired/sz")
                  ((string-match "\\b-[^SXUt]*$" switches)
                   "Dired/name")
                  (t
                   (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))


;; nifty utility function
(defun dc-dired/do-find (&optional arg)
  "Visit each of the marked files, or the file under the point, or when
prefix arg, the next N files "
  (interactive "P")
  (let* ((fn-list
          (dired-get-marked-files nil arg)))
    (mapc 'find-file fn-list)))

(defun dc-dired--mode-for-buffer (&optional buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer (or buffer-or-string (current-buffer))
    major-mode))

(defun dc-dired--other-visible-dired-buffers ()
  "Return a list of other visible `dired-mode' buffers in the current frame."
  (delq nil (mapcar (lambda (w)
                      (let ((b (window-buffer w)))
                        (and (not (eq b (current-buffer)))
                             (eq (dc-dired--mode-for-buffer b) 'dired-mode)
                             b)))
                    (window-list))))

(defun dc-dired--other-window-dired-dir (&optional predicate)
  "If there is a single other visible `dired-mode' buffer, return its `default-directory'
when PREDICATE (if non-nil) returns non-nil for that directory."
  (let ((others (dc-dired--other-visible-dired-buffers)))
    (when (= (length others) 1)
      (let ((dir (with-current-buffer (car others) default-directory)))
        (when (or (null predicate) (funcall predicate dir))
          dir)))))

(defun dc-dired-copy-or-move-other-window (operation)
  "Copy or move the marked files to another directory.
OPERATION is a symbol, either `COPY' or `MOVE' .
This works with files or directories."
  (unless (eq major-mode 'dired-mode)
    (error "works only when current-buffer is in dired-mode"))
  (let ((other-visible-dired-buffers (dc-dired--other-visible-dired-buffers)))

    (unless (= (length other-visible-dired-buffers) 1)
      (error "Can copy only if exactly 2 dired windows are visible"))

    (let ((fns (cond
                ((eq operation 'COPY)
                 '(copy-file copy-directory))
                ((eq operation 'MOVE)
                 '(rename-file rename-file))
                (t
                 (error "unsupported operation")
                 )))
          (dst-dir (expand-file-name (with-current-buffer (car other-visible-dired-buffers)
                                       default-directory))))
      (mapc #'(lambda (f)
                (let ((fn
                       (cond
                        ((file-directory-p f)
                         (cadr fns))
                        (t
                         (car fns)))))
                 (funcall fn f dst-dir 1)))
            (dired-get-marked-files nil))
      (with-current-buffer (car other-visible-dired-buffers)
        (revert-buffer))
      (revert-buffer))))


(defun dc-dired/move-file-to-dir-in-other-window (&optional arg)
  "If there are two or more windows, and the current one is in
dired-mode, and one of the others is also dired-mode, then move
the file under cursor or the marked files to the directory shown
in the other dired window. If the current buffer is not in
dired-mode, or if not exactly 2 windows show dired, then message
and quit."
  (interactive "P")
  (dc-dired-copy-or-move-other-window 'MOVE))

(defun dc-dired/copy-file-to-dir-in-other-window (&optional arg)
  "If there are two or more windows, and the current one is in
dired-mode, and one of the others is also dired-mode, then copy
the file under cursor or the marked files to the directory shown
in the other dired window. If the current buffer is not in
dired-mode, or if not exactly 2 windows show dired, then message
and quit."
  (interactive "P")
  (dc-dired-copy-or-move-other-window 'COPY))


;; 20260704-1538: Use built-in auto-revert-mode on visible buffers,
;; and refresh immediately when switching back to a Dired buffer.
(setq auto-revert-buffer-list-filter #'get-buffer-window)

(defun dc-dired--autorevert-on-switch ()
  "Immediately run `auto-revert-handler' when switching to a Dired buffer."
  (when (and (eq major-mode 'dired-mode)
             (bound-and-true-p auto-revert-mode))
    (auto-revert-handler)))

(add-hook 'buffer-list-update-hook #'dc-dired--autorevert-on-switch)

;; 2026-11-17 Superseded by built-in `auto-revert-mode' + `dc-dired--autorevert-on-switch' above.
;; ;; Auto-update dired buffers with a timer.
;;
;; (defvar dc-dired--file-modification-hash (make-hash-table :test 'equal))
;;
;; (defun dc-dired--maybe-revert-dired-buffers ()
;;   (walk-windows
;;    #'(lambda (win)
;;        (with-selected-window win
;;         (when (eq major-mode 'dired-mode)
;;          (if (file-attributes default-directory)
;;              ;; there are attributes, the dir exists
;;              (let ((mod (gethash default-directory dc-dired--file-modification-hash)))
;;               (unless (and mod
;;                        (equal mod (nth 5 (file-attributes
;;                                           default-directory))))
;;                (setq mod (nth 5 (file-attributes default-directory)))
;;                (puthash default-directory mod dc-dired--file-modification-hash)
;;                (dired-revert)))
;;            ;; else, the dired buffer points to a dir that no longer exists
;;            (let ((zombie-buffer (window-buffer win)))
;;             (kill-buffer zombie-buffer))))))
;;    ;;(condition-case nil (delete-window win) (error nil)))))))
;;    'no-mini 'all-frames))
;;
;; (run-with-idle-timer 1 t 'dc-dired--maybe-revert-dired-buffers)


;; When opening a file from dired, it will use the command guessed here.  A
;; function by this name was originally by dired-x, which I do not use, hence
;; this definition.
(defun dired-guess-shell-command (prompt files)
  "invoked by `dired-read-shell-command' to read the shell command
for a given file or set of files. This function makes an intelligent guess."
  (if (eq (length files) 1)

      (let* ((file (car files))
             (prompt (concat "! on " file " "))
             (ext (file-name-extension file))
             (initial
              (if (member ext '("png" "jpg" "gif"))
                  ;;(concat "open -a seashore " (car files))
                  ;; TODO: fix. This works for macos but not other platforms.
                  (concat "open -a preview " (car files))
                "")))
        (read-shell-command prompt initial))

    (let ((prompt (concat "! on ["
                          (mapconcat 'identity files " ")
                          "] ")))

      (read-shell-command prompt))))


(defun dc-dired/do-symlink (&optional arg)
  "Create a symlink or junction for marked files or file at point.
On Windows, creates a Junction if target is a directory, or a
SymbolicLink if target is a file using `pwsh` or `powershell`. On other
OSes, calls `dired-do-symlink`."
  (interactive "P")
  (if (eq system-type 'windows-nt)
      (let* ((pwsh (or (executable-find "pwsh")
                       (executable-find "powershell")
                       "powershell"))
             (files (dired-get-marked-files nil arg))
             (other-dired-dir (dc-dired--other-window-dired-dir))
             (target-dir (or other-dired-dir default-directory)))
        (dolist (file files)
          (let* ((file-is-dir (file-directory-p file))
                 (default-link-path (expand-file-name (file-name-nondirectory file) target-dir))
                 (prompt (format "Name for new %s pointing to %s: "
                                 (if file-is-dir "Junction" "SymbolicLink")
                                 (file-name-nondirectory file)))
                 (link-path (read-file-name prompt target-dir default-link-path nil nil #'file-directory-p))
                 (existing-file (expand-file-name file))
                 (abs-link (expand-file-name link-path))
                 (cmd (format
                       (if file-is-dir
                           "New-Item -ItemType Junction -Path '%s' -Value '%s'"
                         "New-Item -ItemType SymbolicLink -Path '%s' -Target '%s'")
                       abs-link existing-file)))
            (message "Running PowerShell: %s" cmd)
            (call-process pwsh nil nil nil "-NoProfile" "-Command" cmd)))
        (revert-buffer)
        (when other-dired-dir
          (mapc (lambda (w)
                  (with-selected-window w
                    (when (eq major-mode 'dired-mode)
                      (revert-buffer))))
                (window-list))))
    (dired-do-symlink arg)))

(defun dc-dired/copy-filename (&optional arg)
  "Copy filename or portion of filename into the kill ring.
If the buffer is writable (e.g. WDired), acts as standard `kill-line'.
If point is past the first character of the filename on the current line,
copies only the remainder of the filename from point.
Otherwise, copies the full basename (or marked files).
With prefix (C-u), copies the fully-qualified path."
  (interactive "P")
  (if (or (not buffer-read-only)
          (not (derived-mode-p 'dired-mode)))
      (call-interactively #'kill-line)
    (let* ((pt (point))
           (file-beg (save-excursion (dired-move-to-filename)))
           (file-end (save-excursion (when file-beg (dired-move-to-end-of-filename t)))))
      (cond
       ;; With prefix: copy full path (or marked files)
       (arg
        (let* ((paths (dired-get-marked-files nil))
               (str (mapconcat #'identity paths " ")))
          (kill-new str)
          (message "%s" str)))

       ;; Point is past the first character of the filename: copy remainder
       ((and file-beg file-end (> pt file-beg) (<= pt file-end))
        (let ((remainder (buffer-substring-no-properties pt file-end)))
          (kill-new remainder)
          (message "%s" remainder)))

       ;; Point is at or before the first character: copy full basename(s)
       (file-beg
        (let* ((paths (dired-get-marked-files nil))
               (basenames (mapcar #'file-name-nondirectory paths))
               (str (mapconcat #'identity basenames " ")))
          (kill-new str)
          (message "%s" str)))

       ;; Fallback on non-file lines (e.g. directory header line)
       (t
        (let ((dir (dired-current-directory)))
          (if dir
              (let ((name (file-name-nondirectory (directory-file-name dir))))
                (kill-new name)
                (message "%s" name))
            (message "No file on this line"))))))))

(defun dc-dired/kill-new-file-contents (&optional arg)
  "Copies the contents of the marked file into the kill-ring"
  (interactive "P")
  (let ((filename-list (dired-get-marked-files nil arg)))
    (mapc #'(lambda (f)
              (with-temp-buffer
                (insert-file-contents f)
                (kill-new
                 (buffer-substring-no-properties (point-min) (point-max)))))
          filename-list)))


;; This fn redefined here to change the doc string, only.
;; The current doc string is not helpful.
(defun dired-do-touch (&optional arg)
  "Change the timestamp of the marked (or next ARG) files.
This calls touch. With prefix, accepts timestamp in
[[CC]YY]MMDDhhmm[.SS] format.  When invoked interactively, you can pull
the current file timestamp of the file at point into the minibuffer, by
typing M-n ."
  (interactive "P")
  (dired-do-chxxx "Timestamp" dired-touch-program 'touch arg))

;; eliminate the error message:
;; "ls does not support --dired; see `dired-use-ls-dired' for more details."
(setq dired-use-ls-dired nil)


(setq dired-listing-switches (concat "-la " dc-dired-time-format-switch))
;; (setq dired-actual-switches "--time-style=long-iso -laS")


(defvar dc-dired-cloudtop-base "/scp:cloudtop:/usr/local/google/home/dchiesa/"
  "Base TRAMP path for Cloudtop. Uses the scp method so large files go out-of-band.")

(defvar dc-dired-cloudtop-history nil
  "History list of recent Cloudtop destination paths (max 5).")

(defvar dc-dired-local-base "~/Downloads/"
  "Default local destination when copying from a remote dired buffer.")

(defvar dc-dired-local-history nil
  "History list of recent local destination paths (max 5).")

(defun dc-dired--to-scp-arg (path)
  "Convert PATH (either a local path or a TRAMP path) into an argument for `scp'."
  (if (file-remote-p path)
      (let ((user (file-remote-p path 'user))
            (host (file-remote-p path 'host))
            (localname (file-remote-p path 'localname)))
        (format "%s%s:%s"
                (if user (concat user "@") "")
                host
                localname))
    (expand-file-name path)))

(defun dc-dired--run-scp (src dest-dir)
  "Run `scp -r -p' synchronously to copy SRC into DEST-DIR, signaling an error on failure."
  (let ((src-arg (dc-dired--to-scp-arg src))
        (dest-arg (dc-dired--to-scp-arg dest-dir))
        ;; `call-process' runs locally and fails if `default-directory' is a
        ;; remote TRAMP path or does not exist on the local machine.
        (default-directory (expand-file-name "~/")))
    (with-temp-buffer
      (let ((exit-code (call-process "scp" nil t nil "-r" "-p" src-arg dest-arg)))
        (unless (zerop exit-code)
          (error "scp failed (exit %d): %s" exit-code (string-trim (buffer-string))))))))

(defun dc-dired--copy-files-to (files prompt base history-var format-path)
  "Copy FILES to a directory read in the minibuffer with PROMPT.
If another visible window is a `dired-mode' buffer on the target side
\(local when copying from remote, or remote when copying from local\), its
directory is suggested as the default destination; otherwise the most recent
entry in HISTORY-VAR, else BASE.
The destination paths, transformed by FORMAT-PATH, go on the kill ring."
  (let* ((history (symbol-value history-var))
         (src-is-remote (file-remote-p default-directory))
         (other-dired-dir
          (dc-dired--other-window-dired-dir
           (lambda (dir)
             (if src-is-remote
                 (not (file-remote-p dir))
               (file-remote-p dir)))))
         (default-dest (or other-dired-dir (car history) base))
         (target-dir
          (let ((file-name-history history)
                (default-directory default-dest))
            (read-directory-name
             (format prompt (length files))
             ;;dc-dired-cloudtop-base
             default-dest
             default-dest
             t)))
         (expanded-target (file-name-as-directory (expand-file-name target-dir base)))
         copied-paths)

    ;; Cache destination: deduplicate and keep only the 5 most recent
    (set history-var
         (seq-take (cons expanded-target (delete expanded-target history)) 5))

    (dolist (file files)
      (let* ((filename (file-name-nondirectory file))
             (dest (expand-file-name filename expanded-target))
             (is-dir (file-directory-p file))
             (confirm-prompt (if is-dir
                                 (format "%s exists; merge into it? " dest)
                               (format "File %s already exists; overwrite? " dest))))
        (when (or (not (file-exists-p dest))
                  (y-or-n-p confirm-prompt))
          (message "Copying %s to %s..." filename expanded-target)
          (dc-dired--run-scp file expanded-target)
          (message "Copying %s to %s...done" filename expanded-target)
          (push (funcall format-path dest) copied-paths))))

    ;; Copy to kill-ring and system clipboard
    (when copied-paths
      (kill-new (string-join (nreverse copied-paths) "\n")))))

(defun dc-dired/scp-other-side ()
  "Copy the current file (or marked files) to the other side.
From a local dired buffer, copy to a directory on Cloudtop.
From a remote (TRAMP) dired buffer, copy to a local directory."
  (interactive)
  (let ((files (dired-get-marked-files nil current-prefix-arg)))
    (if (file-remote-p default-directory)
        (dc-dired--copy-files-to
         files "Copy %d file(s) to local: "
         dc-dired-local-base 'dc-dired-local-history
         #'abbreviate-file-name)
      (dc-dired--copy-files-to
       files "SCP %d file(s) to Cloudtop: "
       dc-dired-cloudtop-base 'dc-dired-cloudtop-history
       ;; Strip "/scp:cloudtop:/usr/local/google/home/dchiesa/" and prepend "~/"
       (lambda (dest)
         (concat "~/" (file-relative-name dest dc-dired-cloudtop-base)))))))

(provide 'dc-dired)

;;; dc-dired.el ends here
