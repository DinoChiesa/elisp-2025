;;; -*- coding: utf-8; lexical-binding: t;  -*-

;;; dino-dired-fixups.el --- fixups for dired mode
;;
;; Author: Dino Chiesa
;; Created: Sat, 31 Mar 2012  10:31
;; Package-Requires: ()
;; Version: 2025.05.26
;; URL: https://github.com/DinoChiesa/dpchiesa-elisp/blob/master/dino-dired-fixups.el
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
;;   (require 'dino-dired-fixups)
;;   ;; eliminate the gid in dired when using ls-lisp (eg, on windows)
;;   (setq ls-lisp-verbosity '(links uid)))


(require 'dired)
(require 'dired-aux)
(require 'ls-lisp)
(require 's)


(defvar dino-dired-time-format-switch "--time-style=long-iso"
  "Switch to use for time format. Always prefixed after rotating the sort switches.")

;; (defun ls-lisp-format-time (file-attr time-index now)
;;   "################")

(defun dino-dired-available-switches ()
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

(defvar dino-dired-sort-switches-to-cycle (dino-dired-available-switches) ;; eg '("t" "U" "S" "")
  "The one-character switches to cycle through for dired with `dired-sort-toggle'.
The toggle scheme: cycle through these switches:  -t  -U -S, and (maybe) -X,
to sort by:
  -t = last mod time
  -U = time of file creation
  -S = size
  -X = extension (not always available)
  default = name")

(setf (symbol-function 'dino-dired-available-switches) nil)

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


(defun dino-dired-next-sorting-switch (old)
  "returns the next sorting switch, a one-character string, after OLD"
  (let ((found (member old dino-dired-sort-switches-to-cycle)))
    (or (and found
             (or (cadr found) "t"))
        "t")))


(defun dino-dired-sort-cycle (&optional arg)
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
           ((and arg (member arg dino-dired-sort-switches-to-cycle))
            (dino-dired-generate-new-sorting-switch-string arg))

           ;; invalid arg; just sort by name
           (arg
            (dino-dired-generate-new-sorting-switch-string ""))

           ;; Rotate among the available sort switches.
           (t
            ;; Strip off the time-format switch, which this logic forces to always
            ;; be prefixed to any dired switches.
            (let ((switches-without-time-format
                   (s-trim
                    (if (not (s-blank? dino-dired-time-format-switch))
                        (s-chop-prefix dino-dired-time-format-switch dired-actual-switches)
                      dired-actual-switches))))

              ;; The following will rotate the sorting switches, and then re-apply the time
              ;; format switch if it is available.
              (concat dino-dired-time-format-switch " "
                      ;; There are two cases. (a) The existing switches have a space, (b) the existing
                      ;; switches do not use a space.
                      (cond
                       ;; This case handles the situation in which the options to ls are not concatted.
                       ((string-match " " switches-without-time-format)
                        (let ((n 0)
                              (L (1- (length dino-dired-sort-switches-to-cycle)))
                              result)
                          (while (and (< n L) (not result))
                            (let ((cur-switch (nth n dino-dired-sort-switches-to-cycle)))

                              (if (and (not (string= cur-switch ""))
                                       (string-match (concat " -" cur-switch "\\'") switches-without-time-format))
                                  (setq result
                                        (concat
                                         (substring switches-without-time-format 0 (match-beginning 0))
                                         " -" (dino-dired-next-sorting-switch cur-switch))))))
                          (or result
                              (concat switches-without-time-format " -t"))))

                       ;; the options to ls are collapsed, no intervening spaces
                       (t
                        (let* ((switches-regex
                                (concat "[" (mapconcat 'identity dino-dired-sort-switches-to-cycle "") "]"))
                               (old-sorting-switch
                                (if (string-match switches-regex switches-without-time-format)
                                    (substring switches-without-time-format (match-beginning 0) (match-end 0))
                                  "ZZ")))
                          (dino-dired-generate-new-sorting-switch-string
                           (dino-dired-next-sorting-switch old-sorting-switch)
                           switches-without-time-format))))))))))

  (dino-dired-sort-set-modeline (s-trim
                                 (s-chop-prefix dino-dired-time-format-switch dired-actual-switches)))
  (revert-buffer))


(defun dino-dired-generate-new-sorting-switch-string (new &optional old)
  (concat
   "-l"
   ;; strip -l and any other sorting switches
   (replace-regexp-in-string
    (concat "[-l" (mapconcat 'identity dino-dired-sort-switches-to-cycle "") "]")
    ""
    (or old ""))
   new))


(defun dino-dired-sort-set-modeline (switches)
  "This is cribbed from the dired-sort-set-modeline fn in `dired.el'.
This one supports the new search modes defined in the new
`dino-dired-sort-cycle'.
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
(defun dino-dired-do-find (&optional arg)
  "Visit each of the marked files, or the file under the point, or when
prefix arg, the next N files "
  (interactive "P")
  (let* ((fn-list
          (dired-get-marked-files nil arg)))
    (mapc 'find-file fn-list)))

(defun mode-for-buffer (&optional buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer (or buffer-or-string (current-buffer))
    major-mode))

(defun dino-dired-copy-or-move-other-window (operation)
  "Copy or move the marked files to another directory.
OPERATION is a symbol, either `COPY' or `MOVE' .
This works with files or directories."
  (unless (eq major-mode 'dired-mode)
    (error "works only when current-buffer is in dired-mode"))
  (let ((other-visible-dired-buffers
         (delq nil (mapcar #'(lambda (w)
                               (let* ((b (window-buffer w))
                                      (m (mode-for-buffer b)))
                                (and (eq m 'dired-mode)
                                 (not (eq b (current-buffer)))
                                 b)))
                           (window-list)))))

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


(defun dino-dired-move-file-to-dir-in-other-window (&optional arg)
  "If there are two or more windows, and the current one is in
dired-mode, and one of the others is also dired-mode, then move
the file under cursor or the marked files to the directory shown
in the other dired window. If the current buffer is not in
dired-mode, or if not exactly 2 windows show dired, then message
and quit."
  (interactive "P")
  (dino-dired-copy-or-move-other-window 'MOVE))

(defun dino-dired-copy-file-to-dir-in-other-window (&optional arg)
  "If there are two or more windows, and the current one is in
dired-mode, and one of the others is also dired-mode, then copy
the file under cursor or the marked files to the directory shown
in the other dired window. If the current buffer is not in
dired-mode, or if not exactly 2 windows show dired, then message
and quit."
  (interactive "P")
  (dino-dired-copy-or-move-other-window 'COPY))


;; Auto-update dired buffers with a timer.

(defvar dired-file-modification-hash (make-hash-table :test 'equal))

(defun maybe-revert-dired-buffers ()
  (walk-windows
   #'(lambda (win)
       (with-selected-window win
        (when (eq major-mode 'dired-mode)
         (if (file-attributes default-directory)
             ;; there are attributes, the dir exists
             (let ((mod (gethash default-directory dired-file-modification-hash)))
              (unless (and mod
                       (equal mod (nth 5 (file-attributes
                                          default-directory))))
               (setq mod (nth 5 (file-attributes default-directory)))
               (puthash default-directory mod dired-file-modification-hash)
               (dired-revert)))
           ;; else, the dired buffer points to a dir that no longer exists
           (let ((zombie-buffer (window-buffer win)))
            (kill-buffer zombie-buffer))))))
   ;;(condition-case nil (delete-window win) (error nil)))))))
   'no-mini 'all-frames))

(run-with-idle-timer 1 t 'maybe-revert-dired-buffers)


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


(defun dino-dired-kill-new-file-contents (&optional arg)
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


(setq dired-listing-switches "-la")

(provide 'dino-dired-fixups)

;;; dino-dired-fixups.el ends here
