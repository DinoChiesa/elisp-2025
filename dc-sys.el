;;; dc-sys.el --- System, Dired, Package Management, and Calendar utilities   -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Dino Chiesa
;; Created: Wed, 17 Jul 2013
;; Package-Requires: ((emacs "29.1"))
;; Keywords: sys utility

;;; Commentary:

;; System management, dired enhancements, package cleanup, window title, and calendar display.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'ansi-color)
(require 'package nil t)
(require 'dired nil t)

(defvar view-mode-map)
(declare-function dired-move-to-filename "dired" (&optional raise-error eol))
(declare-function dired-move-to-end-of-filename "dired" (&optional no-error))
(declare-function dired-get-marked-files "dired" (&optional localp arg filter dist error))
(declare-function dired-current-directory "dired" (&optional localp))
(declare-function package--removable-packages "package" ())

(defun dc-sys/toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
This works only when the frame is split into exactly two windows."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can toggle only if the frame is split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil)))

(defun dc-sys/resize-big ()
  "Quick resize to 128x68."
  (interactive)
  (set-frame-height (selected-frame) 68)
  (set-frame-width (selected-frame) 128))

(defun dc-sys/dired-copy-filename (&optional arg)
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

(defun dc-sys/fetch-latest (path &optional match)
  "Return the latest file in directory PATH, optionally matching regex MATCH."
  (let ((entries (directory-files-and-attributes path nil match t))
        (dc-mtime (lambda (entry) (nth 5 (cdr entry)))))
    (concat
     (file-name-as-directory path)
     (car
      (car (sort entries (lambda (a b)
                           (not (time-less-p (funcall dc-mtime a)
                                             (funcall dc-mtime b))))))))))

(defun dc-sys/filter-list (condp lst)
  "Filter LST keeping elements that satisfy CONDP."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun dc-sys/is-directory (dir-name)
  "Test whether DIR-NAME refers to an existing directory."
  (and (file-exists-p dir-name)
       (let ((attrs (file-attributes dir-name)))
         (and (car attrs)
              (not (stringp (car attrs)))))))

(defvar dc-sys-move-timer-list nil
  "List of timers for `dc-sys/move-whenever'.")

(defun dc-sys/get-unique-filename (fname dir)
  "Return a unique filename for FNAME in directory DIR by appending -N."
  (let ((testname (concat (file-name-as-directory dir) fname)))
    (if (file-exists-p testname)
        (let ((parts (split-string fname "\\."))
              (continue t))
          (let ((basename (mapconcat #'identity (reverse (cdr (reverse parts))) "."))
                (extension (if (> (length parts) 1)
                               (car (reverse parts))
                             ""))
                (ix 1))
            (while continue
              (setq testname
                    (concat (file-name-as-directory dir) basename
                            "-" (format "%d" ix) "." extension))
              (setq continue (file-exists-p testname)
                    ix (1+ ix))))))
    testname))

(defun dc-sys/check-files-and-move (src targ)
  "Check SRC directory and move any files there to TARG directory with unique names."
  (unless (dc-sys/is-directory src)
    (error "%s is not a directory" src))
  (unless (dc-sys/is-directory targ)
    (error "%s is not a directory" targ))
  (let ((flist (directory-files-and-attributes src))
        moved)
    (while flist
      (let ((one-file (car flist)))
        (unless (car (cdr one-file))
          (let ((unique-name (dc-sys/get-unique-filename (car one-file) targ)))
            (rename-file (concat (file-name-as-directory src) (car one-file))
                         unique-name)
            (push unique-name moved))))
      (setq flist (cdr flist)))
    moved))

(defun dc-sys/move-whenever (src targ)
  "Watch SRC directory and periodically move files to TARG."
  (unless (dc-sys/is-directory src)
    (error "%s is not a directory" src))
  (unless (dc-sys/is-directory targ)
    (error "%s is not a directory" targ))
  (let ((timer (run-with-timer 0 75 #'dc-sys/check-files-and-move src targ)))
    (push (list src targ timer) dc-sys-move-timer-list)))

(defun dc-sys/rename-file-and-buffer (new-name)
  "Rename both current buffer and file it visits to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (cond
     ((not filename)
      (message "Buffer '%s' is not visiting a file!" name))
     ((get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name))
     (t
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)))))

(defun dc-sys/find-file-with-line-number (original-fn &rest args)
  "Advice for `find-file'. Parses filenames like file.js:14:10."
  (save-match-data
    (let* ((path (nth 0 args))
           (match (and path (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\)$" path)))
           (line-no (and match
                         (match-string 2 path)
                         (string-to-number (match-string 2 path))))
           (col-no (and match
                        (match-string 3 path)
                        (string-to-number (match-string 3 path))))
           (new-path (and match (match-string 1 path))))
      (when line-no
        (pop args)
        (push new-path args))
      (let ((result (apply original-fn args)))
        (when line-no
          (goto-char (point-min))
          (forward-line (1- line-no))
          (when (> col-no 0)
            (forward-char (1- col-no))))
        result))))

(defun dc-sys/setup-find-file-advice ()
  "Enable line-number parsing advice on `find-file'."
  (advice-add 'find-file :around #'dc-sys/find-file-with-line-number))

(defun dc-sys/jshintrc-find ()
  "Find a .jshintrc file in the current working directory or upward."
  (interactive)
  (locate-dominating-file default-directory ".jshintrc"))

(defun dc-sys/jshintrc-get-or-create ()
  "Create a .jshintrc file in the current working directory if one does not exist."
  (interactive)
  (unless (file-exists-p ".jshintrc")
    (with-temp-file ".jshintrc"
      (insert "{\n  \"node\": true,\n  \"browser\": true,\n  \"esversion\": 6,\n  \"esnext\": true,\n  \"bitwise\": true,\n  \"camelcase\": true,\n  \"curly\": true,\n  \"eqeqeq\": true,\n  \"immed\": true,\n  \"indent\": 2,\n  \"latedef\": true,\n  \"newcap\": true,\n  \"noarg\": true,\n  \"quotmark\": \"single\",\n  \"regexp\": true,\n  \"undef\": true,\n  \"unused\": true,\n  \"strict\": true,\n  \"trailing\": true,\n  \"smarttabs\": true,\n  \"predef\": [\n    \"Promise\"\n  ]\n}\n"))))

(defun dc-sys/read-globals-from-jshintrc ()
  "Read global identifiers from nearest .jshintrc file."
  (when-let* ((jshintrc (dc-sys/jshintrc-find))
              (json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'string)
              (json (json-read-file jshintrc))
              (globals (assoc "globals" json)))
    (mapcar #'car (cdr globals))))

(defun dc-sys/gitignore ()
  "Create a standard .gitignore file in the current directory if absent."
  (interactive)
  (unless (file-exists-p ".gitignore")
    (with-temp-file ".gitignore"
      (insert "**/#*.*#\n**/*.*~\n**/#*#\n**/*~\n\n**/target/\n**/*.class\n\n**/node_modules/\n\n.env\n\n.venv/\n\n__pycache__/\n*.pyc\n*.pyo\n*.pyd\n\nNOT-USED/\n"))))

(defun dc-sys/show-packages-considered-removable ()
  "Display packages that `package' considers removable."
  (interactive)
  (if-let* ((removable (package--removable-packages)))
      (message "Packages to delete: %d (%s)"
               (length removable)
               (mapconcat #'symbol-name removable " "))
    (message "No packages are 'removable'")))

(defun dc-sys/cleanup-stale-elpa-packages (&optional target-dir dry-run)
  "Remove older versions of timestamped package directories in TARGET-DIR.
By default operates on `package-user-dir`."
  (interactive
   (if current-prefix-arg
       (list (read-directory-name "Select directory: " package-user-dir)
             (y-or-n-p "Perform a dry-run? "))
     (list package-user-dir nil)))
  (setq target-dir (or target-dir package-user-dir))
  (let ((dir-map (make-hash-table :test 'equal))
        (regex "\\(.+\\)-\\([0-9]+\\(?:\\.[0-9]+\\)*\\)$")
        (deleted-count 0)
        (removed-from-load-path nil))
    (if (not (file-exists-p target-dir))
        (message "Target directory does not exist: %s" target-dir)
      (dolist (file (directory-files target-dir nil regex t))
        (let* ((full-path (expand-file-name file target-dir))
               (module-name (progn (string-match regex file) (match-string 1 file)))
               (timestamp (match-string 2 file))
               (existing (gethash module-name dir-map)))
          (when (file-directory-p full-path)
            (if (or (null existing)
                    (version< (cdr existing) timestamp))
                (puthash module-name (cons file timestamp) dir-map)))))

      (let ((kept-files (let (acc)
                          (maphash (lambda (_k v) (push (car v) acc)) dir-map)
                          acc)))
        (dolist (file (directory-files target-dir nil regex t))
          (unless (member file kept-files)
            (let* ((full-path (expand-file-name file target-dir))
                   (norm-path (directory-file-name full-path)))
              (when (member norm-path load-path)
                (push file removed-from-load-path))
              (if dry-run
                  (message "[Dry-run] Would delete: %s" file)
                (delete-directory full-path t)
                (setq deleted-count (1+ deleted-count)))))))

      (cond
       (dry-run
        (message "Dry-run complete. No files were actually deleted."))
       ((> deleted-count 0)
        (message "Cleanup complete: Removed %d stale directories." deleted-count)
        (when removed-from-load-path
          (warn "ACTIVE PATHS REMOVED: The following were in your load-path: %s. You should restart Emacs."
                (mapconcat #'identity removed-from-load-path ", "))))
       (t
        (when (called-interactively-p 'interactive)
          (message "No stale packages found. Everything is up to date.")))))))

(defun dc-sys/display-full-year-calendar ()
  "Run python script to generate full year calendar and display with ANSI colors."
  (interactive)
  (let* ((buf-name "*Yearly Calendar*")
         (home-dir (replace-regexp-in-string "\\\\" "/" (getenv "HOME")))
         (script-path (concat home-dir "/bin/cal"))
         (python (or (executable-find "python") (executable-find "python3")))
         (calendar-output (if (and python (file-exists-p script-path))
                              (with-temp-buffer
                                (call-process python nil t nil script-path "-6")
                                (buffer-string))
                            "")))
    (when (string-empty-p calendar-output)
      (user-error "Cannot find calendar script at %s" script-path))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n\n")
        (insert (replace-regexp-in-string "^" " " calendar-output))
        (let ((ansi-color-apply-face-function
               (lambda (beg end face)
                 (letrec ((fix-face
                           (lambda (f)
                             (cond
                              ((equal f "blue1") "light sky blue")
                              ((listp f) (mapcar fix-face f))
                              (t f)))))
                   (put-text-property beg end 'face (funcall fix-face face))))))
          (ansi-color-apply-on-region (point-min) (point-max)))
        (setq-local show-trailing-whitespace nil)
        (setq buffer-read-only t)
        (view-mode 1)
        (local-set-key (kbd "g") #'dc-sys/display-full-year-calendar)
        (setq-local minor-mode-overriding-map-alist
                    `((view-mode . ,(let ((map (make-sparse-keymap)))
                                      (set-keymap-parent map view-mode-map)
                                      (define-key map (kbd "g") #'dc-sys/display-full-year-calendar)
                                      map))))
        (display-line-numbers-mode -1)
        (setq-local cursor-type nil)
        (setq-local left-margin-width 2)
        (setq-local right-margin-width 2)
        (set-window-margins (selected-window) 4 4)))
    (unless (get-buffer-window buf-name)
      (switch-to-buffer buf-name))))

(defun dc-sys/setup-calendar-advice ()
  "Override `calendar' with `dc-sys/display-full-year-calendar'."
  (advice-add 'calendar :override #'dc-sys/display-full-year-calendar))

(defun dc-sys/update-os-window-title ()
  "Update terminal tab/window title to emacs: user@host via OSC sequences."
  (unless (display-graphic-p)
    (when-let* ((sysname-parts (string-split (system-name) "\\."))
                (sysname
                 (if (and (eq (length sysname-parts) 4)
                          (string= (nth 2 sysname-parts) "googlers"))
                     "cloudtop"
                   (system-name)))
                (title (format "emacs: %s@%s" (user-login-name) sysname)))
      (send-string-to-terminal (format "\033]0;%s\007" title)))))

(defun dc-sys/setup-window-title-hook ()
  "Attach `dc-sys/update-os-window-title' to window configuration changes."
  (add-hook 'window-configuration-change-hook #'dc-sys/update-os-window-title))

(provide 'dc-sys)

;;; dc-sys.el ends here