;;; dc-paths.el --- Path, environment, and external executable utilities  -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Dino Chiesa
;; Created: Wed, 17 Jul 2013
;; Version: 2026.10.24
;; Keywords: utility path environment
;; License: New BSD

;;; Commentary:
;;
;; Utilities for normalizing filesystem paths, configuring `exec-path' and
;; the PATH environment variable across Windows and POSIX systems, locating
;; NVM-managed Node.js binaries, and discovering external language servers.
;;

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)

(defun dc-paths/find-executable-in-paths (filename search-paths)
  "Find FILENAME in one of the given SEARCH-PATHS.
Return the fully-qualified path if found, or nil otherwise."
  (let ((found nil))
    (while (and search-paths (not found))
      (let* ((dir (expand-file-name (car search-paths)))
             (target (file-name-concat dir filename)))
        (if (file-executable-p target)
            (setq found target)
          (setq search-paths (cdr search-paths)))))
    found))

(defun dc-paths/add-path-if-not-present (pathlist)
  "Add each directory in the PATHLIST to the system path and to `exec-path'.
Path directories are added only if the path exists, and is not already present."
  (let ((path-elts (split-string (getenv "PATH") path-separator)))
    (dolist (path pathlist)
      (when (file-directory-p path)
        (unless (member path path-elts)
          (setenv "PATH" (concat (getenv "PATH") path-separator path)))
        (add-to-list 'exec-path path)))))

(defun dc-paths--parent-directory (dir)
  "Return parent directory of DIR, or nil if already at root."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun dc-paths--find-file-in-tree (current-dir fname)
  "Search for a file named FNAME upwards through directory hierarchy starting at CURRENT-DIR."
  (let ((file (concat current-dir fname))
        (parent (dc-paths--parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (dc-paths--find-file-in-tree parent fname)))))

(defun dc-paths/normalize-path (path for-exec-path)
  "Canonicalize a path for comparison, handling Windows specifics.
Ensures lowercase drive letter, consistent slashes, and no trailing slash."
  (let ((canonical-path (directory-file-name (file-truename path))))
    (if (eq system-type 'windows-nt)
        (let* ((p (dc-paths/maybe-convert-path-slashes canonical-path for-exec-path))
               (drive-match (string-match "^\\([a-zA-Z]\\):" p)))
          (if drive-match
              (concat (downcase (substring p (match-beginning 1) (match-end 1)))
                      (substring p (match-end 1)))
            p))
      canonical-path)))

(defun dc-paths/maybe-convert-path-slashes (path for-exec-path)
  "On Windows, convert a PATH to the preferred display format.
Uses forward slashes if FOR-EXEC-PATH is non-nil, or backslashes otherwise,
preserving original drive letter casing."
  (if (eq system-type 'windows-nt)
      (if for-exec-path
          (let* ((p (replace-regexp-in-string "\\\\" "/" path t t))
                 (p (replace-regexp-in-string "//+" "/" p t t)))
            p)
        (let* ((p (replace-regexp-in-string "/" "\\" path t t))
               (p (replace-regexp-in-string "\\\\+" "\\" p t t)))
          p))
    path))

(defun dc-paths/normalize-and-dedupe (path-list for-exec-path)
  "Normalize and deduplicate a list of paths, preserving order (first occurrence wins).
Filters out non-existent directories. Uses `dc-paths/normalize-path' for comparison."
  (let ((seen-normalized-paths (make-hash-table :test 'equal))
        (result-paths '())
        (reversed-input (reverse path-list)))
    (dolist (path reversed-input)
      (when (stringp path)
        (let* ((truename-path (file-truename path)))
          (when (file-directory-p truename-path)
            (let ((normalized-path (dc-paths/normalize-path truename-path for-exec-path)))
              (unless (gethash normalized-path seen-normalized-paths)
                (puthash normalized-path t seen-normalized-paths)
                (push normalized-path result-paths)))))))
    result-paths))

(defun dc-paths/reorder-list (list predicate)
  "Reorder LIST so that elements satisfying PREDICATE come first.
Returns a new list with elements reordered according to PREDICATE."
  (let (matches non-matches)
    (dolist (item list)
      (if (funcall predicate item)
          (push item matches)
        (push item non-matches)))
    (nconc (reverse matches) (reverse non-matches))))

(defun dc-paths--preferred-path-entry ()
  "Return a predicate that returns t if ENTRY is a preferred path on Windows.
Prefers the Git usr/bin directory and the ~/bin directory."
  (let* ((fixed-home (replace-regexp-in-string "\\\\" "/" (file-truename (getenv "HOME")) t t))
         (home-bin (concat fixed-home "/bin")))
    (lambda (entry)
      (or
       (string-match-p (regexp-quote "Git/usr/bin") entry)
       (string-match-p (regexp-quote home-bin) entry)))))

(defun dc-paths/maybe-reorder-exec-path-entries ()
  "Reorder `exec-path' entries to ensure preferred directories take precedence on Windows."
  (setq exec-path (dc-paths/reorder-list exec-path (dc-paths--preferred-path-entry))))

(defun dc-paths/filename-if-exists (file-or-dir-name)
  "Return FILE-OR-DIR-NAME if it exists, otherwise nil."
  (and file-or-dir-name
       (file-exists-p file-or-dir-name)
       file-or-dir-name))

(defun dc-paths--get-or-guess-nvm-dir ()
  "Get the NVM home directory based on environment variables or common locations."
  (cond
   ((eq system-type 'windows-nt)
    (or
     (getenv "NVM_HOME")
     (dc-paths/filename-if-exists "c:/ProgramData/nvm")
     (dc-paths/filename-if-exists (concat (getenv "HOME") "\\AppData\\Roaming\\nvm"))))
   (t
    (or
     (and (getenv "NVM_DIR")
          (concat (getenv "NVM_DIR") "/versions/node"))
     (and (getenv "HOME")
          (dc-paths/filename-if-exists
           (concat (getenv "HOME") "/.config/nvm/versions/node")))))))

(defun dc-paths/find-nvm-bin-dir ()
  "Find the NVM-managed nodejs bin directory by respecting default or active version.
Works on Windows (nvm4w) and Linux (nvm)."
  (let ((nvm-dir (dc-paths--get-or-guess-nvm-dir)))
    (cond
     ((eq system-type 'windows-nt)
      (let ((symlink-path (or (getenv "NVM_SYMLINK")
                              "C:\\nvm4w\\nodejs")))
        (if (file-exists-p symlink-path)
            (file-name-as-directory (expand-file-name symlink-path))
          (message "NVM symlink not found at %s" symlink-path)
          nil)))
     (t
      (let ((default-alias (expand-file-name "alias/default" nvm-dir)))
        (if (file-exists-p default-alias)
            (let* ((version (with-temp-buffer
                              (insert-file-contents default-alias)
                              (string-trim (buffer-string))))
                   (bin-path (expand-file-name
                              (concat "versions/node/" version "/bin")
                              nvm-dir)))
              (if (file-exists-p bin-path)
                  (file-name-as-directory bin-path)
                nil))
          (let ((current-path (expand-file-name "current/bin" nvm-dir)))
            (if (file-exists-p current-path)
                (file-name-as-directory current-path)
              nil))))))))

(defun dc-paths/fixup-dirs-for-exec-path (paths-file)
  "Read lines from PATHS-FILE and resolve them for inclusion in `exec-path'."
  (if-let* ((file-contents (and (file-exists-p paths-file)
                                (with-temp-buffer
                                  (insert-file-contents paths-file)
                                  (buffer-substring-no-properties (point-min) (point-max)))))
            (lines (mapcar #'string-trim
                           (split-string file-contents "\n" t)))
            (filtered-lines (seq-filter (lambda (s)
                                          (and (not (string-empty-p s))
                                               (not (string-prefix-p "#" s))))
                                        lines))
            (home-dir (replace-regexp-in-string "\\\\" "/" (getenv "HOME")))
            (mapped-lines
             (mapcar (lambda (s)
                       (replace-regexp-in-string "\\$HOME" home-dir s t))
                     filtered-lines)))
      mapped-lines))

(defun dc-paths/maybe-add-to-exec-path (paths)
  "Given PATHS, a list of directory names, deduplicate and add each item
to `exec-path' and PATH environment variable, if the directory exists."
  (let ((path-env-entries (split-string (getenv "PATH") path-separator)))
    (let ((exec-path-added-count 0)
          (path-env-added-count 0)
          (exec-path-start-length (length exec-path))
          (path-env-start-length (length path-env-entries))
          (initial-exec-path (dc-paths/normalize-and-dedupe exec-path t))
          (initial-envvar-entries (dc-paths/normalize-and-dedupe path-env-entries nil))
          (new-exec-paths-to-append '())
          (envvar-entries-to-append '())
          (normalized-all-exec-paths (make-hash-table :test 'equal))
          (normalized-all-env-paths (make-hash-table :test 'equal)))

      (setq exec-path-added-count (- exec-path-start-length (length initial-exec-path)))
      (setq path-env-added-count (- path-env-start-length (length initial-envvar-entries)))

      (dolist (p initial-exec-path)
        (puthash (dc-paths/normalize-path p t) t normalized-all-exec-paths))
      (dolist (p initial-envvar-entries)
        (puthash (dc-paths/normalize-path p nil) t normalized-all-env-paths))

      (dolist (path paths)
        (when (and path (file-directory-p path))
          (let* ((truename-path (directory-file-name (file-truename path)))
                 (normalized-for-exec-path (dc-paths/normalize-path truename-path t))
                 (normalized-for-env-var (dc-paths/normalize-path truename-path nil))
                 (current-item-added nil))

            (unless (gethash normalized-for-exec-path normalized-all-exec-paths)
              (puthash normalized-for-exec-path t normalized-all-exec-paths)
              (push normalized-for-exec-path new-exec-paths-to-append)
              (setq exec-path-added-count (1+ exec-path-added-count))
              (setq current-item-added (cons "exec-path" current-item-added)))

            (unless (gethash normalized-for-env-var normalized-all-env-paths)
              (puthash normalized-for-env-var t normalized-all-env-paths)
              (push normalized-for-env-var envvar-entries-to-append)
              (setq path-env-added-count (1+ path-env-added-count))
              (setq current-item-added (cons "PATH" current-item-added)))

            (when current-item-added
              (message (format "add %s to %s" truename-path current-item-added))))))

      (setq exec-path (append initial-exec-path (nreverse new-exec-paths-to-append)))
      (when (eq system-type 'windows-nt)
        (dc-paths/maybe-reorder-exec-path-entries))
      (setenv "PATH" (mapconcat 'identity (append initial-envvar-entries (nreverse envvar-entries-to-append)) path-separator))
      (message "dc-paths/maybe-add-to-exec-path: exec-path: Added %d. PATH: Added %d."
               exec-path-added-count path-env-added-count))))

(defun dc-paths/setup-shmode-for-apheleia ()
  "Ensure shfmt is configured in `apheleia-formatters' and mode alists."
  (when (boundp 'apheleia-formatters)
    (if-let* ((shfmt-cmd "~/go/bin/shfmt")
              (_ (file-exists-p shfmt-cmd))
              (cmd-string (car (alist-get 'shfmt apheleia-formatters))))
        (unless (string= cmd-string shfmt-cmd)
          (setcar (alist-get 'shfmt apheleia-formatters) shfmt-cmd))
      (unless (assq 'sh-mode apheleia-mode-alist)
        (push '(sh-mode . shfmt) apheleia-mode-alist))
      (unless (assq 'bash-ts-mode apheleia-mode-alist)
        (push '(bash-ts-mode . shfmt) apheleia-mode-alist)))))

(defun dc-paths/get-env-setting (env-filename keyname)
  "Read a properties or .env file and return value for KEYNAME."
  (let ((expanded-fname (expand-file-name env-filename)))
    (if (file-exists-p expanded-fname)
        (with-temp-buffer
          (insert-file-contents env-filename)
          (keep-lines keyname (point-min) (point-max))
          (let* ((pattern-to-check (concat "^" keyname "\\s*[:=]\\s*\\(.*\\)"))
                 (value
                  (when (string-match pattern-to-check (buffer-string))
                    (match-string 1 (buffer-string)))))
            value))
      (error "Cannot find that file: %s" env-filename))))

(defvar dc-paths--vscode-lang-server-programs
  '(((css-mode css-ts-mode) . "css-language-features/server/dist/node/cssServerMain.js")
    (html-mode . "html-language-features/server/dist/node/htmlServerMain.js")
    (json-mode . "json-language-features/server/dist/node/jsonServerMain.js"))
  "Alist of relative locations of language servers in the VSCode install.")

(defun dc-paths/maybe-register-vscode-server-for-eglot (mode-or-modelist)
  "Register VSCode-provided language server into `eglot-server-programs', if available."
  (if-let* ((relative-program-location (alist-get mode-or-modelist dc-paths--vscode-lang-server-programs))
            (vscode-server-program-fqpath
             (file-name-concat (if (eq system-type 'windows-nt)
                                   (file-name-concat (getenv "HOME")
                                                     "AppData/Local/Programs/Microsoft VS Code")
                                 "/usr/share/code")
                               "resources/app/extensions"
                               relative-program-location)))
      (when (file-exists-p vscode-server-program-fqpath)
        (add-to-list 'eglot-server-programs
                     `(,mode-or-modelist
                       . ,(list "node" vscode-server-program-fqpath "--stdio"))))))

(provide 'dc-paths)

;;; dc-paths.el ends here