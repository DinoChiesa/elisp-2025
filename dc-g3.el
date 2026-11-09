;;; dc-g3.el --- google3 and CitC navigation utilities -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Dino Chiesa
;; Keywords: google3, citc, convenience

;;; Commentary:
;; Quick navigation for google3 favorite directories (at head or inside a CitC
;; workspace) and CitC workspace management via g4d.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function dc-ss/completion-fn "dc-sane-sorting" (candidates category-symbol))

(defvar dc-g3-head-root "/google/src/files/head/depot/google3/"
  "Filesystem root for read-only google3 head.")

(defvar dc-g3-cloud-dir "/google/src/cloud/dchiesa"
  "Base directory containing CitC workspaces.")

(defvar dc-g3-default-favorites
  '("experimental/users/dchiesa/"
    "edge/prism/"
    "experimental/users/"
    "devtools/kokoro/config/prod/edge-internal/featureplatform/edge-hybrid-e2etests/")
  "Initial list of favorite subdirectories relative to google3/.")

(defvar dc-g3-favorites-file
  (expand-file-name "g3-favorites.el" user-emacs-directory)
  "File storing persisted google3 favorite subdirectories.")

(defvar dc-g3--favorites 'unset
  "In-memory list of google3 favorite subdirectories, or `unset' before load.")

(defun dc-g3--completing-read (prompt candidates &optional require-match)
  "Prompt with PROMPT over CANDIDATES while preserving list order."
  (let ((table (if (fboundp 'dc-ss/completion-fn)
                   (dc-ss/completion-fn candidates 'unsorted)
                 (lambda (string pred action)
                   (if (eq action 'metadata)
                       '(metadata (display-sort-function . identity)
                                  (cycle-sort-function . identity))
                     (complete-with-action action candidates string pred))))))
    (completing-read prompt table nil require-match)))

(defun dc-g3--load-favorites ()
  "Return the list of favorite google3 subpaths, loading from disk if needed."
  (when (eq dc-g3--favorites 'unset)
    (setq dc-g3--favorites
          (if (file-readable-p dc-g3-favorites-file)
              (condition-case err
                  (with-temp-buffer
                    (insert-file-contents dc-g3-favorites-file)
                    (let ((data (read (current-buffer))))
                      (if (listp data)
                          (seq-filter #'stringp data)
                        (copy-sequence dc-g3-default-favorites))))
                (error
                 (message "Failed to read %s (%s); using defaults" dc-g3-favorites-file err)
                 (copy-sequence dc-g3-default-favorites)))
            (copy-sequence dc-g3-default-favorites))))
  dc-g3--favorites)

(defun dc-g3--save-favorites ()
  "Persist `dc-g3--favorites' to `dc-g3-favorites-file'."
  (make-directory (file-name-directory dc-g3-favorites-file) t)
  (with-temp-file dc-g3-favorites-file
    (insert ";; Persisted google3 favorite directories for dc-g3.el\n(\n")
    (dolist (fav dc-g3--favorites)
      (insert (format " %S\n" fav)))
    (insert ")\n")))

(defun dc-g3--normalize-subpath (path)
  "Normalize PATH into a google3-relative directory ending with `/'."
  (let* ((trimmed (string-trim path))
         (rel (cond
               ((string-match "/google3/\\(.+\\)\\'" trimmed)
                (match-string 1 trimmed))
               ((not (string-prefix-p "/" trimmed))
                trimmed)
               (t
                (user-error "Directory is not under a google3 root: %s" trimmed))))
         (clean (string-trim rel "/+" "/+")))
    (when (string-empty-p clean)
      (user-error "Favorite must be a subdirectory under google3/"))
    (file-name-as-directory clean)))

(defun dc-g3--current-citc-workspace ()
  "Return the active CitC workspace name from `default-directory', or nil."
  (let* ((expanded (expand-file-name default-directory))
         (pattern (concat "\\`" (regexp-quote (file-name-as-directory dc-g3-cloud-dir))
                          "\\([^/]+\\)/")))
    (when (string-match pattern expanded)
      (match-string 1 expanded))))

(defun dc-g3--get-workspaces (base-dir)
  "Return sorted list of CitC workspaces in BASE-DIR."
  (if (file-directory-p base-dir)
      (let ((dirs '()))
        (dolist (file (directory-files base-dir nil nil t))
          (unless (member file '("." ".." ".citc"))
            (when (file-directory-p (expand-file-name file base-dir))
              (push file dirs))))
        (sort dirs #'string<))
    (message "Base directory %s does not exist" base-dir)
    nil))

(defun dc-g3--choose-citc-root ()
  "Prompt for an existing CitC workspace and return its google3/ directory."
  (let* ((workspaces (dc-g3--get-workspaces dc-g3-cloud-dir))
         (current-ws (dc-g3--current-citc-workspace))
         (ordered (if (and current-ws (member current-ws workspaces))
                      (cons current-ws (remove current-ws workspaces))
                    workspaces)))
    (unless ordered
      (user-error "No CitC workspaces found in %s" dc-g3-cloud-dir))
    (let ((ws (dc-g3--completing-read "CitC workspace: " ordered t)))
      (format "%s/%s/google3/" dc-g3-cloud-dir ws))))

(defun dc-g3/find-favorite (&optional arg)
  "Open `find-file' starting at a chosen google3 favorite directory.
By default, resolves the favorite under read-only head.
With prefix ARG (C-u), prompts for a CitC workspace and opens it there."
  (interactive "P")
  (let ((favorites (dc-g3--load-favorites)))
    (unless favorites
      (user-error "No google3 favorites configured; run `dc-g3/add-favorite'"))
    (let* ((subpath (dc-g3--completing-read "g3 favorite: " favorites t))
           (root (if arg (dc-g3--choose-citc-root) dc-g3-head-root))
           (target-dir (file-name-as-directory (expand-file-name subpath root)))
           (default-directory target-dir)
           (current-prefix-arg nil))
      (unless (file-directory-p target-dir)
        (user-error "Directory does not exist: %s" target-dir))
      (call-interactively #'find-file))))

(defun dc-g3/add-favorite (&optional dir)
  "Add DIR (a google3 directory) to the persisted favorites list."
  (interactive
   (let ((start-dir (if (string-match-p "/google3/.+" (expand-file-name default-directory))
                        default-directory
                      dc-g3-head-root)))
     (list (read-directory-name "Add g3 favorite dir: " start-dir nil t))))
  (let ((subpath (dc-g3--normalize-subpath dir))
        (favorites (dc-g3--load-favorites)))
    (if (member subpath favorites)
        (message "Already in g3 favorites: %s" subpath)
      (setq dc-g3--favorites (append favorites (list subpath)))
      (dc-g3--save-favorites)
      (message "Added g3 favorite: %s" subpath))))

(defun dc-g3/remove-favorite (&optional subpath)
  "Remove SUBPATH from the persisted google3 favorites list."
  (interactive
   (let ((favorites (dc-g3--load-favorites)))
     (unless favorites
       (user-error "No google3 favorites to remove"))
     (list (dc-g3--completing-read "Remove g3 favorite: " favorites t))))
  (let ((favorites (dc-g3--load-favorites)))
    (if (not (member subpath favorites))
        (user-error "Not in g3 favorites: %s" subpath)
      (setq dc-g3--favorites (delete subpath (copy-sequence favorites)))
      (dc-g3--save-favorites)
      (message "Removed g3 favorite: %s" subpath))))

(defun dc-g3/find-workspace ()
  "Open the CitC workspaces directory in Dired."
  (interactive)
  (if (file-directory-p dc-g3-cloud-dir)
      (dired dc-g3-cloud-dir)
    (message "Directory %s does not exist" dc-g3-cloud-dir)))

(defun dc-g3--g4d-open (workspace)
  "Open WORKSPACE in Dired."
  (let ((dir (format "%s/%s/google3" dc-g3-cloud-dir workspace)))
    (if (file-directory-p dir)
        (dired dir)
      (message "Directory %s does not exist" dir))))

(defun dc-g3--g4d-create-and-open (workspace)
  "Create WORKSPACE using g4d and open it."
  (message "Creating workspace %s..." workspace)
  (let ((exit-code (call-process "g4d" nil nil nil "-f" workspace)))
    (if (= exit-code 0)
        (progn
          (message "Workspace %s created successfully." workspace)
          (dc-g3--g4d-open workspace))
      (error "Failed to create workspace %s (exit code %d)" workspace exit-code))))

(defun dc-g3/g4d ()
  "Choose or create a g4d workspace and open it in Dired."
  (interactive)
  (let* ((workspaces (dc-g3--get-workspaces dc-g3-cloud-dir))
         (candidates (cons "New workspace" workspaces))
         (selection (dc-g3--completing-read "Workspace: " candidates nil)))
    (cond
     ((string= selection "New workspace")
      (let ((new-ws (read-string "New workspace name: ")))
        (when (not (string-empty-p new-ws))
          (dc-g3--g4d-create-and-open new-ws))))
     ((member selection workspaces)
      (dc-g3--g4d-open selection))
     ((not (string-empty-p selection))
      ;; User typed a new workspace name directly
      (dc-g3--g4d-create-and-open selection)))))

(provide 'dc-g3)

;;; dc-g3.el ends here
