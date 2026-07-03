;;; dino-g3.el --- g3 and g4 things for dino   -*- coding: utf-8; lexical-binding: t;  -*-
;;

(defun dino/find-g3-experimental-file ()
  "Open find-file starting at a specific long project directory."
  (interactive)
  (let ((default-directory "/google/src/files/head/depot/google3/experimental/users"))
    (call-interactively 'find-file)))

(defun dino/find-g3-workspace ()
  "dired the g3 workspace dir."
  (interactive)
  (if-let* ((dir "/google/src/cloud/dchiesa"))
      (dired dir)
    (message "Directory %s does not exist" dir)))


(defun dino/find-g3-edge-internal-file ()
  "Open find-file starting at a specific long project directory."
  (interactive)
  (let ((default-directory "/google/src/files/head/depot/google3/devtools/kokoro/config/prod/edge-internal/featureplatform/edge-hybrid-e2etests"))
    (call-interactively 'find-file)))

(defun dino/g4d-get-workspaces (base-dir)
  "Get list of existing workspaces in BASE-DIR."
  (if (file-directory-p base-dir)
      (let ((all-files (directory-files base-dir nil nil t))
            (dirs '()))
        (dolist (file all-files)
          (unless (member file '("." ".."))
            (when (file-directory-p (expand-file-name file base-dir))
              (push file dirs))))
        (sort dirs 'string<))
    (message "Base directory %s does not exist" base-dir)
    nil))

(defun dino/g4d-open (workspace)
  "Open WORKSPACE in dired."
  (let ((dir (format "/google/src/cloud/dchiesa/%s/google3" workspace)))
    (if (file-directory-p dir)
        (dired dir)
      (message "Directory %s does not exist" dir))))

(defun dino/g4d-create-and-open (workspace)
  "Create WORKSPACE using g4d and open it."
  (message "Creating workspace %s..." workspace)
  (let ((exit-code (call-process "g4d" nil nil nil "-f" workspace)))
    (if (= exit-code 0)
        (progn
          (message "Workspace %s created successfully." workspace)
          (dino/g4d-open workspace))
      (error "Failed to create workspace %s (exit code %d)" workspace exit-code))))

(defun dino/g4d ()
  "Choose or create a g4d workspace and open it in dired."
  (interactive)
  (let* ((base-dir "/google/src/cloud/dchiesa")
         (workspaces (dino/g4d-get-workspaces base-dir))
         (candidates (cons "New workspace" workspaces))
         (selection (completing-read "Workspace: " candidates nil nil)))
    (cond
     ((string= selection "New workspace")
      (let ((new-ws (read-string "New workspace name: ")))
        (when (and new-ws (not (string= new-ws "")))
          (dino/g4d-create-and-open new-ws))))
     ((member selection workspaces)
      (dino/g4d-open selection))
     ((and selection (not (string= selection "")))
      ;; User typed a new workspace name directly
      (dino/g4d-create-and-open selection)))))



(provide 'dino-g3)

;;; dino-g3.el ends here
