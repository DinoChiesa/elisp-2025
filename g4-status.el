;;; g4-status.el --- Interactive g4 status buffer analogous to magit-status  -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'diff-mode)
(require 'transient)
(require 'cl-lib)

(defcustom g4-status-script-path
  (let ((dir (file-name-directory (or load-file-name byte-compile-current-file buffer-file-name))))
    (expand-file-name "g4-critique-tool" dir))
  "Path to the g4-critique-tool helper script."
  :type 'file
  :group 'g4-status)

(defcustom g4-status-confirm-actions nil
  "When non-nil, prompt for confirmation before staging and unstaging files.
When nil (the default), stage/unstage actions execute immediately."
  :type 'boolean
  :group 'g4-status)

(defface g4-status-header
  '((((class color) (background light))
     :inherit magit-section-header :foreground "blue" :weight bold)
    (((class color) (background dark))
     :inherit magit-section-header :foreground "cyan" :weight bold)
    (t :weight bold))
  "Face for g4-status section headers.")

(defface g4-status-changelist
  '((((class color) (background light))
     :inherit magit-branch-local :foreground "red" :weight bold)
    (((class color) (background dark))
     :inherit magit-branch-local :foreground "red" :weight bold)
    (t :weight bold))
  "Face for the g4 changelist number.")

(define-button-type 'g4-status-cl-link
  'action (lambda (button)
            (browse-url (concat "http://cl/" (button-get button 'cl-number))))
  'follow-link t
  'face 'g4-status-changelist
  'help-echo "Read changelist in browser")

(defvar g4-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'g4-status-open-file)
    (define-key map (kbd "TAB") 'g4-status-dwim-toggle)
    (define-key map (kbd "g") 'g4-status-refresh)
    (define-key map (kbd "u") 'g4-status-unstage-file)
    (define-key map (kbd "s") 'g4-status-stage-file)
    (define-key map (kbd "F") 'g4-status-sync)
    (define-key map (kbd "S") 'g4-status-sync) ;; synonym
    (define-key map (kbd "k") 'g4-status-delete-untracked-file)
    (define-key map (kbd "c") 'g4-status-edit-description)
    (define-key map (kbd "?") 'g4-status-help)
    map)
  "Keymap for `g4-status-mode'.")

(transient-define-prefix g4-status-help ()
  "Help menu for `g4-status-mode'."
  ["g4-status keybindings"
   ["Navigation"
    ("RET" "Open file" g4-status-open-file)
    ("TAB" "Toggle section/diff" g4-status-dwim-toggle)]
   ["Changelist"
    ("c" "Edit description" g4-status-edit-description)
    ("g" "Refresh buffer" g4-status-refresh)]
   ["File Actions"
    ("s" "Stage (g4 add) / Upload file" g4-status-stage-file)
    ("u" "Remove from CL (local copy kept)" g4-status-unstage-file)
    ("k" "Delete untracked file" g4-status-delete-untracked-file)
    ("F" "Fetch/sync from depot" g4-status-sync)]])

(define-derived-mode g4-status-mode special-mode "g4-status"
  "Major mode for displaying g4 status."
  (setq-local buffer-read-only t))

;;;###autoload
(defun g4-status-or-magit ()
  "Run `g4-status' if in a CitC workspace (/google/src/), otherwise `magit-status'."
  (interactive)
  (if (and default-directory
           (string-prefix-p "/google/src/" default-directory))
      (g4-status)
    (magit-status)))

;;;###autoload
(defun g4-status ()
  "Create and display a g4-status buffer."
  (interactive)
  (let ((buf (get-buffer-create "*g4-status*")))
    (with-current-buffer buf
      (g4-status-mode)
      (g4-status-refresh))
    (switch-to-buffer buf)))

(defun g4-status-parse-output (output)
  "Parse OUTPUT of g4-diff-critique.
Return list of (CL MODIFIED NEW STAGED UNTRACKED) files."
  (let (cl modified new staged untracked)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^On changelist \\([0-9]+\\)" line)
            (setq cl (match-string 1 line)))
           ((string-match "^Modified: \\(.*\\)" line)
            (push (cons "modified" (match-string 1 line)) modified))
           ((string-match "^New (not uploaded): \\(.*\\)" line)
            (push (cons "new" (match-string 1 line)) new))
           ((string-match "^Staged: \\([a-z]+\\) \\(.*\\)" line)
            (push (cons (match-string 1 line) (match-string 2 line)) staged))
           ((string-match "^Untracked: \\(.*\\)" line)
            (push (match-string 1 line) untracked))))
        (forward-line 1)))
    (list cl (nreverse modified) (nreverse new) (nreverse staged) (nreverse untracked))))

(defun g4-status-insert-section (title files type)
  "Insert a section with TITLE and FILES.
TYPE is either `staged', `unstaged', or `untracked'."
  (when files
    (let ((header-start (point))
          body-start body-end)
      (insert (format "%s (%d)\n" title (length files)))
      (let ((header-end (1- (point))))
        (add-text-properties header-start header-end
                             '(face g4-status-header
                               g4-status-section-header t)))
      (setq body-start (point))
      (dolist (item files)
        (let (prefix file)
          (cond
           ((or (eq type 'staged) (eq type 'unstaged))
            (setq prefix (format "%-10s " (car item))
                  file (cdr item)))
           ((eq type 'untracked)
            (setq prefix "untracked  "
                  file item)))
          (let ((file-start (point)))
            (insert (format "%s%s\n" prefix file))
            (add-text-properties file-start (1- (point))
                                 (list 'g4-status-file-type type)))))
      (setq body-end (point))
      (let ((ov (make-overlay body-start body-end nil nil t)))
        (overlay-put ov 'invisible nil)
        (overlay-put ov 'is-g4-body t)
        (add-text-properties header-start (1- body-start)
                             (list 'g4-section-overlay ov)))
      (insert "\n"))))

(defun g4-status-refresh ()
  "Refresh the g4-status buffer, preserving point position where possible."
  (interactive)
  (let ((saved-file (g4-status-get-file-at-point))
        (saved-line (line-number-at-pos))
        (inhibit-read-only t)
        (client-root (string-trim (shell-command-to-string "g4 info 2>/dev/null | grep -i '^Client root:' | awk '{print $3}'")))
        (cmd (format "%s status" g4-status-script-path)))
    (if (string-empty-p client-root)
        (error "Not in a CitC workspace")
      (setq-local default-directory (file-name-as-directory client-root))
      (erase-buffer)
      (remove-overlays)
      (let* ((output (shell-command-to-string cmd))
             (parsed (g4-status-parse-output output))
             (cl (nth 0 parsed))
             (modified-files (nth 1 parsed))
             (new-files (nth 2 parsed))
             (staged-files (nth 3 parsed))
             (untracked-files (nth 4 parsed))
             (unstaged-changes (append modified-files new-files)))
        (if cl
            (let* ((snapshot-path (format "/google/src/cloud/review/%s/.snapshot/head" cl))
                   (snapshot-target (and (file-exists-p snapshot-path)
                                         (file-symlink-p snapshot-path)))
                   (snapshot-num (and snapshot-target
                                      (file-name-nondirectory snapshot-target)))
                   (snapshot-time (and snapshot-target
                                       (nth 5 (file-attributes snapshot-path)))))
              (insert "changelist ")
              (insert-text-button cl
                                  'type 'g4-status-cl-link
                                  'cl-number cl)
              (when snapshot-num
                (if snapshot-time
                    (insert (format " (snapshot %s, %s)"
                                    snapshot-num
                                    (downcase (format-time-string "%Y-%B-%d %H:%M" snapshot-time))))
                  (insert (format " (snapshot %s)" snapshot-num))))
              (insert "\n\n"))
          (insert "No active changelist found.\n\n"))
        (cond
         ((or staged-files unstaged-changes untracked-files)
          (g4-status-insert-section "Untracked files" untracked-files 'untracked)
          (g4-status-insert-section "Unstaged / Local changes" unstaged-changes 'unstaged)
          (g4-status-insert-section "Staged / Uploaded changes" staged-files 'staged))
         (t
          (insert "No changed files.\n"))))
      ;; Restore point: seek to the same file if still present, else same line.
      (goto-char (point-min))
      (when saved-file
        (let ((target (file-name-nondirectory saved-file))
              (found nil))
          (while (and (not (eobp)) (not found))
            (when (and (g4-status-get-file-at-point)
                       (string= (file-name-nondirectory (g4-status-get-file-at-point)) target))
              (setq found t))
            (unless found (forward-line 1)))
          (unless found
            ;; File gone (e.g. deleted); go to saved line, clamped to buffer end.
            (goto-char (point-min))
            (forward-line (1- saved-line))
            (when (eobp) (forward-line -1))))))))

(defun g4-status-dwim-toggle ()
  "Toggle section visibility, hunk visibility, or file diff depending on point."
  (interactive)
  (let ((bol (line-beginning-position)))
    (cond
     ((get-text-property bol 'g4-status-section-header)
      (g4-status-toggle-section))
     ((get-text-property bol 'g4-hunk-overlay)
      (g4-status-toggle-hunk))
     ((g4-status-get-file-at-point)
      (g4-status-toggle-diff))
     (t (message "Nothing to toggle here")))))

(defun g4-status-toggle-section ()
  "Toggle visibility of the section at point."
  (interactive)
  (let ((ov (get-text-property (line-beginning-position) 'g4-section-overlay)))
    (if ov
        (let ((inhibit-read-only t))
          (overlay-put ov 'invisible (not (overlay-get ov 'invisible))))
      (message "No section overlay found at point"))))

(defun g4-status-get-diff-line-number ()
  "Determine the corresponding line number in the modified file at point.
Return nil if not on a diff line."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^   ") ; We are on a diff line (indented by 3 spaces)
      (let ((cursor-line (line-number-at-pos)))
        ;; Search backwards for the hunk header
        (while (and (not (bobp))
                    (looking-at "^   ")
                    (not (looking-at "^   @@")))
          (forward-line -1))
        (when (looking-at "^   @@ -[0-9]+,[0-9]+ \\+\\([0-9]+\\)")
          (let ((new-start (string-to-number (match-string 1)))
                (target-line nil))
            (setq target-line new-start)
            (forward-line 1)
            (while (< (line-number-at-pos) cursor-line)
              (unless (looking-at "^   -")
                (setq target-line (1+ target-line)))
              (forward-line 1))
            target-line))))))

(defun g4-status-open-file ()
  "Open the file at point.
If point is on a diff line, jump to the corresponding line in the file."
  (interactive)
  (let ((file (g4-status-get-file-at-point))
        (line-num (g4-status-get-diff-line-number)))
    (when file
      (find-file file)
      (when line-num
        (goto-char (point-min))
        (forward-line (1- line-num))))))

(defun g4-status-toggle-hunk ()
  "Toggle visibility of the diff hunk at point."
  (interactive)
  (let ((ov (get-text-property (line-beginning-position) 'g4-hunk-overlay)))
    (if ov
        (let ((inhibit-read-only t))
          (overlay-put ov 'invisible (not (overlay-get ov 'invisible))))
      (message "No hunk overlay found at point"))))

(defun g4-status-create-hunk-overlays (start end)
  "Create overlays for each diff hunk in the region between START and END."
  (save-excursion
    (goto-char start)
    (let (hunk-header-pos hunk-body-start)
      (while (< (point) end)
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (cond
           ((looking-at "^   @@")
            (when hunk-body-start
              (let ((ov (make-overlay hunk-body-start line-start nil nil t)))
                (overlay-put ov 'invisible nil)
                (overlay-put ov 'is-g4-hunk-body t)
                (add-text-properties hunk-header-pos (1- hunk-body-start)
                                     (list 'g4-hunk-overlay ov))))
            (setq hunk-header-pos line-start
                  hunk-body-start (1+ line-end)))
           ((looking-at "^   \\(--- \\|\\+\\+\\+\\ \\)")
            nil))
          (forward-line 1))
        (when hunk-body-start
          (let ((ov (make-overlay hunk-body-start end nil nil t)))
            (overlay-put ov 'invisible nil)
            (overlay-put ov 'is-g4-hunk-body t)
            (add-text-properties hunk-header-pos (1- hunk-body-start)
                                 (list 'g4-hunk-overlay ov))))))))

(defun g4-status-fontify-diff (start end)
  "Apply diff faces to the region between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((line-end (line-end-position)))
        (cond
         ((looking-at "^   \\(--- \\|\\+\\+\\+\\ \\)")
          (add-text-properties (point) line-end '(face diff-file-header)))
         ((looking-at "^   @@")
          (add-text-properties (point) line-end '(face diff-hunk-header)))
         ((looking-at "^   -")
          (add-text-properties (point) line-end '(face diff-removed)))
         ((looking-at "^   \\+")
          (add-text-properties (point) line-end '(face diff-added))))
        (forward-line 1)))))

(defun g4-status--file-prefix-at-point ()
  "Return the prefix word (e.g. \"new\", \"modified\", \"add\") on the file line at point."
  (save-excursion
    (beginning-of-line)
    (while (and (looking-at "^ ") (not (bobp)))
      (forward-line -1))
    (when (looking-at "^\\([a-z-]+\\) ")
      (match-string 1))))

(defun g4-status--insert-indented-diff (content)
  "Insert diff CONTENT indented by 3 spaces, fontified and with hunk overlays."
  (let* ((start (point))
         (indented
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            (while (not (eobp))
              (insert "   ")
              (forward-line 1))
            (buffer-string))))
    (insert indented)
    (g4-status-fontify-diff start (point))
    (g4-status-create-hunk-overlays start (point))))

(defun g4-status-toggle-diff ()
  "Toggle the diff of the file at point inline."
  (interactive)
  (let ((file (g4-status-get-file-at-point))
        (type (g4-status-get-file-type-at-point))
        (prefix (g4-status--file-prefix-at-point)))
    (when file
      (save-excursion
        (beginning-of-line)
        (while (and (looking-at "^ ") (not (bobp)))
          (forward-line -1))
        (forward-line 1)
        (let ((inhibit-read-only t))
          (if (looking-at "^ ")
              ;; Already expanded — collapse it.
              (let ((start (point)))
                (while (looking-at "^ ")
                  (forward-line 1))
                (delete-region start (point)))
            ;; Not expanded — show diff based on file type/prefix.
            (cond
             ((and (eq type 'unstaged) (equal prefix "new"))
              ;; New local file not in CL: diff against /dev/null shows full content.
              (let ((content (shell-command-to-string
                              (format "diff /dev/null %s" (shell-quote-argument file)))))
                (g4-status--insert-indented-diff content)))
             ((eq type 'unstaged)
              ;; Modified file already in CL: diff between snapshot and local copy.
              (let* ((cmd (format "%s diff %s" g4-status-script-path (shell-quote-argument file)))
                     (content (shell-command-to-string cmd)))
                (when (and content (not (string-empty-p content)))
                  (g4-status--insert-indented-diff content))))
             (t
              ;; Staged: fetch diff from Critique snapshot.
              (let* ((script-cmd (if (eq type 'staged) "diff-staged" "diff"))
                     (cmd (format "%s %s %s" g4-status-script-path script-cmd file))
                     (content (shell-command-to-string cmd)))
                (when (and content (not (string-empty-p content)))
                  (g4-status--insert-indented-diff content)))))))))))

(defun g4-status-get-file-at-point ()
  "Get the file path on current line or file associated with the diff at point."
  (save-excursion
    (beginning-of-line)
    (while (and (looking-at "^ ") (not (bobp)))
      (forward-line -1))
    (let ((case-fold-search nil))
      (when (looking-at "^[a-z-]+ +\\(.*\\)$")
        (match-string 1)))))

(defun g4-status-get-file-type-at-point ()
  "Get the file type (staged, modified, added) at point."
  (save-excursion
    (beginning-of-line)
    (while (and (looking-at "^ ") (not (bobp)))
      (forward-line -1))
    (get-text-property (point) 'g4-status-file-type)))

(defun g4-status-files-in-region ()
  "Return a list of (FILE . TYPE) pairs for files in the active region.
If no region is active, return nil."
  (when (use-region-p)
    (let ((start (region-beginning))
          (end (region-end))
          files)
      (save-excursion
        (goto-char start)
        (beginning-of-line)
        (let ((keep-going t))
          (while keep-going
            (let ((file (g4-status-get-file-at-point))
                  (type (g4-status-get-file-type-at-point)))
              (when (and file type)
                (push (cons file type) files)))
            (if (>= (line-end-position) end)
                (setq keep-going nil)
              (if (not (zerop (forward-line 1)))
                  (setq keep-going nil))))))
      (delete-dups (nreverse files)))))

(defun g4-status--remove-file-from-cl (_cl file)
  "Remove FILE from the CL, keeping the local copy on disk.
Backs up the file, runs `g4 revert -k', then restores from backup.
This handles both add-opened and edit-opened files correctly."
  (let ((backup (make-temp-file "g4-status-unstage")))
    (copy-file file backup t)
    (shell-command-to-string (format "g4 revert -k %s" (shell-quote-argument file)))
    (copy-file backup file t)
    (delete-file backup)))

(defun g4-status-unstage-file ()
  "Remove the file at point (or files in active region) from the g4 changelist.
Keeps the local copy intact. Runs `g4 revert -k' locally and removes the file
from the server-side CL via `g4 change -i'.
Prompts for confirmation only when `g4-status-confirm-actions' is non-nil."
  (interactive)
  (let* ((cl (nth 0 (g4-status-parse-output
                     (shell-command-to-string (format "%s status" g4-status-script-path)))))
         (region-files (g4-status-files-in-region)))
    (unless cl
      (error "No active changelist found"))
    (if region-files
        (let ((staged-files (cl-remove-if-not (lambda (f) (eq (cdr f) 'staged)) region-files)))
          (if (not staged-files)
              (message "No staged files in selected region")
            (let ((count (length staged-files)))
              (if (or (not g4-status-confirm-actions)
                      (y-or-n-p (format "Remove %d files from CL (local copies kept)? " count)))
                  (progn
                    (message "Unstaging %d files..." count)
                    (redisplay)
                    (dolist (item staged-files)
                      (g4-status--remove-file-from-cl cl (car item)))
                    (deactivate-mark)
                    (message "Removed %d files from CL" count)
                    (g4-status-refresh))
                (message "Aborted")))))
      (let ((file (g4-status-get-file-at-point))
            (type (g4-status-get-file-type-at-point)))
        (if (and file (eq type 'staged))
            (if (or (not g4-status-confirm-actions)
                    (y-or-n-p (format "Remove %s from CL (local copy kept)? " file)))
                (progn
                  (message "Unstaging %s..." (file-name-nondirectory file))
                  (redisplay)
                  (g4-status--remove-file-from-cl cl file)
                  (message "Removed %s from CL" (file-name-nondirectory file))
                  (g4-status-refresh))
              (message "Aborted"))
          (message "No staged file at point to remove from CL"))))))

(defun g4-status-sync ()
  "Sync the workspace from the depot (g4 sync).
Warns if there are unstaged locally-modified files that g4 sync could overwrite."
  (interactive)
  (message "Checking workspace status...")
  (redisplay)
  (let* ((output (shell-command-to-string (format "%s status" g4-status-script-path)))
         (parsed (g4-status-parse-output output))
         (modified (nth 1 parsed))
         (new-files (nth 2 parsed))
         (at-risk (append modified new-files)))
    (when at-risk
      (let ((names (mapcar #'cdr at-risk)))
        (unless (y-or-n-p
                 (format "Warning: %d locally-modified file(s) not in CL (%s) may be overwritten by sync. Proceed? "
                         (length names)
                         (string-join names ", ")))
          (user-error "Sync aborted"))))
    (message "Running g4 sync...")
    (redisplay)
    (let ((result (shell-command-to-string "G4RECONCILEADDS=0 g4 sync")))
      (message "%s" (string-trim result)))
    (g4-status-refresh)))

(defun g4-status-delete-untracked-file ()
  "Delete the untracked file at point, or untracked files in the active
region. Only operates on files in the Untracked section. Always prompts
for confirmation."
  (interactive)
  (let ((region-files (g4-status-files-in-region)))
    (if region-files
        (let ((untracked (cl-remove-if-not (lambda (f) (eq (cdr f) 'untracked)) region-files)))
          (if (not untracked)
              (message "No untracked files in selected region")
            (let ((count (length untracked)))
              (if (or (not g4-status-confirm-actions)
                      (y-or-n-p (format "Delete %d untracked file(s)? " count)))
                  (progn
                    (dolist (item untracked)
                      (delete-file (car item)))
                    (deactivate-mark)
                    (message "Deleted %d file(s)" count)
                    (g4-status-refresh))
                (message "Aborted")))))
      (let ((file (g4-status-get-file-at-point))
            (type (g4-status-get-file-type-at-point)))
        (if (and file (eq type 'untracked))
            (if (or (not g4-status-confirm-actions)
                    (y-or-n-p (format "Delete %s? " (file-name-nondirectory file))))
                (progn
                  (delete-file file)
                  (message "Deleted %s" (file-name-nondirectory file))
                  (g4-status-refresh))
              (message "Aborted"))
          (message "No untracked file at point to delete"))))))

(defun g4-status-stage-file ()
  "Add/stage the file at point or files in active region to the g4 changelist.
If the file is untracked, run `g4 add'.
If the file is unstaged (modified), run `g4 upload' to sync the CL snapshot.
Prompts for confirmation only when `g4-status-confirm-actions' is non-nil."
  (interactive)
  (let ((region-files (g4-status-files-in-region)))
    (if region-files
        (let ((untracked-files (cl-remove-if-not (lambda (f) (eq (cdr f) 'untracked)) region-files)))
          (if (not untracked-files)
              (message "No untracked files in selected region to stage")
            (let ((count (length untracked-files)))
              (if (or (not g4-status-confirm-actions)
                      (y-or-n-p (format "Stage (g4 add) %d untracked files? " count)))
                  (progn
                    (message "Staging %d files..." count)
                    (redisplay)
                    (dolist (item untracked-files)
                      (shell-command-to-string (format "g4 add %s" (shell-quote-argument (car item)))))
                    (deactivate-mark)
                    (message "Staged %d files" count)
                    (g4-status-refresh))
                (message "Aborted")))))
      (let ((file (g4-status-get-file-at-point))
            (type (g4-status-get-file-type-at-point)))
        (cond
         ((eq type 'untracked)
          (if (or (not g4-status-confirm-actions)
                  (y-or-n-p (format "Add %s to CL (g4 add)? " file)))
              (progn
                (message "Staging %s..." (file-name-nondirectory file))
                (redisplay)
                (let ((output (shell-command-to-string (format "g4 add %s" (shell-quote-argument file)))))
                  (message "%s" (string-trim output)))
                (g4-status-refresh))
            (message "Aborted")))
         ((eq type 'unstaged)
          (if (or (not g4-status-confirm-actions)
                  (y-or-n-p "Upload CL snapshot to Critique? (G4RECONCILEADDS=0) "))
              (progn
                (message "Uploading CL snapshot...")
                (redisplay)
                (let ((output (shell-command-to-string "G4RECONCILEADDS=0 g4 upload")))
                  (message "%s" (string-trim output)))
                (g4-status-refresh))
            (message "Aborted")))
         (t
          (message "No file at point that can be staged")))))))
(defvar-local g4-status-edit-cl nil)
(defvar-local g4-status-edit-parent-buffer nil)

(defvar g4-status-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'g4-status-edit-save)
    (define-key map (kbd "C-c C-k") 'g4-status-edit-cancel)
    map)
  "Keymap for `g4-status-edit-mode'.")

(define-derived-mode g4-status-edit-mode text-mode "g4-edit"
  "Major mode for editing g4 changelist descriptions.
Press \\[g4-status-edit-save] to save changes, or \\[g4-status-edit-cancel] to cancel."
  (setq-local header-line-format "Edit CL description. Press C-c C-c to save, C-c C-k to cancel."))

(defun g4-status-get-cl-description (cl)
  "Get the description of changelist CL."
  (let* ((spec (shell-command-to-string (format "g4 change -o %s" cl)))
         (lines (split-string spec "\n"))
         (in-desc nil)
         (desc-lines nil))
    (dolist (line lines)
      (cond
       ((string-match "^Description:" line)
        (setq in-desc t))
       ((and in-desc (string-match "^[ \t]\\(.*\\)" line))
        (push (match-string 1 line) desc-lines))
       ((and in-desc (string-match "^[^ \t]" line))
        (setq in-desc nil))))
    (string-join (nreverse desc-lines) "\n")))

(defun g4-status-edit-description ()
  "Edit the description of the active changelist."
  (interactive)
  (let* ((client-root (string-trim (shell-command-to-string "g4 info 2>/dev/null | grep -i '^Client root:' | awk '{print $3}'"))))
    (if (string-empty-p client-root)
        (error "Not in a CitC workspace")
      (let* ((cmd (format "%s status" g4-status-script-path))
             (output (shell-command-to-string cmd))
             (parsed (g4-status-parse-output output))
             (cl (nth 0 parsed)))
        (if (not cl)
            (error "No active changelist found")
          (let ((desc (g4-status-get-cl-description cl))
                (parent-buf (current-buffer))
                (buf (get-buffer-create "*g4-edit-description*")))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert desc)
                (set-buffer-modified-p nil)
                (g4-status-edit-mode)
                (setq g4-status-edit-cl cl)
                (setq g4-status-edit-parent-buffer parent-buf)))
            (switch-to-buffer buf)))))))

(defun g4-status-edit-save ()
  "Save the changelist description and close the buffer."
  (interactive)
  (let ((new-desc (buffer-string))
        (cl g4-status-edit-cl)
        (parent-buf g4-status-edit-parent-buffer))
    ;; Indent each line of the new description with a tab
    (setq new-desc
          (string-join
           (mapcar (lambda (line) (concat "\t" line))
                   (split-string new-desc "\n"))
           "\n"))
    ;; Fetch the current spec
    (let ((spec (shell-command-to-string (format "g4 change -o %s" cl))))
      ;; Replace the Description section in the spec
      (with-temp-buffer
        (insert spec)
        (goto-char (point-min))
        (if (re-search-forward "^Description:\n\\([ \t].*\n\\)*" nil t)
            (replace-match (concat "Description:\n" new-desc "\n") t t)
          (error "Could not find Description section in change spec"))
        (let ((status (call-process-region (point-min) (point-max) "g4" nil nil nil "change" "-i")))
          (if (not (eq status 0))
              (error "g4 change -i failed with exit code %s" status))))
      (message "Changelist description updated")
      (kill-buffer)
      (when (buffer-live-p parent-buf)
        (with-current-buffer parent-buf
          (g4-status-refresh))))))

(defun g4-status-edit-cancel ()
  "Cancel editing the changelist description."
  (interactive)
  (kill-buffer)
  (message "Edit cancelled"))

(provide 'g4-status)
