;;; dc-cloud.el --- Cloud, OAuth, and API utilities for Dino -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Dino Chiesa
;; Package-Requires: ((emacs "29.1") (json "1.4") (s "1.12.0"))
;; Keywords: cloud, oauth, gcp, apigee

;;; Commentary:
;; Utilities for Google Cloud token management, service account auth,
;; OAuth PKCE generation, Apigee helpers, and GitHub repository creation.

;;; Code:

(require 'json)
(require 's)
(require 'subr-x)

(declare-function magit-toplevel "magit" (&optional dir))
(declare-function magit-git-repo-p "magit" (dir))
(declare-function magit-get "magit" (&rest args))
(declare-function magit-list-remote-names "magit" ())
(declare-function magit-remote-add "magit" (remote url))
(declare-function ghub-get-user-name "ghub" (&optional host))
(declare-function ghub-post "ghub" (resource &optional payload params extra-headers noerror unpaginate auth reader timeout silent))

(defun dc-cloud/get-apigee-edge-cached-token (cache-file username &optional mgmt-endpt login-endpt)
  "Parse the json CACHE-FILE and extract the token."
  (let ((cache-file (expand-file-name cache-file)))
    (and
     (file-exists-p cache-file)
     (let* ((mgmt-endpt (or mgmt-endpt "https://api.enterprise.apigee.com"))
            (login-endpt (or login-endpt "https://login.apigee.com"))
            (jsonkey (concat username "##" mgmt-endpt "##" login-endpt)))
       (let ((json (json-read-file cache-file)))
         (cdr
          (assoc 'access_token
                 (cdr (assoc (intern jsonkey) json)))))))))

(defun dc-cloud/googleapis-project-id (json-keyfile)
  "Parse the json keyfile and extract the project id."
  (and
   (file-exists-p json-keyfile)
   (cdr (assoc 'project_id (json-read-file json-keyfile)))))

(defvar dc-cloud-gcloud-auth-cache-ttl (* 8 60)
  "Number of seconds to cache a valid gcloud token. Defaults to 480 (8 minutes).")

(defvar dc-cloud--gcloud-token-cache (make-hash-table :test 'equal)
  "Hash table to store gcloud tokens.
Key is tokentype, value is (timestamp . token-string).")

(defun dc-cloud--gcloud-auth-print-token (tokentype)
  "Return output of $(gcloud auth print-{identity,access}-token).
Memoizes the result for a period of `dc-cloud-gcloud-auth-cache-ttl' ONLY if
the token starts with \"ya29.\" (for access tokens) or \"ey\" (for
identity tokens)."
  (let* ((now (float-time))
         (cache-entry (gethash tokentype dc-cloud--gcloud-token-cache))
         (cache-age (if cache-entry (- now (car cache-entry)) 1.0e+INF)))
    (if (and cache-entry (< cache-age dc-cloud-gcloud-auth-cache-ttl))
        ;; Cache Hit: Return the stored string
        (cdr cache-entry)
      ;; Cache Miss (or expired): Run the expensive shell command
      (let* ((gcloud-pgm "gcloud")
             (app-default "application-default ")
             (command-string (format "%s auth %s print-%s-token"
                                     gcloud-pgm app-default tokentype))
             (output (replace-regexp-in-string "\n$" "" (shell-command-to-string command-string)))
             (lines (split-string output "\n"))
             (token (car (last lines))))
        ;; Conditional Memoization: Only store in hash table if valid
        (when (and (stringp token)
                   (or (string-prefix-p "ya29." token)
                       (string-prefix-p "ey" token))) ;; Check for JWTs too
          (puthash tokentype (cons now token) dc-cloud--gcloud-token-cache))
        token))))

(defun dc-cloud/gcloud-auth-print-access-token ()
  "Return output of $(gcloud auth print-access-token)."
  (interactive)
  (let ((tok (dc-cloud--gcloud-auth-print-token "access")))
    (when (called-interactively-p 'interactive)
      (kill-new tok)
      (message "%s" tok))
    tok))

(defun dc-cloud/gcloud-auth-print-identity-token ()
  "Return output of $(gcloud auth print-identity-token)."
  (interactive)
  (let ((tok (dc-cloud--gcloud-auth-print-token "identity")))
    (when (called-interactively-p 'interactive)
      (kill-new tok)
      (message "%s" tok))
    tok))

(defun dc-cloud/googleapis-token-for-sa (json-keyfile)
  "Generate and return a new OAuth token for googleapis.com for a service account,
given a JSON-KEYFILE that contains a downloaded JSON Key. This
method of authenticating is a Bad Idea in general."
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (let ((project-id (dc-cloud/googleapis-project-id json-keyfile)))
    (and
     project-id
     (let* ((command-string
             (format "node ~/dev/apigee-edge-js-examples/getToken.js -J %s -o %s -v" json-keyfile project-id))
            (output (replace-regexp-in-string "\n$" "" (shell-command-to-string command-string)))
            (lines (split-string output "\n")))
       (car (last lines))))))

(defun dc-cloud/copy-value-from-key-into-killring (key)
  "Extract a value from the secure keystore into the killring, using KEY."
  (interactive "sKey: ")
  (let ((buf (get-buffer "pwds.txt.gpg"))
        (regexp (concat "^[ \t]*\\b" key "\\b.+ \\([^ \r\n]+\\)$")))
    (if buf
        (let ((result
               (with-current-buffer buf
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (point-min))
                     (if (re-search-forward regexp nil t)
                         (or (match-string 1) 1)
                       2))))))
          (cond
           ((numberp result)
            (message "Not found")
            (kill-new "--"))
           (t
            (kill-new result)
            (message "Found"))))
      (kill-new "xx")
      (message "No buffer pwds.txt.gpg"))))

(defun dc-cloud/buganizer-open ()
  "Convert buganizer ticket to URL and open in browser."
  (interactive)
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'word))))
    (when bounds
      (let* ((raw-text (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (ticket-id (s-chop-prefixes '("b" "b/") raw-text))
             (url (concat "https://b.corp.google.com/issues/" ticket-id)))
        (browse-url url)))))

(defun dc-cloud/pkce-s256-code-challenge (verifier)
  "Generate a S256 code challenge from a given VERIFIER: produce
a base64-url-encoded version of a SHA256 of the given verifier."
  (let ((hash (secure-hash 'sha256 verifier nil nil t)))
    (replace-regexp-in-string
     "=" ""
     (replace-regexp-in-string
      "/" "_"
      (replace-regexp-in-string "+" "-" (base64-encode-string hash t))))))

(defun dc-cloud/create-github-repo (&optional dir)
  "Create a GitHub repository for the current project.
If DIR is nil (or when called interactively with a prefix arg),
prompt for the directory. Otherwise, use the current `magit-toplevel`."
  (interactive
   (list (if current-prefix-arg
             (read-directory-name "Local repo directory: "
                                  (or (and (fboundp 'magit-toplevel) (magit-toplevel default-directory))
                                      default-directory))
           nil)))
  (let* ((default-directory (or dir (and (fboundp 'magit-toplevel) (magit-toplevel default-directory)) default-directory))
         (repo-name (file-name-nondirectory (directory-file-name default-directory)))
         (gh-user (and (fboundp 'magit-get) (magit-get "github.user")))
         (git-user (and (fboundp 'magit-get) (magit-get "user.name")))
         (user (and (fboundp 'ghub-get-user-name) (ghub-get-user-name "api.github.com"))))
    (unless (and (fboundp 'magit-git-repo-p) (magit-git-repo-p default-directory))
      (user-error "Directory '%s' is not a Git repository. Run 'magit-init' first" default-directory))
    (unless gh-user
      (user-error "Your GitHub username is not set. Run: git config --global github.user <your-handle>"))
    (unless git-user
      (user-error "Git name not set! Run: git config --global user.name \"Your Name\""))
    (unless user
      (user-error "Could not determine GitHub username. Check your git config or auth-source (~/.authinfo?)"))
    (if (and (fboundp 'magit-list-remote-names) (member "origin" (magit-list-remote-names)))
      (user-error "Remote 'origin' already exists in this repository")
      (condition-case err
          (progn
            (ghub-post "/user/repos" `((name . ,repo-name)))
            (magit-remote-add "origin" (format "git@github.com:%s/%s.git" user repo-name))
            (message "Successfully created and linked GitHub repo: %s/%s" user repo-name))
        (error (message "GitHub API Error: %s" (error-message-string err)))))))

;; Backward compatibility aliases

(provide 'dc-cloud)

;;; dc-cloud.el ends here