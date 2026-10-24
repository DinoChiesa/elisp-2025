;;; dc-magit-gerrit.el --- Custom Magit extensions for Gerrit -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Dino Chiesa
;; Keywords: git magit gerrit

;;; Commentary:

;; Custom Magit extensions for Gerrit code-review workflows:
;; - completions sorting override
;; - fetching Gerrit changes into local branches
;; - refetching / resetting to latest patchsets
;; - pushing to Gerrit's master review queue
;; - Magit transient menu integrations

;;; Code:

(require 'magit)
(require 'dpc-sane-sorting)

;; Override some magit things so that branches get sorted
;; alphabetically. This works only because I am _redefining
;; some functions from magit-base.el !
;; See https://github.com/magit/magit/discussions/5390

(defun dc-mg/completing-read
    (prompt choices &optional predicate require-match initial-input hist def)
  "Override for magit wrapper for the `completing-read' function.

Specifically, it modifies the behavior when selecting a remote branch to
push to, as indicated by the PROMPT string.

In this case, the choices passed to `magit-completing-read-function' are
pre-sorted; the origin and potentially other remotes appear at the top
of the list. Without proper sort metadata, when using icomplete, the
options get sorted later by minibuffer, by length, which ... is super
dumb. This custom completing-read fn keeps the previous sort order.

To use it:

  (setq magit-completing-read-function
        #'dc-mg/completing-read)"
  (setq choices
        (let* ((case-fold-search nil)
               (category (if (string-match "Push [^[:space:]]+ to" prompt)
                             'unsorted
                           'sorted-sanely)))
          (dpc-ss-completion-fn choices category)))

  (let ((ivy-sort-functions-alist nil))
    (completing-read prompt
                     choices
                     predicate require-match
                     initial-input hist def)))

(setq magit-completing-read-function #'dc-mg/completing-read)

(defun dc-mg--remote ()
  "Determine which Git remote to use for Gerrit."
  (let ((remotes (magit-list-remotes)))
    (cond
     ((member "origin" remotes) "origin")
     ((member "gerrit" remotes) "gerrit")
     ((magit-primary-remote))
     ((car remotes))
     (t "origin"))))

(defun dc-mg--extract-change-num (input)
  "Extract numeric change number from INPUT (string, number, ref, or branch name)."
  (cond
   ((numberp input) (number-to-string input))
   ((stringp input)
    (cond
     ((string-match "refs/changes/[0-9]\\{2\\}/\\([0-9]+\\)" input)
      (match-string 1 input))
     ((string-match "\\(?:change\\|review\\|\\+\\)[/-]?\\([0-9]+\\)" input)
      (match-string 1 input))
     ((string-match "\\`[ \t]*\\([0-9]+\\)[ \t]*\\'" input)
      (match-string 1 input))
     (t (user-error "Could not extract Gerrit change number from: %s" input))))
   (t (user-error "Invalid change number input: %s" input))))

(defun dc-mg--latest-patchset-ref (remote change-num)
  "Return the latest patchset ref on REMOTE for CHANGE-NUM.
For example, \"refs/changes/58/641358/17\"."
  (let* ((num (dc-mg--extract-change-num change-num))
         (shard (format "%02d" (% (string-to-number num) 100)))
         (pattern (format "refs/changes/%s/%s/*" shard num))
         (lines (magit-git-lines "ls-remote" remote pattern))
         (max-ps 0)
         (latest-ref nil))
    (dolist (line lines)
      (when (string-match (format "refs/changes/%s/%s/\\([0-9]+\\)\\'" shard num) line)
        (let ((ps (string-to-number (match-string 1 line))))
          (when (> ps max-ps)
            (setq max-ps ps
                  latest-ref (format "refs/changes/%s/%s/%d" shard num ps))))))
    (or latest-ref
        (user-error "No patchset refs found for change %s on remote '%s'" num remote))))

(defun dc-mg--guess-current-change-num ()
  "Infer the Gerrit change number from current branch name or upstream."
  (let ((branch (magit-get-current-branch)))
    (or
     (and branch
          (string-match "\\(?:change\\|review\\)[/-]?\\([0-9]+\\)" branch)
          (match-string 1 branch))
     (and branch
          (let ((upstream (magit-get "branch" branch "merge")))
            (and upstream
                 (string-match "refs/changes/[0-9]\\{2\\}/\\([0-9]+\\)" upstream)
                 (match-string 1 upstream)))))))

(defun dc-mg--assert-clean ()
  "Raise an error if the working tree or index has uncommitted changes."
  (when (magit-anything-modified-p)
    (user-error "Uncommitted changes present; please stash or commit first")))

(defun dc-mg/fetch-gerrit-change (change-num)
  "Prompt for Gerrit CHANGE-NUM, fetch latest patchset, and create a branch."
  (interactive
   (list (read-string "Gerrit change number: ")))
  (let* ((num (dc-mg--extract-change-num change-num))
         (remote (dc-mg--remote))
         (ref (dc-mg--latest-patchset-ref remote num))
         (branch (format "change-%s" num)))
    (dc-mg--assert-clean)
    (message "Fetching %s from %s..." ref remote)
    (magit-run-git "fetch" remote ref)
    (if (magit-branch-p branch)
        (if (y-or-n-p (format "Branch '%s' already exists. Switch and reset to FETCH_HEAD? " branch))
            (progn
              (magit-run-git "checkout" branch)
              (magit-reset-hard "FETCH_HEAD"))
          (user-error "Aborted"))
      (magit-run-git "checkout" "-b" branch "FETCH_HEAD"))
    (message "Checked out %s at %s" branch ref)))

(defun dc-mg/fetch-refetch-gerrit-change (&optional change-num)
  "Fetch latest patchset for CHANGE-NUM and hard-reset the current branch.
If CHANGE-NUM is omitted, infers it from the current branch name or upstream.
With a prefix argument (\\[universal-argument]), prompts for the change number."
  (interactive
   (let ((guessed (dc-mg--guess-current-change-num)))
     (list (if (or current-prefix-arg (null guessed))
               (read-string (if guessed
                                (format "Gerrit change number (default %s): " guessed)
                              "Gerrit change number: ")
                            nil nil guessed)
             guessed))))
  (let* ((num (dc-mg--extract-change-num change-num))
         (remote (dc-mg--remote))
         (ref (dc-mg--latest-patchset-ref remote num)))
    (dc-mg--assert-clean)
    (message "Fetching %s from %s and resetting..." ref remote)
    (magit-run-git "fetch" remote ref)
    (magit-reset-hard "FETCH_HEAD")
    (message "Reset %s to %s" (magit-get-current-branch) ref)))

(defun dc-mg/push-to-gerrit-master ()
  "Push the current HEAD to Gerrit's master review queue."
  (interactive)
  (magit-git-command-topdir "git push origin HEAD:refs/for/master"))

;; Inject the custom push command into the existing Magit Push ('P') transient menu
(with-eval-after-load 'magit
  (transient-append-suffix 'magit-push "p"
    '("g" "Push to Gerrit (master)" dc-mg/push-to-gerrit-master)))

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-fetch "u"
    '("c" "Fetch Gerrit change into new branch" dc-mg/fetch-gerrit-change))
  (transient-append-suffix 'magit-fetch "c"
    '("g" "Fetch Gerrit ref & reset hard" dc-mg/fetch-refetch-gerrit-change)))

(provide 'dc-magit-gerrit)
;;; dc-magit-gerrit.el ends here
