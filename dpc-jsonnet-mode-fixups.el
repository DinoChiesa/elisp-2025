;;; dpc-jsonnet-mode-fixups.el --- some changes for jsonnet-mode
;;
;; Author: Dino Chiesa
;; Created: Wednesday,  1 January 2025, 02:49
;; Package-Requires: (jsonnet-mode.el)
;; URL:
;; X-URL:
;; Version: 2024.12.31
;; Keywords: jsonnet
;; License: New BSD

;;; Commentary:

;; To use:
;;   (require 'dpc-jsonnet-mode-fixups)
;;

;; jsonnet-mode uses `call-process' to invoke the jsonnet program. This does not work
;; for .jsonnet files that are visited over TRAMP.
;; I filed a PR https://github.com/tminor/jsonnet-mode/pull/34
;; but who knows if it will be merged. In the meantime, this will work.

(require 'jsonnet-mode)

(defun dino/jsonnet-eval-buffer ()
  "Run jsonnet with the path of the current file. This also works for
remote files accessed over TRAMP."
  (interactive)
  (let ((file-to-eval (file-truename (buffer-file-name)))
        (search-dirs jsonnet-library-search-directories)
        (output-buffer-name "*jsonnet output*"))
    (save-some-buffers (not compilation-ask-about-save)
                       (let ((directories (cons (file-name-directory file-to-eval)
                                                search-dirs)))
                         (lambda ()
                           (member (file-name-directory (file-truename (buffer-file-name)))
                                   directories))))
    (let ((cmd jsonnet-command)
          (args (append jsonnet-command-options
                        (cl-loop for dir in search-dirs
                                 collect "-J"
                                 collect dir)
                        (list (file-local-name file-to-eval))))
          (dir-to-use-for-process-file (file-name-directory (buffer-file-name))))
      (with-current-buffer (get-buffer-create output-buffer-name)
        (setq buffer-read-only nil)
        (erase-buffer)
        (let ((default-directory dir-to-use-for-process-file))
          (if (zerop (apply #'process-file cmd nil t nil args))
              (progn
                (when (fboundp 'json-mode)
                  (json-mode))
                (view-mode))
            (compilation-mode nil)))
        (goto-char (point-min))
        (display-buffer (current-buffer) '(nil (inhibit-same-window . t)))))))

(define-key jsonnet-mode-map (kbd "C-c C-c") 'dino/jsonnet-eval-buffer)


(defun dpc/jsonnet-lsp (&optional interactive)
  "Computes the command to use for the jsonnet-language-server
for eglot. `eglot-server-programs' stores an alist of
(MAJOR-MODE . CONTACT) pairs. CONTACT can be a function of a
single argument - non-nil if the connection was requested
interactively (e.g. from the ‘eglot’ command), and nil if it
wasn’t (e.g. from ‘eglot-ensure’), and it should return one of
the forms of CONTACT defined by that alist. The most common case
is to return a list of strings (PROGRAM [ARGS...]) . For lsp,
PROGRAM is called with ARGS and is expected to serve LSP requests
over the standard input/output channels.

This function dynamically sets the jpath for
jsonnet-language-server based the value of
`jsonnet-library-search-directories', which specifies a list of
search dirs.

This variable can be set in any way, including via .dir-locals.el.
"
  (let ((search-dirs jsonnet-library-search-directories)
        (cmd "jsonnet-language-server"))
    (if (and search-dirs (> (length search-dirs) 0))
        (append (list cmd)
                (cl-loop for dir in search-dirs
                         collect "-J"
                         collect dir)
                nil)
      (list cmd))))



;; (defun dino-fixup-jsonnet-command ()
;;   "For some reason, When I put this in the =dino-jsonnet-package-config= fn
;; I get an error:
;; Error (use-package): jsonnet-mode/:config: Symbol’s function definition
;;    is void: \(setf\ flycheck-checker-get\)
;;
;; Putting it in this fn is an attempt to isolate and avoid the problem.
;; Jeez.
;; "
;;   (interactive)
;;
;;
;;   ;; (flycheck-define-checker jsonnet-for-dino
;;   ;;   "A Python syntax and style checker using flake8"
;;   ;;          :command ("flake8"
;;   ;;                     "--format=default"
;;   ;;                     (config-file "--config" flycheck-flake8rc)
;;   ;;                     (option "--max-complexity" flycheck-flake8-maximum-complexity nil
;;   ;;                       flycheck-option-int)
;;   ;;                     (option "--max-line-length" flycheck-flake8-maximum-line-length nil
;;   ;;                       flycheck-option-int)
;;   ;;                     "-")
;;   ;;          :standard-input t
;;   ;;          :error-filter fix-flake8
;;   ;;          :error-patterns
;;   ;;          ((warning line-start
;;   ;;             "stdin:" line ":" (optional column ":") " "
;;   ;;             (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;   ;;             (message (one-or-more not-newline))
;;   ;;             line-end))
;;   ;;          :next-checkers ((t . python-pylint))
;;   ;;          :modes python-mode)
;;
;;   ;;         ;; replace flake8 with new chaining one from above
;;   ;;        (setq flycheck-checkers (cons 'python-flake8-chain (delq 'python-flake8 flycheck-checkers)))
;;
;;
;;   ;; ALL I WANT TO DO IS REDEFINE THE COMMAND for AN EXISTING CHECKER BUT IT SEEMS IMPOSSIBLE.
;;   ;; ALSO, IT MAY BE IRRELEVANT BECAUSE I AM USING EGLOT AND TURNING OFF FLYCHECK.
;;
;;   ;; testing for the function in an attempt to diagnose the weird error.
;;   (if (functionp 'flycheck-checker-get)
;;       (setf (flycheck-checker-get 'jsonnet 'command)
;;             `("jsonnet"
;;               (option-list "-J"
;;                            (eval (or jsonnet-library-search-directories flycheck-jsonnet-include-paths)))
;;               (eval
;;                (or jsonnet-command-options flycheck-jsonnet-command-args))
;;               source-inplace)))
;;   )

(provide 'dpc-jsonnet-mode-fixups)

;;; dpc-jsonnet-mode-fixups.el ends here
