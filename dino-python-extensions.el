;;; dino-python-extensions.el  -*- coding: utf-8; lexical-binding: t;  -*-

;; Copyright (c) 2025 Dino Chiesa
;;
;; Author: Dino Chiesa
;; Version: 20251228
;;

;;; Commentary:

;; Logic to help make python editing more pleasant.
;;
;; - LSP workspace configuraiton for eglot + basedpyright-langserver to allow
;;   the LSP to use the appropriate venv when PEP723 markup is present in a
;;   python script. This is an early attempt. Not fully verified as yet.
;;   basedpyright seems a bit over-zealous and may ingest more than just one
;;   file despite the PEP723 markup.  Expect more changes in the various
;;   language servers to support PEP723.
;;
;;   It looks for PEP723 markup in the python file, and when present, it tells
;;   basedpyright-langserver to treat that file as contained within its own
;;   "project", with a particular venv. This is precisely what we want - with uv,
;;   each file with PEP723 dependencies gets its own magic venv. And this glue
;;   code just tells elisp to use it when launching pyright / basedpyright.
;;
;;   This _assumes_ the venv is managed by uv. If managed by poetry or some other
;;   tool, then this glue logic won't work as is. If you use something other than
;;   basedpyright for the LSP Server, like ty or pylsp or etc, then this logic
;;   won't work as is.
;;
;; - a function, `dcpe/basedpyright-check-command`, to automatically set the
;;   python-check-command for use with basedpyright, for `python-check`.
;;
;; - FUTURE - similar work to set the python-check-command for use with ty language server.
;;
;; - others?
;;
;;
;; Future enhancement would include allowing the user to specify the use of
;; poetry vs uv, etc for thevenv manager.
;;
;;
;; To use it, invoke this within the python-mode-hook
;;  (require 'dino-python-extensions)
;;  (dcpe/setup-basedpyright-lsp-workspace-handling)
;;  (setq python-check-custom-command nil)
;;  (setq python-check-command (dcpe/basedpyright-check-command))
;;
;;


;;; Code:

(require 'eglot)

;; (defvar dcpe/original-python-contact nil
;;   "Storage for the original eglot-server-programs entry for Python.
;; Will be set on first run.")

(defun dcpe/has-pep723-p (&optional _file)
  "Return non-nil if current buffer contains PEP 723 script metadata."
  (let ((case-fold-search nil))
    (save-match-data
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          ;; Search first 2048 chars for the script tag
          (re-search-forward "^# /// script" 2048 t))))))

(defun dcpe/uv-get-pythonexe (file-name)
  "Ensure that uv magic invisible venv is built, then find the python
interpreter. On Windows, the result will be something like
C:/Users/me/AppData/Local/uv/cache/env-v2/filename-af3c4e/Scripts/python.exe
"
  ;; Ensure uv has built the venv at least once
  (if (file-exists-p (executable-find "uv"))
      (progn
        (with-temp-buffer
          (call-process "uv" nil '(t t) nil "sync" "--script" file-name))
        ;; Now that it's built, get the string.
        (with-temp-buffer
          (if (zerop (call-process "uv" nil '(t t) nil "python" "find" "--script" file-name))
              (if-let* ((pythonexe (string-trim (buffer-string)))
                        (truname (file-truename pythonexe))
                        (_ (file-exists-p truname)))
                  truname
                (error "uv found venv python for %s but it does not exist" file-name))
            (error "uv could not find/build venv python for %s" file-name))))
    (error "emacs could not find uv. is it installed?")))

(defvar dcpe/basedpyright-base-config
  (list :basedpyright.analysis
        '( :autoImportCompletions :json-false
           :typeCheckingMode "standard")) ;; off/basic/standard/recommended
  "Default base configuration data for Python LSP workspace/configuration requests,
used by `dcpe/basedpyright-workspace-config`.

This list is tailored for basedpyright, specifying analysis options.")

(defun dcpe/basedpyright-workspace-config (server)
  "Provide data to allow emacs to respond to workspace/configuration
inquiries from the basedpyright LSP Server. With basedpyright, the
python.pythonPath tells it how to resolve the venv. The required
workspace/configuration may not be the same for other LSP servers, like
ty or pylsp."
  (let ((config dcpe/basedpyright-base-config))
    (if-let* ((list (eglot--managed-buffers server))
              (_ (and list (null (cdr list))))
              (buffer (car list)))
        (with-current-buffer buffer
          (if (and buffer-file-name
                   (derived-mode-p 'python-base-mode)
                   (dcpe/has-pep723-p))
              (let ((python-path (dcpe/uv-get-pythonexe buffer-file-name)))
                (append config `(:python (:pythonPath ,python-path))))
            config))
      config)))

(defun dcpe/setup-basedpyright-lsp-workspace-handling ()
  "Setup for PEP723 extensions for basedpyright LSP. Invoke this in your
python-mode-hook before `eglot-ensure'."
  (setq-default eglot-workspace-configuration #'dcpe/basedpyright-workspace-config))


;; (defun dcpe/python-script-project-finder (dir)
;;   "If the file has PEP 723 markup, treat its directory as a transient project root.
;; This allows eglot to trigger the contact-wrapper for this specific file.
;;
;; Using a distinct LSP launch/contact per file makes sense and is
;; required when each file has distinct PEP723 dependency markup."
;;   (when (and buffer-file-name (dcpe/has-pep723-p))
;;     (cons 'transient dir)))
;;
;; (with-eval-after-load 'project
;;   (add-hook 'project-find-functions #'dcpe/python-script-project-finder -10))



;; ;; Do not even need this.  This would be required if we needed initializationOptions, but
;; ;; that is not required for basedpyright.
;;
;; (defun dcpe/python-contact-wrapper (interactive)
;;   (let* ((file (buffer-file-name))
;;          (want-per-file-lsp (and file (dcpe/has-pep723-p)))
;;          (base-contact
;;           (if (functionp dcpe/original-python-contact)
;;               (funcall dcpe/original-python-contact interactive)
;;             dcpe/original-python-contact)))
;;
;;     (if want-per-file-lsp
;;         (let* ((python-path (expand-file-name (dcpe/uv-get-pythonexe file)))
;;                (full-venv-path (file-name-parent-directory (file-name-parent-directory python-path)))
;;                (where-venvs-reside (file-name-parent-directory full-venv-path))
;;                (shortname-of-the-actual-venv (file-name-nondirectory (directory-file-name full-venv-path)))
;;                ;;(site-pkgs (file-name-as-directory (expand-file-name "Lib/site-packages" venv-root)))
;;                ;;(unique-id (concat "--eglot-unique-id=" (file-name-nondirectory file)))
;;                (modified-contact
;;                 (append base-contact
;;                         (list :initializationOptions
;;                               ;; I think for basedpyright, providing pythonPath here is ineffectual.
;;                               ;; From https://github.com/DetachHead/basedpyright/blob/a4030c650bd3f027a4f15bedc74ab4789437bd2f/packages/pyright-internal/src/types.ts#L36
;;                               ;; The only options supported within initializationOptions are diagnosticMode and disablePullDiagnostics . But even those are not documented clearly, so even those seem suspect.
;;                               (list :python (list :pythonPath python-path
;;                                                   :venvPath  (directory-file-name where-venvs-reside)
;;                                                   :venv shortname-of-the-actual-venv
;;                                                   )
;;                                     ;; From this discussion https://github.com/DetachHead/basedpyright/issues/581#issuecomment-2324853696
;;                                     ;; it seems like both venvPath and venv are supported, and according to the documentation here:
;;                                     ;; venvPath is "Path to folder with subdirectories that contain virtual environments."
;;                                     ;; Which means it is not the venv, but the parent of where venvs are found.  And .... I guess venv is
;;                                     ;; the short name of the actual venv.  But this is all inferred from a few comments. Not from code
;;                                     ;; and certainly not from documentation.
;;                                     :basedpyright (list :venvPath  (directory-file-name where-venvs-reside)
;;                                                         :venv shortname-of-the-actual-venv
;;                                                         )
;;
;;                                     )))))
;;           modified-contact)
;;       base-contact)))
;;
;;
;; (with-eval-after-load 'eglot
;;   (setq dcpe/original-python-contact
;;         (copy-tree (cdr (assoc '(python-mode python-ts-mode) eglot-server-programs))))
;;
;;   (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs
;;                    nil nil #'equal)
;;         #'dcpe/python-contact-wrapper))


;; ;; delete ALL:
;; (assq-delete-all '(python-mode python-ts-mode) eglot-server-programs)

;; ;; delete FIRST:
;; (let ((key '(python-mode python-ts-mode)))
;;   (setq eglot-server-programs
;;         (let ((found nil))
;;           (cl-remove-if (lambda (item)
;;                           (if (and (not found) (equal (car item) key))
;;                               (setq found t) ;; Mark as found and return true to remove
;;                             nil))            ;; Keep everything else
;;                         eglot-server-programs))))


(defun dcpe/find-basedpyright ()
  "find basedpyright on the current machine."
  (if (eq system-type 'windows-nt)
      (with-temp-buffer
        (if (zerop (call-process "where.exe" nil '(t t) nil "basedpyright.ps1" ))
            (string-trim (buffer-string))))
    (executable-find "basedpyright")))


  (defun dcpe/rename-python-check-buffer ()
    "Rename the Python check buffer to omit the full command. Without this,
the name of the compilation buffer can be over 180 characters. Not helpful."
    (let ((current-name (buffer-name)))
      ;; Check if this is a python-check buffer
      (when (string-match "Python check: .* \"\\([^\"]+\\)\"\\*$" current-name)
        (let* ((extracted-file (match-string 1 current-name))
               (new-name (format "*Python check basedpyright: %s*" extracted-file)))
          ;; Clear out old buffers with the same name to prevent <2>, <3>
          (when (and (get-buffer new-name) (not (eq (current-buffer) (get-buffer new-name))))
            (let ((kill-buffer-query-functions nil))
              (kill-buffer new-name)))
          (rename-buffer new-name)))))

(defun dcpe/basedpyright-check-command ()
  "Get the python check command for the current file. This will
return something sensible, if uv and basedpyright are installed, whether or
not PEP723 markup is present.

If PEP723 markup, then find the right pythonexe and give that to the
basedpyright command. If no markup, or if uv does not return the python
path, but basedpyright is installed, then just run basedpyright, and let
it figure out the venv.

If no basedpyright, then fall back to *ruff check*, without checking for
existence."
  (if-let* ((basedpyright (dcpe/find-basedpyright)))
      (progn
        (add-hook 'compilation-mode-hook #'dcpe/rename-python-check-buffer)
        (with-eval-after-load 'compile
              (add-to-list 'compilation-error-regexp-alist-alist
                           '(basedpyright
                             "^[[:space:]]+\\([A-Za-z]:[^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                             1 2 3))
              (add-to-list 'compilation-error-regexp-alist 'basedpyright))
        (if-let* ((_ (dcpe/has-pep723-p))
                  (python-path (dcpe/uv-get-pythonexe buffer-file-name)))
            (format (if (eq system-type 'windows-nt)
                      ;; 20260103-1802
                      ;; I found that using -Command on Windows with pwsh.exe is
                      ;; essential to correct behavior.  When I use -File, the
                      ;; compilation hangs forever.
                      "pwsh.exe -NonInteractive -Command %s --pythonpath %s "
                    "%s --pythonpath %s ")
                  basedpyright python-path)
        basedpyright))
    "ruff check"))

(provide 'dino-python-extensions)

;;; dino-python-extensions.el ends here
