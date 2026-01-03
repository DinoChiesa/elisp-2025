;;; eglot-python-pep723-extensions.el  -*- coding: utf-8; lexical-binding: t;  -*-

;; Copyright (c) 2025 Dino Chiesa
;;
;; Author: Dino Chiesa
;; Version: 20251228
;;

;;; Commentary:

;; Logic to help make eglot/basedpyright find and use the appropriate venv when
;; PEP723 markup is present in a python script. This is an early attempt. Not
;; fully verified as yet.
;;

;; To use it, invoke this within the python-mode-hook
;;  (require 'eglot-python-pep723-extensions)
;;  (epep723/setup-basedpyright-lsp-workspace-handling)
;;
;;
;; It looks for PEP723 markup in the python file, and when present, it tells
;; basedpyright-langserver to treat that file as contained within its own
;; "project", with a particular venv. This is precisely what we want - with uv,
;; each file with PEP723 dependencies gets its own magic venv. And this glue
;; code just tells elisp to use it when launching pyright / basedpyright.
;;
;; This _assumes_ the venv is managed by uv. If managed by poetry or some other
;; tool, then this glue logic won't work as is. If you use something other than
;; basedpyright for the LSP Server, like ty or pylsp or etc, then this logic
;; won't work as is.

;;; Code:

(require 'eglot)

;; (defvar epep723/original-python-contact nil
;;   "Storage for the original eglot-server-programs entry for Python.
;; Will be set on first run.")

(defun epep723/has-pep723-p (&optional _file)
  "Return non-nil if current buffer contains PEP 723 script metadata."
  (let ((case-fold-search nil))
    (save-match-data
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          ;; Search first 2048 chars for the script tag
          (re-search-forward "^# /// script" 2048 t))))))

(defun epep723/uv-get-pythonexe (file-name)
  "Ensure the uv magic invisible venv is built, then find the python
interpreter. On Windows, the result will be something like
C:/Users/me/AppData/Local/uv/cache/env-v2/filename-af3c4e/Scripts/python.exe
"
  ;; Ensure uv has built the venv at least once
  (with-temp-buffer
    (call-process "uv" nil '(t t) nil "sync" "--script" file-name))
  ;; Now that it's built, get the string.
  (with-temp-buffer
    (if (zerop (call-process "uv" nil '(t t) nil "python" "find" "--script" file-name))
        (if-let* ((pythonexe (string-trim (buffer-string)))
                  (truname (file-truename pythonexe))
                  (_ (file-exists-p truname)))
            truname
          (error "uv found venv for %s but it does not exist" file-name))
      (error "uv could not find/build venv for %s" file-name))))


(defun epep723/eglot-python-workspace-config (server)
  "Provide data to allow emacs to respond to workspace/configuration
inquiries from the LSP Server. With basedpyright, the python.pythonPath
tells it how to resolve the venv. The required workspace/configuration
may not be the same for other LSP servers, like ty or pylsp."
  ;; Ideally the base config will be customizable, rather than
  ;; hardcoded here.
  (let ((config (list :basedpyright.analysis
                      '( :autoImportCompletions :json-false
                         :typeCheckingMode "recommended")))) ;; off/basic/standard
    (if-let* ((list (eglot--managed-buffers server))
              (_ (and list (null (cdr list))))
              (buffer (car list)))
        (with-current-buffer buffer
          (if (and buffer-file-name
                   (derived-mode-p 'python-base-mode)
                   (epep723/has-pep723-p))
              (let ((python-path (epep723/uv-get-pythonexe buffer-file-name)))
                (append config `(:python (:pythonPath ,python-path))))
            config))
      config)))


;; (setq-local eglot-workspace-configuration
;;             `(:python (:pythonPath ,python-path
;;                        :analysis (:extraPaths [,site-pkgs]
;;                                   ;; This forces the external folder into the index
;;                                   :include [,(file-name-directory file) ,site-pkgs]
;;                                   :useLibraryCodeForTypes t))
;;               :basedpyright (:analysis (:extraPaths [,site-pkgs]
;;                                         :useLibraryCodeForTypes t)
;;                              :venvPath ,(file-name-directory (directory-file-name venv-root))
;;                              :venv ,(file-name-nondirectory (directory-file-name venv-root)))))


(defun epep723/setup-basedpyright-lsp-workspace-handling ()
  "Setup for PEP723 extensions for basedpyright LSP. Invoke this in your
python-mode-hook before `eglot-ensure'."
  (setq-default eglot-workspace-configuration #'epep723/eglot-python-workspace-config))


;; (defun epep723/python-script-project-finder (dir)
;;   "If the file has PEP 723 markup, treat its directory as a transient project root.
;; This allows eglot to trigger the contact-wrapper for this specific file.
;;
;; Using a distinct LSP launch/contact per file makes sense and is
;; required when each file has distinct PEP723 dependency markup."
;;   (when (and buffer-file-name (epep723/has-pep723-p))
;;     (cons 'transient dir)))
;;
;; (with-eval-after-load 'project
;;   (add-hook 'project-find-functions #'epep723/python-script-project-finder -10))



;; ;; Do not even need this.  This would be required if we needed initializationOptions, but
;; ;; that is not required for basedpyright.
;;
;; (defun epep723/python-contact-wrapper (interactive)
;;   (let* ((file (buffer-file-name))
;;          (want-per-file-lsp (and file (epep723/has-pep723-p)))
;;          (base-contact
;;           (if (functionp epep723/original-python-contact)
;;               (funcall epep723/original-python-contact interactive)
;;             epep723/original-python-contact)))
;;
;;     (if want-per-file-lsp
;;         (let* ((python-path (expand-file-name (epep723/uv-get-pythonexe file)))
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
;;   (setq epep723/original-python-contact
;;         (copy-tree (cdr (assoc '(python-mode python-ts-mode) eglot-server-programs))))
;;
;;   (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs
;;                    nil nil #'equal)
;;         #'epep723/python-contact-wrapper))


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


(provide 'eglot-python-pep723-extensions)

;;; eglot-python-pep723-extensions.el ends here
