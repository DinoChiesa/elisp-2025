;;; dc-python-extensions.el  -*- coding: utf-8; lexical-binding: t;  -*-

;; Copyright © 2025-2026 Dino Chiesa
;;
;; Author: Dino Chiesa
;; Version: 20260306
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
;; - a function, `dc-pe/basedpyright-check-command`, to automatically set the
;;   python-check-command for use with basedpyright, for `python-check`.
;;
;; - FUTURE - similar work to set the python-check-command for use with ty language server.
;;
;; - others?
;;
;;
;; Future enhancement would include allowing the user to specify the use of
;; poetry vs uv, etc for the venv manager.
;;
;;
;; To use it, invoke this within the python-mode-hook
;;  (require 'dc-python-extensions)
;;  (dc-pe/setup-basedpyright-lsp-workspace-handling)
;;  (setq python-check-custom-command nil)
;;  (setq python-check-command (dc-pe/basedpyright-check-command))
;;
;;


;;; Code:

(require 'eglot)
(require 'dpc-gemini)
(require 'json)

;; (defvar dc-pe/original-python-contact nil
;;   "Storage for the original eglot-server-programs entry for Python.
;; Will be set on first run.")

(defun dc-pe/has-pep723-p (&optional _file)
  "Return non-nil if current buffer contains PEP 723 script metadata."
  (let ((case-fold-search nil))
    (save-match-data
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          ;; Search first 2048 chars for the script tag
          (re-search-forward "^# /// script" 2048 t))))))

(defun dc-pe/inject-pep723-metadata ()
  "Inject a PEP 723 script metadata block at the top of the buffer.
If a block already exists, do nothing."
  (interactive)
  (if (dc-pe/has-pep723-p)
      (message "PEP 723 block already exists.")
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "^#!")
        (forward-line 1))
      (let* ((dependencies (dc-pe/infer-dependencies))
             ;; Trim and prefix internal newlines with "# "
             (formatted (replace-regexp-in-string "\n" "\n# " (string-trim dependencies))))
        (insert "# /// script\n"
                "# requires-python = \">=3.13\"\n"
                "# dependencies = "
                formatted
                "\n# ///\n\n")
        (message "Injected PEP 723 metadata.")))))

(defun dc-pe/add-pep723-dependency (dependency)
  "Add DEPENDENCY in the PEP 723 script metadata block at the top of the file.
This uses a temporary file to avoid forcing a save of the current buffer,
allowing the change to be undone within Emacs."
  (interactive "sDependency to add: ")
  (unless (derived-mode-p 'python-mode 'python-ts-mode)
    (error "Current buffer is not in python-mode or python-ts-mode"))
  (let ((uv (executable-find "uv"))
        (temp-file (make-temp-file "dc-pe-uv-" nil ".py")))
    (unless uv
      (error "Could not find 'uv' executable"))
    (unwind-protect
        (progn
          ;; Write current buffer content (including unsaved changes) to temp file
          (write-region (point-min) (point-max) temp-file nil 'quiet)
          ;; Let uv modify the temp file
          (if (zerop (call-process uv nil "*uv-output*" t "add" "--script" temp-file dependency))
              (progn
                ;; Replace buffer content with modified temp file content.
                ;; This is undo-friendly and preserves markers.
                (insert-file-contents temp-file nil nil nil t)
                (message "Added dependency '%s'" dependency))
            (error "Failed to add dependency. See *uv-output* buffer for details")))
      ;; Ensure cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun dc-pe/uv-get-pythonexe (file-name)
  "Ensure that uv magic invisible venv is built, then find the python
interpreter. On Windows, the result will be something like
C:/Users/me/AppData/Local/uv/cache/env-v2/filename-af3c4e/Scripts/python.exe"
  (let ((uv (executable-find "uv")))
    (unless uv
      (error "Could not find 'uv' executable"))
    ;; Ensure uv has built the venv at least once
    (with-temp-buffer
      (call-process uv nil '(t t) nil "sync" "--script" file-name))
    ;; Now that it's built, get the string.
    (with-temp-buffer
      (if (zerop (call-process uv nil '(t t) nil "python" "find" "--script" file-name))
          (if-let* ((pythonexe (string-trim (buffer-string)))
                    (truname (file-truename pythonexe))
                    (_ (file-exists-p truname)))
              truname
            (error "uv found venv python for %s but it does not exist" file-name))
        (error "uv could not find/build venv python for %s" file-name)))))

(defvar dc-pe/basedpyright-base-config
  (list :basedpyright.analysis
        '( :autoImportCompletions :json-false
           :typeCheckingMode "standard")) ;; off/basic/standard/recommended
  "Default base configuration data for Python LSP workspace/configuration requests,
used by `dc-pe/basedpyright-workspace-config`.

This list is tailored for basedpyright, specifying analysis options.")

(defun dc-pe/basedpyright-workspace-config (server)
  "Provide data to allow emacs to respond to workspace/configuration
inquiries from the basedpyright LSP Server. With basedpyright, the
python.pythonPath tells it how to resolve the venv. The required
workspace/configuration may not be the same for other LSP servers, like
ty or pylsp."
  (let ((config dc-pe/basedpyright-base-config))
    (if-let* ((list (eglot--managed-buffers server))
              (_ (and list (null (cdr list))))
              (buffer (car list)))
        (with-current-buffer buffer
          (if (and buffer-file-name
                   (derived-mode-p 'python-base-mode)
                   (dc-pe/has-pep723-p))
              (let ((python-path (dc-pe/uv-get-pythonexe buffer-file-name)))
                (append config `(:python (:pythonPath ,python-path))))
            config))
      config)))

(defun dc-pe/setup-basedpyright-lsp-workspace-handling ()
  "Setup for PEP723 extensions for basedpyright LSP. Invoke this in your
python-mode-hook before `eglot-ensure'."
  (setq-default eglot-workspace-configuration #'dc-pe/basedpyright-workspace-config))


;; (defun dc-pe/python-script-project-finder (dir)
;;   "If the file has PEP 723 markup, treat its directory as a transient project root.
;; This allows eglot to trigger the contact-wrapper for this specific file.
;;
;; Using a distinct LSP launch/contact per file makes sense and is
;; required when each file has distinct PEP723 dependency markup."
;;   (when (and buffer-file-name (dc-pe/has-pep723-p))
;;     (cons 'transient dir)))
;;
;; (with-eval-after-load 'project
;;   (add-hook 'project-find-functions #'dc-pe/python-script-project-finder -10))



;; ;; Do not even need this.  This would be required if we needed initializationOptions, but
;; ;; that is not required for basedpyright.
;;
;; (defun dc-pe/python-contact-wrapper (interactive)
;;   (let* ((file (buffer-file-name))
;;          (want-per-file-lsp (and file (dc-pe/has-pep723-p)))
;;          (base-contact
;;           (if (functionp dc-pe/original-python-contact)
;;               (funcall dc-pe/original-python-contact interactive)
;;             dc-pe/original-python-contact)))
;;
;;     (if want-per-file-lsp
;;         (let* ((python-path (expand-file-name (dc-pe/uv-get-pythonexe file)))
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
;;   (setq dc-pe/original-python-contact
;;         (copy-tree (cdr (assoc '(python-mode python-ts-mode) eglot-server-programs))))
;;
;;   (setf (alist-get '(python-mode python-ts-mode) eglot-server-programs
;;                    nil nil #'equal)
;;         #'dc-pe/python-contact-wrapper))


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


(defun dc-pe/find-basedpyright ()
  "find basedpyright on the current machine."
  (if (eq system-type 'windows-nt)
      (with-temp-buffer
        (if (zerop (call-process "where.exe" nil '(t t) nil "basedpyright.ps1" ))
            (string-trim (buffer-string))))
    (executable-find "basedpyright")))


(defvar-local dc-pe/flymake-gpylint-process nil)

(defun dc-pe/flymake-gpylint (report-fn &rest _args)
  "Flymake backend for gpylint.
This pipes the current buffer content to gpylint via stdin
and parses the output into diagnostics. Using --from-stdin
allows gpylint to report the correct filename in its output."
  (let ((gpylint (executable-find "gpylint")))
    (unless gpylint
      (error "Cannot find gpylint executable"))
    (dc-pe/--flymake-run-pylint report-fn (list gpylint "--mode=base"))))

(defun dc-pe/flymake-pylint (report-fn &rest _args)
  "Flymake backend for open-source pylint.
Uses the .pylintrc in the nearest enclosing directory of the
visited file, if any. Otherwise pylint finds its own config."
  (let ((pylint (executable-find "pylint"))
        (rc-dir (and buffer-file-name
                     (locate-dominating-file buffer-file-name ".pylintrc"))))
    (unless pylint
      (error "Cannot find pylint executable"))
    (dc-pe/--flymake-run-pylint
     report-fn
     (if rc-dir
         (list pylint (concat "--rcfile=" (expand-file-name ".pylintrc" rc-dir)))
       (list pylint)))))

(defun dc-pe/--flymake-run-pylint (report-fn command-prefix)
  "Run COMMAND-PREFIX, a pylint-compatible command, on the buffer via stdin.
Parse the output into flymake diagnostics and pass them to REPORT-FN."
  (let ((file-name (buffer-file-name)))
    (when (processp dc-pe/flymake-gpylint-process)
      (delete-process dc-pe/flymake-gpylint-process))

    (let ((source (current-buffer))
          (process-label (file-name-nondirectory (car command-prefix))))
      (setq dc-pe/flymake-gpylint-process
            (make-process
             :name (concat process-label "-flymake")
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer (format " *%s-flymake*" process-label))
             :command (append command-prefix
                              (list
                               ;;"--output-format=msvs"
                               "--from-stdin"
                               "--msg-template={path}:{line}: [{msg_id}({symbol}), {obj}] {msg}"
                               (or file-name "stdin")))
             :sentinel
             (lambda (process _event)
               (when (eq (process-status process) 'exit)
                 (unwind-protect
                     (when (buffer-live-p source)
                       (with-current-buffer (process-buffer process)
                         (goto-char (point-min))
                         (let ((diags nil))
                           ;; this regex corresponds to the --msg-template used above
                           (while (re-search-forward "^.+:\\([0-9]+\\): \\[[[:space:]]*\\([CRWEF]\\)\\([0-9]+\\)(\\([^)]+\\)),[[:space:]]*\\([^]]*\\)\\] \\(.*\\)$" nil t)
                             (let* ((line (string-to-number (match-string 1)))
                                    (type-char (match-string 2))
                                    (raw-msg (match-string 6))
                                    ;; If it's a syntax error, Pylint strips the extension in the msg.
                                    ;; We put it back for clarity.
                                    (msg (if (and file-name (string-match (concat "(" (file-name-base file-name) ", line ") raw-msg))
                                             (replace-regexp-in-string (concat "(" (file-name-base file-name) ", line ")
                                                                       (concat "(" (file-name-nondirectory file-name) ", line ")
                                                                       raw-msg)
                                           raw-msg))
                                    (severity (cond
                                               ((member type-char '("E" "F")) :error)
                                               ((string= type-char "W") :warning)
                                               (t :note)))
                                    (region (flymake-diag-region source line)))
                               (push (flymake-make-diagnostic source
                                                              (car region)
                                                              (cdr region)
                                                              severity
                                                              msg)
                                     diags)))
                           (funcall report-fn diags))))
                   (when (buffer-live-p (process-buffer process))
                     (kill-buffer (process-buffer process))))))))

      ;; Send the buffer content to the linter's stdin
      (process-send-region dc-pe/flymake-gpylint-process (point-min) (point-max))
      (process-send-eof dc-pe/flymake-gpylint-process))))


(defun dc-pe/rename-python-check-buffer ()
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

(defun dc-pe/basedpyright-check-command ()
  "Get the python check command for the current file. This will
return something sensible, if uv and basedpyright are installed, whether or
not PEP723 markup is present.

If PEP723 markup, then find the right pythonexe and give that to the
basedpyright command. If no markup, or if uv does not return the python
path, but basedpyright is installed, then just run basedpyright, and let
it figure out the venv.

If no basedpyright, then fall back to *ruff check*, without checking for
existence."
  (if-let* ((basedpyright (dc-pe/find-basedpyright)))
      (progn
        (add-hook 'compilation-mode-hook #'dc-pe/rename-python-check-buffer)
        (with-eval-after-load 'compile
          (add-to-list 'compilation-error-regexp-alist-alist
                       '(basedpyright
                         "^[[:space:]]+\\([A-Za-z]:[^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                         1 2 3))
          (add-to-list 'compilation-error-regexp-alist 'basedpyright))
        (if-let* ((_ (dc-pe/has-pep723-p))
                  (python-path (dc-pe/uv-get-pythonexe buffer-file-name)))
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

;; (defun dc-pe/refactor-multiline-imports ()
;;   "Refactor multiline imports \"from ... import (...)\" into multiple single-
;; line imports.
;;
;; This handles indentation and preserves \"as\" aliases. It also attempts to
;; strip comments within the import block."
;;   (interactive)
;;   (unless (derived-mode-p 'python-mode 'python-ts-mode)
;;     (error "Current buffer is not in python-mode or python-ts-mode"))
;;   (let ((count 0))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\([ \t]*\\)from \\([^ \t\n]+\\) import\\s-*(" nil t)
;;         (let* ((indent (match-string 1))
;;                (module (match-string 2))
;;                (open-paren-pos (match-end 0))
;;                (start-of-import (match-beginning 0)))
;;           (backward-char) ;; move back to the '('
;;           (condition-case nil
;;               (let ((end-of-import (progn (forward-list) (point))))
;;                 (let* ((symbols-text (buffer-substring-no-properties open-paren-pos (1- end-of-import)))
;;                        ;; Strip comments
;;                        (clean-symbols-text (replace-regexp-in-string "#.*" "" symbols-text))
;;                        ;; Split by comma, trim whitespace, and filter empty strings
;;                        (symbols (delete "" (mapcar #'string-trim (split-string clean-symbols-text "," t))))
;;                        (new-imports (mapconcat (lambda (s) (format "%sfrom %s import %s" indent module s))
;;                                                symbols
;;                                                "\n")))
;;                   (delete-region start-of-import end-of-import)
;;                   (insert new-imports)
;;                   (setq count (1+ count))))
;;             (error (goto-char (line-end-position)))))))
;;     (message "Refactored %d multiline import(s)." count)))

(defun dc-pe/refactor-multiline-imports ()
  "Refactor multiline or multi-symbol imports into multiple single-line imports."
  (interactive)
  (unless (derived-mode-p 'python-mode 'python-ts-mode)
    (error "Current buffer is not in python-mode or python-ts-mode"))
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      ;; Search for "from module import "
      (while (re-search-forward "^\\([ \t]*\\)from \\([^ \t\n]+\\) import\\s-*" nil t)
        (let* ((indent (match-string 1))
               (module (match-string 2))
               (start-of-import (match-beginning 0))
               (is-paren (looking-at "("))
               symbols-text end-of-import)
          (if is-paren
              (condition-case nil
                  (progn
                    (setq end-of-import (save-excursion (forward-list) (point)))
                    (setq symbols-text (buffer-substring-no-properties (1+ (point)) (1- end-of-import))))
                (error (setq end-of-import nil)))
            ;; Single line case: get rest of line
            (setq end-of-import (line-end-position))
            (setq symbols-text (buffer-substring-no-properties (point) end-of-import)))

          (when (and end-of-import symbols-text)
            (let* ((clean-symbols (replace-regexp-in-string "#.*" "" symbols-text))
                   (symbols (delete "" (mapcar #'string-trim (split-string clean-symbols "," t)))))
              ;; Only refactor if there's more than one symbol
              (if (> (length symbols) 1)
                  (let ((new-imports (mapconcat (lambda (s) (format "%sfrom %s import %s" indent module s))
                                                symbols "\n")))
                    (delete-region start-of-import end-of-import)
                    (insert new-imports)
                    (setq count (1+ count)))
                (goto-char end-of-import)))))))
    (message "Refactored %d import block(s)." count)))


;; ### Key Changes:
;; 1.  **Regex**: Removed the trailing `(` from the search regex to match any `from ... import` statement.
;; 2.  **Detection**: Used `(looking-at "(")` to determine if the import uses parentheses.
;; 3.  **Extraction**: If no parentheses are found, it grabs the remainder of the current line.
;; 4.  **Guard**: Added `(if (> (length symbols) 1) ...)` to ensure it only refactors lines actually containing multiple symbols, preventing infinite loops or redundant replacements.

(defun dc-pe/extract-python-imports ()
  "Scan the current buffer and extract Python import statements.
Verifies that the buffer is in ='python-mode'= or ='python-ts-mode'=.
Returns the text of the import lines without text properties."
  (unless (derived-mode-p 'python-mode 'python-ts-mode)
    (error "Current buffer is not in python-mode or python-ts-mode"))
  (let ((imports))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        ;; Scan for lines starting with \"import\" or \"from\"
        (while (re-search-forward "^\\(?:import\\|from\\)\\s-+" nil t)
          (let ((start (match-beginning 0)))
            ;; Check for multiline imports using parentheses
            (if (re-search-forward "(" (line-end-position) t)
                (progn
                  (backward-char)
                  (condition-case nil
                      (forward-list)
                    (error (goto-char (line-end-position))))
                  (push (buffer-substring-no-properties start (point)) imports))
              ;; Otherwise, capture the rest of the line
              (let ((end (line-end-position)))
                (push (buffer-substring-no-properties start end) imports)
                (goto-char end)))))))
    (mapconcat #'identity (nreverse imports) "\n")))

(defun dc-pe/infer-dependencies ()
  "Infers Python dependencies based on imports in the current buffer.
Returns the inferred list of modules as a string like \"[ \"mod1\", \"mod2\" ]\"."
  (let* ((imports (dc-pe/extract-python-imports))
         (prompt (concat
                  "You are a python development expert. Given these imports in a python module, what python modules do I need to require/install ?\n\n"
                  "<IMPORTS START>\n"
                  imports "\n"
                  "<IMPORTS END>\n\n"
                  "Return your response as a python list, of quoted module names.  For example the result could take this form:\n\n"
                  "  [ \"requests\", \"python-dotenv\" ]")))
    (unless dpc-gemini-selected-model
      (dpc-gemini/read-settings-from-properties-file))
    (let ((gem-url (concat dpc-gemini-base-url "v1beta/models/" dpc-gemini-selected-model ":generateContent")))
      (dpc-gemini/post-prompt gem-url prompt)
      (save-excursion
        (with-current-buffer "*gemini-response*"
          (let* ((json-object-type 'hash-table)
                 (json-array-type 'list)
                 (json-key-type 'string)
                 (parsed-response (json-read-from-string (buffer-substring-no-properties (point-min) (point-max)))))
            (if-let* ((candidates (gethash "candidates" parsed-response))
                      (first-candidate (car candidates))
                      (content (gethash "content" first-candidate))
                      (parts (gethash "parts" content))
                      (first-part (car parts))
                      (text (gethash "text" first-part)))
                (if (string-match "\\[[[:space:]\n]*\"[^\"]+\"\\(?:[[:space:]\n]*,[[:space:]\n]*\"[^\"]+\"\\)*[[:space:]\n]*\\]" text)
                    (match-string 0 text)
                  (error "Could not find a dependency list in Gemini's response: %s" text))
              (error "Unexpected response format from Gemini"))))))))


(defun dc-pe/py-column-width ()
  "Return a number suitable for various formatters, indicating the python
buffer max line length."
  (if (bound-and-true-p fill-column) fill-column 86))

(defun dc-pe/_tool-label (key name)
  "Return NAME with KEY bracketed at its first occurrence, eg \"py[i]nk\"."
  (let ((pos (string-match (regexp-quote (char-to-string key)) name)))
    (if pos
        (concat (substring name 0 pos) "[" (char-to-string key) "]"
                (substring name (1+ pos)))
      (format "[%c]%s" key name))))

(defun dc-pe/_prefer-tool (available preferred)
  "Return AVAILABLE with the entry named PREFERRED (a symbol) moved first.
This makes a project's `dc-pe-linter' the no-prefix default."
  (let ((hit (and preferred (rassoc (symbol-name preferred) available))))
    (if hit
        (cons hit (delq hit (copy-sequence available)))
      available)))

(defun dc-pe/_determine-tool-choice (action available prompt-prefix)
  "Determine which tool to use from AVAILABLE options.
ACTION is a string like \"Format\" or \"Lint\" used in the prompt.
AVAILABLE is an alist mapping character keys to tool name strings.
PROMPT-PREFIX is non-nil if we should prompt the user for the choice."
  (if prompt-prefix
      (let* ((prompt-parts (mapcar (lambda (item)
                                     (dc-pe/_tool-label (car item) (cdr item)))
                                   available))
             (prompt (concat action " with " (mapconcat 'identity prompt-parts ", ") "? "))
             (allowed (apply 'append
                             (mapcar (lambda (item)
                                       (list (car item) (upcase (car item))))
                                     available))))
        (downcase (read-char-choice prompt allowed)))
    (car (car available))))

(defun dc-pe/lint (&optional arg)
  "Run gpylint, pylint, or ruff check on the file in the currently visited buffer.

The default is the buffer's `dc-pe-linter' when set (eg via
.dir-locals.el), otherwise gpylint. pylint uses the .pylintrc in the
nearest enclosing directory, if any.

With a prefix ARG, prompt the user to select which linter to use."
  (interactive "P")
  (if (and buffer-file-name
           (derived-mode-p 'python-base-mode))
      (let* ((gpylint (executable-find "gpylint"))
             (pylint (executable-find "pylint"))
             (ruff (executable-find "ruff"))
             (file-name (shell-quote-argument buffer-file-name))
             (available (dc-pe/_prefer-tool
                         (delq nil
                               (list (when gpylint '(?g . "gpylint"))
                                     (when pylint  '(?l . "pylint"))
                                     (when ruff    '(?r . "ruff"))))
                         (bound-and-true-p dc-pe-linter))))
        (if (not available)
            (error "Could not find 'gpylint', 'pylint', or 'ruff' executable")
          (let ((choice (dc-pe/_determine-tool-choice "Lint" available arg)))
            (cond
             ((eq choice ?g)
              (compilation-start
               (concat
                gpylint " --mode=base --output-format=text --msg-template='{path}:{line}: [{msg_id}({symbol}), {obj}] {msg}' "
                buffer-file-name)))
             ((eq choice ?l)
              (let ((rc-dir (locate-dominating-file buffer-file-name ".pylintrc")))
                (compilation-start
                 (concat
                  pylint
                  (if rc-dir
                      (concat " --rcfile="
                              (shell-quote-argument
                               (expand-file-name ".pylintrc" rc-dir)))
                    "")
                  " --output-format=text --msg-template='{path}:{line}: [{msg_id}({symbol}), {obj}] {msg}' "
                  file-name))))
             ((eq choice ?r)
              (compilation-start
               (concat ruff " check " file-name)))))))))


(defconst dc-pe/python-apheleia-formatters '(pyformat pyink black-custom ruff)
  "Apheleia formatter names that `dc-pe/format' offers for python buffers.
Each is a key in `apheleia-formatters'. pyink added by my own emacs init.")

(defun dc-pe/_installed-apheleia-formatters ()
  "Return the names in `dc-pe/python-apheleia-formatters' that are usable.
A name is usable if it has an entry in `apheleia-formatters' and the
program that entry runs is installed."
  (seq-filter (lambda (name)
                (let ((command (alist-get name apheleia-formatters)))
                  (and command
                       (stringp (car command))
                       (executable-find (car command)))))
              dc-pe/python-apheleia-formatters))

(defun dc-pe/format (&optional arg)
  "Format the current python buffer with apheleia.

Uses the buffer's `apheleia-formatter', which `dc-pe/select-tools'
sets from `dc-pe-formatter' (eg via .dir-locals.el) or picks
automatically. With a prefix ARG, prompt for one of the usable names in
`dc-pe/python-apheleia-formatters'.

This formats the buffer contents, including unsaved edits, the same way
format-on-save does, so running it explicitly is mostly unnecessary when
apheleia-mode is enabled. If apheleia misbehaves, use
`dc-pe/format-direct'."
  (interactive "P")
  (unless (derived-mode-p 'python-base-mode)
    (error "Not in a Python mode"))
  (require 'apheleia)
  (let ((formatter
         (if arg
             (intern
              (completing-read
               "Format with: "
               (mapcar #'symbol-name (dc-pe/_installed-apheleia-formatters))
               nil t nil nil
               (and (symbolp apheleia-formatter) apheleia-formatter
                    (symbol-name apheleia-formatter))))
           apheleia-formatter)))
    (unless formatter
      (error "No apheleia formatter is set for this buffer"))
    (apheleia-format-buffer formatter)))

(defun dc-pe/format-direct (&optional arg)
  "Fallback: run pyformat, pyink, black, or ruff format directly on the
file visited by the current buffer, outside apheleia.

Use this only if apheleia misbehaves; normally use `dc-pe/format'. It
formats the file on disk (save first), shows output in a compilation
buffer, and builds its own command lines, which may differ from the
`apheleia-formatters' entries. pyink takes its settings from the nearest
pyproject.toml. The default is the first installed of pyformat, pyink,
black, ruff.

With a prefix ARG, prompt the user to select which formatter to use."
  (interactive "P")
  (if (and buffer-file-name
           (derived-mode-p 'python-base-mode))
      (let* ((file-name (shell-quote-argument buffer-file-name))
             (pyformat (executable-find "pyformat"))
             (pyink (executable-find "pyink"))
             (black (executable-find "black"))
             (ruff (executable-find "ruff"))
             (available (delq nil
                              (list (when pyformat '(?p . "pyformat"))
                                    (when pyink    '(?i . "pyink"))
                                    (when black    '(?b . "black"))
                                    (when ruff     '(?r . "ruff"))))))
        (if (not available)
            (error "Could not find any of 'pyformat', 'pyink', 'black', or 'ruff' executables")
          (let ((choice (dc-pe/_determine-tool-choice "Format" available arg)))
            (cond
             ((eq choice ?p)
              (compilation-start (concat pyformat " -s 2 -i " file-name)))
             ((eq choice ?i)
              (compilation-start (concat pyink " --quiet " file-name)))
             ((eq choice ?b)
              (compilation-start
               (format
                "%s --line-length %d --preview --enable-unstable-feature string_processing %s"
                black
                (dc-pe/py-column-width)
                file-name)))
             ((eq choice ?r)
              (compilation-start
               (format
                "%s format --silent --line-length %d %s"
                ruff
                (dc-pe/py-column-width)
                file-name)))))))))

(defun dc-pe/uv-run ()
  "Run the current buffer's file using `uv run` in a compilation buffer."
  (interactive)
  (if (and buffer-file-name
           (derived-mode-p 'python-base-mode))
      (let ((uv (executable-find "uv"))
            (file-name (shell-quote-argument buffer-file-name)))
        (if uv
            (compilation-start (concat uv " run " file-name))
          (error "Could not find 'uv' executable")))
    (error "Buffer is not visiting a file, or not in a Python mode")))

;; 20261009 - Per-project overrides for the python formatter and linter, set via
;; .dir-locals.el. `dino-python-mode-fn' requires this file from the python-mode
;; hook, and Emacs applies dir-locals after mode hooks run, so the
;; safe-local-variable properties below exist in time and Emacs doesn't prompt.
(defvar-local dc-pe-formatter nil
  "Name of the apheleia formatter to use for this python buffer, eg `pyink'.

The value is not a program name. It is a lookup key into
`apheleia-formatters', whose entry for that key holds the actual command
line (program plus arguments) that apheleia runs. For example `pyink'
maps to (\"pyink\" \"--quiet\" \"--stdin-filename\" filepath \"-\"), and
`black-custom' maps to a black command line. `dc-pe/select-tools'
copies this value into `apheleia-formatter'. nil means choose
automatically. Usually set per project in .dir-locals.el.")
(put 'dc-pe-formatter 'safe-local-variable #'symbolp)

(defvar-local dc-pe-linter nil
  "Flymake linter for python buffers: `gpylint', `pylint', or `ruff'.
nil means choose automatically. `pylint' uses the .pylintrc found in the
nearest enclosing directory.")
(put 'dc-pe-linter 'safe-local-variable #'symbolp)

(defun dc-pe/select-tools ()
  "Set the apheleia formatter and flymake linter for the current python buffer.
Honors `dc-pe-formatter' and `dc-pe-linter' when set."
  ;; explicitly select a single specific formatter in priority order.
  (cond
   (dc-pe-formatter
    (setq-local apheleia-formatter dc-pe-formatter))
   ((executable-find "pyformat")
    (setq-local apheleia-formatter 'pyformat))
   ;; for now, 20260704, black splits long strings, while
   ;; "ruff format" does not. So even though black is slower,
   ;; prefer it for this reason.
   ((executable-find "black")
    (setq-local apheleia-formatter 'black-custom))
   ((executable-find "ruff")
    (setq-local apheleia-formatter 'ruff)))

  (pcase (or dc-pe-linter
             (cond ((executable-find "gpylint") 'gpylint)
                   ((executable-find "ruff") 'ruff)))
    ('gpylint
     (add-hook 'flymake-diagnostic-functions #'dc-pe/flymake-gpylint nil t))
    ('pylint
     (add-hook 'flymake-diagnostic-functions #'dc-pe/flymake-pylint nil t))
    ('ruff
     (require 'flymake-ruff)
     ;; Insert settings into ruff.toml in the project root.
     (add-hook 'flymake-diagnostic-functions #'flymake-ruff--run-checker nil t))))

(provide 'dc-python-extensions)


;;; dc-python-extensions.el ends here
