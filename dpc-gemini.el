;;; dpc-gemini.el --- invoke the Gemini API from emacs
;;
;; Author: Dino Chiesa
;; Created: Monday, 23 December 2024, 20:53
;; Package-Requires: (json.el seq.el subr-x.el s.el)
;; URL:
;; X-URL:
;; Version: 2025.05.25
;; Keywords: gemini llm
;; License: New BSD

;;; Commentary:


;;   (require 'dpc-gemini)
;;   (setq dpc-gemini-api-key "XXXXXXXXXXXX")

;;    -or-

;;   (require 'dpc-gemini)
;;   (setq dpc-gemini-properties-file "~/path/to/.google-gemini-properties")
;;   (dpc-gemini/read-settings-from-properties-file)

;; Optionally, set a key binding:
;;   (keymap-global-set "C-c C-y" #'dpc-gemini/ask-gemini)

;;
;;; Revisions:
;;
;; 2025.05.25  Dino Chiesa
;;    removed `dpc-gemini/set-api-key-from-file' in favor of `dpc-gemini/read-settings-from-properties-file'
;;    And the new `dpc-gemini-properties-file' variable.
;; 2025.02.14  Dino Chiesa
;;    added `dpc-gemini/select-model' to allow the user to select a specific model to use
;;    in `dpc-gemini/ask-gemini'. Also added `dpc-gemini/get-generative-models'
;;    which is used by select-model and can be used by other packages too.
;;    And `dpc-gemini/list-models' which is sort of a novelty fn to list the models
;;    in a buffer without offering a selection.
;; 2024.12.23  Dino Chiesa
;;    first cut. invoke `dpc-gemini/ask-gemini' to get a Gemini
;;    response. The function tries to fold paragraphs, excepting code,
;;    in the response.


;;; License
;;
;; Copyright (c) 2024-2025, Google LLC
;;

(require 'json)
(require 'seq)
(require 's)
(require 'subr-x)

;;; Code:

(defgroup dpc-gemini nil
  "Provides a facility to invoke Gemini."
  :group 'Editing)

(defcustom dpc-gemini-base-url "https://generativelanguage.googleapis.com/"
  "The base URL for the Gemini API."
  :type 'string
  :group 'dpc-gemini)

(defcustom dpc-gemini-api-key nil
  "The api key for connecting to generativelanguage.googleapis.com.

Get one by visiting  https://aistudio.google.com/app/apikey"
  :type 'string
  :group 'dpc-gemini)

(defcustom dpc-gemini-properties-file  "~/elisp/.google-gemini-properties"
  "File in which to find gemini-related properties like the API key to use
and the default model, etc. The file should be structured as a properties file.
Eg,

  apikey: something
  default-model: gemini-2.5-flash-preview-05-20
  other-setting: blah-blah
"
  :type 'string
  :group 'dpc-gemini)

(defcustom dpc-gemini-selected-model "gemini-2.5-flash-preview-05-20"
  "The model key for Gemini. This package uses the specified model, and other
packages may choose to reference this, too.

Find models at https://ai.google.dev/gemini-api/docs/models/gemini .
examples: \"gemini-2.5-flash-preview-05-20\",
\"gemini-2.5-pro-exp-03-25\"
See also `dpc-gemini/list-models'"
  :type 'string
  :group 'dpc-gemini)

(defvar dpc-gemini--properties-cache nil
  "Cache for properties read from `dpc-gemini-properties-file'.
  An alist where each element is (PROPERTY-NAME . VALUE). Keys are downcased symbols.")

(defvar dpc-gemini--properties-file-mtime nil
  "Last modification time of `dpc-gemini-properties-file' when it was last read into cache.
  Used to detect if the file has changed and needs re-reading.")



(defun dpc-gemini/post-prompt (gem-url gem-prompt)
  "Perform an HTTP POST to GEM-URL with a one-part text prompt given
in GEM-PROMPT.

Place the result into a newly created buffer, and pop to that buffer
when complete."
  ;; curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=$YOUR_API_KEY" \
  ;; -H 'Content-Type: application/json' \
  ;; -X POST \
  ;; -d '{
  ;;   "contents": [{
  ;;     "parts":[{"text": "Explain how AI works"}]
  ;;     }]
  ;;    }'

  (let ((buf (generate-new-buffer "gemini-response")))
    (switch-to-buffer buf)
    (call-process "curl" nil t t
                  "-s"
                  "-X" "POST"
                  "-H" "content-type: application/json"
                  "-d"
                  (json-encode
                   `(("contents" .
                      [(("parts" . [(("text" . ,gem-prompt))]))]
                      )))
                  gem-url)))

(defun dpc-gemini/get-generative-models (&optional predicate)
  "Get the available Gemini models that satisfy the optional PREDICATE."
  (with-temp-buffer
    (let ((gem-url
           (concat
            dpc-gemini-base-url
            "v1beta/models?key="
            dpc-gemini-api-key)))
      (call-process "curl" nil t t
                    "-s"
                    "-X" "GET"
                    gem-url))
    (let ((json-object-type 'hash-table)
          (json-array-type 'list)
          (json-key-type 'string))
      (let ((pred
             (lambda (model)
               (and (dpc-gemini/-current-generative-model model)
                    (if predicate
                        (funcall predicate model)
                      t))))
            (parsed-response
             (json-read-from-string
              (buffer-substring-no-properties (point-min) (point-max)))))
        (seq-filter (lambda (model) (funcall pred model))
                    (gethash "models" parsed-response))))))

(defun dpc-gemini/list-models (&optional full-text)
  "List the available Gemini models that have  \"generateContent\" as one of the
members of supportedGenerationMethods.

Optional arg FULL-TEXT says to return the full text of the response and
do not filter by supportedGenerationMethods."
  (interactive "P")
  (let ((buf (generate-new-buffer "gemini-response"))
        (models (dpc-gemini/get-generative-models)))
    (switch-to-buffer buf)
    (insert (json-encode models)))
  (if full-text
      (json-pretty-print-buffer)
    (let ((json-object-type 'hash-table)
          (json-array-type 'list)
          (json-key-type 'string))
      (let* ((models
              (json-read-from-string
               (buffer-substring-no-properties (point-min) (point-max))))
             (modelnames
              (mapcar (lambda (model) (gethash "name" model))
                      models)))
        (when (listp modelnames)
          (setq modelnames
                (mapcar (lambda (str)
                          (if (and (stringp str) (>= (length str) 7))
                              (substring str 7)
                            str))
                        modelnames))
          (delete-region (point-min) (point-max))
          (insert "Gemini Models that support generateContent:\n\n  ")
          (insert
           (mapconcat 'identity modelnames "\n  "))
          (insert "\n")
          )))))

(defun dpc-gemini/-current-generative-model (response)
  "A predicate that looks at a model and returns non-nil if the
model is current and supports \"generateContent\"."
  (when-let*
      ((cur-model response)
       (description (gethash "description" cur-model))
       (supported-methods (gethash "supportedGenerationMethods" cur-model))
       (is-current (not (string-match-p (rx (or "discontinued" "deprecated")) description))))
    (seq-contains-p supported-methods "generateContent")))


;; 20250308-1828
;; This, and anything related to chatgpt-shell mode in this module,
;; will become obsolete if my PR is accepted and merged.
(defun dpc-gemini/chapgpt-shell-converter (api-response)
  "Convert between the API-RESPONSE returned by Gemini, and
the model description needed by chatgpt-shell."
  (let* ((model-name (gethash "name" api-response))
         (model-version (string-remove-prefix "models/" model-name))
         (model-shortversion (string-remove-prefix "gemini-" model-version))
         (model-urlpath (concat "/v1beta/" model-name))
         (model-cwindow (gethash "inputTokenLimit" api-response)))
    (chatgpt-shell-google-make-model :version model-version
                                     :short-version model-shortversion
                                     :path model-urlpath
                                     :token-width 4
                                     :context-window model-cwindow)))


(defun dpc-gemini/looking-at-three-backticks ()
  "Check if the point is before three backticks, with optional preceding newline."
  (if (looking-at "\\(?:^\\|\n\\)```")
      (match-end 0)))

;;;###autoload
(defun dpc-gemini/fill-paragraphs-skipping-codeblocks ()
  "Fill all paragraphs in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((posn-of-three-backticks (dpc-gemini/looking-at-three-backticks))
             (keep-going
              (or (not posn-of-three-backticks)
                  (progn
                    (goto-char posn-of-three-backticks)
                    (when (search-forward "\n```" nil t)
                      (forward-char)
                      t)))))
        (when keep-going
          (fill-paragraph nil)
          (forward-paragraph))))))

;;;###autoload
(defun dpc-gemini/ask-gemini (beginning end)
  "Retrieve a response from Gemini."
  (interactive "r")
  (if (not (and (boundp 'dpc-gemini-api-key)
                (stringp dpc-gemini-api-key)))
      (let ((msg (concat "You need to get an \"api key\" from Google.\n"
                         "You can set it in your .emacs with a statement like:\n"
                         "    (setq dpc-gemini-api-key \"XXXXXXXXXXXX\") \n")))
        (message msg)
        (browse-url "https://aistudio.google.com/app/apikey")
        nil)
    (let* ((initial-prompt (if (region-active-p)
                               (buffer-substring-no-properties beginning end)
                             ""))
           (gem-url
            (concat
             dpc-gemini-base-url
             "v1beta/models/"
             dpc-gemini-selected-model
             ":generateContent?key="
             dpc-gemini-api-key))
           (gem-prompt
            ;;(read-from-minibuffer "ask gemini ? " nil nil nil nil initial-prompt)))
            (read-string "ask gemini ? " initial-prompt)))
      (message (concat "invoking " gem-url))
      (dpc-gemini/post-prompt gem-url gem-prompt)
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (parsed-response
              (json-read-from-string
               (buffer-substring-no-properties (point-min) (point-max))))
             (candidates (gethash "candidates" parsed-response))
             (first-candidate (car candidates))
             (content (gethash "content" first-candidate))
             (parts (gethash "parts" content))
             (first-part (car parts))
             (text (gethash "text" first-part)))
        (delete-region (point-min) (point-max))
        (insert text)
        (dpc-gemini/fill-paragraphs-skipping-codeblocks)
        ))))

;;;###autoload
(defun dpc-gemini/select-model ()
  "Set the Gemini model to use."
  (interactive)
  (let* ((available-models
          (dpc-gemini/get-generative-models
           (lambda (m) (string-match-p "gemini" (gethash "name" m)))))
         (short-model-names
          (mapcar (lambda (n) (string-remove-prefix "models/" n))
                  (mapcar (lambda (m) (gethash "name" m)) available-models)))
         (selected-model
          (completing-read "Model?: "
                           short-model-names nil t)))
    (when selected-model
      (setq dpc-gemini-selected-model selected-model))))


;;;###autoload
(defun dpc-gemini/get-gemini-api-key ()
  "Return the gemini api key, that was possibly read from a file."
  (interactive)
  (if (and (boundp 'dpc-gemini-api-key)
           (stringp dpc-gemini-api-key))
      dpc-gemini-api-key
    (error "No gemini api key is set yet")))


(defun dpc-gemini/--parse-properties-file-into-cache ()
  "Reads the properties file and populates `dpc-gemini--properties-cache'.
  Returns t if the file was successfully parsed or re-parsed, nil otherwise.
  Invalidates cache and re-reads if the file's modification time changes.
  Handles 'key: value' format, skips empty lines and lines starting with '#'."
  (let* ((expanded-props-file (expand-file-name dpc-gemini-properties-file (getenv "HOME")))
         (file-attrs (file-attributes expanded-props-file)))

    (if file-attrs
        (let ((new-mtime (file-attribute-modification-time file-attrs)))
          ;; Check if cache is fresh (exists AND modification time hasn't changed)
          (unless (and dpc-gemini--properties-cache
                       (equal new-mtime dpc-gemini--properties-file-mtime))
            (setq dpc-gemini--properties-cache nil
                  dpc-gemini--properties-file-mtime nil)

            (with-temp-buffer
              (insert-file-contents expanded-props-file)
              (setq dpc-gemini--properties-cache
                    ;; Filter out nil entries (lines that didn't match the format)
                    (delq nil
                          (mapcar (lambda (line)
                                    ;; Use string-match-p for quick check, then string-match to set match-data
                                    (when (string-match-p "^\\([^:]+\\): *\\(.*\\)$" line)
                                      (string-match "^\\([^:]+\\): *\\(.*\\)$" line) ; Re-run to set match-data
                                      ;; Store key as downcased symbol for consistent lookup
                                      (cons (intern (downcase (match-string 1 line)))
                                            (match-string 2 line))))
                                  ;; Split buffer into lines, filtering out empty and comment lines
                                  (seq-filter (lambda (line)
                                                (and (> (length line) 0)
                                                     (not (string-prefix-p "#" line))))
                                              (split-string (buffer-string) "\n" t))))
                    dpc-gemini--properties-file-mtime new-mtime)))
          ;; Return t if we processed (either by parsing or determining cache is fresh)
          t)
      ;; File does not exist, clear cache and mtime to reflect its absence
      (setq dpc-gemini--properties-cache nil
            dpc-gemini--properties-file-mtime nil)
      nil)))

;; This function replaces your original `dpc-gemini/--read-property-file`
(defun dpc-gemini/--read-property-value-from-cache (propname)
  "Retrieves a property value by PROPNAME (string) from the cached properties.
  PROPNAME is converted to a downcased symbol for lookup.
  Returns nil if not found, or if the file doesn't exist/can't be parsed."
  ;; First, ensure the cache is populated and up-to-date.
  ;; `dpc-gemini/--parse-properties-file-into-cache` will handle
  ;; reading the file only when necessary.
  (when (dpc-gemini/--parse-properties-file-into-cache)
    ;; Look up the property in the alist using a downcased symbol key
    (cdr (assoc (intern (downcase propname)) dpc-gemini--properties-cache))))


;; --- Optimized Main Function ---

;;;###autoload
(defun dpc-gemini/read-settings-from-properties-file ()
  "Read the gemini api key and default model from the properties file named by
`dpc-gemini-properties-file'. This has a side effect of setting
`dpc-gemini-api-key' and `dpc-gemini-selected-model'.
As an alternative to calling this fn, programs can directly set these variables."
  (interactive)
  ;; By calling `dpc-gemini/--read-property-value-from-cache`, the file
  ;; is automatically parsed and cached if needed (first call, or file changed).

  (if-let* ((apikey (dpc-gemini/--read-property-value-from-cache "apikey")))
      (setq dpc-gemini-api-key (s-trim apikey))
    (setq dpc-gemini-api-key nil))

  (if-let* ((model-name (dpc-gemini/--read-property-value-from-cache "default-model")))
      (setq dpc-gemini-selected-model (s-trim model-name))
    (setq dpc-gemini-selected-model nil))

  ;; --- Optional: Provide user feedback for debugging ---
  (unless (file-exists-p (expand-file-name dpc-gemini-properties-file (getenv "HOME")))
    (message "Warning: Gemini properties file does not exist: %s"
             (expand-file-name dpc-gemini-properties-file (getenv "HOME"))))
  (unless dpc-gemini-api-key
    (message "Warning: Gemini APIkey not found in %s"
             (expand-file-name dpc-gemini-properties-file (getenv "HOME"))))
  (unless dpc-gemini-selected-model
    (message "Warning: Default Gemini model not found in %s"
             (expand-file-name dpc-gemini-properties-file (getenv "HOME"))))
  )

(defun dpc-gemini/get-config-property (propname)
  "Returns a property extracted from the gemini properties file,
`dpc-gemini-properties-file'. You must have previously called
`dpc-gemini/read-settings-from-properties-file' ."
  (if-let* ((value (dpc-gemini/--read-property-value-from-cache propname)))
      (s-trim value)))




(provide 'dpc-gemini)

;;; dpc-gemini.el ends here
