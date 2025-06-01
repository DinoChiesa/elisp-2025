;;; -*- coding: utf-8; lexical-binding: t;  -*-

;;; dpc-gemini.el --- invoke the Gemini API from emacs
;;
;; Author: Dino Chiesa
;; Created: Monday, 23 December 2024, 20:53
;; Package-Requires: (json.el seq.el subr-x.el s.el)
;; URL:
;; X-URL:
;; Version: 2025.05.26
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
(require 'dpc-sane-sorting)

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

(defcustom dpc-gemini-properties-file  "~/.google-gemini-properties"
  "File in which to find gemini-related properties like the API key to use
and the default model, etc. The file should be structured as a properties file.
Eg,

  apikey: something
  default-model: gemini-2.5-flash-preview-05-20
  other-setting: blah-blah
"
  :type 'string
  :group 'dpc-gemini)

(defvar dpc-gemini-selected-model "gemini-2.5-flash-preview-05-20"
  "The model key for Gemini. This package uses the specified model, and other
packages may choose to reference this, too.

examples: \"gemini-2.5-flash-preview-05-20\",
\"gemini-2.5-pro-exp-03-25\"
See also `dpc-gemini/list-models'")

(defvar dpc-gemini--properties-cache nil
  "Cache for properties read from `dpc-gemini-properties-file'.
  An alist where each element is (PROPERTY-NAME . VALUE). Keys are downcased symbols.")

(defvar dpc-gemini--properties-file-mtime nil
  "Last modification time of `dpc-gemini-properties-file' when it was last read into cache.
  Used to detect if the file has changed and needs re-reading.")

(defface
  dpc-gemini-text '((t (:background "black" :foreground "MediumSeaGreen")))
  "For prompt composition text."  :group 'faces)

(defun dpc-gemini/--internal-get-gemini-key ()
  "reads the key; if none is set, prompts the user."
  (let ((gemini-apikey (or
                        (dpc-gemini/get-config-property "apikey")
                        dpc-gemini-api-key)))
    (if (not gemini-apikey)
        (let ((msg (concat "You need to get an \"api key\" from Google.\n"
                           "You can specify it in the properties file (currently "
                           dpc-gemini-properties-file
                           ")\n, or set it in your .emacs with a statement like:\n"
                           "    (setq dpc-gemini-api-key \"XXXXXXXXXXXX\") \n")))
          (message msg)
          (browse-url "https://aistudio.google.com/app/apikey")
          nil)
      gemini-apikey)))

(defun dpc-gemini/post-prompt (gem-url gem-prompt)
  "Perform an HTTP POST to GEM-URL with a one-part text prompt given
in GEM-PROMPT.

Places the result into a newly created buffer, and then
`switch-to-buffer' is called on that buffer when complete."
  ;; curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent" \
  ;; -H 'X-goog-api-key: $YOUR_API_KEY' \
  ;; -H 'Content-Type: application/json' \
  ;; -X POST \
  ;; -d '{
  ;;   "contents": [{
  ;;     "parts":[{"text": "Explain how AI works"}]
  ;;     }]
  ;;    }'

  (if-let* ((gemini-apikey (dpc-gemini/--internal-get-gemini-key)))
      (let ((buf (get-buffer-create "gemini-response")))
        (switch-to-buffer buf)
        (erase-buffer)
        (call-process "curl" nil t t
                      "-s"
                      "-X" "POST"
                      "-H" "content-type: application/json"
                      "-H" (concat "X-Goog-API-Key:" gemini-apikey)
                      "-d"
                      (json-encode
                       `(("contents" .
                          [(("parts" . [(("text" . ,gem-prompt))]))]
                          )))
                      gem-url))))

(defun dpc-gemini/-current-generative-model-p (response)
  "A predicate that looks at a model in RESPONSE and returns non-nil
if the model is current and supports \"generateContent\"."
  (when-let*
      ((cur-model response)
       (description (gethash "description" cur-model))
       (supported-methods (gethash "supportedGenerationMethods" cur-model))
       (is-current (not (string-match-p (rx (or "discontinued" "deprecated")) description))))
    (seq-contains-p supported-methods "generateContent")))

(defun dpc-gemini/get-generative-models (&optional predicate)
  "Get the available Gemini models that satisfy the optional PREDICATE."
  (if-let* ((gemini-apikey (dpc-gemini/--internal-get-gemini-key)))
      (with-temp-buffer
        (let ((gem-url (concat dpc-gemini-base-url "v1beta/models")))
          (call-process "curl" nil t t
                        "-H" (concat "X-Goog-API-Key:" gemini-apikey)
                        "-s"
                        "-X" "GET"
                        gem-url))
        (let ((json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'string))
          (let ((pred
                 (lambda (model)
                   (and (dpc-gemini/-current-generative-model-p model)
                        (if predicate
                            (funcall predicate model)
                          t))))
                (parsed-response
                 (json-read-from-string
                  (buffer-substring-no-properties (point-min) (point-max)))))
            (seq-filter (lambda (model) (funcall pred model))
                        (gethash "models" parsed-response)))))))

(defun dpc-gemini/list-models (&optional full-text)
  "List the available Gemini models that have  \"generateContent\" as one of the
members of supportedGenerationMethods.

Optional arg FULL-TEXT says to return the full text of the response and
do not filter by supportedGenerationMethods."
  (interactive "P")
  (let* ((all-models (dpc-gemini/get-generative-models))
         (buf (get-buffer-create "gemini-models")))
    (with-current-buffer buf
      (erase-buffer)
      (if full-text
          (progn
            (insert (json-encode all-models))
            (json-pretty-print-buffer))

        (let* ((modelnames
                (dpc-ss-sort-alpha
                 (mapcar (lambda (model)
                           (let ((name (gethash "name" model)))
                             (if (and (stringp name) (s-starts-with-p "models/" name))
                                 (substring name (length "models/"))
                               name)))
                         all-models))))
          (insert "Gemini Models that support generateContent:\n\n  ")
          (insert (mapconcat 'identity modelnames "\n  "))
          (insert "\n"))))
    (pop-to-buffer buf)))


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

;; (defun dpc-gemini/fill-paragraphs-skipping-codeblocks ()
;;   "Fill all paragraphs in the current buffer."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (not (eobp))
;;       (let* ((posn-of-three-backticks (dpc-gemini/looking-at-three-backticks))
;;              (keep-going
;;               (or (not posn-of-three-backticks)
;;                   (progn
;;                     (goto-char posn-of-three-backticks)
;;                     (when (search-forward "\n```" nil t)
;;                       (forward-char)
;;                       t)))))
;;         (when keep-going
;;           (fill-paragraph nil)
;;           (forward-paragraph))))))


;;;###autoload
(defun dpc-gemini/fill-paragraphs-skipping-codeblocks ()
  "Fill all paragraphs in the current buffer, skipping code blocks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at "\\(?:^\\|\n\\)```")
          (progn ; Skip code block
            (goto-char (match-end 0)) ; Move past opening ```
            (unless (search-forward "\n```" nil t) ; Search for closing ```
              (goto-char (point-max))) ; If not found, go to end
            (when (looking-at "\n```") (forward-char))) ; Move past closing ``` if found
        ;; Not in a code block (or just exited one)
        (fill-paragraph nil)
        (forward-paragraph)))))


(defun dpc-gemini/--compose-message ()
  "Compose a message, using a temporary buffer for multi-line text.

This function is interactive and behaves differently based on the
state of the region:

1.  If a region is active and contains MORE THAN ONE newline:
    - A new temporary buffer is created in 'text-mode'.
    - The user can compose a message in this buffer.
    - Pressing 'C-c C-c' accepts the content.
    - Pressing 'C-c C-k' abandons the composition (returns nil).
    - The function returns the composed text as a plain string (or nil if abandoned).

2.  If a region is active and contains ONE LINE (or less):
    - The minibuffer is used to read a string ('read-string').
    - The minibuffer is pre-populated with the text from the region.
    - The function returns the string from the minibuffer.

3.  If a region is NOT active:
    - The minibuffer is used to read a string.
    - The function returns the string from the minibuffer."
  (let ((initial-input "")
        (line-count 0))

    (when (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end)))
        (setq initial-input (buffer-substring-no-properties beg end)
              line-count (count-lines beg end))))

    (cond
     ((and (region-active-p) (> line-count 1))
      (with-temp-buffer
        (let ((abandoned nil))
          (text-mode)
          (insert initial-input)
          ;; Append the instruction lines
          (goto-char (point-max))
          (insert "\n\n## Type your message above.\n")
          (insert "##   Press C-c C-c to accept, C-c C-k to cancel.\n")
          (goto-char (point-min)) ;; Move back to the beginning
          (keymap-local-set "C-c C-c"
                            (lambda () (interactive) (exit-recursive-edit)))
          (keymap-local-set "C-c C-k"
                            (lambda () (interactive) (setq abandoned t) (exit-recursive-edit)))

          (font-lock-add-keywords nil '(("^##.*" . 'font-lock-comment-face)))
          ;;(buffer-face-set 'font-lock-type-face)
          (buffer-face-set 'dpc-gemini-text)
          (font-lock-fontify-buffer) ;; i think this is required?
          (pop-to-buffer (current-buffer))

          ;; Enter a recursive edit. Emacs will now wait for the user
          ;; to edit and then press the key that calls `exit-recursive-edit`.
          (recursive-edit)
          ;; After `exit-recursive-edit` is called, the code continues.
          ;; The value of the `with-temp-buffer` block is the value of its
          ;; last expression. We return the full contents of the buffer
          ;; as a plain string, unless abandoned.
          (unless abandoned
            ;; Extract buffer content, split into lines, filter lines starting with "##", and join back
            (s-trim (s-join "\n" (seq-filter (lambda (line) (not (string-prefix-p "##" (s-trim line))))
                                             (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))))))))
     (t
      (read-string "Ask Gemini: " initial-input)))))


;;;###autoload
(defun dpc-gemini/ask-gemini ()
  "Retrieve a response from Gemini. Prompt the user for input.
If the region is active, use region content as suggested prompt."
  (interactive)
  ;; read properties before referencing `dpc-gemini-selected-model'
  (dpc-gemini/read-settings-from-properties-file)
  (if-let* ((gem-prompt (dpc-gemini/--compose-message))
            (gem-url
             (concat
              dpc-gemini-base-url "v1beta/models/" dpc-gemini-selected-model ":generateContent")))
      (progn
        (message (concat "invoking " gem-url))
        (dpc-gemini/post-prompt gem-url gem-prompt)
        (let* ((json-object-type 'hash-table)
               (json-array-type 'list)
               (json-key-type 'string)
               (parsed-response
                (json-read-from-string
                 (buffer-substring-no-properties (point-min) (point-max)))))
          (cond
           ((gethash "error" parsed-response)
            (let ((error-obj (gethash "error" parsed-response)))
              (error "Gemini API Error: %s (Code: %s)"
                     (gethash "message" error-obj "Unknown error")
                     (gethash "code" error-obj "N/A"))))
           ((null (gethash "candidates" parsed-response))
            (error "Gemini response missing 'candidates' field."))
           (t
            (if-let* ((candidates (gethash "candidates" parsed-response))
                      (first-candidate (car candidates))
                      (content (gethash "content" first-candidate))
                      (parts (gethash "parts" content))
                      (first-part (car parts))
                      (text (gethash "text" first-part)))
                (if text
                    (progn
                      (delete-region (point-min) (point-max))
                      (insert text)
                      (dpc-gemini/fill-paragraphs-skipping-codeblocks))
                  (error "Could not extract text from Gemini response. Structure: %S" parsed-response)))))))
    (message "quit.")))


;;;###autoload
(defun dpc-gemini/select-model ()
  "Set the Gemini model to use."
  (interactive)
  (if-let* ((gemini-apikey (dpc-gemini/--internal-get-gemini-key)))
      (let* ((available-models
              (dpc-gemini/get-generative-models
               (lambda (m) (string-match-p "gemini" (gethash "name" m)))))
             (short-model-names
              (mapcar (lambda (n) (string-remove-prefix "models/" n))
                      (mapcar (lambda (m) (gethash "name" m)) available-models)))
             (selected-model
              (completing-read "Model?: "
                               (dpc-ss-sort-completion-fn short-model-names 'sorted-sanely) nil t)))
        (when selected-model
          (setq dpc-gemini-selected-model selected-model)))))


;;;###autoload
(defun dpc-gemini/get-gemini-api-key ()
  "Return the gemini api key that was previously set into `dpc-gemini-api-key'.
If it has not been previously set, this function will try to read
`dpc-gemini-properties-file' to extract the key, and will return the value."
  (interactive)
  (if (not (and (boundp 'dpc-gemini-api-key)
                (stringp dpc-gemini-api-key)))
      (if-let* ((apikey (dpc-gemini/--read-property-value-from-cache "apikey")))
          (setq dpc-gemini-api-key (s-trim apikey))))
  (or
   dpc-gemini-api-key
   (error "No gemini api key is set yet")))


(defun dpc-gemini/--parse-properties-file-into-cache ()
  "Reads the properties file `dpc-gemini-properties-file' and
populates `dpc-gemini--properties-cache'. Returns t if the file was
successfully parsed or re-parsed, nil otherwise. Invalidates cache and
re-reads if the file's modification time changes. Within the file, this
function handles 'key: value' format, skips empty lines and lines
starting with '#'."
  (let* ((expanded-props-file (expand-file-name dpc-gemini-properties-file))
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
                          (mapcar
                           ;; Split line into key/value pair, trimming whitespace
                           (lambda (line)
                             (when (string-match "^\\([^:]+\\): *\\(.*\\)$" line)
                               (cons (intern (downcase (s-trim (match-string 1 line))))
                                     (s-trim (match-string 2 line)))))
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

  ;; ;; --- Optional: Provide user feedback for debugging ---
  ;; (let ((expanded-filename  (expand-file-name dpc-gemini-properties-file)))
  ;;   (unless (file-exists-p expanded-filename)
  ;;     (message "Warning: Gemini properties file does not exist: %s" expanded-filename))
  ;;   (unless dpc-gemini-api-key
  ;;     (message "Warning: Gemini APIkey not found in %s" expanded-filename))
  ;;   (unless dpc-gemini-selected-model
  ;;     (message "Warning: Default Gemini model not found in %s" expanded-filename)))
  )

(defun dpc-gemini/get-config-property (propname)
  "Returns a property named PROPNAME extracted from the gemini properties
file, `dpc-gemini-properties-file'. If that file had not been previously
read, this function reads the file and caches the result."
  (if-let* ((value (dpc-gemini/--read-property-value-from-cache propname)))
      (let ((trimmed-value (s-trim value)))
        (if (string= propname "apikey")
            (setq dpc-gemini-api-key trimmed-value))
        trimmed-value)))

(provide 'dpc-gemini)

;;; dpc-gemini.el ends here
