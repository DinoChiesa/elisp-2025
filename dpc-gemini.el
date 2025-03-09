;;; dpc-gemini.el --- invoke the Gemini API from emacs
;;
;; Author: Dino Chiesa
;; Created: Monday, 23 December 2024, 20:53
;; Package-Requires: (json.el seq.el subr-x.el)
;; URL:
;; X-URL:
;; Version: 2025.02.14
;; Keywords: gemini llm
;; License: New BSD

;;; Commentary:


;;   (require 'dpc-gemini)
;;   (setq dpc-gemini-api-key "XXXXXXXXXXXX")

;;    -or-

;;   (require 'dpc-gemini)
;;   (dpc-gemini/set-api-key-from-file "~/path/to/.google-gemini-apikey")

;; Optionally, set a key binding:
;;   (keymap-global-set  (kbd "C-;") 'dpc-gemini/get-buffer-for-prompt 1))

;;
;;; Revisions:
;;
;; 2025.02.14  Dino Chiesa
;;    added `dpc-gemini/select-model' to allow the user to select a specific model to use
;;    in `dpc-gemini/get-buffer-for-prompt'. Also added `dpc-gemini/get-generative-models'
;;    which is used by select-model and can be used by other packages too.
;;    And `dpc-gemini/list-models' which is sort of a novelty fn to list the models
;;    in a buffer without offering a selection.
;; 2024.12.23  Dino Chiesa
;;    first cut. invoke `dpc-gemini/get-buffer-for-prompt' to get a Gemini
;;    response. The function tries to fold paragraphs, excepting code,
;;    in the response.


;;; License
;;
;; Copyright (c) 2024-2025, Google LLC
;;


(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup dpc-gemini nil
  "Provides a facility to invoke Gemini."
  :group 'Editing)

(defcustom dpc-gemini-base-url "https://generativelanguage.googleapis.com/"
  "the base URL for the Gemini API"
  :type 'string
  :group 'dpc-gemini)

(defcustom dpc-gemini-api-key nil
  "The api key for connecting to generativelanguage.googleapis.com.

Get one by visiting  https://aistudio.google.com/app/apikey

"
  :type 'string
  :group 'dpc-gemini)

(defcustom dpc-gemini-model "models/gemini-2.0-flash"
  "The model key for Gemini. Eg, \"models/gemini-2.0-flash\" .
Find models at https://ai.google.dev/gemini-api/docs/models/gemini
"
  :type 'string
  :options '("models/gemini-2.0-flash"
             "models/gemini-2.0-flash-exp"
             "models/gemini-2.0-pro-exp"
             "models/gemini-2.0-flash-lite-preview"
             "models/gemini-1.5-flash" )
  :group 'dpc-gemini)

(defun dpc-gemini/post-prompt (gem-url gem-prompt)
  "Perform an HTTP POST to GEM-URL with a one-part text prompt given
in GEM-PROMPT. Place the result into a newly created buffer, and
pop to that buffer when complete.
"
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

(defun dpc-gemini/list-models (&optional full-text)
  "List the available Gemini models that have  \"genrateContent\" as one of the
members of supportedGenerationMethods. Optional arg FULL-TEXT says
to return the full text of the response and do not filter by supportedGenerationMethods."
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

(defun dpc-gemini/-current-generative-model (model)
  "a predicate that looks at a model and returns non-nil if the
model is current and supports \"generateContent\"."
  (let ((description (gethash "description" model))
        (supported-methods
         (gethash "supportedGenerationMethods" model)))
    (and
     (not (string-match-p (rx (or "discontinued" "deprecated")) description))
     (seq-contains-p supported-methods "generateContent"))))


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


(defun dpc-gemini/chapgpt-shell-converter (model)
  "converts between the model returned by Gemini, and
the model description needed by chatgpt-shell ."
  (let ((model-name (gethash "name" model))
        (model-cwindow (gethash "inputTokenLimit" model)))
    (let ((model-version (string-remove-prefix "models/" model-name)))
      (let ((model-shortversion (string-remove-prefix "gemini-" model-version))
            (model-urlpath (concat "/v1beta/" model-name)))
        (chatgpt-shell-google-make-model :version model-version
                                         :short-version model-shortversion
                                         :path model-urlpath
                                         :token-width 4
                                         :context-window model-cwindow)))))


(defun dpc-gemini/looking-at-three-backticks ()
  "Check if the point is before three backticks, with optional preceding newline."
  (if (looking-at "\\(?:^\\|\n\\)```")
      (match-end 0)))


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

(defun dpc-gemini/get-buffer-for-prompt ()
  "retrieve a response from Gemini."
  (interactive)
  (if (not (and (boundp 'dpc-gemini-api-key)
                (stringp dpc-gemini-api-key)))
      (let ((msg (concat "You need to get an \"api key\" from Google.\n"
                         "Then, set it in your .emacs with a statement like:\n"
                         "    (setq dpc-gemini-api-key \"XXXXXXXXXXXX\") \n")))
        (message msg)
        (browse-url "https://aistudio.google.com/app/apikey")
        nil)
    (let* ((gem-url
            (concat
             dpc-gemini-base-url
             "v1beta/"
             dpc-gemini-model
             ":generateContent?key="
             dpc-gemini-api-key))
           (gem-prompt
            (read-from-minibuffer "ask gemini ? " nil nil nil nil nil)))
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

(defun dpc-gemini/select-model ()
  "set the Gemini model to use."
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
      (setq dpc-gemini-model (concat "models/" selected-model)))))


;;;###autoload
(defun dpc-gemini/get-gemini-api-key ()
  "return the gemini api key, that was possibly read from a file"
  (interactive)
  (if (and (boundp 'dpc-gemini-api-key)
           (stringp dpc-gemini-api-key))
      dpc-gemini-api-key
    (error "no gemini api key is set yet")))

;;;###autoload
(defun dpc-gemini/set-api-key-from-file (filename)
  "read the gemini api key from a file"
  (interactive)
  (setq dpc-gemini-api-key
        (and (file-exists-p filename)
             (with-temp-buffer
               (insert-file-contents filename)
               (replace-regexp-in-string
                "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" ""
                (buffer-substring-no-properties (point-min) (point-max)))))))

(provide 'dpc-gemini)

;;; dpc-gemini.el ends here
