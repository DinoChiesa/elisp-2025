;;; dpc-gemini.el --- invoke the Gemini API from emacs
;;
;; Author: Dino Chiesa,
;; Created: Monday, 23 December 2024, 20:53
;; Package-Requires: (url.el)
;; URL:
;; X-URL:
;; Version: 2024.12.23
;; Keywords: gemini llm
;; License: New BSD

;;; Commentary:


;;   (require 'dpc-gemini)
;;   (setq dpc-gemini-api-key "XXXXXXXXXXXX")

;;    -or-

;;   (require 'dpc-gemini)
;;   (dpc-gemini/set-api-key-from-file "~/path/to/.google-gemini-apikey")

;; Optionally, set a key binding:
;; (define-key global-map (kbd "C-c C-g") 'dpc-gemini/get-buffer-for-prompt)

;;
;;; Revisions:
;;
;; 2024.12.23  Dino Chiesa
;;    first cut. invoke `dpc-gemini/get-buffer-for-prompt' to get a Gemini
;;    response. The function tries to foll paragraphs, excepting code,
;;    in the response.


;;; License
;;
;; Copyright (c) 2024, Google LLC
;;


(require 'json)

(defgroup dpc-gemini nil
  "Provides a facility to invoke Gemini."
  :group 'Editing)

(defcustom dpc-gemini-api-key nil
  "The api key for connecting to generativelanguage.googleapis.com.

Get one by visiting  https://aistudio.google.com/app/apikey

"
  :type 'string
  :group 'dpc-gemini)

;; (defun dpc-gemini-string-last-index-of (s c)
;;   "Returns the index of the last occurrence of character C in string S.
;; Returns nil if not found.
;;
;; "
;;   (let ((i (1- (length s)))
;;         ix c2)
;;     (while (and (>= i 0) (not ix))
;;       (setq c2 (aref s i))
;;       (if (= c c2)
;;           (setq ix i))
;;       (setq i (- i 1)))
;;     ix))

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
    (let* ((base-url "https://generativelanguage.googleapis.com/")
           (gem-url
           (concat
            base-url
            "v1beta/models/gemini-1.5-flash:generateContent?key="
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


;;;###autoload
(defun dpc-gemini/set-api-key-from-file (filename)
  "read the gemini api key from a file"
  (setq dpc-gemini-api-key
        (and (file-exists-p filename)
             (with-temp-buffer
               (insert-file-contents filename)
               (replace-regexp-in-string
                "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" ""
                (buffer-substring-no-properties (point-min) (point-max)))))))

(provide 'dpc-gemini)

;;; dpc-gemini.el ends here
