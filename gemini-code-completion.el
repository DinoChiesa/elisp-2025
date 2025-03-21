;;; gemini-code-completion.el --- Code completion empowered by Google Gemini -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hitoshi Uchida <hitoshi.uchida@gmail.com>

;; Author: Hitoshi Uchida <hitoshi.uchida@gmail.com>
;; Version: 1.0
;; Package-Requires: ((google-gemini "0.1.0") (emacs "25.1"))
;; URL: https://github.com/shishimaru/gemini-code-completion.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; gemini-code-completion.el is an Emacs package to get code completion with
;; Google Gemini for a code at current cursor postion or a selected region.

;;; Code:

(require 'google-gemini)

(defvar gemini-code-completion-default-prompt
  "Based on the surrounding context, suggest code that should appear here. Limit
each of your suggestions to a few lines of code, no more than 10 lines each. And no
more than 5 suggestions. Take care with proper indentation in suggestions you offer."
  "Default prompt to input into Google Gemini.")

(defvar gemini-code-completion-model "gemini-2.0-flash"
  "The short name of the model to use.")

(defun gemini-code-completion-extract-completion (response)
  "Extract and clean completion text from the RESPONSE returned by Google Gemini.
This currently just selects the first suggestion of all the candidates.

TODO: integration with a completion framework to present a completion list."

  (let* ((candidates (alist-get 'candidates response))
         (first-candidate (aref candidates 0))
         (content (alist-get 'content first-candidate))
         (parts (alist-get 'parts content))
         (first-part (aref parts 0))
         (text (alist-get 'text first-part)))
    (gemini-code-completion-clean-text text)))

(defun gemini-code-completion-clean-text (text)
  "Remove unwanted backquotes from the TEXT."
  (replace-regexp-in-string "```[a-z]*\n\\|```" "" text))

(defun gemini-code-completion-handler (response)
  "Handle Gemini code completion RESPONSE."

  (let ((current-position (point))
        (completion-text (gemini-code-completion-extract-completion response)))
    (insert
     (if (use-region-p)
         "\n"
       "")
     (propertize completion-text 'face 'shadow))
    (when (use-region-p)
      (deactivate-mark))
    (goto-char current-position))) ; Restore cursor position

;;;###autoload
(defun gemini-complete (prefix)
  "Get completion from Google Gemini for current buffer or a selected region.
If called with a PREFIX argument (\\[universal-argument]), prompt for additional
text to customize the completion."
  (interactive "P")

  (let* ((user-prompt (if prefix (concat (read-string "Prompt: ") "\n\n") ""))
         (selected-text
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) (point))))
         (end
          (if (use-region-p)
              (region-end)
            (point))))
    (if (not google-gemini-key)
        (error "must specify `google-gemini-key'")
      (save-excursion
        (goto-char end)
        (google-gemini-content-generate
         (concat
          user-prompt gemini-code-completion-default-prompt selected-text)
         #'gemini-code-completion-handler
         :model gemini-code-completion-model
         :key google-gemini-key
         )))))


(provide 'gemini-code-completion)
;;; gemini-code-completion.el ends here
