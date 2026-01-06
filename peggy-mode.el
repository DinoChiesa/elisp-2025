;;; peggy-mode.el --- A major mode for editing Peggy.js grammar files -*- lexical-binding: t; -*-

;; Copyright © 2026 Google LLC
;; Author: Gemini
;; Keywords: languages, peggy, pegjs, parser
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; This file provides `peggy-mode`, a major mode for editing Peggy.js
;; (https://peggyjs.org/) grammar files. It provides syntax highlighting
;; for grammar rules, operators, and embedded JavaScript code blocks.
;;
;; To use it, save this file as `peggy-mode.el` in a directory in your
;; `load-path`, and add `(require 'peggy-mode)` to your init file.
;; Files ending in `.pegjs` will automatically open in `peggy-mode`.

;;; Code:

(require 'js nil t) ; require js-mode, but don't fail if not found

(defvar peggy-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C++ style comments "//"
    (modify-syntax-entry ?/ ". 1" table)
    (modify-syntax-entry ?\n ">" table)

    ;; String literal delimiter
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    ;; Matching pairs for navigation
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Identifiers can contain underscores.
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for `peggy-mode`.")


(defun peggy--fontify-js-region (start end)
  "Fontify region from START to END with JS syntax."
  (let ((js-keywords (when (fboundp 'js--font-lock-keywords-1)
                       js--font-lock-keywords-1)))
    (when js-keywords
      (font-lock-fontify-region start end js-keywords))))

(defface peggy-js-block-face
  '((t :inherit font-lock-doc-face))
  "Face for JS blocks in peggy-mode."
  :group 'peggy)

(put 'peggy-js-block-face 'font-lock-fontify-region-function #'peggy--fontify-js-region)



(defvar peggy-mode-font-lock-keywords
  (list
   ;; Comments
   '("//.*" 0 font-lock-comment-face t)

   ;; JS initializer block {{...}}. Fontify contents as JS.
   ;; This isn't quite right, because multi-line fontification does not work like this.
   ;; There is a different approach the mode must take.
   ;;
   ;; TODO: Fix this.
   '("{{\\(\\(?:.\\|\n\\)*?\\)}}" 1 peggy-js-block-face t)

   ;; JS action block {...}. The regex is too simple for nested braces,
   ;; so we just fontify the whole thing with a single face to avoid errors.
   '("{\\(?:.\\|\n\\)*?}" 0 font-lock-doc-face t)

   ;; Character class
   '("\\[\\(?:[^\\]\\\\]\\|\\\\.\\)*\\]" . font-lock-string-face)

   ;; String literals (includes the optional 'i' flag for case-insensitivity)
   '("'\\(?:[^'\\\\]\\|\\\\.\\)*'i?" . font-lock-string-face)
   '("\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"i?" . font-lock-string-face)

   ;; Rule definition (at beginning of line)
   '("^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)" (1 font-lock-function-name-face))

   ;; Label
   '("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\):" (1 font-lock-variable-name-face))

   ;; PEG operators: *, +, ?, &, !, /
   '("[*+?&!/]" . font-lock-keyword-face)

   ;; Definition operator
   '("=" . font-lock-keyword-face)

   ;; Rule reference (any other identifier)
   '("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\b" . font-lock-constant-face)
   )
  "Font lock keywords for Peggy mode.")

;;;###autoload
(define-derived-mode peggy-mode prog-mode "Peggy"
  "Major mode for editing Peggy.js grammar files."
  :syntax-table peggy-mode-syntax-table
  (setq-local font-lock-defaults '((peggy-mode-font-lock-keywords)))
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\s-*//+ *"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pegjs\\'" . peggy-mode))

(provide 'peggy-mode)

;;; peggy-mode.el ends here
