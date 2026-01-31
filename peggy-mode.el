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

    ;; String literal delimiter - in an undelimited char range, not quotes
    ;;(modify-syntax-entry ?\" "\"" table)
    ;;(modify-syntax-entry ?' "\"" table)

    ;; make quotes plain word chars?  (they sometimes are)
    (modify-syntax-entry ?\" "w" table)
    (modify-syntax-entry ?' "w" table)

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
  (let ((js-keywords (when (boundp 'js--font-lock-keywords-1)
                       js--font-lock-keywords-1)))
    (when js-keywords
      (font-lock-fontify-region start end js-keywords))))

(defface peggy-js-block-face
  '((t :inherit font-lock-doc-face))
  "Face for JS blocks in peggy-mode. Do I need this?"
  :group 'peggy)

(defface peggy-pattern-face
  '((t :inherit font-lock-builtin-face))
  "Face for regex patterns or named patterns in peggy-mode."
  :group 'peggy)

(defface peggy-block-delimiter-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for the block delimiters."
  :group 'peggy)

;; What does this do?
(put 'peggy-js-block-face 'font-lock-fontify-region-function #'peggy--fontify-js-region)


;; (defun peggy-fontlock-mark-block ()
;;   "A fn that marks a multi-line block with the
;; `font-lock-multiline' text property, and returns  `peggy-js-block-face'"
;;
;;   (let ((type (get (intern-soft (match-string 1)) 'lisp-define-type)))
;;     (cond ((eq type 'var) font-lock-variable-name-face)
;;             ((eq type 'type) . #9=(font-lock-type-face))
;;             ((or (not #1=(match-string 2)) (and #1# (match-string 4)))
;;              font-lock-function-name-face)))
;;   )


;; JS initializer block {{...}}. Fontify contents as JS.
;;
;; This is WIP, because multi-line fontification is an obscure, poorly
;; implemented, poorly-described art. There is a `font-lock-multiline' variable
;; as well as a `font-lock-multiline' text property. Why the needless name clash?
;; A better name for the variable might be "font-lock-attempt-to-infer-multiline-blocks".
;; I believe it is documented as being... "not reliable"?
;;
;;
;; I think the correct approach is
;;
;; - add a fn to `font-lock-extend-region-functions', to mark multi-line regions
;;   via a text-property, `font-lock-multiline'.
;;
;; - let font-lock-keywords scan and do the matching.  font-lock will strip
;;   the `font-lock-multiline' text-property as part of this scan, so the
;;   highlighting logic must RE-apply it.


;; From the documentation:
;;
;; The font-lock scan normally starts at the beginning of a line.  Elements
;; of font-lock-keywords should not match across multiple lines. That doesn’t
;; work reliably, because Font Lock usually scans just part of the buffer,
;; and it can miss a multi-line construct that crosses the line boundary
;; where the scan starts.
;;
;;    NB: The documentation doesn't discuss if the use
;;    of `font-lock-extend-region-functions' leads to an exception of this
;;    rule.
;;
;; When Font Lock is about to highlight a range of text, it first extends the
;; boundaries of the range as necessary so that they do not fall within text
;; marked with the font-lock-multiline property. Then it removes any
;; font-lock-multiline properties from the range, and highlights it. The
;; highlighting specification (mostly font-lock-keywords) must reinstall this
;; property each time, whenever it is appropriate.

;;
;; FACENAME is an _expression_ whose value is the face to use. It can
;; be any form that returns a facename.

;; Hmmm, Maybe I don't need to do anything with these double braces.
;; Just let the font-lock keywords highlight what is within.
;;
;; ;;
;; ;; TODO: Fix this.
;; ;;'("{{\\(\\(?:.\\|\n\\)*?\\)}}" 1 peggy-js-block-face t)
;; `("^{{\\(\\(?:.\\|\n\\)*?\\)}}" 1
;;   ,(let ((begin-block (match-beginning 0))
;;          (end-block  (match-end 0)))
;;     (if (> begin-block 0)
;;         (with-silent-modifications
;;           (put-text-property begin-block end-block 'font-lock-multiline t))))
;;   t)

(defvar peggy-mode-font-lock-keywords
  (list
   ;; Comments
   '("//.*" 0 'font-lock-comment-face t)

   ;; double-curly BEGIN or END blocks
   '("^\\({{\\|}}\\)\\s-*$"  1 'peggy-block-delimiter-face t)

   ;; MAYBE
   ;; ;; JS action block {...}. The regex is too simple for nested braces,
   ;; ;; so we just fontify the whole thing with a single face to avoid errors.
   ;; '("{\\(?:[^{]\\|\n\\)*?}" 0 font-lock-doc-face t)

   ;; NO NO NO
   ;; ;; Character class within square brackets - not sure I want this
   ;; '("\\[\\(?:[^\\]\\\\]\\|\\\\.\\)*\\]" . font-lock-string-face)

   ;; NO NO NO
   ;; ;; regex pattern following a fieldname and colon
   ;; ;;'("^\\s-*\\(?:=\\|/\\)[^{}]+\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\):\\(\\S-+\\)"
   ;; '("^\\s-*\\(?:=\\|/\\)[^{}]*\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\):\\(\\S-+\\)"
   ;;   (1 'font-lock-variable-name-face t)
   ;;   (2 'peggy-pattern-face t)
   ;;   )

   ;; hacky special case for a parenthesis-wrapped string containing a single single-quote
   '("(\\(\"'\"\\))"
     (1 font-lock-string-face t))

   ;; and similar for a string containing a single double-quote
   '("(\\('\"'\\))"
     (1 font-lock-string-face t))

   ;; defining a type
   '("^\\s-*\\(=\\|/\\)\\([^{\n]+\\)\\({\\)"
     (1 'font-lock-builtin-face t)
     (3 'peggy-block-delimiter-face t)

     ;; Anchored Matchers for patterns within the line
     ;; a fixed, quoted string. (Single quotes not supported here)
     ("\\s-+\\(\"[^\"]*\"\\)\\s-+"
      (progn (goto-char (match-beginning 2)) (match-end 2))
      (goto-char (match-end 2))
      (1 'font-lock-string-face))

     ;; a fieldname and colon, followed by a pattern name or regex
     ("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(:\\(\\S-+\\)\\)?"
      (progn (goto-char (match-beginning 2)) (match-end 2))
      (goto-char (match-end 2))
      ;; Highlight the sub-match
      (1 'font-lock-variable-name-face )
      (3 'peggy-pattern-face nil t))

     )

   ;; closing brace of a return value
   '("\\([^\n]+\\)\\(}\\)\\s-*$"
     (2 'peggy-block-delimiter-face t))

   ;; paren or comma at end of line
   '("\\([^\n]+\\)\\([,;]\\)\\s-*$"
     (2 'font-lock-punctuation-face t))

   ;; Single-quote String literals (with optional trailing 'i' flag)
   '("\\('\\(?:\\\\'\\|[^'\n]\\)*'\\)\\(i\\)?"
     (1 font-lock-string-face)
     (2 font-lock-preprocessor-face nil t)) ;; do not override previously applied face

   ;; Double-quote String literals (with optional trailing 'i' flag)
   `("\\(\"\\(?:\\\\\"\\|[^\"\n]\\)*\"\\)\\(i\\)?"
     (1 font-lock-string-face)
     (2 font-lock-preprocessor-face nil t))


   ;; JS function definition
   `("^\\s-*\\(function\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(\\(.*\\))"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     ;; Anchored Matcher for fn arguments
     ("\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
      (goto-char (match-beginning 3))
      (goto-char (match-end 3))
      (0 font-lock-variable-name-face)
      ))

   ;; JS keywords
   '("\\b\\(for\\|if\\|let\\|var\\|const\\|return\\)\\b"
     (1 font-lock-keyword-face))

   ;; JS constants
   '("\\b\\(null\\|false\\|true\\)\\b"
     (1 font-lock-constant-face))

   ;; Rule definition with line continuation
   '("^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*$" (1 font-lock-type-face))

   ;; Rule definition with equals sign
   '("^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-+\\(=\\)"
     (1 font-lock-type-face)
     (2 font-lock-builtin-face))

   ;; MAYBE NOT
   ;;    ;; Label
   ;; '("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\):" (1 font-lock-variable-name-face))

   ;; PEG operators: *, +, ?, &, !, /
   '("[*+?&!/]" . font-lock-keyword-face)

   ;; Definition operator
   '("=" . font-lock-keyword-face)

   ;; MAYBE NOT
   ;;    ;; Rule reference (any other identifier)
   ;; '("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\b" . font-lock-constant-face)

   )
  "Font lock keywords for Peggy mode.")




;; 20260130-1515
;; This did not work. In fact it seemed to pre-empt complete fontification of the
;; buffer. Everything AFTER the marked multi-line section ... was... ignored.
;; This stuff is poorly documented and has zero explanatory tutorials. Also there
;; are seven ways to do things.
;;
;;
;; (defun peggy-extend-font-lock-region ()
;;   "In peggy-mode, find multi-line constructs, then mark them,
;; and extend the region to include them.
;;
;; This is intended to be added to `font-lock-extend-region-functions', as with
;;
;;   (push 'peggy-extend-font-lock-region font-lock-extend-region-functions)"
;;
;;   (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
;;   (message (format "peflr: point %d flb %d" (point) font-lock-beg))
;;   (if (get-text-property (point) 'font-lock-multiline)
;;       (progn
;;         (message (format "peflr: already multiline"))
;;         nil)
;;     (save-excursion
;;       (goto-char font-lock-beg)
;;       (if-let* ((start-ml-block  "^{{[:space:]*$" )
;;                 (begin-block
;;                  (when (and
;;                         (re-search-backward start-ml-block nil t)
;;                         ;;(not (get-text-property (point) 'font-lock-multiline))
;;                         )
;;                    (point)))
;;                 (end-block
;;                  (if (re-search-forward "^}}[:space:]*$" nil t)
;;
;;                      ;; (when (not (get-text-property (point) 'font-lock-multiline))
;;                      ;;   (point))
;;
;;                      (point)
;;
;;                    (point-max))))
;;           (progn
;;             (setq font-lock-beg begin-block
;;                   font-lock-end end-block)
;;             (message (format "peflr: beg %d end %d" begin-block end-block))
;;             (with-silent-modifications
;;               (put-text-property begin-block end-block 'font-lock-multiline t))
;;             (cons begin-block end-block))
;;         (message (format "peflr: flb not between begin and end"))
;;         nil)
;;       )))


;;;###autoload
(define-derived-mode peggy-mode prog-mode "Peggy"
  "Major mode for editing Peggy.js grammar files."
  :syntax-table peggy-mode-syntax-table
  (setq-local font-lock-defaults '((peggy-mode-font-lock-keywords)))
  ;;(push 'peggy-extend-font-lock-region font-lock-extend-region-functions)
  ;;(setq-local syntax-propertize-function #'peggy-syntax-propertize)
  ;;(setq-local font-lock-fontify-region-function #'peggy-fontify-region)
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\s-*//+ *"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pegjs\\'" . peggy-mode))

(provide 'peggy-mode)

;;; peggy-mode.el ends here
