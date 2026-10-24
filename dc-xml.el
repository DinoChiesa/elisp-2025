;;; dc-xml.el --- XML and HTML utilities   -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Dino Chiesa
;; Created: Wed, 17 Jul 2013
;; Package-Requires: ((emacs "29.1"))
;; Keywords: xml html utility

;;; Commentary:

;; XML AST parsing overrides, pretty printing, comment handling, and HTML/XML entity escaping.

;;; Code:

(require 'subr-x)
(require 'nxml-mode nil t)

(eval-and-compile
  ;; [4] NameStartChar
  (defconst dc-xml-name-start-char-re "[[:word:]:_]")

  ;; [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7
  ;;                 | [#x0300-#x036F] | [#x203F-#x2040]
  (defconst dc-xml-name-char-re "[[:word:]:_.0-9\\u00B7\\u0300-\\u036F\\u203F\\u2040-]")

  ;; [5] Name     ::= NameStartChar (NameChar)*
  (defconst dc-xml-name-re (concat dc-xml-name-start-char-re dc-xml-name-char-re "*")))

(defvar xml-validating-parser)
(defvar xml-default-ns)
(defvar xml-sub-parser)
(declare-function xml-maybe-do-ns "xml" (name default-ns xml-ns))
(declare-function xml-parse-dtd "xml" (&optional parse-ns))
(declare-function xml-parse-string "xml" ())
(declare-function xml-parse-attlist "xml" (&optional xml-ns))
(declare-function xml-node-name "xml" (node))
(declare-function xml-node-attributes "xml" (node))
(declare-function xml-node-children "xml" (node))
(declare-function xml-escape-string "xml" (string))
(declare-function xmltok-start-tag-local-name "xmltok" ())
(declare-function nxml-backward-up-element "nxml-mode" ())

;;; XML parse override to preserve comments
(defun xml-parse-tag-1 (&optional parse-dtd parse-ns)
  "Like `xml-parse-tag', but possibly modify the buffer while working.
Retains XML comments as (nil . \"<!-- comment -->\") nodes."
  (let* ((xml-validating-parser (or parse-dtd xml-validating-parser))
         (xml-ns
          (cond ((eq parse-ns 'symbol-qnames)
                 (cons 'symbol-qnames xml-default-ns))
                ((or (consp (car-safe parse-ns))
                     (and (eq (car-safe parse-ns) 'symbol-qnames)
                          (listp (cdr parse-ns))))
                 parse-ns)
                (parse-ns
                 xml-default-ns))))
    (cond
     ;; Processing instructions
     ((looking-at-p "<\\?")
      (search-forward "?>")
      (skip-syntax-forward " ")
      (xml-parse-tag-1 parse-dtd xml-ns))
     ;; CDATA
     ((looking-at "<!\\[CDATA\\[")
      (let ((pos (match-end 0)))
        (unless (search-forward "]]>" nil t)
          (error "XML: (Not Well Formed) CDATA section does not end anywhere in the document"))
        (concat
         (buffer-substring-no-properties pos (match-beginning 0))
         (xml-parse-string))))
     ;; DTD
     ((looking-at-p "<!DOCTYPE[ \\t\\n\\r]")
      (let ((dtd (xml-parse-dtd parse-ns)))
        (skip-syntax-forward " ")
        (if xml-validating-parser
            (cons dtd (xml-parse-tag-1 nil xml-ns))
          (xml-parse-tag-1 nil xml-ns))))
     ;; comment - preserved!
     ((looking-at-p "<!--")
      (let ((start (point))
            comment)
        (search-forward "-->")
        (setq comment (buffer-substring-no-properties start (point)))
        (skip-syntax-forward " ")
        (cons nil comment)))
     ;; end tag
     ((looking-at-p "</")
      '())
     ;; opening tag
     ((looking-at (eval-when-compile (concat "<\\(" dc-xml-name-re "\\)")))
      (goto-char (match-end 1))
      (let* ((node-name (match-string-no-properties 1))
             (attrs (xml-parse-attlist xml-ns))
             children)
        (when (consp xml-ns)
          (dolist (attr attrs)
            (when (and (consp (car attr))
                       (equal "http://www.w3.org/2000/xmlns/" (caar attr)))
              (push (cons (cdar attr) (cdr attr))
                    (if (symbolp (car xml-ns))
                        (cdr xml-ns)
                      xml-ns)))))
        (setq children (list attrs (xml-maybe-do-ns node-name "" xml-ns)))
        (cond
         ((looking-at-p "/>")
          (forward-char 2)
          (nreverse children))
         ((eq (char-after) ?>)
          (forward-char 1)
          (let ((end (concat "</" node-name "\\s-*>")))
            (while (not (looking-at end))
              (cond
               ((eobp)
                (error "XML: (Not Well-Formed) End of document while reading element `%s'" node-name))
               ((looking-at-p "</")
                (forward-char 2)
                (error "XML: (Not Well-Formed) Invalid end tag `%s' (expecting `%s')"
                       (let ((pos (point)))
                         (buffer-substring pos (if (re-search-forward "\\s-*>" nil t)
                                                   (match-beginning 0)
                                                 (point-max))))
                       node-name))
               ((= (char-after) ?<)
                (let ((tag (xml-parse-tag-1 nil xml-ns)))
                  (when tag
                    (push tag children))))
               (t
                (let ((expansion (xml-parse-string)))
                  (push (if (stringp (car children))
                            (concat (pop children) expansion)
                          expansion)
                        children))))))
          (goto-char (match-end 0))
          (nreverse children))
         (t
          (error "XML: (Well-Formed) Couldn't parse tag: %s"
                 (buffer-substring-no-properties (- (point) 10) (+ (point) 1)))))))
     (t
      (unless xml-sub-parser
        (error "XML: (Well-Formed) Invalid character"))
      (let ((s (xml-parse-string)))
        (when (zerop (length s))
          (error "XML: (Not Well-Formed) Could not parse: %s"
                 (buffer-substring-no-properties (point) (min (+ (point) 10) (point-max)))))
        s)))))

(defun xml-debug-print-internal (xml indent-string)
  "Outputs the XML tree in the current buffer with comments supported."
  (let ((tree xml)
        attlist)
    (insert indent-string ?< (symbol-name (xml-node-name tree)))
    (setq attlist (xml-node-attributes tree))
    (while attlist
      (insert ?\s (symbol-name (caar attlist)) "=\""
              (xml-escape-string (cdar attlist)) ?\")
      (setq attlist (cdr attlist)))
    (setq tree (xml-node-children tree))
    (if (null tree)
        (insert ?/ ?>)
      (insert ?>)
      (dolist (node tree)
        (cond
         ((listp node)
          (cond
           ((not (car node))
            (insert (cdr node)))
           (t
            (insert ?\n)
            (xml-debug-print-internal node (concat indent-string "  ")))))
         ((stringp node)
          (insert (xml-escape-string node)))
         (t
          (error "Invalid XML tree"))))
      (when (not (and (null (cdr tree)) (stringp (car tree))))
        (insert ?\n indent-string))
      (insert ?< ?/ (symbol-name (xml-node-name xml)) ?>))))

(defun dc-xml/pretty-print-region (begin end)
  "Pretty format XML markup in region between BEGIN and END."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (setq end (+ end 1)))
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (setq end (+ end 1)))
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n")
      (setq end (+ end 1)))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))

(defun dc-xml/pretty-print-buffer ()
  "Pretty print XML in the entire buffer."
  (interactive)
  (dc-xml/pretty-print-region (point-min) (point-max)))

(defvar dc-xml-html-escape-pairs '(("&" "&amp;")
                                   ("<" "&lt;")
                                   (">" "&gt;"))
  "List of pairs of strings to swap when escaping and unescaping HTML.")

(defun dc-xml--replace-s-non-interactively (from-string to-string)
  "Non-interactive replacement of FROM-STRING with TO-STRING."
  (while (search-forward from-string nil t)
    (replace-match to-string nil t)))

(defun dc-xml/escape-html-in-region (start end)
  "Escape HTML special characters in region between START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mapc (lambda (elt)
              (goto-char (point-min))
              (dc-xml--replace-s-non-interactively (car elt) (cadr elt)))
            dc-xml-html-escape-pairs))))

(defun dc-xml/unescape-html-in-region (start end)
  "Unescape HTML entities in region between START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mapc (lambda (elt)
              (goto-char (point-min))
              (dc-xml--replace-s-non-interactively (cadr elt) (car elt)))
            dc-xml-html-escape-pairs))))

(defvar dc-xml-escape-pairs '(("&" "&amp;")
                              ("<" "&lt;")
                              (">" "&gt;")
                              ("'" "&apos;")
                              ("\"" "&quot;"))
  "List of pairs of strings to swap when escaping and unescaping XML.")

(defun dc-xml/unescape-xml-in-region (start end)
  "Replace XML entities with their character referents in region between START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mapc (lambda (elt)
              (goto-char (point-min))
              (dc-xml--replace-s-non-interactively (cadr elt) (car elt)))
            dc-xml-escape-pairs))))

(defun dc-xml/comment-region (beg end &optional arg)
  "Comment or uncomment XML in region from BEG to END, handling nesting."
  (interactive "*r\nP")
  (if (> beg end)
      (let (tmp) (setq tmp beg beg end end tmp)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (cond
       ((and (listp arg) (> (length arg) 0)
             (re-search-forward (concat "<!-- *" "\\(.*\\n\\)+" " *-->") nil t))
        (goto-char (point-min))
        (re-search-forward "<!-- *" nil t)
        (replace-match "")
        (goto-char (- (point-max) 1))
        (re-search-backward " *-->" nil t)
        (replace-match "")
        (goto-char (point-min))
        (while (re-search-forward "< ! - - " nil t)
          (replace-match "<!-- "))
        (goto-char (point-min))
        (while (re-search-forward " - - >" nil t)
          (replace-match " -->")))
       (t
        (goto-char (point-min))
        (while (re-search-forward "<!-- " nil t)
          (replace-match "< ! - - "))
        (goto-char (point-min))
        (while (re-search-forward " -->" nil t)
          (replace-match " - - >"))
        (goto-char (point-min))
        (insert "<!--\n")
        (goto-char (- (point-max) 1))
        (unless (= 10 (following-char))
          (forward-char))
        (insert "\n-->"))))))

(defun dc-xml--maybe-comment-region (old-function &rest arguments)
  "Invoke `dc-xml/comment-region' when in `nxml-mode'."
  (apply
   (if (eq major-mode 'nxml-mode)
       #'dc-xml/comment-region
     old-function)
   arguments))

(defun dc-xml/setup-comment-advice ()
  "Enable XML-aware comment region advice on `comment-region'."
  (advice-add #'comment-region :around #'dc-xml--maybe-comment-region))

(defun dc-xml/nxml-where ()
  "Display the XPath hierarchy of XML elements at point."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point))
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element)
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (let ((xpath (concat "/" (mapconcat #'identity path "/"))))
          (if (called-interactively-p 'interactive)
              (progn
                (kill-new xpath)
                (message "%s" xpath))
            xpath))))))

(provide 'dc-xml)

;;; dc-xml.el ends here