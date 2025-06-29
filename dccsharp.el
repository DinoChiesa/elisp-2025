;;; dccsharp.el -*- coding: utf-8; lexical-binding: t;  -*-

;; utility functions for working with CSharp
;;
;; Copyright © 2025 Google, LLC
;;
;; Author     : Dino Chiesa
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : June 2025
;; Version    : 1.0
;; Requires   : s.el dash.el
;; License    : Apache 2.0
;; Last-saved : <2025-June-29 07:24:17>
;;
;;; Commentary:
;;
;; This module defines a few elisp functions that are handy for working
;; with C# code in emacs.
;;
;; This module adds a few extra things to that basic set up:
;;
;;  - `dccsharp-auto-add-using' adds a using statement to the current
;;    file, if necessary, based on the short name of the class under
;;    point. When using "IHttpClientFactory", the module will add a using
;;    System.Net.Http.  TODO: If there are multiple classes by the given name,
;;    the user will geta popup choice.
;;

;;; License
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;; Code:
;;

(require 's) ;; magnars' long lost string library
(require 'cl-seq) ;; for cl-remove-if-not

(defvar dccsharp--load-path (or load-file-name "~/elisp/dccsharp.el")
  "For internal use only. ")

(defconst dccsharp--class-or-ns-regex "\\([a-zA-Z_$][a-zA-Z0-9_$]*\\.\\)*[a-zA-Z_$][a-zA-Z0-9_$]*"
  "a regex that matches a qualified or unqualified classname or package name")

(defconst dccsharp--edge-of-symbol-regex
  "[ \t(,\\<\\[=]"
  "A regex that matches the leading edge of a c# symbol or classname")

(defconst dccsharp--using-stmt-regex (concat "^using[\t ]+" dccsharp--class-or-ns-regex
                                             "[\t ]*;")
  "a regex that matches a C# using statement")

(defconst dccsharp--comment-line-regex "^[\t ]*//"
  "a regex that matches a line that begins with a comment")

(defconst dccsharp--empty-line-regex "^[\t ]*$")

;; (defun dccsharp--filter (condp lst)
;;   "filters the list LST, removing each item for which condp returns nil"
;;   (delq nil
;;         (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; (defun dccsharp--is-class-name (str)
;;   "returns true if the string appears to be formed like a c# class name"
;;   (let ((case-fold-search nil))
;;     (string-match dccsharp--class-or-ns-regex str)))

(defun dccsharp--using-line-sorter-predicate (a b)
  "Return t if A should come before B.
Sorts alphabetically, but places comments and empty lines at the end."
  (let ((empty-a (string-match-p dccsharp--empty-line-regex a))
        (empty-b (string-match-p dccsharp--empty-line-regex b))
        (comment-a (string-match-p dccsharp--comment-line-regex a))
        (comment-b (string-match-p dccsharp--comment-line-regex b)))
    (cond
     ((and (not empty-a) (not comment-a) (or empty-b comment-b)) t)
     ((and (not empty-b) (not comment-b) (or empty-a comment-a)) nil)
     ((and comment-a empty-b) t)
     ((and comment-b empty-a) nil)
     (t (string-lessp a b)))))

(defun dccsharp--sort-using-lines (beg end)
  "Sort lines in region alphabetically, moving comments and empty lines to the end."
  (let* ((text (buffer-substring-no-properties beg end))
         (lines (split-string text "\n"))
         (sorted-lines (sort lines #'dccsharp--using-line-sorter-predicate))
         (sorted-text (mapconcat #'identity sorted-lines "\n")))
    (delete-region beg end)
    (insert sorted-text)))

(defun dccsharp-sort-using-statements ()
  "sorts the using statements in a file. This is sort-of not
super useful, as csharpier includes this function implicitly,
and apheleia-mode runs csharpier on save."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward dccsharp--using-stmt-regex nil t)
        (let ((start nil))
          (beginning-of-line)
          (setq start (point))
          (while (or
                  (looking-at dccsharp--using-stmt-regex t)
                  (looking-at dccsharp--comment-line-regex t)
                  (looking-at dccsharp--empty-line-regex t))
            (forward-line))
          (beginning-of-line)
          (dccsharp--sort-using-lines  start (point)))
      (message "cannot find using statements"))))

(defun dccsharp--class-or-qualified-member-name-at-point ()
  "returns the Shortclass.Methodname under point"
  (save-excursion
    (let ((case-fold-search nil))
      (if (re-search-backward dccsharp--edge-of-symbol-regex (line-beginning-position) 1)
          (forward-char))
      (if (looking-at dccsharp--class-or-ns-regex)
          (buffer-substring-no-properties (match-beginning 0) (match-end 0))))))

(defun dccsharp--position-of-class-or-qualified-member-name-at-point ()
  "returns the position of the  Shortclass.Methodname under point"
  (save-excursion
    (let ((case-fold-search nil))
      (if (re-search-backward dccsharp--edge-of-symbol-regex (line-beginning-position) 1)
          (forward-char))
      (if (looking-at dccsharp--class-or-ns-regex)
          (list (match-beginning 0) (match-end 0))))))

(defun dccsharp-auto-add-using ()
  "Adds a using statement for the class or interface at point, if possible.
Uses eglot and LSP to find the right action."
  (interactive)
  (if-let* ((server (and (fboundp 'eglot--current-server-or-lose)
                         (eglot--current-server-or-lose)))
            (symbol-position (dccsharp--position-of-class-or-qualified-member-name-at-point))
            (actions (eglot-code-actions (car symbol-position) (cadr symbol-position)))
            (add-using-action
             (cl-find-if (lambda (action)
                           (let ((title (plist-get action :title)))
                             (string-match-p "\\`using " title)))
                         actions)))
      (progn
        (eglot-execute server add-using-action)
        (message "Applied 'add using' action."))
    (message "No 'add using' action found.")))


(provide 'dccsharp)

;;; dccsharp.el ends here
