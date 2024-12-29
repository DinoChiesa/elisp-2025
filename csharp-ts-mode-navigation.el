;;; csharp-ts-mode-navigation.el --- define some helpers for navigating in csharp-ts-mode
;;
;; Author: Dino Chiesa
;; Created: Sunday, 29 December 2024, 00:34
;; Package-Requires: (csharp-ts-mode.el)
;; URL:
;; X-URL:
;; Version: 2024.12.29
;; Keywords: csharp treesit
;; License: Apache 2.0

;;; Commentary:

;; This package provides Code navigation fns for `csharp-ts-mode', built on
;; `treesit-beginning-of-thing' and other treesit AST traversal functions.
;;
;; `csharp-ts-mode' is a treesitter mode, which means it will build an Abstract Syntax
;; Tree (AST) via treesit.el and a language-specific tree-sitter grammar. By linking the
;; nodes in the AST to positions in the code buffer, it is possible to navigate cleanly
;; through the code, jumping to the top of the class, bottom of the enclosing method,
;; and so on.  These helper methods facilitate that.

;; To install, place this .el file in your load path and specify this in your init file:
;;
;; (use-package csharp-ts-mode-navigation
;;     :after csharp-mode
;;     :config (add-hook 'csharp-ts-mode-hook 'ctsn/setup))
;;
;; By default it sets up key bindings in csharp-ts-mode buffers:
;;
;;   C-M-a   treesit-beginning-of-defun
;;   C-M-e   treesit-end-of-defun
;;   C-<     ctsn/beginning-of-method
;;   C->     ctsn/end-of-method
;;   C-M-<   ctsn/beginning-of-class
;;   C-M->   ctsn/end-of-class


(require 'treesit)

(defun ctsn/navigate-to-thing (node-type-name &optional want-end)
  "move to beginning or end of a prior thing with node-type of
NODE-TYPE-NAME. Examples are \"method_declaration\",
\"class_declaration\", \"block\", \"comment\", etc."
  ;; strategy: find the node for the thing, move point to the beginning of it,
  ;; then optionally move point to the end of it.
  (let ((regexp node-type-name)
        (treesit-defun-tactic 'top-level)) ;; unsure if necessary
    (treesit-beginning-of-thing regexp 1) ;; 1 means one backward

    ;; The AST is a tree of nodes. Given a node within the tree, a command can move
    ;; point in the code buffer to the beginning or the end of that node very easily, by
    ;; combining `goto-char' with `treesit-node-start' or `treesit-node-end'. But, using
    ;; `treesit-beginning-of-thing' to navigate to a thing, does not give us the node we
    ;; just navigated to. It just moves point.

    ;; Callers that want to navigate to the END of the node at point must inquire,
    ;; "what node is at point?"  There is a function `treesit-node-at' that tells us
    ;; the node at a specified position.  But there may be more than one node starting
    ;; at any given position. For example a method declaration, as well as the access
    ;; modifier for the method (like "public"), both begin at the same position.

    ;; The behavior of `treesit-node-at' is not documented, as far as I know, when the
    ;; specified position denotes the beginning of multiple nodes. In this case it is
    ;; not known which node will be returned.

    ;; This code presumes `treesit-node-at' returns either the thing, or a _descendant_
    ;; of the thing.  In the case of a method_declaration, the descendant might be
    ;; either a modifier (like public) or a type (like void). This code looks at the
    ;; type of the starting node, and uses that if it matches NODE-TYPE-NAME, otherwise
    ;; it traverses UP the AST to the first parent node with a type that matches
    ;; NODE-TYPE-NAME, then moves point to the end of that matching node.
    (if want-end
        (let* ((target-node-or-descendant (treesit-node-at (point)))
               (target-node
                (if (string= (treesit-node-type target-node-or-descendant) node-type-name)
                    target-node-or-descendant
                  (treesit-parent-until
                   target-node-or-descendant
                   (lambda (node)
                     (string= (treesit-node-type node) node-type-name))))))
          (if target-node
              (goto-char (treesit-node-end target-node)))))))

(defun ctsn/beginning-of-method ()
  "move to the beginning of the enclosing method. Compare to
`treesit-beginning-of-defun', which moves to the beginning of the
enclosing method OR to the beginning of a prior lambda expression.
See also, `ctsn/end-of-method'."
  (interactive)
  (ctsn/navigate-to-thing  "method_declaration"))

(defun ctsn/end-of-method ()
  "move to the end of the enclosing method. See also `ctsn/beginning-of-method',
`ctsn/beginning-of-class', and `ctsn/end-of-class'."
  (interactive)
  (ctsn/navigate-to-thing "method_declaration" t))

(defun ctsn/beginning-of-class ()
  "move to beginning of the enclosing class. Compare to `ctsn/beginning-of-method'.
See also `ctsn/end-of-class' and `ctsn/end-of-method'."
  (interactive)
  (ctsn/navigate-to-thing  "class_declaration"))

(defun ctsn/end-of-class ()
  "move to the end of the enclosing class. See also `ctsn/beginning-of-class',
`ctsn/beginning-of-method', and `ctsn/end-of-method'."
  (interactive)
  (ctsn/navigate-to-thing "class_declaration" t))

(defun ctsn/setup ()
  "set up navigation functions and key bindings for same, for `csharp-ts-mode'."

  ;; `treesit-defun-type-regexp' is a regexp that matches the node type (in the
  ;; AST) of defun (==function) nodes. We can construe "function" to be "method"
  ;; nodes or nodes that define lambda expressions.
  ;;
  ;; The following, and variations of the following, did not work for me, not
  ;; sure why. I tried walking through the treesit code and could not make sense
  ;; of what it was doing, or why this did not work.
  ;;
  ;;   (setq-local treesit-defun-type-regexp
  ;;         "lambda_expression\\|method_declaration")

  ;; There is a parameter_list within each lambda or method, so the following
  ;; sort of works. Using this regexp and invoking `treesit-beginning-of-defun'
  ;; will position the point at the beginning of the parameter list for either
  ;; the enclosing method or the nearest upward sibling lambda.
  (setq-local treesit-defun-type-regexp "parameter_list")

  ;; To navigate this way, it is also required to set `treesit-defun-tactic'.
  ;; `treesit-beginning-of-defun' seems to search only within the narrowed AST
  ;; for the current line/item, for where point is. This is sort of mentioned in
  ;; the treesit.el code comments, but I wasn't completely clear on how it
  ;; is intended to work.
  ;;
  ;; Using 'top-level' here seems to tell treesit to widen its search out of the
  ;; single statement to enclosing scopes. In practice with C#, this name
  ;; "top-level" is a misnomer. It does not imply navigating only to "top level"
  ;; constructs. The other option, and the default, is 'nested', which seems not
  ;; very useful. With tactic='nested', if your point is on a comment and you
  ;; invoke `treesit-beginning-of-defun' thinking you will navigate to the
  ;; enclosing method, ... nothing happens.
  (setq-local treesit-defun-tactic 'top-level)

  ;; bindings for the basic treesit defun navigation fns
  (local-set-key (kbd "C-M-a") 'treesit-beginning-of-defun)
  (local-set-key (kbd "C-M-e") 'treesit-end-of-defun)

  ;; bindings for the elaborated navigation fns
  (local-set-key (kbd "C-<") 'ctsn/beginning-of-method)
  (local-set-key (kbd "C->") 'ctsn/end-of-method)
  (local-set-key (kbd "C-M-\<") 'ctsn/beginning-of-class)
  (local-set-key (kbd "C-M-\>") 'ctsn/end-of-class)
  )

(provide 'csharp-ts-mode-navigation)

;;; csharp-ts-mode-navigation.el ends here
