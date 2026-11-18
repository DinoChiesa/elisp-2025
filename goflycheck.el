;;; goflycheck.el --- a flycheck checker for the Go Language -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Dino Chiesa <dpchiesa@hotmail.com>
;; Requires: go-mode.el, dc-edit.el

;;; Commentary:

;;; Code:

(require 'go-mode)
(require 'dc-edit)


;;;###autoload
(defcustom goflycheck-cmd-line-limit 28
  "This is a limit on the searching for commands.

If positive, this is the number of lines at the top of the source
file to look in, to find the command that goflycheck will use to
compile or flycheck the current buffer.

If negative, then only in the final N lines.

If the value of this variable is zero, then goflycheck looks
everywhere in the file.

The line should appear in a comment inside the Go buffer."
  :type 'integer
  :group 'goflycheck)


(defun goflycheck-set-flycheck-command ()
  "Set the flycheck command for a Go module, dynamically.

This function is intended for use as a before-syntax-check-hook with
flycheck.  Use it like this:

    (add-hook 'flycheck-before-syntax-check-hook  #'goflycheck-set-flycheck-command)

Then, in your go file, specify this in the comments near the top
of the file.  (See `goflycheck-cmd-line-limit' for details.)

    // flycheck: go build one.go two.go %f

This will cause flycheck to run the given command, replacing the %f with
the source file name.

This function is intended for internal use.  The goflycheck adds this hook
automatically when flycheck loads."

  (and (eq major-mode 'go-mode)
       (let ((cmd-string
              (dc-edit/get-value-from-comments "flycheck" goflycheck-cmd-line-limit)))
         (and cmd-string
              (not (eq cmd-string ""))
              (and (boundp 'flycheck-log-level)
                   (> flycheck-log-level 2)
                   (message "found command: %s" cmd-string))
              (let* ((cmd (split-string cmd-string " "))
                     (ferf (member "%f" cmd)))
                (and ferf (setcar ferf 'source))
                (put 'go :flycheck-command cmd))))))


(eval-after-load "flycheck"
  '(progn
     (flycheck-define-checker gocheck
       "A GO syntax checker. It uses \"go build\" as the tool.
If you would like to use a different compiler, see
`goflycheck-set-flycheck-command'."
       :command ("go" "build" source)
       :error-patterns
       ;; ex43.go:6:4: expected 'STRING', found newline
       ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
       :modes go-mode)
     (add-hook 'flycheck-before-syntax-check-hook  #'goflycheck-set-flycheck-command)
     (setq flycheck-log-level 0)))


(provide 'goflycheck)

;;; goflycheck.el ends here
