;;; -*- coding: utf-8; lexical-binding: t;  -*-
;;; ibuffer-preview.el --- A minor mode that modifies ibuffer-mode to peep each buffer as you cursor through.

;; Public domain.

;; Author:  Dino Chiesa <dchiesa001@gmail.com>
;; version: 20250713-1920

;;; Commentary:
;;
;; This is a minor mode that adds a flourish to ibuffer-mode. As you cursor
;; through the buffer names, this mode will display in another window, the
;; contents of that buffer.
;;
;; Let's see if this is actually useful, or annoying.

;;; Code:
;;

(declare-function ibuffer-current-buffer "ibuffer")

(defun ibpm/setup-ibuffer ()
  "set up ibuffer-mode to preview each buffer as the line is highlighted."
  ;; Override `display-buffer-alist' in ibuffer, to
  ;; always show the window in a pop-up window.
  (when (eq major-mode 'ibuffer-mode)
    (make-local-variable 'display-buffer-alist)
    (setq display-buffer-alist '((t display-buffer-pop-up-window)))
    (add-hook 'post-command-hook #'ibpm/preview-buffer nil t)))

(define-minor-mode ibuffer-preview-mode
  "Toggle global ibuffer-preview-mode.
Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix argument
disables it.  From Lisp, argument omitted or nil enables the mode,
`toggle' toggles the state.

Typically you will do this:

(use-package ibuffer-preview
  :load-path \"~/my-elisp-load-path\"
  :config (keymap-set ibuffer-mode-map \"v\"
          #'ibuffer-preview-mode))

And then pressing v when in ibuffer-mode, will toggle
ibuffer-preview-mode."
  :init-value nil   ; Initial value, nil for disabled
  :global t
  :group 'dotfiles
  :lighter " «ibp»"

  (if ibuffer-preview-mode
      (when (eq major-mode 'ibuffer-mode)
        (ibpm/setup-ibuffer))))

(defun ibpm/preview-buffer ()
  "fn to attach to post-command-hook in ibuffer-mode.
When the command changes the which line is highlighted, this
fn tells ibuffer to display the highlighted buffer in the other
window."
  (if ibuffer-preview-mode
      (when (or (eq this-command 'next-line)
                (eq this-command 'ibuffer-forward-line)
                (eq this-command 'previous-line)
                (eq this-command 'ibuffer-backward-line))
        (condition-case _err
            (if-let* ((buffer-under-point (ibuffer-current-buffer t)))
                (display-buffer buffer-under-point t))
          (error "No buffer there")))))

(provide 'ibuffer-preview)
;;; ibuffer-preview.el ends here
