;;; dpc-sys-darwin.el --- functions for MacOS, for Dino.
;;
;; Author: Dino Chiesa
;; Created: Saturday,  5 April 2025, 20:28
;; Package-Requires: (package)
;; URL:
;; X-URL:
;; Version: 2025.04.05
;; Keywords: utility
;; License: New BSD

;;; Commentary:

;;; Code:


(defun open-in-finder ()
  "Open current folder in Finder. Works on Mac, in dired mode."
  (interactive)
  (shell-command "open ."))
(keymap-global-set "<f8>" #'open-in-finder)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the path correctly on MacOS, based on /etc/paths.d .
;; I am unsure whether this helps on linux or Windows, I've never
;; examined it closely. I have my own dino/fixup-exec-path fn that
;; seems to work for me, see below.
(use-package path-helper
  :ensure t
  :config
  (path-helper-setenv "PATH"))


;; The builtin ls on macos does not support --group-directories-first flag,
;; which apparently can cause "Listing directory failed but ‘access-file’" when
;; using dired.  This supposedly fixes it. But requires a Santa exception.
;;(setq insert-directory-program "/opt/homebrew/bin/gls")

;; Alternative: just use lisp code to get the directory listings. This is
;; slower, but it is functional.
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)



(defun dpcmac/font-available-p (font-name)
  "Return t if FONT-NAME is available on the current system."
  (if (find-font (font-spec :name font-name)) t nil))


(let ((fonts '("IBM Plex Mono" "JetBrains Mono" "Menlo")))
  (cl-loop for font in fonts
           when (dpcmac/font-available-p font)
           return (set-face-attribute 'default nil :family font :height 120 :weight 'normal)))


(provide 'dpc-sys-darwin)
