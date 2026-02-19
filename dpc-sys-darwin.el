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
(keymap-global-set "<f8>" #'open-in-finder)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the path correctly on MacOS, based on /etc/paths.d .
;; I am unsure whether this helps on linux or Windows, I've never
;; examined it closely. I have my own dino/fixup-exec-path fn that
;; seems to work for me, see below.
(use-package path-helper
  :ensure t
  :config
  (path-helper-setenv "PATH"))


(provide 'dpc-sys-darwin)
