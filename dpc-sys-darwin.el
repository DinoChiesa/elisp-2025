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



;; Not sure any of this is necessary. It seems to just work without any
;; of this customization. Maybe this is legacy from very old emacs.

;; (setq select-enable-clipboard t)
;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))
;;
;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))
;;
;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)


(provide 'dpc-sys-darwin)
