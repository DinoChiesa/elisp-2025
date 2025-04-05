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

(provide 'dpc-sys-darwin)
