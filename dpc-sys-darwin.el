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

;; The builtin ls on macos does not support --group-directories-first flag,
;; which apparently can cause "Listing directory failed but ‘access-file’" when
;; using dired.  This supposedly fixes it. But requires a homebrew gls.

(let ((gls-fqpath "/opt/homebrew/bin/gls"))
  (cond
   ((file-exists-p gls-fqpath)
    (setq insert-directory-program "/opt/homebrew/bin/gls"))
   (t
    (message "Did not find gls; consider brew install coreutils")
    ;; Alternative: just use lisp code to get the directory listings. This is
    ;; slower, but it is functional.
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))))


(defun dpcmac/font-available-p (font-name)
  "Return t if FONT-NAME is available on the current system."
  (if (find-font (font-spec :name font-name)) t nil))


(let ((fonts '("IBM Plex Mono" "JetBrains Mono" "Menlo")))
  (cl-loop for font in fonts
           when (dpcmac/font-available-p font)
           return (set-face-attribute 'default nil :family font :height 120 :weight 'normal)))


(provide 'dpc-sys-darwin)
