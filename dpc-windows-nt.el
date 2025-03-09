;;; dpc-windows-nt.el --- functions for windows-nt, for Dino.
;;
;; Author: Dino Chiesa
;; Created: Saturday,  8 March 2025, 19:52
;; Package-Requires: (package)
;; URL:
;; X-URL:
;; Version: 2025.03.08
;; Keywords: utility
;; License: New BSD

;;; Commentary:

(defun dpc-windows-nt-shortpath (s)
  "Resolves paths containing spaces (like paths starting from \"c:\\program files\")
into the short-path form on windows. This makes it easier for emacs to execute
commands in such directories."
  (and s (s-trim
          (shell-command-to-string
           (concat "pwsh.exe -Command (New-Object -ComObject Scripting.FileSystemObject).GetFolder('"
                   s
                   "').ShortPath")))))

(defun dpc-windows-nt-configure-external-utilities ()
  "Fixup some external utilities for use on Windows."
  (if (file-exists-p "C:/ProgramData/chocolatey/bin/dnzunzip.exe")
      (setq archive-zip-use-pkzip nil   ; i.e. use unzip instead
            archive-zip-extract '("C:/ProgramData/chocolatey/bin/dnzunzip.exe" "-")))

  (eval-after-load "grep"
    '(progn
       (dpc-windows-nt-fixup-grep-command))))


(defun dpc-windows-nt-fixup-grep-command ()
  "Fixup the grep command.

Git ships with unix-ish tools that actually work, so let's use the grep
there (and xargs, etc)."
  (let* ((git-cmd (executable-find "git"))
         (git-bin-dir (and git-cmd
                           (dpc-windows-nt-shortpath
                            (replace-regexp-in-string "cmd/git\.exe" "usr/bin" git-cmd)))))
    (if (and git-bin-dir (file-exists-p git-bin-dir))
        (let ((rfn1 (lambda (s) (replace-regexp-in-string "PATH" (regexp-quote git-bin-dir) s)))
              ;; I don't know why forward slashes don't work for the grep exes, but they don't.
              (rfn2 (lambda (s) (replace-regexp-in-string "/" (regexp-quote "\\") s))))

          (progn
            (grep-apply-setting 'grep-use-null-device nil)
            (grep-apply-setting 'grep-command (funcall rfn2 (funcall rfn1 "PATH/grep.exe -H -i -n ")))

            ;; I don't know why there is both grep-find-command and grep-find-template
            (grep-apply-setting
             'grep-find-command
             (funcall rfn2
                      (funcall rfn1
                               "PATH/find.exe . -type f -print0 | PATH/xargs.exe -0 PATH/grep.exe -i -n ")))

            (grep-apply-setting
             'grep-find-template
             (funcall rfn2
                      (funcall rfn1
                               "PATH/find.exe <D> <X> -type f <F> -print0 | PATH/xargs.exe -0 PATH/grep.exe <C> -n --null -e <R>"))))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
 '(ls-lisp-use-localized-time-format t)
 '(temporary-file-directory (concat (getenv "HOME") "/AppData/Local/Temp")))

(provide 'dpc-windows-nt)
