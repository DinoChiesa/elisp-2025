

(defun dc-winnt-configure-external-utilities ()
  "fixup external utilities"
  (if (file-exists-p "C:/ProgramData/chocolatey/bin/dnzunzip.exe")
      (setq archive-zip-use-pkzip nil   ; i.e. use unzip instead
            archive-zip-extract '("C:/ProgramData/chocolatey/bin/dnzunzip.exe" "-")))

  (eval-after-load "grep"
    '(progn
       (dc-winnt-fixup-grep-command))))


(defun dc-winnt-fixup-grep-command ()
  "fixup grep command. Git ships with
unix-ish tools that actually work, so let's use the grep there (and xargs, etc)."
  (let* ((git-cmd (executable-find "git"))
         (git-bin-dir (and git-cmd
                           (dc-windows-shortpath
                            (replace-regexp-in-string "cmd/git\.exe" "usr/bin" git-cmd)))))
    (if (and git-bin-dir (file-exists-p git-bin-dir))
        (let ((rfn1 (lambda (s) (replace-regexp-in-string "PATH" (regexp-quote git-bin-dir) s)))
              ;; I don't know why forward slashes don't work for the grep exes, but they don't.
              (rfn2 (lambda (s) (replace-regexp-in-string "/" (regexp-quote "\\") s))))

          (progn
            (grep-apply-setting 'grep-command (funcall rfn2 (funcall rfn1 "PATH/grep.exe -i -n ")))

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
