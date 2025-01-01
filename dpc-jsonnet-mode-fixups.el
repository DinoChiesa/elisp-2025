;;; dpc-jsonnet-mode-fixups.el --- some changes for jsonnet-mode
;;
;; Author: Dino Chiesa
;; Created: Wednesday,  1 January 2025, 02:49
;; Package-Requires: (jsonnet-mode.el)
;; URL:
;; X-URL:
;; Version: 2024.12.31
;; Keywords: jsonnet
;; License: New BSD

;;; Commentary:

;; To use:
;;   (require 'dpc-jsonnet-mode-fixups)
;;

;; jsonnet-mode uses `call-process' to invoke the jsonnet program. This does not work
;; for .jsonnet files that are visited over TRAMP.
;; I filed a PR https://github.com/tminor/jsonnet-mode/pull/34
;; but who knows if it will be merged. In the meantime, this will work.

(require 'jsonnet-mode)

(defun dino/jsonnet-eval-buffer ()
  "Run jsonnet with the path of the current file. This also works for
remote files accessed over TRAMP."
  (interactive)
  (let ((file-to-eval (file-truename (buffer-file-name)))
        (search-dirs jsonnet-library-search-directories)
        (output-buffer-name "*jsonnet output*"))
    (save-some-buffers (not compilation-ask-about-save)
                       (let ((directories (cons (file-name-directory file-to-eval)
                                                search-dirs)))
                         (lambda ()
                           (member (file-name-directory (file-truename (buffer-file-name)))
                                   directories))))
    (let ((cmd jsonnet-command)
          (args (append jsonnet-command-options
                        (cl-loop for dir in search-dirs
                                 collect "-J"
                                 collect dir)
                        (list (file-local-name file-to-eval))))
          (dir-to-use-for-process-file (file-name-directory (buffer-file-name))))
      (with-current-buffer (get-buffer-create output-buffer-name)
        (setq buffer-read-only nil)
        (erase-buffer)
        (let ((default-directory dir-to-use-for-process-file))
          (if (zerop (apply #'process-file cmd nil t nil args))
              (progn
                (when (fboundp 'json-mode)
                  (json-mode))
                (view-mode))
            (compilation-mode nil)))
        (goto-char (point-min))
        (display-buffer (current-buffer) '(nil (inhibit-same-window . t)))))))

(define-key jsonnet-mode-map (kbd "C-c C-c") 'dino/jsonnet-eval-buffer)

(provide 'dpc-jsonnet-mode-fixups)

;;; dpc-jsonnet-mode-fixups.el ends here
