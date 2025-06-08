;;; salted.el  -*- coding: utf-8; lexical-binding: t;  -*-

;; Functions for opening files encrypted with "salt_file"
;;
;; Copyright (c) 2019-2025 Google LLC
;;
;; Author: Dino Chiesa
;; Version: 20250517
;; Created: Monday, 28 October 2019, 16:15
;; Url: https://github.com/DinoChiesa/salted
;;

;;; Commentary:
;;
;; Pretty simple: if you open a file with a name that ends in .salted , this
;; code will ask you for a passphrase and will decrypt the file. Subsequent
;; saves will re-encrypt the file.
;;
;; To use it, go get the salt_file utility from
;; https://github.com/DinoChiesa/salted. Then put the following in your
;; ~/.emacs:
;;
;;     (require 'salted)
;;     (setq salted--salt-file-utility "~/location/of/salt_file")
;;     (setq auto-mode-alist
;;         (append
;;          '(("\\.salted\\'" . salted-file-mode))))
;;
;; or:
;;    (use-package salted
;;      :defer t
;;      :commands (salted-encrypt-buffer-to-new-file salted-file-mode)
;;      :config
;;      (setq salted--salt-file-utility
;;            (executable-find
;;             (if (eq system-type 'windows-nt)
;;                 "salt_file.exe"
;;               "salt_file")))
;;      (setq auto-mode-alist
;;            (append
;;             '(("\\.salted\\'" . salted-file-mode)))))
;;

;; Then just use emacs as normal, to open a file with a .salted extension.
;;
;; To encrypt a file, open a plain text file, then
;;    M-x salted-save-buffer-to-new-encrypted-file
;;
;; NB: If you have "untabify" set in a before-save-hook, you need to remove or
;; disable it for buffers with .salted files; untabifying in the
;; before-save-hook can modify the ciphertext byte stream, which makes it
;; un-decryptable.
;;

;;; Code:

(require 'simple)

(defvar salted--salt-file-utility
  (if (eq system-type 'windows-nt)
      "~/bin/salt_file.exe"
    "~/bin/salt_file")

  "The location of the salt_file utilty to encrypt and decrypt")

(defvar salted--salt-file-passphrase ""
  "The passphrase")

(defvar salted--saved-position 0 "the position in the file before saving")

;; (defun salted-decrypt-file (passwd)
;;   "decrypt the file"
;;   (let ((coding-system-for-write 'no-conversion)
;;         (coding-system-for-read 'no-conversion))
;;     (call-process-region (point-min) (point-max) salted--salt-file-utility
;;                          t t nil "-in" buffer-file-name "-out" "-" "-passphrase" passwd "-decrypt")))

(defun salted-decrypt-buffer (&optional passphrase)
  "decrypt the buffer"
  (interactive (list (if current-prefix-arg
                         ;; If a prefix argument is given (e.g., C-u M-x ...),
                         ;; force prompting for the passphrase interactively.
                         (read-passwd "passphrase: ")
                       ;; Otherwise, provide nil as the initial argument.
                       nil)))

  ;; Check if the passphrase was provided. If not, prompt the user.
  (unless (stringp passphrase)
    (setq passphrase (read-passwd "passphrase: ")))

  (let ((coding-system-for-write 'no-conversion)
        (coding-system-for-read 'no-conversion))
    (call-process-region (point-min) (point-max) salted--salt-file-utility
                         t t nil "-in" "-" "-out" "-" "-passphrase" passphrase "-decrypt")))

(defun salted-encrypt-buffer-to-file (&optional passphrase)
  "encrypt the buffer"
  (interactive (list (if current-prefix-arg
                         ;; If a prefix argument is given (e.g., C-u M-x ...),
                         ;; force prompting for the passphrase interactively.
                         (read-passwd "passphrase: ")
                       ;; Otherwise, provide nil as the initial argument.
                       nil)))

  ;; Check if the passphrase was provided. If not, prompt the user.
  (unless (stringp passphrase)
    (setq passphrase (read-passwd "passphrase: ")))

  (message "encrypting to (%s)" buffer-file-name)
  (let ((coding-system-for-write 'no-conversion)
        (coding-system-for-read 'no-conversion))
    (call-process-region (point-min) (point-max) salted--salt-file-utility
                         t t nil "-in" "-" "-out" "-" "-passphrase" passphrase)))

(defun salted-save-buffer-to-new-encrypted-file (&optional passphrase)
  "save the buffer in encrypted form to a new .salted file"
  (interactive (list (if current-prefix-arg
                         ;; If a prefix argument is given (e.g., C-u M-x ...),
                         ;; force prompting for the passphrase interactively.
                         (read-passwd "passphrase: ")
                       ;; Otherwise, provide nil as the initial argument.
                       nil)))

  (let ((file-path (buffer-file-name)))
    (if file-path
        (let ((out-file-path (concat file-path ".salted")))

          ;; Check if the passphrase was provided. If not, prompt the user.
          (unless (stringp passphrase)
            (setq passphrase (read-passwd "passphrase: ")))

          (message "encrypting to (%s)" out-file-path)
          (let ((coding-system-for-write 'no-conversion)
                (coding-system-for-read 'no-conversion))
            (call-process-region (point-min) (point-max) salted--salt-file-utility
                                 t t nil "-in" "-" "-out"
                                 out-file-path "-passphrase" passphrase)))
      (message "buffer is not visiting a file - cannot encrypt"))))



(define-generic-mode 'salted-file-mode
  (list ?#)
  nil nil
  '(".salted\\'")
  (list (lambda ()
          (add-hook 'before-save-hook
                    (lambda ()
                      (setq salted--saved-position (point))
                      (salted-encrypt-buffer-to-file salted--salt-file-passphrase))
                    nil t)

          (add-hook 'after-save-hook
                    (lambda ()
                      ;;(salted-decrypt-file salted--salt-file-passphrase)
                      (salted-decrypt-buffer salted--salt-file-passphrase)
                      (goto-char salted--saved-position)
                      (set-buffer-modified-p nil)
                      (auto-save-mode nil))
                    nil t)

          (set (make-local-variable 'salted--salt-file-passphrase) (read-passwd "passphrase: "))
          (set-buffer-file-coding-system 'no-conversion t)
          (salted-decrypt-buffer salted--salt-file-passphrase)
          (goto-char (point-min))
          (set (make-local-variable 'salted--saved-position) (point))
          (auto-save-mode nil)
          (set-buffer-modified-p nil)))
  "Mode for salted encrypted files")

(provide 'salted)

;;; salted.el ends here
