;;; dpc-sys-gnu-linux.el --- functions for GNU/Linux, for Dino.  -*- lexical-binding: t; -*-
;;
;; Author: Dino Chiesa
;;
;;; Commentary:

;;; Code:

;; On gLinux chromebook, setting `initial-frame-alist' is ineffectual. This may be because
;; ChromeOS/Wayland window managers often ignore client-requested top/left/width/height hints.

(setq initial-frame-alist
      '((top . 10) (left . 10)
        (height . 42) (width . 148)))
(set-frame-font "Noto Mono 11" nil t)

(when-let* ((emoji-font (seq-find (lambda (f) (find-font (font-spec :name f)))
                                  '("Noto Color Emoji" "Symbola"))))
  (set-fontset-font t 'emoji (font-spec :family emoji-font) nil 'prepend))

(provide 'dpc-sys-gnu-linux)
;;; dpc-sys-gnu-linux.el ends here
