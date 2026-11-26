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

(provide 'dpc-sys-gnu-linux)
;;; dpc-sys-gnu-linux.el ends here
