;; 20250704-1025
;; I never use this. Just using aider in a separate terminal seems
;; fine to me.
;;
;; ;; 20250324-1743
;; ;; For aider, there are two packages: aider.el and aidermacs.el .
;; ;; Both are active.
;; ;;
;; ;; aider.el seems less ambitious. aidermacs aims to integrate with
;; ;; existing emacs stuff; uses ediff, for example.
;; ;; Beyond that I don't have a good feel for the differences.
;; ;;
;; ;; Also, it is possible to use aider side-by-side with emacs with
;; ;; no elisp package at all.
;;
;; (use-package aidermacs
;;   :defer 35
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   ;; Enable minor mode for Aider files
;;   (aidermacs-setup-minor-mode)
;;   (setenv "AIDER_WEAK_MODEL" "gemini/2.5-flash-preview-05-20")
;;   (setenv "AIDER_EDITOR_MODEL" "gemini/2.5-pro-exp-03-25")
;;

;;   :custom
;;   ;; See the Configuration section below
;;   (aidermacs-auto-commits t)
;;   (aidermacs-use-architect-mode t)
;;   (aidermacs-default-model "gemini/gemini-2.5-pro-exp-03-25"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; embark
;; ;;
;; ;; This is sort of a helper that provides hints for things you might want to do
;; ;; with candidates. For examine in a minibuffer presenting a list of buffers or
;; ;; files or functions, there is a set of things you might want to do with it.
;; ;; For example, M-x find-file  ja C-x  will
;; ;; - find file (prompt you)
;; ;; - present files matching ja*
;; ;; - C-x will be embark-export the list into a new Embark buffer, and you can
;; ;;    then C-; (embark-act) on any of those items or a combination of those items.
;; ;;
;; ;; 20250405-2010 - This sounds neat but so far I am not using it very much.
;; ;;
;; (use-package embark
;;   :ensure t
;;   :demand t
;;   :defer t
;;
;;   :bind
;;   (("C-c ," . embark-act)         ;; pick some comfortable binding
;;    ("C-c ;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;
;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;
;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; wordnik (dictionary)
;; ;; 20241227 - still works
;; ;;
;; (use-package wordnik
;;   :defer t
;;   :ensure nil
;;   :config (progn
;;             (wordnik-set-api-key-from-file "~/elisp/.wordnik.apikey.txt")
;;             (define-key global-map (kbd "C-c ?") 'wordnik-show-definition)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; spelchek
;; ;; 20241227-1854 - Do I ever use this? It does not appear to be working.
;; (require 'spelchek)
;; (define-key global-map (kbd "C-x c") 'spelchek-choose-alternative-and-replace)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 20241214-2357
;; js2-mode was created in 2008? by Steve Yegge, to fill a gap in js-mode,
;; I think specifically regarding syntax highlighting. js2-mode's critical advance
;; was to build an abstract syntax tree for the JS code, rather than just parsing the
;; text. This was really cool, but at this point, js-mode + lsp-mode seems to be a
;; better combination.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; JavaScript - js2-mode (new - 20180416-1511)
;; (autoload 'js2-mode "js2-mode" nil t)
;; ;;(eval-after-load 'js2-mode '(require 'setup-js2-mode))
;; ;; Better imenu
;; (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;;
;;
;; (defun dino-js2-mode-fn ()
;;   ;;(tern-mode)
;;   (auto-complete-mode 0) ;; turn off auto-complete-mode
;;   (lsp)
;;
;;   ;; lsp, when configured to use posframe for signature display, uses the
;;   ;; background and foreground from the lsp-signature-posframe face to display
;;   ;; the signature.
;;   (set-face-attribute 'lsp-signature-posframe nil :background "LightSteelBlue1" )
;;   ;; (set-face-attribute 'lsp-signature-posframe t :background "LightSteelBlue1")
;;   ;; (face-attribute 'lsp-signature-posframe :background nil t)
;;   ;; (face-attribute 'lsp-signature-posframe :foreground nil t)
;;   (company-mode)
;;   (company-box-mode)
;;   (define-key company-mode-map (kbd "M-<tab>") 'company-complete)
;;   (setq company-minimum-prefix-length 2
;;         lsp-signature-function 'lsp-signature-posframe
;;         js2-basic-offset 2)
;;   )
;; (add-hook 'js2-mode-hook #'dino-js2-mode-fn)
;;
;; (defun dino-js2-apply-globals-from-jshintrc (&rest arguments)
;;   "load extra globals from jshintrc when sending requests"
;;   (js2-add-additional-externs (dino-read-globals-from-jshintrc)))
;;
;; (eval-after-load "js2-mode"
;;   '(progn
;;      (advice-add 'js2-apply-jslint-globals :after #'dino-js2-apply-globals-from-jshintrc)))

;; (js2r-add-keybindings-with-prefix "C-c C-m")

;; xxx WHY have I deleted this?
;;
;; (defun dino-js2-mode-fn ()
;;   (turn-on-font-lock)
;;
;;   (local-set-key "\M-\C-R"  'indent-region)
;;   (local-set-key "\M-#"     'dino-indent-buffer)
;;   (local-set-key "\C-c\C-c" 'comment-region)
;;   (local-set-key (kbd "<C-tab>") 'yas-expand)
;;
;;   (set (make-local-variable 'indent-tabs-mode) nil)
;;   ;; indent increment
;;   (set (make-local-variable 'js-indent-level) 2)
;;   (linum-on)
;;
;;   ;; use autopair for curlies, parens, square brackets.
;;   ;; electric-pair-mode works better than autopair.el in 24.4,
;;   ;; and is important for use with popup / auto-complete.
;;   (if (or (not (fboundp 'version<)) (version< emacs-version "24.4"))
;;       (progn (require 'autopair) (autopair-mode))
;;     (electric-pair-mode))
;;
;;   ;; json-mode is a child mode of js-mode. Select different checker
;;   ;; based on the file extension.
;;   (require 'flycheck)
;;   (if (and buffer-file-name
;;            (file-name-directory buffer-file-name))
;;        (progn
;;          (flycheck-mode)
;;          (flycheck-select-checker
;;           (if (string-suffix-p ".json" buffer-file-name)
;;               'json-jsonlint
;;             'javascript-jshint))))
;;
;;   (yas-minor-mode-on)
;;
;;   (require 'smart-op) ;; for smart insertion of ++ and == and += etc
;;   (smart-op-mode)
;;   )
;;
;;
;; (add-hook 'js2-mode-hook   'dino-js2-mode-fn)
