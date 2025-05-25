;;; -*- coding: utf-8; lexical-binding: t;  -*-

;;; emacs.el -- Dino's .emacs setup file.
;;
;; Last saved: <2025-May-24 16:24:57>
;;
;; Works with v30.1 of emacs.
;;

;;; Commentary:

;;; Code:
(message "Running emacs.el...")
;;(debug-on-entry 'package-initialize)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; we don't need no steenking icons

(setq inhibit-splash-screen t)
(setq initial-scratch-message ";;;  -*- lexical-binding: t; -*-\n;; scratch buffer\n")

;; not sure why but unicode chars are not being retained in this file.
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)

;; 20250406 I think this needs to be system-wide. Setting it in a mode hook is too late,
;; as the file local vars will have already been evaluated and set.
(setq enable-local-variables t)

;; 20250405-1501
;;
;; The Windows API is built entirely on top of UTF-16.
;; This will not affect the coding system of the buffer, only of
;; handling of the clipboard.
(when (eq system-type 'windows-nt)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

(setq visible-bell nil)
(setq ring-bell-function `(lambda ()
                            (set-face-background 'default "DodgerBlue")
                            (sit-for 0.002)
                            (set-face-background 'default "black")))

(setq scroll-error-top-bottom t) ;; move cursor when scrolling not possible
(setq user-mail-address "dpchiesa@hotmail.com")
(setq comment-style 'indent) ;; see doc for variable comment-styles
(setq Buffer-menu-name-width 40)
(setq edebug-print-length nil)
(setq read-file-name-completion-ignore-case t)
(setq default-directory "~/")
;; helpful for debugging lisp code:
(setq messages-buffer-max-lines 2500)
(setq completion-auto-help nil)
(put 'eval-expression 'disabled nil)

;; set truncation on side-by-side windows to nil.
(setq truncate-partial-width-windows nil)

(setq-default fill-column 80)
(setq auto-save-interval 500)
(setq case-fold-search nil)
(setq comment-empty-lines t)

(add-to-list 'load-path "~/elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up faces etc
(custom-set-faces
 '(default                         ((t (:background "black" :foreground "white") )))
 '(region                          ((t (:background "gray19"))))
 '(flycheck-error                  ((t (:background "firebrick4"))))
 '(font-lock-comment-face          ((t (:foreground "PaleVioletRed3"))))
 '(font-lock-keyword-face          ((t (:foreground "CadetBlue2"))))
 '(tooltip                         ((t (:foreground "Navy" :background "khaki1"))))
 ;;'(font-lock-keyword-face        ((t (:foreground "Cyan1"))))
 '(font-lock-type-face             ((t (:foreground "PaleGreen"))))
 '(font-lock-constant-face         ((t (:foreground "DodgerBlue"))))
 '(font-lock-function-name-face    ((t (:foreground "RoyalBlue1"))))
 '(font-lock-variable-name-face    ((t (:foreground "LightGoldenrod"))))
 '(font-lock-string-face           ((t (:background "gray11" :foreground "MediumOrchid1")))))

;; to try just one:
;; (set-face-attribute 'dictionary-button-face nil :background "gray11"  :foreground "LightSteelBlue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for tetris
(and (boundp 'tetris-score-file)
     (setq tetris-score-file "~/elisp/tetris-scores")
     ;; 20250223-1425
     ;;     ;;  Turn off leaderboard prompt at end of game?
     ;;      (defadvice tetris-end-game (around zap-scores activate)
     ;;        (save-window-excursion ad-do-it))
     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package manager
;;
(require 'package)
(dolist (item (list
               ;; '("MELPA Stable"     . "https://stable.melpa.org/packages/")
               '("MELPA"     . "https://melpa.org/packages/")
               '("jcs-elpa"  . "https://jcs-emacs.github.io/jcs-elpa/packages/")
               '("org"       . "http://orgmode.org/elpa/")))
  (add-to-list 'package-archives item))

;;(package-initialize)

;; Set the load-path for all packages installed by package. Some of the
;; package-installed versions over builtin versions, if they are updated more
;; recently.
;;
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some must-have utility things
;;
(use-package s
  :ensure t)

(use-package memoize
  :defer t
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dino's utility functions
;;
(use-package dino-utility
  :load-path "~/elisp"
  :commands (dino/camel-to-snakecase-word-at-point
             dino/snake-to-camelcase-word-at-point
             dino/indent-buffer
             dino/indent-line-to-current-column
             dino/shfmt-buffer)
  :autoload (dino/maybe-add-to-exec-path
             dino/find-latest-nvm-version-bin-dir
             dino/setup-shmode-for-apheleia
             dino/find-executable-in-paths)
  :config
  (add-hook 'before-save-hook 'dino/untabify-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some configuration specific for system-types.
;; Loads system-type config; e.g. "darwin.el" on Mac
;; TODO: for replace slashes in "gnu/linux"  and add in
;; customizations there.
(let ((system-specific-elisp (concat "~/elisp/dpc-sys-" (symbol-name system-type) ".el")))
  (if (file-exists-p system-specific-elisp)
      (load system-specific-elisp)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ls-lisp-format-time-list (quote ("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")))
 '(ls-lisp-use-localized-time-format t)
 '(temporary-file-directory "/tmp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((eq system-type 'windows-nt)
  (dpc-windows-nt-configure-external-utilities)
  )
 (t   ;; not windows-nt
  (eval-after-load "grep"
    '(progn
       (grep-apply-setting 'grep-command "grep -H -i -n " )
       ;; (setq-default grep-command "grep -H -i -n ")
       ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set path correctly on MacOS, based on /etc/paths.d
(use-package path-helper
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (path-helper-setenv "PATH")
  ;;(path-helper-setenv-all) ;; not sure how this is different
  )

;; 20241122-1947 - various tools and packages - apheleia, csslint, magit,
;; csharpier, shfmt, aider and more - need exec-path AND/or environment PATH to be set.
;; Any nodejs tool installed via "npm i -g" (eg ) should be on the path already.
(dino/maybe-add-to-exec-path
 (let ((home-dir (getenv "HOME")))
   (list
    "c:/Program Files/Git/usr/bin"     ;; for diff, for apheleia
    (dino/find-latest-nvm-version-bin-dir)
    (concat home-dir "/.dotnet/tools") ;; csharpier
    (concat home-dir "/bin")
    (concat home-dir "/.local/bin")    ;; aider
    (concat home-dir "/go/bin")        ;; shfmt, jsonnetfmt
    "/usr/local/bin"
    "/usr/bin"
    "/usr/lib/google-golang/bin"
    "/usr/local/git/current/bin"
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apheleia - clever code reformatting for multiple languages.
;;
;; https://github.com/radian-software/apheleia
;;
;; It runs code formatters on after-save-hook, and then resaves only if there
;; are changes. It does its best to preserve cursor location across these changes. Sweet.
;; Works on java-mode js-mode css-mode sh-mode csharp-mode...
;;
;; It works on the Windows machine, IFF the unix-y commands apheleia requires,
;; like diff, are available. This can be a problem because there are multiple
;; disparate diff.exe tools installed on Windows typically, and apheleia depends
;; on the --rcs option for diff, which is not always supported.  The solution is
;; to ensure that the right gnu diff.exe is available. There is one shipped with
;; git; putting c:\program files\Git\usr\bin at the top of `exec-path' allows
;; apheleia to work. This is done just above.  Of course, we still need the
;; appropriate apheleia formatter to be configured and available.
;;

(use-package apheleia
  :ensure t
  :defer t
  :config (progn
            (require 'apheleia-log)
            (setq apheleia-log-debug-info t)

            (if (eq system-type 'windows-nt)
                (cl-dolist (item apheleia-formatters)
                  (when (and (consp (cdr item)) (equal "apheleia-npx" (cadr item)))
                    (setf (cadr item) "npx.ps1"))))

            ;; 20250223-1330
            ;;
            ;; I learned today that modifying `apheleia-mode-alist' is
            ;; unnecessary. In my csharp-ts-mode-fn, I could just (setq-local
            ;; apheleia-formatter 'csharpier) ... to override the
            ;; apheleia-mode-alist lookup.
            (setq apheleia-formatters (cl-remove-if (lambda (element)
                                                      (eq (car element) 'csharpier))
                                                    apheleia-formatters))
            (let ((cmd-list
                   (if (eq system-type 'windows-nt)
                       '("dotnet" "csharpier" "--write-stdout")
                     '("csharpier" "format")
                     )))
              (push (cons 'csharpier cmd-list) apheleia-formatters))
            ;; To retrieve a formatter by name:
            ;; (alist-get 'csharpier apheleia-formatters)
            ;; To remove a formatter incorrectly added:
            ;;(setq apheleia-formatters (delq (assoc 'csharpier apheleia-formatters) apheleia-formatters))
            (push '(csharp-mode . csharpier) apheleia-mode-alist)
            (push '(csharp-ts-mode . csharpier) apheleia-mode-alist)
            )
  ;; To check a mode from the mode-alist:
  ;;(alist-get 'csharp-mode apheleia-mode-alist)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;
;; COMPlete-ANYthing.

(use-package company
  :defer 31
  :config
  ;; Not sure why we need both "TAB" and "<tab>".
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") (lambda () (interactive) (company-complete-common-or-cycle -1))))


(use-package company-box
  :defer t
  :after (company)
  :hook (company-mode . company-box-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package indent-bars
  :defer 19
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x509-mode
;;
;; View certs and CRLs, other stuff, in emacs. It relies on the
;; openssl utility.  Untested on Windows as yet.
;;
(use-package x509-mode
  :defer 36
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rego
;;
;; OPA configuration language. As of 20250312-0217,
;; rego-mode on MELPA is out of date and unmaintained.

(use-package rego-mode
  :if (file-exists-p "~/elisp/rego-mode.el")
  :load-path "~/elisp"
  :pin manual
  :defer t
  :commands (rego-repl-show rego-mode)

  :config
  (when (boundp 'apheleia-formatters)
    (when (not (alist-get 'opa-fmt apheleia-formatters))
      (push '(opa-fmt . ("opa" "fmt"))
            apheleia-formatters))
    (when (not (alist-get 'rego-mode apheleia-mode-alist))
      (push '(rego-mode . opa-fmt) apheleia-mode-alist)))

  (defun dino-rego-mode-fn ()
    (display-line-numbers-mode)
    (when (and (boundp 'apheleia-formatters)
               (alist-get 'rego-mode apheleia-mode-alist))
      (apheleia-mode)))

  (add-hook 'rego-mode-hook 'dino-rego-mode-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package default-text-scale
  :ensure t
  :defer 21
  :config (progn
            (default-text-scale-mode)
            (keymap-global-set "C-=" #'default-text-scale-increase)
            (keymap-global-set "C--" #'default-text-scale-decrease)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treesit
  :defer t
  :config
  ;;(tree-sitter-require 'c-sharp) ;; this is the old (pre v29, not integrated) tree-sitter package
  ;; 20241229 - There is a grammar for c# at https://github.com/tree-sitter/tree-sitter-c-sharp
  ;; but the Makefile there says that "Windows is not supported."  Wow!
  (message (concat "csharp TS lang available ?: "
                   (prin1-to-string
                    (treesit-language-available-p 'c-sharp)))))


(use-package hl-line
  ;;:defer 9
  :config (progn
            (set-face-background hl-line-face "gray18")
            (global-hl-line-mode)))

;; ;; 20250223-1338 - this may be obsolete now.
;; ;; I couldn't get eval-after-load to work with hl-line, so
;; ;; I made this an after advice.
;; (defadvice hl-line-mode (after
;;                          dino-advise-hl-line-mode
;;                          activate compile)
;;   (set-face-background hl-line-face "gray18"))

(global-hl-line-mode)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; icomplete
;;
;; This package is old but good and actively maintained.  icomplete-vertical WAS
;; a third-party thing: https://github.com/oantolin/icomplete-vertical , and
;; it's also still available on MELPA, but as of emacs 28, it is now a feature
;; of the builtin icomplete-mode.  It gives equivalent or better behavior as
;; compared to the old independent module, though the configuration options are
;; slightly different.


;; 20250521-1755
(defun dpc--sort-alpha-but-dot-slash-last (candidates)
  "Sort a list of CANDIDATES alphabetically, except that  \"./\"
always appears at the end.

This is intended for use as a `cycle-sort-function' for
the `file' category in `completion-category-overrides', like so:

   (setq completion-category-overrides
         `((buffer
            (styles . (initials flex))
            (cycle . 10))
           (command
            (styles . (substring))
            (cycle-sort-function . ,#'dpc--sort-alpha-but-dot-slash-last))
           (file
            (styles . (basic substring))
            (cycle-sort-function . ,#'dpc--sort-alpha-but-dot-slash-last)
            (cycle . 10))
           (symbol-help
            (styles basic shorthand substring))))

Without this, the default behavior of `icomplete-vertical-mode' when
used with `find-file' is to show the list of candidates sorted by length
of the filename and then alphabetically. Similarly for commands, when
using M-x. This seems counter-intuitive and user-hostile to me. The
implementation for that is in `completion-all-sorted-completions' which
is defined in minibuffer.el; it uses a function called
`minibuffer--sort-by-length-alpha'. (headslap)

TODO: add key bindings to the `icomplete-vertical-mode-minibuffer-map'
that invoke commands to alter this sorting. Eg, by file mtime, or by
filename extension. That might be YAGNI.
"
  (sort candidates
        (lambda (a b)
          (cond
           ((string= a "./") nil)
           ((string= b "./") t)
           (t (string< a b))))))

(use-package icomplete
  :demand t
  :custom
  ;; For info: C-h v completion-styles-alist
  (completion-styles '(flex partial-completion substring)) ;; flex initials basic

  (completion-category-overrides
   ;; I couldn't find where the categories were defined, but Gemini told me this:
   ;;
   ;;   file: file names (e.g., C-x C-f, find-file)
   ;;   buffer: buffer-names (e.g., C-x b, switch-to-buffer).
   ;;   command: commands (eg, M-x, execute-extended-command).
   ;;   variable: (e.g., C-h v, describe-variable).
   ;;   function:  (e.g., C-h f, describe-function).
   ;;   face: (e.g., M-x customize-face).
   ;;   symbol: A more general category that might apply to various Emacs Lisp symbols.
   ;;   unicode-name: Used when completing Unicode character names (e.g., C-x 8 RET, insert-char).
   ;;   info-menu: Used when completing Info manual nodes.
   `((buffer
      (styles  . (initials flex))
      (cycle   . 10)) ;; C-h v completion-cycle-threshold
     (command
      (styles . (substring))
      (cycle-sort-function . ,#'dpc--sort-alpha-but-dot-slash-last))
     (file
      (styles              . (basic substring))
      ;; When doing completion using completion tables, there is a concept called
      ;; "metadata", which includes sorting functions among other stuff; to learn more about
      ;; that see `completion-metadata'.
      ;;
      ;; display-sort-function is not used in find-file. This is not documented AFAICT. I
      ;; discovered this fact by wallowing in source code specifically in
      ;; `completion-all-sorted-completions' in minibuffer.el which reads the
      ;; cycle-sort-function but ignores display-sort-function.  Actually I don't know why
      ;; there are two sort functions, but it's emacs, so of course there are two different
      ;; sort functions.
      (cycle-sort-function . ,#'dpc--sort-alpha-but-dot-slash-last)
      (cycle               . 10))
     (symbol
      (styles basic shorthand substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (max-mini-window-height 0.2)
  (icomplete-prospects-height 15)
  (icomplete-show-matches-on-no-input t)

  :config
  (icomplete-mode)
  (icomplete-vertical-mode)

  :bind (:map  icomplete-vertical-mode-minibuffer-map
               ;; icomplete-minibuffer-map <== use this for the non-vertical version.
               ;; The following 4 bindings are defaults, unnecessary to set here:
               ;; ("<down>" . icomplete-forward-completions)
               ;; ("C-n" . icomplete-forward-completions)
               ;; ("<up>" . icomplete-backward-completions)
               ;; ("C-p" . icomplete-backward-completions)
               ("TAB"       . icomplete-force-complete)
               ("RET"       . icomplete-force-complete-and-exit)
               ("C-c C-j"   . exit-minibuffer) ;; exit without completion
               ("C-v"       . icomplete-vertical-mode) ;; toggle - need this? I don't remember ever wanting this.

               ;; ;; I installed & enabled embark, but I never began using it. (shrug)
               ;; ("C-c ,"     . embark-act)
               ;; ("C-x"       . embark-export) ;; temporarily in the minibuffer
               ;; ("C-c ;"     . embark-collect)
               )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; marginalia
;;
;; A complement to icomplete-vertical-mode. Displays annotations adjacent to
;; completions in the minibuffer.  For commands, it displays the docstring and
;; key binding if any. For files, it displays the mode and last modified
;; time. For symbols, docstring.

(use-package marginalia
  :ensure t
  ;; `maginalia-cycle' cycles through different annotations available for a
  ;; particular completion category.  So let's bind this command to a key
  ;; sequence specifically for use in the minibuffer.  To make a binding in the
  ;; *Completions* buffer, add it to the `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :init
  ;; Enable the mode right away. This forces loading the package.
  (marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; electric-operator
;; 20241227-2259
;; for smart insertion of ++ and == and += etc, replaces smart-op.

(use-package electric-operator
  :defer 8
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WSD mode - fontifications for editing websequence diagrams
;;
(use-package wsd-mode
  :defer 13
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;
(use-package magit
  ;; On a machine with no git installed, an obscure error occurs at runtime.
  ;; This check attempts to clarify the problem. If an error does get flagged,
  ;; the fix is to install git! And make sure it is on the path.
  :ensure t
  :defer 24
  :config (progn
            (if (not (boundp 'magit-git-executable))
                (error "variable 'magit-git-executable' is not bound")
              (if (not magit-git-executable)
                  (error "variable 'magit-git-executable' is not set"))
              (if (file-exists-p (executable-find magit-git-executable))
                  (message "found git at %s" magit-git-executable)
                (error (format "git executable (%s) cannot be found" magit-git-executable)))
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;
(use-package flycheck
  :ensure t
  :defer 23
  :config (progn
            ;;(add-hook 'after-init-hook #'global-flycheck-mode)
            ;;(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc jsonnet)) ;; why exclude jsonnet?
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

            ;; The following is irrelevant because I've switched machines several times
            ;; since this last worked.  and I don't use PHP much these days. But in the
            ;; future I might want to start again, so I am leaving this here.
            ;; (setq flycheck-phpcs-standard "Drupal") ;; DinoChiesa
            ;; (setq flycheck-phpcs-standard "/usr/local/share/pear/PHP/CodeSniffer/src/Standards/DinoChiesa")
            ))

;; ;; for diagnosing flycheck checks (any mode)
;; (defun dpc/flycheck-command-logger (lst)
;;   "logs the LST, then returns the LST"
;;   (message "FLYCHECK check: %s" (prin1-to-string lst))
;;   lst)
;; (setq flycheck-command-wrapper-function #'dpc/flycheck-command-logger)

;; to reset it to the default (identity), do this:
;; (setq flycheck-command-wrapper-function #'identity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eglot
;;

(defun dino-start-eglot-unless-remote ()
  (unless (file-remote-p default-directory)
    (eglot-ensure)))

(use-package eglot
  :defer t
  :demand t
  :commands (eglot eglot-ensure)
  ;;:hook (csharp-mode . dino-start-eglot-unless-remote)
  :config  (setq flymake-show-diagnostics-at-end-of-line t)

  ;; NOTE: you still must invoke M-x eglot to start the server. or eglot-ensure
  ;; in the mode hook.
  ;;

  ;; eglot things to explore:
  ;; eglot-find-[declaration,implementation,typeDefinition]

  ;; 20241229-1957
  ;; `eglot-format-buffer' is injecting ^M into my c# buffers, seemingly
  ;; at random. This advice is an attempt to fix that. It works to
  ;; remove those ^M characters.
  ;;
  ;; But eglot-format-buffer can get confused, and overwrite code.
  ;; In my experience it can result in lost data. Avoid!

  ;; Really I should be making advice to block eglot-format-buffer
  ;; from executing at all.

  (defun dpc/strip-cr (&rest _args)
    "Strips ^M from text which sometimes appears after `eglot-format-buffer'"
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\u000D" nil t)
        (backward-char)
        (delete-char 1))))

  (advice-add 'eglot--apply-text-edits :after #'dpc/strip-cr)
  )

(use-package eglot-booster
  :defer t
  :after eglot
  :config (eglot-booster-mode))

;; 20250324-1743
;; For aider, there are two packages: aider.el and aidermacs.el .
;; Both are active.
;;
;; aider.el seems less ambitious. aidermacs aims to integrate with
;; existing emacs stuff; uses ediff, for example.
;; Beyond that I don't have a good feel for the differences.
;;
;; Also, it is possible to use aider side-by-side with emacs with
;; no elisp package at all.

(use-package aidermacs
  :defer 35
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ;; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)
  (setenv "AIDER_WEAK_MODEL" "gemini/2.5-pro-exp-03-25")
  (setenv "AIDER_EDITOR_MODEL" "gemini/2.5-pro-exp-03-25")

  :custom
  ;; See the Configuration section below
  (aidermacs-auto-commits t)
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "gemini/gemini-2.5-pro-exp-03-25"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsonnet
;;

(defun dino-jsonnet-mode-fn ()
  "buffer-specific jsonnet-mode setups"
  ;; 20241231 - eglot on jsonnet is helpful. Completion ("code assist") works nicely.
  ;;
  ;; The jsonnet-language-server from Grafana, available at
  ;; https://github.com/grafana/jsonnet-language-server/releases, seems solid,
  ;; reliable, and appears to be in wide use.  It must be installed separately
  ;; and available on the `exec-path'.  The `dpc/jsonnet-lsp' function, set into
  ;; `eglot-server-programs', sets the right args for the language server on
  ;; startup. It reads from `jsonnet-library-search-directories' to set the
  ;; search directories.
  ;;
  ;; Surprisingly, flycheck does not yet work for remote files.
  ;; https://github.com/flycheck/flycheck/pull/1922
  ;;
  ;; Also, running both flycheck and eglot on a jsonnet buffer makes for
  ;; too much feedback when I'm trying to do completion.
  (flycheck-mode -1)

  ;; 20250406 At one point I believed that eglot worked reliably, only locally,
  ;; eg not when editing a remote file via tramp. Now I am not so sure, but I
  ;; don't edit over tramp commonly, so it's not a pressing issue for me.
  (when (not (file-remote-p default-directory))
    ;;  (flycheck-mode 1)
    (eglot-ensure)
    (company-mode)
    (keymap-local-set "C-<tab>" #'company-complete)

    (when (boundp 'apheleia-formatters)
      (apheleia-mode))

    ;; Emacs runs major-mode hooks (e.g., those set in jsonnet-mode-hook) after
    ;; the file-local variables (defined in blocks like Local Variables:
    ;; ... End:) have been processed and set.

    ;; Interesting variables for jsonnet-mode can be set as file-local variables
    ;; with a block like so:
    ;;
    ;; /* Local Variables:                                  */
    ;; /* jsonnet-library-search-directories: ("./lib")     */
    ;; /* jsonnet-command-options:   ("--ext-str" "xy=ab")  */
    ;; /* End:                                              */
    ;;
    ;; This allows 'jsonnet-eval-buffer' to work nicely.
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (keymap-local-set "C-c C-e" #'jsonnet-eval-buffer)
  (keymap-local-set "C-c C-b" #'browse-url)
  (display-line-numbers-mode))

;; The following is a scratchpad for fixups for jsonnetfmt and jsonnet lang
;; server. For the latter there are two options, and this will allow switching
;; between them at runtime while sorting out which is preferred.
;;
;; (let ((jsonnetfmt-cmd '("jsonnetfmt" "--max-blank-lines" "1" "-")))
;;   (if (alist-get 'jsonnetfmt apheleia-formatters)
;;       (setf (alist-get 'jsonnetfmt apheleia-formatters) jsonnetfmt-cmd)
;;     (push `(jsonnetfmt . ,jsonnetfmt-cmd) apheleia-formatters)))
;;
;;
;; ;; LSP option 1 - jsonnet-language-server from Grafana, seems good.
;; (setf
;;   (alist-get 'jsonnet-mode eglot-server-programs)
;;   '("jsonnet-language-server"))
;;
;; ;; LSP option 2 - jsonnet-lsp from some hacker, seems flaky
;; (setf
;;   (alist-get 'jsonnet-mode eglot-server-programs)
;;   '("jsonnet-lsp" "lsp"))


(defun dino-jsonnet-package-config ()
  "one-time, buffer independent configuration stuff for
=jsonnet-mode=, eg, setting the apheleia formatter and eglot
server program."

  (when (boundp 'apheleia-formatters)
    (let ((jsonnetfmt-cmd '("jsonnetfmt" "--max-blank-lines" "1" "-")))
      (if (alist-get 'jsonnetfmt apheleia-formatters)
          (setf (alist-get 'jsonnetfmt apheleia-formatters) jsonnetfmt-cmd)
        (push `(jsonnetfmt . ,jsonnetfmt-cmd) apheleia-formatters)))
    (when (not (alist-get 'jsonnet-mode apheleia-mode-alist))
      (push '(jsonnet-mode . jsonnetfmt) apheleia-mode-alist)))

  (when (boundp 'eglot-server-programs)
    (if (alist-get 'jsonnet-mode eglot-server-programs)
        (setf (alist-get 'jsonnet-mode eglot-server-programs) #'dpc/jsonnet-lsp)
      (add-to-list 'eglot-server-programs
                   `(jsonnet-mode . dpc/jsonnet-lsp))))

  ;; 20250406-1450
  ;;
  ;; When flycheck is enabled, it also invokes jsonnet to perform checks. By
  ;; default it just passes the file to the jsonnet command. Scripts that import
  ;; libraries from a different directory will cause the jsonnet command to
  ;; barf. To fix that, emacs must pass the -J option to specify locations of
  ;; jsonnet include libraries. There may be other command options, for example
  ;; specifying external variables. (Not sure that is required for flycheck
  ;; though). This modified checker command allows specification of those
  ;; things. I filed a PR https://github.com/flycheck/flycheck/pull/2105 but who
  ;; knows if it will be merged.
  ;;
  ;; In the meantime, the following sets the flycheck command to use
  ;; either the 'jsonnet-library-search-directories', or the special-to-flycheck variable
  ;; 'flycheck-jsonnet-include-paths'.

  ;; This is what I am not sure of:
  ;;
  ;; Does this need to be in the mode function (buffer local)?  or is it system-wide?
  ;; If the flycheck-checker is evaluated each time in the buffer, then... this change
  ;; can be made once, system-wide.
  ;;
  ;;(dino-fixup-jsonnet-command)
  ;;(run-with-timer 3 nil #'dino-fixup-jsonnet-command)
  )


;; (defun dino-fixup-jsonnet-command ()
;;   "For some reason, When I put this in the =dino-jsonnet-package-config= fn
;; I get an error:
;; Error (use-package): jsonnet-mode/:config: Symbol’s function definition
;;    is void: \(setf\ flycheck-checker-get\)
;;
;; Putting it in this fn is an attempt to isolate and avoid the problem.
;; Jeez.
;; "
;;   (interactive)
;;
;;
;;   ;; (flycheck-define-checker jsonnet-for-dino
;;   ;;   "A Python syntax and style checker using flake8"
;;   ;;          :command ("flake8"
;;   ;;                     "--format=default"
;;   ;;                     (config-file "--config" flycheck-flake8rc)
;;   ;;                     (option "--max-complexity" flycheck-flake8-maximum-complexity nil
;;   ;;                       flycheck-option-int)
;;   ;;                     (option "--max-line-length" flycheck-flake8-maximum-line-length nil
;;   ;;                       flycheck-option-int)
;;   ;;                     "-")
;;   ;;          :standard-input t
;;   ;;          :error-filter fix-flake8
;;   ;;          :error-patterns
;;   ;;          ((warning line-start
;;   ;;             "stdin:" line ":" (optional column ":") " "
;;   ;;             (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;   ;;             (message (one-or-more not-newline))
;;   ;;             line-end))
;;   ;;          :next-checkers ((t . python-pylint))
;;   ;;          :modes python-mode)
;;
;;   ;;         ;; replace flake8 with new chaining one from above
;;   ;;        (setq flycheck-checkers (cons 'python-flake8-chain (delq 'python-flake8 flycheck-checkers)))
;;
;;
;;   ;; ALL I WANT TO DO IS REDEFINE THE COMMAND for AN EXISTING CHECKER BUT IT SEEMS IMPOSSIBLE.
;;   ;; ALSO, IT MAY BE IRRELEVANT BECAUSE I AM USING EGLOT AND TURNING OFF FLYCHECK.
;;
;;   ;; testing for the function in an attempt to diagnose the weird error.
;;   (if (functionp 'flycheck-checker-get)
;;       (setf (flycheck-checker-get 'jsonnet 'command)
;;             `("jsonnet"
;;               (option-list "-J"
;;                            (eval (or jsonnet-library-search-directories flycheck-jsonnet-include-paths)))
;;               (eval
;;                (or jsonnet-command-options flycheck-jsonnet-command-args))
;;               source-inplace)))
;;   )


(use-package jsonnet-mode
  :init (progn (require 'flycheck) (require 'eglot) (require 'dpc-jsonnet-mode-fixups))
  ;;:ensure t
  :defer t
  :commands (jsonnet-eval-buffer jsonnet-mode jsonnet-reformat-buffer)
  :config (dino-jsonnet-package-config)
  :mode (
         ("\\.libjsonnet\\'" . jsonnet-mode)
         ("\\.jsonnet\\'" . jsonnet-mode)
         ("\\.jsonnet.TEMPLATE\\'" . jsonnet-mode)
         )
  :hook (jsonnet-mode . dino-jsonnet-mode-fn))

(use-package dpc-jsonnet-mode-fixups
  :defer t
  :if (file-exists-p "~/elisp/dpc-jsonnet-mode-fixups.el")
  :load-path "~/elisp"
  :pin manual)

;; Tips from https://www.youtube.com/watch?v=p3Te_a-AGqM
;; for marking ever-larger regions iteratively
(use-package expand-region
  :defer t
  ;; TODO: fix this keymap binding; it conflicts with the font resize key binding.
  :config (keymap-global-set "C-=" 'er/expand-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors - For visually editing similar things with one key sequence.
;;
;; I use this rarely and my fingers don't remember the bindings.
(use-package multiple-cursors
  :ensure t
  :demand t
  :defer t
  :init
  ;; 20241228-2133 - supposedly there is a bug, and reloading avoids it?
  (add-hook 'multiple-cursors-mode-hook
            (defun dpc/work-around-multiple-cursors-issue ()
              (load "multiple-cursors-core.el")
              (remove-hook 'multiple-cursors-mode-hook
                           #'dpc/work-around-multiple-cursors-issue)))
  (defun dpc/multiple-cursors-set-key-bindings ()
    ;; TODO: am I ever going to remember this
    (keymap-local-set "ESC C->"     #'mc/mark-next-like-this)
    (keymap-local-set "ESC C-<"     #'mc/mark-previous-like-this)
    (keymap-local-set "ESC C-c C-<" #'mc/mark-all-like-this))

  (add-hook 'multiple-cursors-mode-hook #'dpc/multiple-cursors-set-key-bindings)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep - edit files in a grep output buffer. Amazing.
;;
(use-package wgrep
  :defer t
  :if (file-exists-p "~/elisp/wgrep.el")
  :load-path "~/elisp"
  :commands (wgrep-setup)
  :config (keymap-set grep-mode-map "C-c C-p" #'wgrep-change-to-wgrep-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apigee
;;

(use-package apigee
  :if (file-exists-p "~/elisp/apigee/apigee.el")
  :load-path "~/elisp/apigee"
  :defer t
  :commands (apigee-new-proxy apigee-lint-asset)
  :config
  (progn
    (let* ((apigeecli-path "~/.apigeecli/bin/apigeecli")
           (found-apigeecli (file-exists-p apigeecli-path)))
      (if (not found-apigeecli)
          (setq apigeecli-path (executable-find "apigeecli")
                found-apigeecli (file-exists-p apigeecli-path)))
      (if (not found-apigeecli)
          (error "cannot find apigeecli")
        (setf (alist-get 'apigeecli apigee-programs-alist)
              apigeecli-path)))
    (let* ((gcloud-cmd "gcloud")
           (found-gcloud (executable-find gcloud-cmd)))
      (if (not found-gcloud)
          (error "cannot find gcloud")
        (setf (alist-get 'gcloud apigee-programs-alist)
              found-gcloud)))
    (let* ((apigeelint-cli-path "~/apigeelint/cli.js")
           (found-apigeelint (file-exists-p apigeelint-cli-path)))
      (if (not found-apigeelint)
          (error "cannot find apigeelint")
        (setf (alist-get 'apigeelint apigee-programs-alist)
              (format "node %s" apigeelint-cli-path))))
    (if (and (getenv "ENV")(getenv "ORG"))
        (setq apigee-environment (getenv "ENV")
              apigee-organization (getenv "ORG")))
    ;; If the ORG and ENV environment vars are not available, the
    ;; apigee module will prompt the user interactively for values
    ;; when they are first necessary.
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sh-mode

(defun dino-sh-mode-fn ()
  (display-line-numbers-mode)
  (setq sh-basic-offset 2)
  (sh-electric-here-document-mode)

  (apheleia-mode)
  (dino/setup-shmode-for-apheleia)

  (keymap-local-set "C-c C-g"  #'dino/shfmt-buffer)
  (keymap-local-set "C-c C-c"  #'comment-region))

(add-hook 'sh-mode-hook 'dino-sh-mode-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang
;;

(defun dino-go-mode-fn ()
  ;; 20250310-2243
  ;; not sure, but this does not appear to get executed with go-mode or go-ts-mode
  ;;(setq-default)
  (setq tab-width 2
        standard-indent 2
        indent-tabs-mode t) ;; golang prefers tabs, ugh

  (keymap-local-set "ESC C-R" #'indent-region)
  (keymap-local-set "ESC #"   #'dino/indent-buffer)
  (keymap-local-set "C-c C-w" #'compare-windows)
  (keymap-local-set "C-c C-c" #'comment-region)

  (eval-after-load "smarter-compile"
    '(progn
       (add-to-list
        'smart-compile-compile-command-in-comments-extension-list
        ".go")))

  (eval-after-load "flycheck"
    '(progn
       (add-to-list
        'flycheck-disabled-checkers 'go-build))) ;; go-gofmt?

  (display-line-numbers-mode)

  ;; 20230918-1015
  (when (boundp 'apheleia-formatters)
    (apheleia-mode)
    (setq apheleia-log-debug-info t)
    (setq apheleia-remote-algorithm 'local))

  ;; still need this? 20241203-0215
  (require 'goflycheck)
  (flycheck-mode 1)

  (add-hook 'before-save-hook 'dino-delete-trailing-whitespace nil 'local)
  )

(add-hook 'go-mode-hook 'dino-go-mode-fn)


(use-package go-ts-mode
  :defer t
  :ensure t
  :after (smarter-compile flycheck)
  :config (progn
            ;;(require 'go-mode-autoloads) ;; editing mode
            (load "go-mode-autoloads") ;; editing mode; there is no provide statement!
            ;; for flycheck or compile support, I need the go binary on the path
            (add-hook 'go-ts-mode-hook 'dino-go-mode-fn)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; org mode, including html5 presentations from .org documents
;; ;;
;; ;;  (require 'org)
;;
;; ;; (org-babel-do-load-languages
;; ;;  'org-babel-load-languages
;; ;;  '(
;; ;;    (sh . t)
;; ;;    (python . t)
;; ;;    (perl . t)
;; ;;    ))
;; (defun my-org-confirm-babel-evaluate (lang body)
;;   (not  ; don't ask for any of the following languages
;;    (or
;;     (string= lang "sh")
;;     )))
;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
;;
;; ;; (require 'ox-taskjuggler)
;; ;; (add-to-list 'org-export-backends 'taskjuggler)
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; including html5 presentations from .org documents
;; ;;
;;
;; ;; 2023 Feb 26 TODO -  remove ox-reveal.el, it is broken
;; ;; my own home-built thing. Not quite as cool as Org-export with reveal.js .
;; ;; (require 'dpreso)
;; ;;(require 'ox-reveal)
;; ;;(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
;; ;; to use: M-x org-reveal-export-to-html
;;
;; ;; (setq org-reveal-root "http://dinochiesa.github.io/rv/")
;; ;;
;; ;; (require 'org-fixups)
;; ;; (add-hook 'org-fixups/after-export-reveal-file
;; ;;           (lambda (filename)
;; ;;             (message "the file was exported to %s" expanded-filename)))
;; ;;
;; ;; (add-hook 'org-fixups/after-export-reveal-file
;; ;;           'my-copy-and-open)
;; ;;
;; ;; (defun my-copy-and-open (filename)
;; ;;   "fn to copy a file and open it in the browser."
;; ;;   (let* ((base-fname
;; ;;           (file-name-nondirectory filename))
;; ;;          (new-fname
;; ;;           (concat "/Users/dchiesa/dev/html/dpreso/" base-fname)))
;; ;;
;; ;;     (rename-file filename new-fname t)
;; ;;     (call-process "open" nil t t
;; ;;                   (concat "http://localhost:80/html/dpreso/"
;; ;;                           base-fname))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;
;; I'm pretty sure this must be added before auto-complete-config, because the
;; latter appends some things to yasnippet.

(use-package yasnippet
  :defer 13
  :ensure t
  :defines (yas-snippet-dirs yas-prompt-functions)
  :functions (yas-global-mode yas-expand-snippet)
  :config (progn
            (setq yas-snippet-dirs (list "~/elisp/yasnippets"))
            (yas-global-mode 1)
            ;; (setq yas-prompt-functions '(yas-dropdown-prompt
            ;;                              yas-completing-prompt
            ;;                              yas-maybe-ido-prompt yas-no-prompt))
            ;; prettier popup choices when running in a windowed environment:
            (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))

            ;;(yas-load-directory (car yas-snippet-dirs))
            ;; ------------------------------------------------
            ;; Expand snippet synchronously
            (defvar yas--recursive-edit-flag nil)

            ;; this is used in some of my apigee.el templates
            (defun yas-expand-sync ()
              "Execute `yas-expand'. This function exits after expanding snippet."
              (interactive)
              (let ((yas--recursive-edit-flag t))
                (call-interactively 'yas-expand)
                (recursive-edit)))

            (defun yas-expand-snippet-sync (content &optional start end expand-env)
              "Execute `yas-expand-snippet'. This function exits after expanding snippet."
              (let ((yas--recursive-edit-flag t))
                ;;(sit-for 0.6) ;; timing issue?
                (yas-expand-snippet content start end expand-env)
                ;;(sit-for 0.2) ;; timing issue?
                (recursive-edit)))

            (defun my-yas-after-exit-snippet-hook--recursive-edit ()
              (when yas--recursive-edit-flag
                (throw 'exit nil)))
            (add-hook 'yas-after-exit-snippet-hook 'my-yas-after-exit-snippet-hook--recursive-edit)
            ))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; autocomplete
;; ;; adds auto-complete to various things like yasnippets or css colors, etc.
;;
;; ;; (add-to-list 'ac-dictionary-directories "/Users/Dino/elisp/autocomplete/ac-dict")
;; (use-package auto-complete-config
;;   :init (require 'yasnippet)
;;   :defer t
;;   :functions (ac-config-default)
;;   :config (ac-config-default))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web
(use-package web-mode
  :defer t
  :config (progn
            (defun dino-web-mode-fn ()
              "My hook for web mode"
              (turn-on-font-lock)
              ;; minor-mode
              ;;(hs-minor-mode 1)
              (display-line-numbers-mode)
              ;; why I have to re-set this key is baffling to me.
              ;; and this does not seem to work...
              (keymap-local-set "ESC C-R"  #'indent-region)
              ;; Make sure autofill is OFF.
              (auto-fill-mode -1))

            (add-hook 'web-mode-hook 'dino-web-mode-fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown

;; 20250116-1921
;;
;; The markdown utility is very outdated. The pandoc command is more up to date
;; and more capable.  It is available on cloudtop only for now. (Install from
;; https://github.com/jgm/pandoc/releases/tag/3.6.2) It would be easier if it
;; were on the path, but it is not. So this sets the path explicitly.
(let ((pandoc-path "~/pandoc-3.6.2/bin/pandoc"))
  (when (file-exists-p pandoc-path)
    (setq markdown-command pandoc-path)))

(defun dpc-markdown-standalone ()
  "process the MD buffer with the markdown command-line tool,
then switch to the markdown output buffer."
  (interactive)
  (let ((buf-name (markdown-standalone))
        (browse-url-browser-function 'eww-browse-url))
    (switch-to-buffer-other-window buf-name)
    (browse-url-of-buffer (get-buffer buf-name))))

(defun dino-markdown-mode-fn ()
  "My hook for markdown mode"
  (modify-syntax-entry ?_ "w")
  (auto-fill-mode -1)
  (keymap-local-set "C-c m s" #'dpc-markdown-standalone)
  (keymap-local-set "C-c m |" #'markdown-table-align) ;; my favorite feature
  )

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  :hook (markdown-mode . dino-markdown-mode-fn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; httpget
;; 20241228-0227 - It seems likely this is probably unnecessary at this point.
(require 'httpget)


;; 20250430-0416 - also not sure if I still use this.
(require 'skeleton)

;; handle text end-of-line conventions the way it oughta be:
(setq inhibit-eol-conversion nil)

;; 20250218-0251 - not sure this is necessary any longer
;; turn on font-lock globally
;; for fontification in emacs progmodes:
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode 1) ;;  'ON

;; fringe (between line numbers and buffer text)
(setq-default left-fringe-width  10)
(set-face-attribute 'fringe nil :background "black")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backups
;;
;; Setting `backup-by-copying' to non-nil says "copy the file to a backup
;; before editing, and edit the original inode."  The default is nil,
;; which *renames* the original file to the backup name, and then edits
;; a copy that is given the original name.  This default behavior means,
;; when editing a hardlinked file, emacs breaks the link.  Very unfortunate.
;;
;; Setting `backup-by-copying' to non-nil  means the right thing gets done
;; with hardlinks.
;;
;; ps: hardlinks can be created in dired mode with H.
;;

(set-variable 'backup-by-copying t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames preferences: initial and default frames
;; see http://www.gnu.org/software/emacs/windows/big.html#windows-frames

(setq default-frame-alist
      '((top . 120) (left . 260)
        (width . 100) (height . 25)
        (cursor-color . "Orange")
        (cursor-type . box)
        ;;(foreground-color . "White")
        ;;(background-color . "Black")
        (mouse-color . "sienna3")
        ;; The font spec is so intuitive! To inquire the current font spec, open a text file, then C-u C-x =
        )
      )


;; initial frame size and position
;; 20241227-0355
;;
;; Supposedly, setting `initial-frame-alist' is ineffectual here, and it must be placed in
;; the ~/.emacs.d/early-init.el file, which is evaluated before the first frame is created.
;; https://emacs.stackexchange.com/a/62051/3856
;;
;; But, the emacs documentation states that "Emacs creates the initial frame
;; before it reads your init file. After reading that file, Emacs checks
;; initial-frame-alist, and applies the parameter settings in the altered value to the
;; already created initial frame."
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Initial-Parameters.html
;;
;; So I think I misinterpreted the SO answer. Setting i-f-a here, is fine.
;; But there are quirks. In my experience,
;; - on Windows NT, setting `initial-frame-alist' here works just fine.
;; - on gLinux chromebook, setting `initial-frame-alist' in either place was ineffectual.
(if (eq system-type 'windows-nt)
    (progn
      (setq initial-frame-alist
            '( (top . 100) (left . 640)
               (width . 228) (height . 64)
               ))
      (set-frame-font "Consolas 14" nil t))
  (progn
    ;; glinux
    (setq initial-frame-alist
          '( (top . 10) (left . 10)
             (width . 148) (height . 42)
             )
          )
    (set-frame-font "Noto Mono 11" nil t))
  )

;; what should a frame look like
(setq frame-title-format '("%f [mode: %m]" )    ; "filename [mode]" in title bar
      icon-title-format '("emacs: %b"))   ; "emacs: buffername" in icon

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq save-abbrevs t)              ;; save abbrevs when files are saved


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjustment to mode mappings
;;
;; NB: In the regexi, the trailing \\' represents "end of string".  The $
;; represents the zero-width place before newline.  They are equivalent unless
;; there is a filename with a new line in it, which is possible but dumb, and not
;; likely. Even so, we use the \\'.
;;

(setq auto-mode-alist
      (append
       '(
         ("\\.salted\\'"                        . salted-file-mode)
         ("\\.yaml\\'"                          . yaml-mode)
         ("\\.\\(war\\|ear\\|WAR\\|EAR\\)\\'"   . archive-mode)
         ("\\(Iirf\\|iirf\\|IIRF\\)\\(Global\\)?\\.ini\\'"   . iirf-mode)
         ("\\.css\\'"                           . css-ts-mode)
         ("\\.\\(bashrc\\|profile\\|bash_aliases\\|bash_functions\\|bash_logout\\)\\'"  . bash-ts-mode)
         ("\\.proto\\'"                         . protobuf-mode)
         ("\\.\\(php\\|module\\)\\'"            . php-mode)
         ("\\.md\\'"                            . markdown-mode)
         ("\\.cs\\'"                            . csharp-ts-mode)
         ("\\.asp\\'"                           . html-mode)
         ;;("\\.aspx\\'"                        . html-helper-mode)
         ("\\.aspx\\'"                          . aspx-mode)
         ("\\.ashx\\'"                          . csharp-mode)
         ("\\.ascx\\'"                          . csharp-mode)
         ("\\.s?html?\\'"                       . html-mode)
         ("\\.html\\'"                          . html-mode)
         ("\\.htm\\'"                           . html-mode)
         ("\\.md\\'"                            . markdown-mode)
         ("\\.py\\'"                            . python-ts-mode)
         ;;("\\.dart\\'"                          . dart-mode)
         ("\\.el\\'"                            . emacs-lisp-mode)
         ;;("\\.js$"                            . js-mode)
         ;;("\\.gs$"                            . js-mode)             ;; google apps-script
         ("\\.\\(js\\|gs\\|jsi\\)\\'"           . js-mode)
         ("\\.\\(avsc\\)\\'"                    . json-mode)           ;; avro schema
         ("\\.txt\\'"                           . text-mode)
         ("\\.asmx\\'"                          . csharp-mode)         ; likely, could be another language tho
         ("\\.\\(vb\\)\\'"                      . vbnet-mode)
         ("\\.\\(vbs\\|vba\\)\\'"               . vbs-mode)
         ("\\.\\(cs\\|vb\\|shfb\\)proj\\'"      . xml-mode)            ; msbuild file
         ("\\.config\\'"                        . xml-mode)            ; .NET config file
         ("\\.\\(xsd\\|wsdl\\)\\'"              . xml-mode)            ; schema or WSDL file
         ("\\.sln\\'"                           . xml-mode)            ; VS2008 .sln file
         ("\\.\\(wxs\\|wxl\\|wixproj\\)\\'"     . xml-mode)            ; WiX, wixproj, etc
         ("\\.ssml\\'"                          . xml-mode)            ; Speech markup
         ("\\.\\(aml\\|xaml\\)\\'"              . xml-mode)            ; SHFB markup, XAML
         ("\\.\\(wsc\\|wsf\\)\\'"               . xml-mode)            ; Windows Script Component, WSCript file.
         ("\\.\\(xjb\\)\\'"                     . xml-mode)            ; JAXB bindings file
         ) auto-mode-alist ))



;; ;; replace all html-mode in this alist with web-mode, which is more betta.
;; (mapc
;;  (lambda (pair)
;;    (if (eq (cdr pair) 'html-mode)
;;        (setcdr pair 'web-mode)))
;;  auto-mode-alist)

;; back to html-mode; web-mode is ... not available?
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'web-mode)
       (setcdr pair 'html-mode)))
 auto-mode-alist)

;; 20230828-1703 replace java-mode with java-ts-mode?
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'java-mode)
       (setcdr pair 'java-ts-mode)))
 auto-mode-alist)

;; (mapc
;;  (lambda (pair)
;;    (if (eq (cdr pair) 'css-mode)
;;        (setcdr pair 'css-ts-mode)))
;;  auto-mode-alist)


;;;; Keep this snip !
;;;; Modify and Run it to temporarily append an extension to the list, "right now".
;;        (setq auto-mode-alist
;;              (append '(
;;                 ("\\.\\(jsi\\|js\\)$"               . javascript-mode)
;;                        ) auto-mode-alist ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode

;; see http://orgmode.org/manual/index.html
(setq org-log-done 'time) ;; timestamps when completing a TODO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speedbar
;; use sr-speedbar-open to open the speedbar window.

(use-package sr-speedbar ;; put speedbar in same frame
  :ensure t
  :defer t
  :defines (speedbar-use-images)
  :config
  (setq speedbar-use-images nil))

(use-package hideshow  ;; builtin
  :autoload (hs-hide-block hs-show-block)
  :defer t )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; htmlize

(use-package htmlize
  :defer t
  :defines (htmlize-output-type)
  :config
  (progn
    (setq htmlize-output-type 'inline-css)
    (autoload 'htmlize-buffer "htmlize"
      "Turning code into HTML." t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gist
;;
;; I stopped  using this because I couldn't figure out the MFA for Github.

;; (use-package gist
;;   :defer t
;;   :config
;;   (progn
;;     ;; Sometimes when invoking gist-buffer, you get an error like this:
;;     ;; Invalid slot type: gh-gist-gist, id, string, nil
;;     ;; If so, just run 'gist-list' and retry the gist-buffer.
;;     ;; note that we added the DESCRIPTION argument
;;     (defun gist-region-with-description (begin end &optional description private callback)
;;       "Post the current region as a new paste at gist.github.com
;; Copies the URL into the kill ring.
;;
;; With a prefix argument, makes a private paste."
;;       (interactive "r\nsGist Description: \nP") ;; we handle the prompt here!
;;       (let* ((file (or (buffer-file-name) (buffer-name)))
;;              (name (file-name-nondirectory file))
;;              (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
;;                       (file-name-extension file)
;;                       "txt"))
;;              (fname (concat (file-name-sans-extension name) "." ext))
;;              (files (list
;;                      (gh-gist-gist-file "file"
;;                                         :filename fname
;;                                         :content (buffer-substring begin end)))))
;;         ;; finally we use our new arg to specify the description in the internal call
;;         (gist-internal-new files private description callback)))
;;
;;     ;; (defun dino-add-user-agent (old-function &rest arguments)
;;     ;;   "set the user agent when invoking github APIs"
;;     ;;   (let ((url-user-agent "emacs/url-http.el"))
;;     ;;     (apply old-function arguments)))
;;     ;;(advice-add #'gh-api-authenticated-request :around #'dino-add-user-agent)
;;     ;;(advice-remove #'gh-api-authenticated-request  #'dino-add-user-agent)
;;     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-dired
(eval-after-load "image-dired"
  '(progn
     (require 'image-dired-fixups)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image-mode
(use-package image-mode
  :defer t
  :autoload (image-transform-fit-to-window)
  :config
  ;;
  ;; 20250405-2339
  ;;
  ;; Supposedly the image-transform-fit-to-{width,height} are now obsolete,
  ;; which I guess means `image-transform-fit-to-window' does the right thing automatically.
  ;;
  ;; (defun dino/image-transform-fit-to-window()
  ;;     "Resize the image to fit the width or height based on
  ;; the image and window ratios."
  ;;     (interactive)
  ;;     (let* ( (img-size (image-display-size (image-get-display-property) t))
  ;;             (img-width (car img-size))
  ;;             (img-height (cdr img-size))
  ;;             (img-h/w-ratio (/ (float img-height) (float img-width)))
  ;;             (win-width (- (nth 2 (window-inside-pixel-edges))
  ;;                           (nth 0 (window-inside-pixel-edges))))
  ;;             (win-height (- (nth 3 (window-inside-pixel-edges))
  ;;                            (nth 1 (window-inside-pixel-edges))))
  ;;             (win-h/w-ratio (/ (float win-height) (float win-width))))
  ;;       ;; Fit image by width if the h/w ratio of window is > h/w ratio of the image
  ;;       (if (> win-h/w-ratio img-h/w-ratio)
  ;;           (image-transform-fit-to-width)
  ;;         ;; Else fit by height
  ;;         (image-transform-fit-to-height))))

  (defun dino-image-mode-fn ()
    "My hook for image-mode"
    (image-transform-fit-to-window))

  (add-hook 'image-mode-hook 'dino-image-mode-fn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml

(use-package yaml-pretty-mode
  :load-path "~/elisp"
  :commands (yaml-pretty-mode))

(use-package yaml-mode
  :defer t
  :config
  (defun dino-yaml-mode-fn ()
    "My hook for YAML mode"
    (interactive)
    (turn-on-font-lock)
    (turn-on-auto-revert-mode)
    (display-line-numbers-mode)
    (yaml-pretty-mode)
    ;;(make-local-variable 'indent-tabs-mode)
    (setq indent-tabs-mode nil))

  (add-hook 'yaml-mode-hook 'dino-yaml-mode-fn)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun dino-typescript-mode-fn ()
  (turn-on-font-lock)
  (keymap-local-set "ESC C-R" #'indent-region)
  (turn-on-auto-revert-mode)
  (setq typescript-indent-level 2)
  (display-line-numbers-mode)
  (auto-fill-mode -1)
  )
(add-hook 'typescript-mode-hook 'dino-typescript-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powershell
;;
;; There is a powershell package on MELPA, but there's nothing new
;; there, I think. One day I will convert.
;;

(use-package powershell-mode
  :if (file-exists-p "~/elisp/powershell-mode.el")
  :load-path "~/elisp"
  :pin manual
  :defer t
  :commands (powershell-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
  (add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))


  (defun dino-powershell-mode-fn ()
    (electric-pair-mode 1)
    (hs-minor-mode t)
    (display-line-numbers-mode)
    (dino-enable-delete-trailing-whitespace)
    )

  (add-hook 'powershell-mode-hook 'dino-powershell-mode-fn)

  (eval-after-load "hideshow"
    '(progn
       ;; hideshow for powershell
       (setq dpc-hs-settings-for-powershell-mode
             '(powershell-mode
               "{"                                 ;; regexp for start block
               "}"                                 ;; regexp for end block
               "[ \\t]*#"                          ;; regexp for comment start
               forward-sexp                        ;; hs-forward-sexp-func
               hs-c-like-adjust-block-beginning    ;; c-like adjust (1 char)
               ))

       ;; replace:
       ;;(setf (cdr (rassoc 'powershell-mode hs-special-modes-alist) ) something-here)

       ;; shadow:
       ;; (add-to-list 'hs-special-modes-alist dpc-hs-settings-for-powershell-mode )

       ;; delete:
       ;; (assq-delete-all 'powershell-mode hs-special-modes-alist)
       (unless (assoc 'powershell-mode hs-special-modes-alist)
         (push dpc-hs-settings-for-powershell-mode hs-special-modes-alist)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua
;; 20250224-0130 - it's rare that I use lua-mode.

(use-package lua-mode
  :defer 36
  :config
  ;; (if (eq system-type 'windows-nt)
  ;;     (setq lua-default-application "c:\\tools\\lua\\lua52.exe")
  ;;   )
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))


;;
;; (eval-after-load "lua-mode"
;;   '(progn
;;      ;; TODO: convert this to
;;      ;;   (advice-add 'lua-start-process :before #'my-specially-defined-function)
;;
;;      (defadvice lua-start-process (before
;;                                    dino-set-lua-shell-buffer-name-nicely
;;                                    activate)
;;        "Use a proper name for the interactive LUA shell. "
;;        (let ((arg0 (ad-get-arg 0))
;;              (arg1 (ad-get-arg 1)))
;;          (if arg0
;;              (when (not arg1)
;;                (ad-set-arg 0 "LuaShell")
;;                (ad-set-arg 1 arg0))
;;            (ad-set-arg 0 "LuaShell")
;;            (ad-set-arg 1 lua-default-application))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML

(defun dino-html-mode-fn ()
  (keymap-local-set "C-c 1" #'just-one-space)
  (keymap-local-set "<f7>"  #'find-file-at-point)
  ;; Make sure autofill is OFF.
  (auto-fill-mode -1)
  )

(add-hook 'html-mode-hook 'dino-html-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; VBNET mode (and also, VBScript)
;;
;; (autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
;; (autoload 'vbs-mode "vbs-mode" "Mode for editing VBScript code." t)
;;
;; (defun dino-vbnet-mode-fn ()
;;   "My hook for VB.NET mode (and VBScript)"
;;   (interactive)
;;   (turn-on-font-lock)
;;
;;   (keymap-local-set "ESC C-R" #'indent-region)
;;   (keymap-local-set "ESC #"   #'dino-indent-buffer)
;;   (keymap-local-set "C-c C-w" #'compare-windows)
;;   (keymap-local-set "C-c C-c"  #'comment-region)
;;
;;   (turn-on-auto-revert-mode)
;;
;;   ;; "no tabs" -- use only spaces
;;   ;;(make-local-variable 'indent-tabs-mode)
;;   (setq indent-tabs-mode nil)
;;
;;   ;; for snippets support:
;;   (require 'yasnippet)
;;   (yas-minor-mode-on)
;;
;;   ;; use autopair for curlies, parens, square brackets.
;;   ;; electric-pair-mode works better than autopair.el in 24.4,
;;   ;; and is important for use with popup / auto-complete.
;;   (if (or (not (fboundp 'version<)) (version< emacs-version "24.4"))
;;       (progn (require 'autopair) (autopair-mode))
;;     (electric-pair-mode))
;;
;;   ;;(require 'myfixme)
;;   ;;(myfixme-mode 1)
;;
;;   (require 'rfringe)
;;   (setq comment-empty-lines t))
;;
;;
;; (add-hook 'vbnet-mode-hook 'dino-vbnet-mode-fn)
;; (add-hook 'vbs-mode-hook 'dino-vbnet-mode-fn)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS mode

;; 20250217-1908
;; It is possible to run the CSS language server from VSCode independently.
;; I am not sure what it gives me, while I am editing CSS. Completion?
;; Also I am not sure what css-ts-mode gives me? or does completion come from TS?

(defun dino-css-mode-fn ()
  "My hook for CSS mode"
  (interactive)
  ;; (turn-on-font-lock) ;; need this?
  (setq css-indent-offset 2)

  (keymap-local-set "ESC C-R" #'indent-region)
  (keymap-local-set "ESC #"   #'dino/indent-buffer)
  (keymap-local-set "C-c C-w" #'compare-windows)
  (keymap-local-set "C-c C-c" #'comment-region)
  (keymap-local-set "ESC ."   #'company-complete)

  (turn-on-auto-revert-mode)
  (electric-pair-mode)
  (company-mode)
  (apheleia-mode)
  (display-line-numbers-mode)

  ;; 20250524-1624
  ;; Microsoft produces a CSS language server as part of (VS)Code OSS.  But they do not
  ;; *package it* as an independent executable.  It's packaged only as part of VSCode. There
  ;; have been attempts to extract the CSS Lang server from VSCode and re-package it, for
  ;; example https://github.com/hrsh7th/vscode-langservers-extracted .  The way it works:
  ;; the build script clones the VSCode repo and then tries building just what's necessary I
  ;; guess, for the CSS lang server (and maybe a few others).  but (a) that repo hasn't been
  ;; updated in a while, and (b) the build doesn't work.
  ;;
  ;; I asked Gemini how I could use https://github.com/Microsoft/vscode-css-languageservice,
  ;; and it advised me to build my own server that packages this library. I tried that, see
  ;; ~/dev/my-css-language-server , it also didn't work.
  ;;
  ;; Finally, resorted to just running the CSS lang server that is packaged in the VSCode
  ;; installation. This works on Windows; not sure the install location of the CSS Lang
  ;; Server on Linux.

  ;; This is ugly but works.
  (let* ((home-dir (getenv "HOME")))
    (vscode-server-program
     (concat home-dir
             "\\AppData\\Local\\Programs\\Microsoft VS Code\\resources\\app\\extensions\\css-language-features\\server\\dist\\node\\cssServerMain.js"))
    (if (file-exists-p vscode-server-program)
        (add-to-list 'eglot-server-programs
                     `((css-mode css-ts-mode)
                       . ,(list "node" vscode-server-program "--stdio")))

      ))


  ;; To install the external flycheck checkers:
  ;; sudo npm install -g csslint
  ;; sudo npm install -g stylelint stylelint-config-standard stylelint-scss

  (flycheck-mode)
  (flycheck-select-checker
   (if (string= mode-name "SCSS") 'scss-stylelint 'css-stylelint))

  (add-hook 'before-save-hook 'dino-delete-trailing-whitespace nil 'local)
  (setq indent-tabs-mode nil) )


(use-package css-mode
  :defer t
  :ensure nil
  :config (progn
            ;;(autoload 'css-mode "css-mode" "Mode for editing Cascading Stylesheets." t)
            (require 'flycheck)
            ;; Overwrite existing scss-stylelint checker to not use --syntax flag
            (flycheck-define-checker scss-stylelint
              "A SCSS syntax and style checker using stylelint. See URL `http://stylelint.io/'."
              :command ("stylelint"
                        (eval flycheck-stylelint-args)
                        ;; "--syntax" "scss"
                        (option-flag "--quiet" flycheck-stylelint-quiet)
                        (config-file "--config" flycheck-stylelintrc))
              :standard-input t
              :error-parser #'flycheck-parse-stylelint
              :modes (scss-mode css-mode))

            (when (boundp 'apheleia-mode-alist)
              (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier-css)
              (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css))

            (add-hook 'css-ts-mode-hook 'dino-css-mode-fn)
            (add-hook 'css-mode-hook 'dino-css-mode-fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON
(use-package json-mode
  :defer t
  :config (progn
            (require 'json-reformat)
            (autoload 'json-mode "json" nil t)

            ;; add a new element to the front of the list and it will shadow matches
            ;; further down the list.
            (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

            (defun dino-json-mode-fn ()
              ;;(turn-on-font-lock)
              ;;(flycheck-mode 0)
              ;; 20250111-1538
              ;;
              ;; The default checker is json-python-json, which has a command "python3".
              ;; On my version of windows,
              ;;
              ;; (1) there is no "python3", the executable i sjust called "python" and it resides in c:\Python313 .
              ;;
              ;; (2) there is a directory on the path, ~\AppData\Local\Microsoft\WindowsApps, that has a
              ;; IO_REPARSE_TAG_APPEXECLINK named "python3.exe". Not all systems can handle these reparse
              ;; points, see
              ;; https://jpsoft.com/forums/threads/dir-reports-meaningless-symlink-information.9839/ .
              ;;
              ;; Not really sure why that directory is on the path.
              ;;
              ;; This change modifies the command to just "python". Because c:\Python313 has python.exe,
              ;; and because c:\python313 lies before that WindowsApps directory with the reparse points,
              ;; flycheck finds the command successfully and is able to execute the checker.
              ;;
              (if (eq system-type 'windows-nt)
                  (setf (car (flycheck-checker-get 'json-python-json 'command))
                        "python"))
              )
            (add-hook 'json-mode-hook   'dino-json-mode-fn)

            ;; function alias
            (defalias 'json-prettify-region 'json-reformat-region)

            (defun json-prettify-buffer ()
              "prettifies a json buffer."
              (interactive)
              (save-excursion
                (json-prettify-region (point-min) (point-max))))
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thesaurus
;; 20241227 - API key is still working
;;
(use-package thesaurus
  :defer t
  :config
  (progn
    (thesaurus-set-bhl-api-key-from-file "~/elisp/.BigHugeLabs.apikey.txt")
    (keymap-global-set "C-c C-t" #'thesaurus-choose-synonym-and-replace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse-url
;;
(use-package browse-url
  :defer t
  :config
  (setq browse-url-browser-function 'browse-url-chrome)
  (if (eq system-type 'windows-nt)
      (let ((path-to-chrome (w32-read-registry 'HKLM "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe" "")))
        (if path-to-chrome
            (setq
             browse-url-chrome-program path-to-chrome))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dictionary
;;
(use-package dictionary
  :defer t
  :commands (dictionary-lookup-definition dictionary-search)
  :config (keymap-global-set "C-c C-d"  #'dictionary-lookup-definition)
  (setq dictionary-use-single-buffer t)
  (setq dictionary-server "dict.org")
  (custom-set-faces
   '(dictionary-word-definition-face ((t (:family "Sans Serif"))))
   '(dictionary-button-button        ((t (:family "Sans Serif"))))
   '(dictionary-button-face          ((t (:background "gray11" :foreground "LightSteelBlue")))))
  )

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dpc-gemini
;;
;; my own little package. Defines an "ask gemini" command that prompts in the
;; minibuffer. It inserts whatever is in the marked region as the default prompt.
;; Also retrieves a key from a keyfile.

(use-package dpc-gemini
  :defer t
  :load-path "~/elisp"
  :ensure nil
  ;; autoloads defined in local modules don't work. That mechanism works only if the
  ;; package is published via elpa.
  :commands (dpc-gemini/set-api-key-from-file dpc-gemini/get-gemini-api-key dpc-gemini/ask-gemini)

  :config (progn
            (dpc-gemini/set-api-key-from-file "~/elisp/.google-gemini-apikey")
            (keymap-global-set "C-c g ?" #'dpc-gemini/ask-gemini)
            (keymap-global-set "C-c g a" #'dpc-gemini/ask-gemini)
            (keymap-global-set "C-c g l" #'dpc-gemini/list-models)
            (keymap-global-set "C-c g s" #'dpc-gemini/select-model)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel
;;
;; A multi-LLM porcelain. Uses transient menu.  See also: chatgpt-shell .
;; Unlike chatgpt-shell I think the intent of gptel is to be available in any
;; buffer, at any moment, via gptel-send. You don't need a dedicated chat
;; buffer.

(defun dpc-gptel-setup ()
  "Invoked when gptel is loaded."
  (dpc-gemini/set-api-key-from-file "~/elisp/.google-gemini-apikey")
  (gptel-make-gemini "Gemini"
    :key (dpc-gemini/get-gemini-api-key)
    :stream t)

  ;; load my prompts
  (let ((prompt-file "~/elisp/.gptel-prompts"))
    (when (file-exists-p prompt-file)
      (condition-case err
          (let ((prompts-data (with-temp-buffer
                                (insert-file-contents prompt-file)
                                (goto-char (point-min))
                                (read (current-buffer)))))
            (when (listp prompts-data)
              (dolist (item prompts-data)
                (when (and (consp item) (symbolp (car item)) (stringp (cdr item)))
                  (let* ((symbol (car item))
                         (prompt-string (cdr item))
                         (existing-entry (assoc symbol gptel-directives)))

                    (if existing-entry
                        (setcdr existing-entry prompt-string)
                      (push (cons symbol prompt-string) gptel-directives)))))))
        (error (message "[gptel-prompts] Error processing %s: %s" prompt-file err)))))

  ;; remove the builtin chat directive
  (setq gptel-directives (cl-remove-if (lambda (pair)
                                         (and (consp pair) (equal (car pair) 'chat)))
                                       gptel-directives))
  (keymap-global-set "C-c C-g s" #'gptel-send))


(use-package gptel
  :defer 21
  :ensure t
  :commands (gptel-send gptel-make-gemini)
  :config (dpc-gptel-setup)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chatgpt-shell
;;
;; A chatbot interface to various LLMs, including Gemini, OpenAI/ChatGPT,
;; Anthropic Claude, DeepSeek Chat, and others via comint in emacs.
;;

(defun dpc-cgs--model-sort (candidates) (sort candidates :lessp #'string<))

(defun dpc--cgs--model-completion-table (candidates)
  "Returns a function to be used as the COMPLETIONS parameter in
`completing-read'.
This is a closure around CANDIDATES.
When using icomplete or icomplete-vertical, `completing-read' uses a
default of «sort first by length and then alphabetically». That is
inappropriate when presenting the list of models.

To override this, users of chatgpt-shell can set
`chatgpt-shell-swap-model-selector' to provide a different experience.

Use this function this way:
  (setq chatgpt-shell-swap-model-selector
        (lambda (candidates)
          (completing-read
            \"New model: \"
            (dpc--cgs--model-completion-table candidates) nil t)))"
  (let ((candidates candidates)) ;; lexical-let?
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata
            (cycle-sort-function . ,#'dpc-cgs--model-sort)
            (display-sort-function . ,#'dpc-cgs--model-sort))
        (complete-with-action action candidates string pred)))))


;; 20250405-2235
;;
;; NB. In `use-package', The :requires keyword specifies a dependency, but _does
;; not_ force load it.  Rather, it prevents loading of THIS package unless the
;; required feature is already loaded.

(use-package chatgpt-shell
  :defer t
  ;; 20250331-1721 - it is not possible to have load-path be dynamically evaluated.
  ;; The value must be known at compile time.
  ;; see https://github.com/jwiegley/use-package/issues/500#issuecomment-335037197
  ;;
  ;; but maybe not? See https://jwiegley.github.io/use-package/keywords/#load-path
  ;;
  ;;:load-path "~/dev/elisp-projects/chatgpt-shell" ;; windows
  ;;:load-path "~/newdev/elisp-projects/chatgpt-shell" ;; linux
  :commands (chatgpt-shell)
  :ensure t ;; restore this later if loading from (M)ELPA
  ;; :requires (dpc-gemini) - No. See note above.

  :config
  ;; avoid flycheck warning about the following functions? This seems dumb.
  (declare-function chatgpt-shell-google-toggle-grounding-with-google-search "ext:chatgpt-shell-google")

  (defun dino-chatgpt-shell-mode-fn ()
    (keymap-local-set "C-c t" #'chatgpt-shell-google-toggle-grounding-with-google-search)
    (keymap-local-set "C-c l" #'chatgpt-shell-google-load-models)
    (keymap-local-set "C-c p" #'chatgpt-shell-prompt-compose)
    (keymap-local-set "C-c r" #'chatgpt-shell-refactor-code)
    (keymap-local-set "C-c d" #'chatgpt-shell-describe-code)
    (keymap-local-set "C-c f" #'chatgpt-shell-proofread-region))

  (defun dpc-cgs-setup ()
    "Invoked when chatgpt-shell is loaded."
    (dpc-gemini/set-api-key-from-file "~/elisp/.google-gemini-apikey")
    (setq chatgpt-shell-google-key (dpc-gemini/get-gemini-api-key))
    (chatgpt-shell-google-load-models) ;; Google models change faster than cgs.
    (setq chatgpt-shell-model-version "gemini-2.5-pro-exp-03-25") ;; default model
    ;; I proposed a change for sorting in cgs when swapping models, but xenodium
    ;; rejected it, saying people who wanted sorting should D-I-Y. So here is my D-I-Y sorting.
    (setq chatgpt-shell-swap-model-selector
          (lambda (candidates)
            (completing-read "Swap to: "
                             (dpc--cgs--model-completion-table candidates) nil t)))

    (add-hook 'chatgpt-shell-mode-hook #'dino-chatgpt-shell-mode-fn))

  (dpc-cgs-setup)
  :catch
  (lambda (_keyword err)
    (message (format "chatgpt-shell init: %s" (error-message-string err)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; google-gemini
;; ;;
;; ;; A library for gemini. It supports "generate content" and listing available
;; ;; models, and maybe some other things. Designed to be a library.  It is homed
;; ;; here: https://github.com/emacs-openai/google-gemini , and is distributed on
;; ;; the jcs-emacs elpa repository. For now I am happy with chatgpt-shell
;; ;;
;;
;; (use-package google-gemini
;;   :defer t
;;   :ensure t
;;   :commands (google-gemini-content-generate)
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; gemini-code-completion
;; ;;
;; ;; Does code completion around point in a code buffer.
;; ;;
;; ;; It is not on an elpa repo. Depends on google-gemini.el. This is not very
;; ;; sophisticated. Not even close to aider.
;; ;;
;; (use-package gemini-code-completion
;;   :load-path "~/elisp"
;;   :defer t
;;   :commands (gemini-complete)
;;   :requires (dpc-gemini google-gemini)
;;   :config
;;   (progn
;;     (setq google-gemini-key (dpc-gemini/get-gemini-api-key))
;;     (keymap-global-set "C-c g c" #'gemini-complete)
;;     )
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-insert - 20241206-0142
;;
;; builtin to emaccs
(use-package autoinsert
  :defer t
  :config (progn
            (auto-insert-mode 1);; global minor mode
            (setq auto-insert-query nil) ;; no prompt before auto-insertion
            (setq auto-insert-directory "~/elisp/auto-insert-content")

            (use-package auto-insert-plus
              :load-path "~/elisp"
              :ensure nil)

            ;; specify the template to use for various filename regexi:
            (setq auto-insert-alist
                  (aip/fixup-auto-insert-alist
                   ;; TODO: generate this alist dynamically from the files found in the directory.
                   '(
                     ("\\.cs$"                      .  "Template.cs" )
                     ("\\.css$"                     .  "Template.css" )
                     ("\\.asp$"                     .  "Template.asp" )
                     ("\\.aspx$"                    .  "Template.aspx" )
                     ("\\.aml$"                     .  "Template.aml" )
                     ("\\.csproj$"                  .  "Template.csproj" )
                     ("\\.vb$"                      .  "Template.vb" )
                     ("\\.vbs$"                     .  "Template.vbs" )
                     ("\\.java$"                    .  "Template.java" )
                     ("PostProcessMsi\\.js$"        .  "Template-PostProcessMsi.js" )
                     ("\\.js$"                      .  "Template.js" )
                     ("\\.jsonnet$"                 .  "Template.jsonnet" )
                     ("\\.wsf$"                     .  "Template.wsf" )
                     ("\\.htm$"                     .  "Template.htm" )
                     ("\\.html$"                    .  "Template.html" )
                     ("\\.c$"                       .  "Template.c" )
                     ("\\.wsc$"                     .  "Template.wsc" )
                     ("\\.ashx$"                    .  "Template.ashx" )
                     ("\\(-\\|\\.\\)vb\\.ashx$"     .  "Template-vb.ashx" )
                     ("\\(-\\|\\.\\)cs\\.ashx$"     .  "Template.ashx" )
                     ("\-vb\\.ashx$"                .  "Template-vb.ashx" )
                     ("\\.wsdl$"                    .  "Template.wsdl" )
                     ("\\.xsd$"                     .  "Template.xsd" )
                     ("\\.xsl$"                     .  "Template.xsl" )
                     ("\\.ssml$"                    .  "Template.ssml" )
                     ("\\.bat$"                     .  "Template.bat" )
                     ("\\.cmd$"                     .  "Template.bat" )
                     ("makefile$"                   .  "Template.makefile" )
                     ("\\.wixproj$"                 .  "Template.wixproj" )
                     ("\\.rss$"                     .  "Template.rss" )
                     ("\\.org$"                     .  "Template.org" )
                     ) ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode

(defun dino-dired-mode-hook-fn ()
  (hl-line-mode 1)
  (keymap-local-set "C-c C-g"  #'dino-dired-kill-new-file-contents)
  (keymap-local-set "C-c C-c"  #'dino-dired-copy-file-to-dir-in-other-window)
  (keymap-local-set "C-c C-m"  #'dino-dired-move-file-to-dir-in-other-window)

  (keymap-local-set "K"  #'dired-kill-subdir) ;; opposite of i (dired-maybe-insert-subdir)
  (keymap-local-set "F"  #'dino-dired-do-find)
  (keymap-local-set "s"  #'dino-dired-sort-cycle)
  (dino-dired-sort-cycle "t") ;; by default, sort by time
  )

;;(add-hook 'dired-mode-hook 'dino-dired-mode-hook-fn)
(use-package dired
  :ensure nil
  :config
  (require 'dino-dired-fixups)
  ;; eliminate the gid in dired when using ls-lisp (eg, on windows)
  (setq ls-lisp-verbosity '(links uid))
  :hook (dired-mode . dino-dired-mode-hook-fn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prog mode - general

(defun dino-define-global-abbrev-table ()
  "Define a custom global abbrev table. Really these are
just auto-corrects on common mis-spellings by me."

  (define-abbrev-table 'global-abbrev-table
    '(
      ("teh" "the" nil 1)
      ("somehting" "something" nil 1)
      ("deprectaed" "deprecated" nil 0)
      ("APigee" "Apigee" nil 1)
      ("Gmeini" "Gemini" nil 1)
      ("hting" "thing" nil 1)
      ("rigueur" "rigeuer" nil 1)
      ("riguer" "rigeuer" nil 1)
      ("submint" "submit" nil 1)
      ("rwquest" "request" nil 1)
      ("request" "requets" nil 1)
      ("hygeine" "hygiene" nil 0)
      ("laucnhed" "launched" nil 0)
      ("supproted" "supported" nil 0)
      ("comittee" "committee" nil 0)
      ("machien" "machine" nil 0)
      ("siilar" "similar" nil 0)
      ("machiens" "machines" nil 0)
      ("cusotmer" "customer" nil 0)
      ("accommplish" "accomplish" nil 0)
      ("accomodate" "accommodate" nil 0)
      ("recieve" "receive" nil 0)
      ("vairous" "various" nil 0)
      ("multipel" "multiple" nil 0)
      ("acheive" "achieve" nil 0)
      ("acheived" "achieved" nil 0)
      ("becasue" "because" nil 1)
      ("btw" "by the way" nil 3)
      ("omw" "on my way" nil 3)
      )
    ))

(dino-define-global-abbrev-table)

(defun dino-prog-mode-hook-fn ()
  (abbrev-mode 1))

(add-hook 'prog-mode-hook 'dino-prog-mode-hook-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode

(defun dino-text-mode-hook-fn ()
  (auto-fill-mode 1)
  (abbrev-mode 1)
  (dino-define-global-abbrev-table)
  (keymap-local-set "C-c C-c"  #'center-paragraph)
  (keymap-local-set "C-c i"    #'dino/indent-line-to-current-column)

  ;;(variable-pitch-mode)
  ;;
  ;; add new abbreviations above.
  ;;

  ;; (require 'refill) ;; automatically refill paragraphs
  ;; (refill-mode 1)
  )

(add-hook 'text-mode-hook 'dino-text-mode-hook-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dino-enable-delete-trailing-whitespace ()
  "remove trailing whitespace"
  (interactive)
  ;; remove trailing whitespace in C files
  ;; http://stackoverflow.com/questions/1931784
  ;;(add-hook 'write-contents-functions 'dino-delete-trailing-whitespace)
  (add-hook 'before-save-hook 'dino-delete-trailing-whitespace nil 'local))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C mode  (common)
                                        ;
(require 'dtrt-indent)
(defun dino-c-mode-common-hook-fn ()
  (cond
   (window-system

    (turn-on-auto-revert-mode)

    ;; RETURN means newline-and-indent in any c-mode buffer
    (local-set-key (kbd "RET") 'newline-and-indent)

    ;; for re-tabbing a region or buffer of code:
    (keymap-local-set "ESC C-R"  #'indent-region)
    (keymap-local-set "ESC #"    #'dino/indent-buffer)

    ;; set this key binding on for c-mode, in lieu of c-subword mode,
    ;; which overwrites my preference
    (keymap-local-set "C-c C-w"  #'compare-windows)

    (hl-line-mode 1)
    (dtrt-indent-mode t)
    ;; ;; allow fill-paragraph to work on xml code doc
    ;; (make-local-variable 'paragraph-separate)

    ;; ;; whitespc
    ;; ;; two or more slashes or one or more stars
    ;; (setq paragraph-separate "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")

    ;; never convert leading spaces to tabs:
    ;;(make-local-variable 'indent-tabs-mode)
    (setq indent-tabs-mode nil)

    ;; remove trailing whitespace in C files
    ;; http://stackoverflow.com/questions/1931784
    ;;(add-hook 'write-contents-functions 'dino-delete-trailing-whitespace)
    (add-hook 'before-save-hook 'dino-delete-trailing-whitespace nil 'local)

    (message "dino-c-mode-common-hook-fn: done."))))

(add-hook 'c-mode-common-hook 'dino-c-mode-common-hook-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C Programming language
(defun dino-c-mode-hook-fn ()
  (cond (window-system

         (c-set-style "myCStyle")

         ;; turn off the c-indent-region thing; I don't like the indents
         ;; it gives me!  by using nil here, I get
         ;; indent-according-to-mode, which is what works for me.
         (setq indent-region-function nil)

         (setq c-auto-newline nil)

         (set (make-local-variable 'comment-start) "// ")
         (set (make-local-variable 'comment-end) "")

         (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe) ;; why?
         (local-set-key (kbd "{") 'skeleton-pair-insert-maybe) ;; why?
         (set (make-local-variable 'skeleton-pair) t)

         ;; allow fill-paragraph to work on xml code doc
         (set (make-local-variable 'paragraph-separate)
              ;; whitespc + two or more slashes or one or more stars
              "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")
         )))

(add-hook 'c-mode-hook 'dino-c-mode-hook-fn)


(c-add-style "myCStyle"
             '("bsd"  ; this must be defined elsewhere - it is in cc-modes.el
               (c-basic-offset . 4)
               (c-echo-syntactic-information-p . t)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . ((c                     . c-lineup-C-comments)
                                   (statement-case-open   . 0)
                                   (case-label            . +)
                                   (substatement-open     . 0)
                                   ))
               ))


(fset 'dino-start-c-comment   "/* ")

(fset 'dino-end-c-comment   " */")

(fset 'dino-end-block-comment
      [escape ?\C-b escape ?  ?\C-a ?\C-w ?\C-y escape ?\C-f ?  ?/ ?/ ?  ?\C-y ?\C-x ?\C-x ?\C-c ?1])


(defun dino-c-get-value-from-comments (marker-string line-limit)
  "gets a string from the header comments in the current buffer.

            This is used to extract the compile command from the comments. It
            could be used for other purposes too.

            It looks for \"marker-string:\" and returns the string that
            follows it, or returns nil if that string is not found.

            eg, when marker-string is \"compile\", and the following
            string is found at the top of the buffer:

            compile: cl.exe /I uthash

            ...then this command will return the string

            \"cl.exe /I uthash\"

            It's ok to have whitespace between the marker and the following
            colon."
  (let (start search-limit found)
    ;; determine what lines to look in
    (save-excursion
      (save-restriction
        (widen)
        (cond ((> line-limit 0)
               (goto-char (setq start (point-min)))
               (forward-line line-limit)
               (setq search-limit (point)))
              ((< line-limit 0)
               (goto-char (setq search-limit (point-max)))
               (forward-line line-limit)
               (setq start (point)))
              (t                        ;0 => no limit (use with care!)
               (setq start (point-min))
               (setq search-limit (point-max))))))

    ;; look in those lines
    (save-excursion
      (save-restriction
        (widen)
        (let ((re-string
               (concat "\\b" marker-string "[ \t]*:[ \t]*\\(.+\\)$")))
          (if (and start
                   (< (goto-char start) search-limit)
                   (re-search-forward re-string search-limit 'move))

              (buffer-substring-no-properties
               (match-beginning 1)
               (match-end 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cpp mode
                                        ;
;;    (c-basic-offset . 4)
;;    (c-comment-only-line-offset . (0 . 0))
;;    (c-offsets-alist
;;     . (
;;        (access-label . -)
;;        (arglist-close . c-lineup-arglist)
;;        (arglist-cont . 0)
;;        (arglist-cont-nonempty . c-lineup-arglist)
;;        (arglist-intro . c-lineup-arglist-intro-after-paren)
;;        (block-close . 0)
;;        (block-open . 0)
;;        (brace-entry-open . 0)
;;        (brace-list-close . 0)
;;        (brace-list-entry . 0)
;;        (brace-list-intro . +)
;;        (brace-list-open . +)
;;        (c . c-lineup-C-comments)
;;        (case-label . +)
;;        (catch-clause . 0)
;;        (class-close . 0)
;;        (class-open . 0)
;;        (comment-intro . c-lineup-comment)
;;        (cpp-macro . 0)
;;        (cpp-macro-cont . c-lineup-dont-change)
;;        (defun-block-intro . +)
;;        (defun-close . 0)
;;        (defun-open . 0)
;;        (do-while-closure . 0)
;;        (else-clause . 0)
;;        (extern-lang-close . 0)
;;        (extern-lang-open . 0)
;;        (friend . 0)
;;        (func-decl-cont . +)
;;        (inclass . +)
;;        (inexpr-class . +)
;;        (inexpr-statement . 0)
;;        (inextern-lang . +)
;;        (inher-cont . c-lineup-multi-inher)
;;        (inher-intro . +)
;;        (inlambda . c-lineup-inexpr-block)
;;        (inline-close . 0)
;;        (inline-open . 0)
;;        (innamespace . +)
;;        (knr-argdecl . 0)
;;        (knr-argdecl-intro . 5)
;;        (label . 0)
;;        (lambda-intro-cont . +)
;;        (member-init-cont . c-lineup-multi-inher)
;;        (member-init-intro . +)
;;        (namespace-close . 0)
;;        (namespace-open . 0)
;;        (objc-method-args-cont . c-lineup-ObjC-method-args)
;;        (objc-method-call-cont . c-lineup-ObjC-method-call)
;;        (objc-method-intro . [0])
;;        (statement . 0)
;;        (statement-block-intro . +)
;;        (statement-case-intro . +)
;;        (statement-case-open . +)
;;        (statement-cont . +)
;;        (stream-op . c-lineup-streamop)
;;        (string . c-lineup-dont-change)
;;        (substatement . +)
;;        (substatement-open . 0)
;;        (template-args-cont c-lineup-template-args +)
;;        (topmost-intro . 0)
;;        (topmost-intro-cont . 0)
;;        ))
;;    ))


(defconst my-cpp-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    (c-tab-always-indent        . t)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "My C++ Programming Style")

(c-add-style "myCppStyle" my-cpp-style)


(defun dino-cpp-mode-fn ()
  (cond (window-system
         (c-set-style "myCppStyle")

         (turn-on-font-lock)

         (message "setting local key bindings....")

         (keymap-local-set "ESC C-R"  #'indent-region)
         (keymap-local-set "ESC #"    #'dino/indent-buffer)
         (keymap-local-set "C-c C-w"  #'compare-windows)

         ;; for skeleton stuff
         (set (make-local-variable 'skeleton-pair) t)
         (setq comment-empty-lines t)

         (local-set-key (kbd "(") #'skeleton-pair-insert-maybe)
         (local-set-key (kbd "[") #'skeleton-pair-insert-maybe)
         ;;(local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

         ;; these allow typeover of matching brackets
         (local-set-key (kbd "\"") #'dino-skeleton-pair-end)
         (local-set-key (kbd ")") 'dino-skeleton-pair-end)
         (local-set-key (kbd "]") 'dino-skeleton-pair-end)

         (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
         ;;(local-set-key (kbd "{") 'dino-insert-open-brace)

         (require 'yasnippet)
         (yas-minor-mode-on)

         ;; for hide/show support
         (hs-minor-mode 1)
         (setq hs-isearch-open t)

         ;; with point inside the block, use these keys to hide/show
         (keymap-local-set "C-c >"  #'hs-hide-block)
         (keymap-local-set "C-c <"  #'hs-show-block)

         ;; autorevert.el is built-in to emacs; if files
         ;; are changed outside of emacs, the buffer auto-reverts.
         (turn-on-auto-revert-mode)

         ;; allow fill-paragraph to work on xml code doc
         (set (make-local-variable 'paragraph-separate)
              ;; whitespace
              ;; two or more slashes or one or more stars
              "[ \t]*\\(//+\\|\\**\\)\\([ \t]+\\|[ \t]+<.+?>\\)$\\|^\f")

         ;; never convert leading spaces to tabs:
         ;;(make-local-variable 'indent-tabs-mode)
         (setq indent-tabs-mode nil)

         (message "dino-cpp-mode-fn: done.")
         )))
(add-hook 'c++-mode-hook 'dino-cpp-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf mode
;; used rarely

(use-package protobuf-mode
  :ensure t
  :defer t
  :config (progn
            (defconst my-protobuf-style
              '((c-basic-offset . 2)
                (indent-tabs-mode . nil)))
            (defun dino-protobuf-mode-hook-fn ()
              "my mode hook for protobuf-mode"
              (keymap-local-set "ESC C-R"  #'indent-region)
              (keymap-local-set "ESC #"    #'dino/indent-buffer)
              (keymap-local-set "C-c C-w"  #'compare-windows)
              (display-line-numbers-mode)

              (c-add-style "my-style" my-protobuf-style t)
              ;;(require 'flycheck)
              ;;(flycheck-mode 1)
              )
            (add-hook 'protobuf-mode-hook 'dino-protobuf-mode-fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; csharp mode

(c-add-style
 "myC#Style"
 '("C#"  ; this must be defined elsewhere
   (c-basic-offset . 4)
   (c-echo-syntactic-information-p . t)
   (c-comment-only-line-offset . (0 . 0))
   (c-offsets-alist . ((c                     . c-lineup-C-comments)
                       (namespace-open        . 0)
                       (namespace-close       . 0)
                       (innamespace           . +)
                       (class-open            . 0)
                       (class-close           . 0)
                       (inclass               . +)
                       (block-open            . 0)  ;; eg, open a block under a function name or if stmt;
                       ;; want this to be flush with prev line.
                       (arglist-cont          . +)
                       (substatement-open     . 0)  ;; I think this is for a try {} or if{} or etc. why this is not block open, I don't know!
                       (defun-open            . 0)  ;; method defn? (but no!)
                       (defun-block-intro     . +)  ;;0 ; block within a function????
                       (inline-open           . 0)  ;; eg, opening a function? ??
                       (statement-block-intro . +)  ;; unknown what this is
                       (brace-list-open       . 0)  ;; list open (like an enum, array initializer)
                       (brace-list-intro      . +)  ;; first item in the list
                       (brace-list-entry      . 0)  ;; subsequent items in the list
                       (brace-list-close      . 0)  ;; list close
                       (cpp-macro             . (csharp-lineup-region 0))    ;; align region/endregion
                       ;;(cpp-macro             . (csharp-lineup-if-and-region 0))    ;; align region/endregion and if/else/endif
                       (statement-cont        . (dinoch-csharp-lineup-string-continuations +))
                       ))
   ))


;; Aligns long strings broken across multiple lines.
;; Also aligns attributes preceding methods or classes,
;; and aligns the lines following attributes.
;; need this in the styles list:
;;      (statement-cont . (dinoch-csharp-lineup-string-cont +))
(defun dinoch-csharp-lineup-string-continuations (langelem)
  "Like `c-lineup-string-cont' but works with csharp string continuations."
  (let ((original-point (point))
        (string-decl-regex
         (concat "\\(\\(public\\|private\\|protected\\)[ \t\n\r\f\v]+\\)?"
                 "\\(string\\|var\\)[ \t\n\r\f\v]+"
                 "\\([[:alpha:]_][[:alnum:]_]*\\)"
                 "[ \t\n\r\f\v]*"
                 "="
                 "[ \t\n\r\f\v]*"
                 "@?\""
                 ))
        tmp)

    (save-excursion

      (cond
       ;; Case 1: declaration of a literal string.
       ((progn
          (goto-char (cdr langelem))
          (looking-at string-decl-regex))
        (goto-char (1- (match-end 0)))
        (vector (current-column)))

       ;; Case 2a: declaration of a literal string, not at the top of the class.
       ;;          on the first line of the continuation, or where the previous
       ;;          line of the continuation begins with an immediate string.
       ;;
       ;; Like this:
       ;;
       ;;   void Method1()
       ;;   {
       ;;   }
       ;;
       ;;   private string Foo = "djdkdj" +
       ;;
       ;;
       ;; For some reason, cc-mode gives us the langelem that belongs to the
       ;; Method1, not to Foo.??
       ;;
       ;; So, hack it.
       ;;
       ((progn
          (goto-char original-point)
          (back-to-indentation)
          (c-backward-syntactic-ws)
          (back-to-indentation)
          (setq tmp (or
                     (looking-at string-decl-regex)
                     (looking-at "@?\""))))

        (goto-char (if (eq (char-after (match-beginning 0)) ?@)
                       (match-beginning 0)
                     (1- (match-end 0))))
        (vector (current-column)))


       ;; Case 2b: declaration of a literal string, not at the top of the class,
       ;;          where the previous line of the continuation begins with
       ;;          a symbol name or an integer.
       ;;
       ;; Like this:
       ;;
       ;;   void Method1()
       ;;   {
       ;;   }
       ;;
       ;;   private string Foo = "djdkdj" +
       ;;                        "zoweee" +
       ;;
       ((progn
          (goto-char original-point)
          (back-to-indentation)
          (c-backward-syntactic-ws)
          (back-to-indentation)
          (or
           (looking-at "\\([[:alpha:]_][[:alnum:]_]*\\)[ \t\n\r\f\v]*\\+")
           (looking-at "\\([0-9]+\\)[ \t\n\r\f\v]*\\+")))

        (goto-char (match-beginning 0))
        (vector (current-column)))


       ;; case 3: use of a literal string
       ((progn
          (goto-char (cdr langelem))
          (looking-at "@?\""))
        (goto-char (match-beginning 0))
        (vector (current-column)))

       ;;        ;; case 4: everything else
       ;;        ;; If neither matches, then check if preceding line is an attribute.
       ;;        ;; If yes, then indent properly.  If not, then return nil, so the next
       ;;        ;; line-up fn will be invoked.
       ;;        (t
       ;;
       ;;         (progn
       ;;           (goto-char original-point)
       ;;           ;; go to the indentation of the previous line
       ;;           (c-backward-syntactic-ws)
       ;;           (back-to-indentation)
       ;;           ;; is it an attribute?
       ;;           (cond
       ;;            ((looking-at "\\[")
       ;;             (vector (- (c-point 'boi) (c-point 'bol) )))
       ;;
       ;;            ;; not an attribute
       ;;            (t nil))))

       (t nil)
       ))))



;; ;; hideshow stuff for csharp - should this be elsewhere?
;; (defun csharp-hs-forward-sexp (&optional arg)
;;
;;   "I set hs-forward-sexp-func to this function.
;;
;;   I found this customization necessary to do the hide/show magic in C#
;;   code, when dealing with region/endregion. This routine
;;   goes forward one s-expression, whether it is defined by curly braces
;;   or region/endregion. It handles nesting, too.
;;
;;   The forward-sexp method takes an arg which can be negative, which
;;   indicates the move should be backward.  Therefore, to be fully
;;   correct this function should also handle a negative arg. However,
;;   the hideshow.el package never uses negative args to its
;;   hs-forward-sexp-func, so it doesn't matter that this function does not
;;   do negative numbers.
;;
;;   The arg can also be greater than 1, which means go forward
;;   multiple times. This function doesn't handle that EITHER.  But
;;   again, I haven't see that as a problem."
;;
;;   (let ((nestlevel 0)
;;         (mark1 (point))
;;         (done nil))
;;
;;     (if (and arg (< arg 0))
;;         ;; a negative argument; we want to back up!
;;         (message "negative arg (%d) is not supported..." arg)
;;
;;       ;; else, we have a positive argument, hence move forward.
;;       ;; simple case is just move forward one brace
;;       (if (looking-at "{")
;;           (and
;;            (forward-sexp arg)
;;            )
;;
;;         ;; The more complex case is dealing with a "region/endregion" block.
;;         ;;We have to deal with nested regions!
;;         (and
;;          (while (not done)
;;            (re-search-forward "^[ \\t]*#[ \\t]*\\(region\\|endregion\\)\\b"
;;                               (point-max) 'move)
;;            (cond
;;
;;             ((eobp))                            ; do nothing if at end of buffer
;;
;;             ((and
;;               (match-beginning 1)
;;
;;               ;; if the match is longer than 6 chars, we know it is "endregion"
;;               (if (> (- (match-end 1) (match-beginning 1)) 6)
;;                   (setq nestlevel (1- nestlevel))
;;                 (setq nestlevel (1+ nestlevel))
;;                 )
;;               )))
;;
;;            (setq done (not (and (> nestlevel 0) (not (eobp)))))
;;
;;            ) ; while
;;
;;          (if (= nestlevel 0)
;;              (goto-char (match-end 2))))))))


;; more for hideshow.el
(unless (assoc 'csharp-ts-mode hs-special-modes-alist)
  (push '(csharp-ts-mode

          ;; "\\(^\\s*#\\s*region\\b\\)\\|{"       ; regexp for start block
          "\\(^[ \\t]*#[ \\t]*region\\b\\)\\|{"    ; regexp for start block


          ;; "\\(^\\s*#\\s*endregion\\b\\)\\|}"    ; regexp for end block
          "\\(^[ \\t]*#[ \\t]*endregion\\b\\)\\|}" ; regexp for end block


          "/[*/]"                                  ; regexp for comment start

          csharp-hs-forward-sexp                   ; hs-forward-sexp-func

          ;;csharp-hs-adjust-block-beginning       ; csharp adjust ?
          hs-c-like-adjust-block-beginning         ; c-like adjust (1 char)
          )
        hs-special-modes-alist))




(defun dino-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (cond (window-system
         (turn-on-font-lock)
         (c-set-style "myC#Style")
         (setq c-basic-offset 2) ;; width of one indent level
         (apheleia-mode)
         (message "setting local key bindings....")

         (keymap-local-set "ESC C-R"  #'indent-region)
         (keymap-local-set "ESC #"    #'dino/indent-buffer)
         (keymap-local-set "C-c C-w"  #'compare-windows)

         (local-set-key "\C-c\C-y"  'csharp-show-syntax-table-prop)
         (local-set-key "\C-c\C-h"  'csharp-show-parse-state)

         (local-set-key (kbd "C-<") 'csharp-move-back-to-beginning-of-defun)
         (local-set-key (kbd "C->") 'csharp-move-fwd-to-end-of-defun)

         ;; 20241228-2146 - this worked at one point. Not sure if still works.
         (local-set-key (kbd "C-M-\<") 'csharp-move-back-to-beginning-of-class)
         (local-set-key (kbd "C-M-\>") 'csharp-move-fwd-to-end-of-class)

         ;; TODO: consider relying on electric-pair
         (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
         (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
         (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)

         ;; insert a pair of quotes
         (local-set-key (kbd "\"") 'dino-insert-paired-quotes)
         (local-set-key (kbd "\'") 'dino-insert-paired-quotes)

         ;; these allow typeover of matching brackets
         ;; (local-set-key (kbd "\"") 'dino-skeleton-pair-end)
         (local-set-key (kbd ">") 'dino-skeleton-pair-end)
         (local-set-key (kbd ")") 'dino-skeleton-pair-end)
         (local-set-key (kbd "]") 'dino-skeleton-pair-end)

         ;;(local-set-key (kbd "{") 'skeleton-pair-insert-maybe)

         ;; Default to auto-indent on Enter
         ;;(define-key csharp-mode-map [(control j)] 'newline)
         ;;(define-key csharp-mode-map [(control m)] 'newline-and-indent)

         ;;(define-key csharp-mode-map [return] 'newline-and-indent)

         ;; for skeleton stuff
         (set (make-local-variable 'skeleton-pair) t)

         (yas-minor-mode-on)
         (show-paren-mode 1)
         (hl-line-mode 1)

         (require 'flycheck)
         (flycheck-mode)
         ;;         (flycheck-select-checker 'csharp)

         ;; for hide/show support
         (hs-minor-mode 1)
         (setq hs-isearch-open t)

         ;; with point inside the block, use these keys to hide/show
         (keymap-local-set "C-c >"  #'hs-hide-block)
         (keymap-local-set "C-c <"  #'hs-show-block)

         ;; autorevert.el is built-in to emacs; if files
         ;; are changed outside of emacs, the buffer auto-reverts.
         (turn-on-auto-revert-mode)

         ;; never convert leading spaces to tabs:
         ;; (setting this variable automatically makes it local)
         (setq indent-tabs-mode nil)

         ;; ;; dino's C# code completion
         ;;(require 'csharp-completion)
         ;; ;;(csharp-analysis-get-analysis)

         ;; the imenu stuff doesn't perform well; impractical
         (setq csharp-want-imenu nil)

         ;; Sun, 08 Apr 2012  15:09
         ;; I had trouble setting csharp as a ac-source.
         ;; it works fine with cscomp popping the completion menu.
         ;; (require 'auto-complete-config)
         ;; (auto-complete-mode 1)
         ;; ;; require an auto-complete key, instead of
         ;; ;; doing it automatically.
         ;; (setq ac-auto-start nil)
         ;; ;;(setq ac-auto-start 2)  ;;or 3?
         ;;(local-set-key "\M-\\"   'ac-complete-csharp)

         (local-set-key "\M-\\"   'cscomp-complete-at-point)
         ;;(local-set-key "\M-\\"   'cscomp-complete-at-point-menu)
         (local-set-key "\M-\."   'cscomp-complete-at-point-menu)

         (display-line-numbers-mode)

         (message "dino-csharp-mode-fn: done.")
         )))


(eval-after-load "csharp-mode"
  '(progn
     (require 'compile)
     (add-hook  'csharp-mode-hook 'dino-csharp-mode-fn t)

     ;; 20250126-1013 - adjustments for indentation.
     ;; This one is for the open curly for the using statement.
     ;; (The using _directive_ (for import at top of file) is different.)
     (setf (cdr (car csharp-ts-mode--indent-rules))
           (cons
            '((parent-is "using_statement") parent-bol 0)
            (cdr (car csharp-ts-mode--indent-rules))))
     ;; This rule is for the opening curly brace on anonymous lambdas, when the
     ;; open curly is on a new line.
     (setf (cdr (car csharp-ts-mode--indent-rules))
           (cons
            '((parent-is "lambda_expression") parent-bol 0)
            (cdr (car csharp-ts-mode--indent-rules))))
     (setq-local treesit-simple-indent-rules csharp-ts-mode--indent-rules)
     ))



(defun dino-csharpier-buffer ()
  "run csharpier on the current buffer."
  (interactive)
  (let ((command
         (if (eq system-type 'windows-nt)
             "dotnet csharpier --write-stdout"
           "dotnet-csharpier"))
        (orig-point (point))
        (output-goes-to-current-buffer t)
        (output-replaces-current-content t))

    (message "dino-csharpier-buffer command: %s" command)
    (save-excursion
      (shell-command-on-region (point-min)
                               (point-max)
                               command
                               output-goes-to-current-buffer
                               output-replaces-current-content))
    (goto-char orig-point)))


(defun dino-csharp-ts-mode-fn ()
  "function that runs when csharp-ts-mode is initialized for a buffer."

  ;;(turn-on-font-lock) ;; apparently no longer needed with TS
  (setq c-basic-offset 4) ;; width of one indent level
  (setq tab-width 4) ;; this variable is used by `eglot-format'
  (setq show-trailing-whitespace t)
  (apheleia-mode)

  ;; I am pretty sure eglot will work only locally.
  (when (not (file-remote-p default-directory))
    (eglot-ensure)
    (company-mode)
    (keymap-local-set "C-<tab>"  #'company-complete)
    )

  (message "setting local key bindings....")
  (keymap-local-set "ESC C-R"  #'indent-region)
  (keymap-local-set "ESC #"    #'dino/indent-buffer)
  (keymap-local-set "C-c C-w"  #'compare-windows)
  (keymap-local-set "C-c C-g"  #'dino-csharpier-buffer)
  (keymap-local-set "C-x C-e"     #'compile)

  ;; 20241229-1756
  (electric-pair-local-mode 1)

  (require 'yasnippet)
  (yas-minor-mode-on)
  (show-paren-mode 1)
  (hl-line-mode 1)
  (company-mode)
  (display-line-numbers-mode)

  ;; 20241228-0619
  ;; Like all TS modes, csharp-ts-mode relies on flyMAKE, not flycheck.
  ;; So I think I should disable this for now.
  ;; (require 'flycheck)
  ;; (flycheck-mode)
  ;;         (flycheck-select-checker 'csharp)

  ;; for hide/show support
  (require 'hideshow)
  (hs-minor-mode 1)
  (setq hs-isearch-open t)

  ;; with point inside the block, use these keys to hide/show
  (keymap-local-set "C-c >"  #'hs-hide-block)
  (keymap-local-set "C-c <"  #'hs-show-block)

  ;; autorevert.el is built-in to emacs; if files
  ;; are changed outside of emacs, the buffer auto-reverts.
  (turn-on-auto-revert-mode)
  ;; never convert leading spaces to tabs:
  ;; (setting this variable automatically makes it local)
  (setq indent-tabs-mode nil)
  ;; the imenu stuff doesn't perform well; impractical
  (setq csharp-want-imenu nil)
  (display-line-numbers-mode)

  (message "dino-csharp-ts-mode-fn: done.")

  (add-hook 'before-save-hook 'dino-delete-trailing-whitespace nil 'local)
  )

(eval-after-load "csharp-mode"
  '(progn
     (require 'compile)
     (add-hook 'csharp-ts-mode-hook 'dino-csharp-ts-mode-fn t)
     ;; 20250223-1328 - this may not be necessary in emacs 30.1; bears further testing.
     ;; (if (fboundp 'apheleia-mode)
     ;;     (add-hook 'apheleia-post-format-hook #'dino-maybe-eglot-reconnect))
     ))

(use-package csharp-ts-mode-navigation
  :defer t
  :after csharp-mode
  :config (add-hook 'csharp-ts-mode-hook 'ctsn/setup))

(defun dino-maybe-eglot-reconnect ()
  "At least in emacxs 29.4, when apheleia reformats a C# buffer it seems to
  confuse the language server. This function will force a reconnect in
  that case. This is overkill and slow, but I have not yet figured out a way
  around it."
  (when (and
         (or (eq major-mode 'csharp-mode) (eq major-mode 'csharp-ts-mode))
         (eglot-managed-p))
    (eglot-reconnect (eglot--current-server-or-lose))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile
;;

;;  (progn
;;    (add-to-list
;;     'compilation-error-regexp-alist-alist
;; '(ms-resx "^[ \t]*\\([A-Za-z0-9\\.][^\n(]*\\.resx\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) ?: +\\(error\\|warning\\) MSB[0-9]+:" 1 2 3 4)
;;    (add-to-list
;;     'compilation-error-regexp-alist
;;     'ms-resx)))

(eval-after-load "compile"
  '(progn

     ;; Each elt has the form (SYMBOL REGEXP FILE [LINE COLUMN TYPE
     ;; HYPERLINK HIGHLIGHT...]).  If REGEXP matches, the FILE'th
     ;; subexpression gives the file name, and the LINE'th subexpression
     ;; gives the line number.  The COLUMN'th subexpression gives the
     ;; column number on that line.

     (mapcar

      (lambda (x)
        (add-to-list 'compilation-error-regexp-alist-alist x)
        (add-to-list
         'compilation-error-regexp-alist (car x)))

      (list
       ;;    '(jslint
       ;;  "^[ \t]*\\([A-Za-z.0-9_: \\-]+\\)(\\([0-9]+\\)[,]\\( *[0-9]+\\))\\( Microsoft JScript runtime error\\| JSLINT\\): \\(.+\\)$" 1 2 3)

       ;; Microsoft VJC:
       ;;sample.java(6,1) : error J0020: Expected 'class' or 'interface'
       '(msvjc "\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) VJS[0-9]+:" 1 3 4)


       ;; Microsoft Xaml:
       ;;sample.xaml(6,1) : error J0020: Expected 'class' or 'interface'
       '(xaml "\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) VJS[0-9]+:" 1 3 4)

       ;; Microsoft C/C++:
       ;;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
       ;;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
       ;;  .\cppcli1.cpp(36): error C2059: syntax error : 'public'
       ;;  e:\projects\myce40\tok.h(85) : error C2236: unexpected 'class' '$S1'
       ;;  myc.cpp(14) : error C3149: 'class System::String' : illegal use of managed type 'String'; did you forget a '*'?
       ;;   ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) ?\: \\(error\\|warning\\) C[0-9]+:" 1 3)
       '(msvc "^[ \t]*\\([A-Za-z0-9\\.][^\n(]*\\.\\(cpp\\|c\\|h\\)\\)(\\([0-9]+\\)) *: +\\(error\\|fatal error\\|warning\\) C[0-9]+:" 1 3)


       ;; Microsoft RESX compiler
       ;;  c:\dev\XPathVisualizerTool.resx(257,5): error MSB3103: Invalid Resx file. Invalid length
       '(ms-resx "^[ \t]*\\([A-Za-z0-9\\.][^\n(]*\\.resx\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) ?: +\\(warning\\) MSB[0-9]+:" 1 2 3 1)

       '(ms-resx "^[ \t]*\\([A-Za-z0-9\\.][^\n(]*\\.resx\\)(\\([0-9]+\\)[,]\\([0-9]+\\)) ?: +\\(error\\) MSB[0-9]+:" 1 2 3 2)


       ;; Candle.exe or light.exe (wix SDK)
       ;;
       ;; c:\dinoch\dev\WiX\FirstTry\First.wxs(19) : error CNDL0006 : Blah blah blah
       '(wix-candle "^[ \t]*\\([A-Za-z0-9][^\r\n](+\\.wxs\\)(\\([0-9]+\\)) ?: +\\(error\\|warning\\) \\(CNDL\\|LGHT\\)[0-9]+ *:" 1 2)


       ;; Sun javac.exe, and javadoc
       ;;
       ;; javac:
       ;; TestClient.java:222: cannot find symbol
       ;;
       ;; javadoc:
       ;; .\Message.java:110: warning - Tag @see: can't find fetch(String) in ionic.Msmq.Message
       '(javac "^\\([\.\\A-Za-z0-9][^\n:]+\\.java\\):\\([0-9]+\\): +\\([^\n]+\\)$" 1 2)

       ;; makefiles (nmake)
       ;;
       ;; makefile(136) : fatal error U1033: syntax error : ':' unexpected
       '(nmake "^\\(makefile\\)(\\([0-9]+\\)) +: +\\([^\n]+\\)$" 1 2)

       ;; elisp  (byte-compile-file)
       ;; emacs.el:53:2:Warning: `set-face-underline' is an obsolete function (as of Emacs 22.1)
       '(elisp "^\\([\.\\A-Za-z0-9][^\n:]+\\.el\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(Error\\|Warning\\): [^\n]+\\)$" 1 2 3)
       ))

     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP mode
;;

(defun mark-current-word ()
  "Select the word under cursor.
«word» here is considered any alphanumeric sequence or _ .

Does not consider word syntax tables.
  "
  (interactive)
  (let (pt)
    (skip-chars-backward "_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "_A-Za-z0-9")
    (set-mark pt)))


(defun dino-php-mode-fn ()
  "Function to run when php-mode is initialized for a buffer."

  (require 'flycheck)
  (flycheck-mode)
  ;;(flycheck-select-checker 'php-phpcs) ;; style and syntax
  (flycheck-select-checker 'php) ;; syntax only

  (setq c-default-style "bsd"
        c-basic-offset 2)

  (keymap-local-set "ESC C-R"  #'indent-region)
  (keymap-local-set "C-c C-c"  #'comment-region)
  (keymap-local-set "C-c C-k s"    #'dino/camel-to-snakecase-word-at-point)
  (keymap-local-set "C-c C-k c"    #'dino/snake-to-camelcase-word-at-point)

  ;; not sure if necessary or not.
  (modify-syntax-entry ?/ ". 124b" php-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" php-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"  php-mode-syntax-table)
  (modify-syntax-entry ?\^m "> b" php-mode-syntax-table)

  ;; why / how is the following getting overridden?
  (setq comment-multi-line nil ;; maybe
        comment-start "// "
        comment-end ""
        comment-style 'indent
        comment-use-syntax t)
  )
(add-hook 'php-mode-hook 'dino-php-mode-fn t)

(eval-after-load "php-mode"
  '(progn
     (require 'compile)
     ))



(defvar dc-php-program "/usr/bin/php" "PHP interpreter")

;; (defun dino-php-flymake-get-cmdline  (source base-dir)
;;   "Gets the cmd line for running a flymake session in a PHP buffer.
;; This gets called by flymake itself."
;;
;;   (dino-log "PHP" "flymake cmdline for %s" source)
;;
;;   (list dc-php-program
;;         (list "-f" (expand-file-name source)  "-l")))
;;
;;
;; (defun dino-php-flymake-init ()
;;   "initialize flymake for php"
;;   (let ((create-temp-f 'dino-flymake-create-temp-intemp)
;;         ;;(create-temp-f 'flymake-create-temp-inplace)
;;         (use-relative-base-dir t)
;;         (use-relative-source t)
;;         (get-cmdline-f 'dino-php-flymake-get-cmdline)
;;         args
;;         temp-source-file-name)
;;
;;     (dino-log "PHP" "flymake-for-php invoke...")
;;
;;     (setq temp-source-file-name (flymake-init-create-temp-buffer-copy create-temp-f)
;;
;;           args (flymake-get-syntax-check-program-args
;;                 temp-source-file-name "."
;;                 use-relative-base-dir use-relative-source
;;                 get-cmdline-f))
;;     args))
;;
;;
;; (defun dino-php-flymake-cleanup ()
;;   (dino-log "PHP" "flymake-for-php cleanup...")
;;   (flymake-simple-cleanup) )
;;
;; (eval-after-load "flymake"
;;   '(progn
;;      (if (file-exists-p dc-php-program)
;;          ;; 1. add a PHP entry to the flymake-allowed-file-name-masks
;;          (let* ((key "\\.php\\'")
;;                 (phpentry (assoc key flymake-allowed-file-name-masks)))
;;            (if phpentry
;;                (setcdr phpentry '(dino-php-flymake-init dino-php-flymake-cleanup))
;;              (add-to-list
;;               'flymake-allowed-file-name-masks
;;               (list key 'dino-php-flymake-init 'dino-php-flymake-cleanup)))))))



;; ;; use PHP CodeSniffer instead of just regular PHP.exe
;; (require 'flyphpcs)
;;
;; (if (file-exists-p dc-php-program)
;;     (setq fly/phpcs-phpcs-dir "c:\\dev\\phpcs"
;;       fly/phpcs-phpexe "c:\\php\\php.exe"
;;       fly/phpcs-standard "Dino" ;; Zend, PEAR, PHPCS, etc
;;       fly/phpcs-phpinc "c:\\dev\\phplibs" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML (nxml-mode)
;;
(when (boundp 'apheleia-formatters)

  ;; TODO: use smarter path resolution here
  (push '(dino-xmlpretty .
                         ("java" "-jar"
                          "/Users/dchiesa/dev/java/XmlPretty/target/com.google.dchiesa-xml-prettifier-20230725.jar"
                          "-"))
        apheleia-formatters)

  ;; TODO: use smarter path resolution here
  (push '(xml-prettier .
                       ("/Users/dchiesa/dev/java/XmlPretty/node_modules/.bin/prettier"
                        "--config" "/Users/dchiesa/dev/java/XmlPretty/prettier-config.json"
                        "--stdin-filepath" "foo.xml"))
        apheleia-formatters)

  ;; 20250401-1837
  ;;
  ;; In the end I stopped using the XML prettier (I thought so anyway) because
  ;; it was a little too aggressive and disruptive in reformatting my XML files,
  ;; for my taste.


  ;; ;; change an existing formatter in the alist (during development only)
  ;; (setf (alist-get 'xml-prettier apheleia-formatters)
  ;;        '("/Users/dchiesa/dev/java/XmlPretty/node_modules/.bin/prettier"
  ;;           "--config" "/Users/dchiesa/dev/java/XmlPretty/prettier-config.json"
  ;;          "--stdin-filepath" "foo.xml"))

  ;; to specify an apheleia plugin for a mode that is not currently in the list:
  ;;(push '(nxml-mode . dino-xmlpretty) apheleia-mode-alist)
  (push '(nxml-mode . xml-prettier) apheleia-mode-alist)

  ;; ;; to switch between previously set plugin for a mode:
  ;; (setf (alist-get 'nxml-mode apheleia-mode-alist) 'xml-prettier)
  ;; ;;(setf (alist-get 'nxml-mode apheleia-mode-alist) 'dino-xmlpretty)

  )

(defun dino-xml-mode-fn ()
  (turn-on-auto-revert-mode)
  ;; for hide/show support
  (hs-minor-mode 1)
  (setq hs-isearch-open t)
  (display-line-numbers-mode)

  (keymap-local-set "ESC C-R" #'indent-region)
  (keymap-local-set "C-c n"   #'sgml-name-char) ;; inserts entity ref of pressed char
  (keymap-local-set "M-#"     #'dino-xml-pretty-print-buffer)
  (keymap-local-set "C-c f"   #'dino-replace-filename-no-extension)

  (keymap-local-set "C-<"      #'nxml-backward-element)
  (keymap-local-set "C->"      #'nxml-forward-element)
  (keymap-local-set "C-c C-c"  #'dino-xml-comment-region)

  ;; C-M-f will jump over complete elements

  (setq nxml-sexp-element-flag t
        nxml-child-indent 2)

  ;; never convert leading spaces to tabs:
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  ;; 20230718-1235
  ;;(apheleia-mode) ;; this can be disruptive

  ;; Include single-quote as a string-quote char
  ;; Without this, it was being treated as part of a word,
  ;; I guess because xml-mode is derived from text-mode where
  ;; it's an apostrophe used in contractions.
  ;; But treating it as part of a word is counter-productive in an XML buffer.
  (if (boundp 'sgml-mode-syntax-table)
      (modify-syntax-entry ?\' "\"" sgml-mode-syntax-table)
    (modify-syntax-entry ?\' ".")) ;; . = punctuation


  (add-hook 'before-save-hook 'dino-delete-trailing-whitespace nil 'local)

  ;; when `nxml-slash-auto-complete-flag' is non-nil, get completion
  (setq nxml-slash-auto-complete-flag t)

  ;; ;;; this pair of sets almost works, except that
  ;; ;;; it un-indents the intervening XML when removing
  ;; ;;; comments.  If I remove the comment-continue thing,
  ;; ;;; then the comment-block does not get really completely removed
  ;; ;;; on uncommenting.
  ;; (set (make-local-variable 'comment-style) 'multi-line)
  ;; (set (make-local-variable 'comment-continue) " ")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (dino-set-alist-entry comment-styles                                                                      ;;
  ;;                       'multi-line                                                                         ;;
  ;;                       (list t nil nil t "One 'block' comment for all lines, end on last commented line")) ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

(add-hook 'sgml-mode-hook 'dino-xml-mode-fn)
(add-hook 'nxml-mode-hook 'dino-xml-mode-fn)

;; not sure why I have to do this again, here.
(add-hook 'nxml-mode-hook
          (lambda () (modify-syntax-entry ?\' ".")))


(add-to-list 'hs-special-modes-alist ;; for hideshow
             '(sgml-mode
               "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
               "-->\\|</[^/>]*[^/]>" ;; regexp for end block
               "<!--"                ;; regexp for comment start. (need this??)
               sgml-skip-tag-forward
               nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to highlight trailing whitespace
;;
(use-package highlight-chars
  :load-path "~/elisp"
  :defer t
  :commands (hc-toggle-highlight-trailing-whitespace)
  :autoload (hc-highlight-trailing-whitespace)
  :config (progn
            (defun dino-enable-highlight-trailing-ws-based-on-extension ()
              "turns on highlighting of trailing whitespace based on file extension"
              (let ((extension (file-name-extension buffer-file-name))
                    (extensions-that-get-highlighting '("md" "css" "java" "js" "go" "py" "ts") ))
                (if (member extension extensions-that-get-highlighting)
                    (hc-highlight-trailing-whitespace))))

            (add-hook 'find-file-hook 'dino-enable-highlight-trailing-ws-based-on-extension)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp mode
;;

(defun dino-elisp-mode-fn ()
  (local-set-key "\r"          #'newline-and-indent)
  (keymap-local-set "C-c C-c"  #'comment-region)
  (keymap-local-set "ESC C-R"  #'indent-region)
  (keymap-local-set "C-c e"    #'eval-buffer)
  (keymap-local-set "C-c C-e"  #'eval-last-sexp) ;; trying something new
  ;; (keymap-local-set "C-c C-e"  #'eval-region)
  (keymap-local-set "C-x C-e"  #'byte-compile-file)

  ;; never convert leading spaces to tabs:
  ;;(make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (hl-line-mode 1)
  (turn-on-auto-revert-mode)
  (display-line-numbers-mode)
  (hc-highlight-trailing-whitespace)
  (if (fboundp 'indent-bars-mode) ;; sometimes it's not pre-installed
      (indent-bars-mode))
  (company-mode)
  (apheleia-mode)
  (flycheck-mode)
  (add-hook 'before-save-hook 'dino-delete-trailing-whitespace nil 'local))

;; add (emacs-lisp-mode . lisp-indent) to apheleia-mode-alist
(add-hook 'emacs-lisp-mode-hook 'dino-elisp-mode-fn)

;; This is for scratch buffer
(add-hook 'lisp-interaction-mode-hook 'dino-elisp-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(defun dino-python-mode-fn ()
  ;;(eglot) ;; never tried this
  ;;(apheleia-mode) ;; never tried this

  (keymap-local-set "ESC C-R" #'indent-region)
  (keymap-local-set "ESC #"   #'dino/indent-buffer)
  (keymap-local-set "C-c C-c" #'comment-region)
  (keymap-local-set "C-c C-d" #'delete-trailing-whitespace)
  ;; python-mode resets \C-c\C-w to  `python-check'.  Silly.
  (keymap-local-set "C-c C-w"  #'compare-windows)

  (set (make-local-variable 'indent-tabs-mode) nil)
  (display-line-numbers-mode)
  (hc-highlight-trailing-whitespace)

  ;; Use autopair for curlies, parens, square brackets.
  ;; electric-pair-mode works better than autopair.el in 24.4,
  ;; and is important for use with popup / auto-complete.
  (if (or (not (fboundp 'version<)) (version< emacs-version "24.4"))
      (progn (require 'autopair) (autopair-mode))
    (electric-pair-mode))

  ;; ya-snippet
  (yas-minor-mode-on)
  (show-paren-mode 1))

(add-hook 'python-ts-mode-hook 'dino-python-mode-fn)
(add-hook 'python-mode-hook 'dino-python-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Macro counters
;;
;; see more on http://www.emacswiki.org/emacs/EmacsKeyboardMacroCounter
(defun init-macro-counter-default ()
  "Set the initial counter to 1 and reset every time it's called.
To set to a different value call `kmacro-set-counter' interactively
i.e M-x kmacro-set-counter."
  (interactive)
  (kmacro-set-counter 1))

(keymap-global-set "<f5>" #'init-macro-counter-default)
(keymap-global-set "<f6>" #'kmacro-insert-counter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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



(defun dino-posframe-swap-background (str)
  "HACK 20230627 the posframe package exposes an emacs bug, I
think. After displaying a frame, as with the information from an
LSP server showing the function definition, the background and
foreground colors for the frame can ... change.  Even while the
frame is still displayed. The background can sometimes revert to the
default background, which is black. Used with a dark foreground, the
signature definition can be unreadable.

You'd think just displaying a new frame would solve it but
posframe caches the old frame and checks the frame params for new
frames against the cached one. I guess for performance. Anyway
the result is, once the bg color is munged, it stays that way.

The documented way to override the fg
and bg for `lsp-signature-posframe' is to set the fg and bg
properties on the `lsp-signature-posframe' face. Eg

(set-face-attribute 'lsp-signature-posframe nil :background \"lightyellow\") .

This should directly affect the cached posframe, but because of the bug,
somehow it does not.

This function swaps between two similar bg colors, to prevent
posframe from caching the frame, which then... allows the frames
to display properly. It swaps only when the input str is blank,
which happens when the help is to disappear. That makes the face
color ready for next time.
"
  (if (not str)
      (let ((cur-bg (face-attribute 'lsp-signature-posframe :background nil t)))
        (set-face-attribute 'lsp-signature-posframe nil :background
                            (if (string= "LightSteelBlue1" cur-bg)
                                "SlateGray1" "LightSteelBlue1")))))
(eval-after-load "lsp"
  '(progn
     ;; I think this might help avoid a frame bug?
     (plist-put lsp-signature-posframe-params :font "Menlo")
     (advice-add 'lsp-signature-posframe :before #'dino-posframe-swap-background)))

;;    (set-face-attribute 'lsp-signature-posframe nil :background "LightSteelBlue1" )

;;     (advice-add 'lsp-signature-posframe :before #'dino-posframe-swap-background)
;;     (advice-remove 'lsp-signature-posframe #'dino-posframe-swap-background)



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript - js-mode (everything old is new again)


(when (boundp 'apheleia-mode-alist)
  (push '(js-mode . prettier-javascript) apheleia-mode-alist))

(defun dino-js-mode-fn ()
  ;; https://stackoverflow.com/a/15239704/48082
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'js-indent-level) 2)

  ;; (add-hook 'font-lock-extend-region-functions
  ;;           'js-fixup-font-lock-extend-region)

  (modify-syntax-entry ?_ "w")
  ;;(turn-on-font-lock) ;; 20250524-1336 not sure this is still necessary with eglot

  (keymap-local-set "ESC C-R"    #'indent-region)
  (keymap-local-set "ESC #"      #'dino/indent-buffer)
  (keymap-local-set "C-c C-c"    #'comment-region)
  (keymap-local-unset "C-c C-k")
  (keymap-local-set "C-c C-k s"  #'dino/camel-to-snakecase-word-at-point)
  (keymap-local-set "C-c C-k c"  #'dino/snake-to-camelcase-word-at-point)
  (keymap-local-set "<TAB>"      #'js-indent-line)
  (keymap-local-set "C-<TAB>"    #'yas-expand)

  ;; Dino 20210127-1259 - trying to diagnose flycheck checker errors
  ;;
  ;; ;; json-mode is a child mode of js-mode. Select different checker
  ;; ;; based on the file extension.

  ;; (if (and buffer-file-name
  ;;          (file-name-directory buffer-file-name))
  ;;      (progn
  ;;        (flycheck-mode)
  ;;        (flycheck-select-checker
  ;;         (if (string-suffix-p ".json" buffer-file-name)
  ;;             'json-jsonlint
  ;;           'javascript-jshint))))

  ;;(flycheck-select-checker 'javascript-eslint) ;; for more control?
  ;;
  ;; Tuesday,  2 January 2018, 15:36
  ;; I tried eslint for emacs and found that it complained a lot about
  ;; the indent style I prefer. Also I could not figure out how to get it to
  ;; stop complaining. So I didn't use it, and still use jshint, which
  ;; seems to work just fine.

  (yas-minor-mode-on)

  ;; always delete trailing whitespace
  (add-hook 'before-save-hook
            (lambda ()
              ;; javascript-mode is an alias for js-mode. Not sure which is required.
              (when (or (eq major-mode 'js-mode) (eq major-mode 'javascript-mode))
                (save-excursion
                  (delete-trailing-whitespace)))
              nil 'local))

  ;;
  ;; eglot by default uses typescript-language-server as the server.
  ;; Examine `eglot-server-programs' to discover this.
  ;; To get this program:
  ;;   npm install -g typescript-language-server typescript
  ;;
  ;; Completions via Company from Eglot just work. Enable both company-mode and
  ;; eglot in the buffer, and completions should work automatically.

  (hs-minor-mode t)
  (company-mode)
  (eglot-ensure)
  (apheleia-mode)
  (electric-pair-mode)
  (electric-operator-mode)
  (display-line-numbers-mode)
  (indent-bars-mode)

  (setq apheleia-remote-algorithm 'local)
  (setq apheleia-log-debug-info t)
  )

(add-hook 'js-mode-hook   'dino-js-mode-fn)

;; for {jshint, jslint, flycheck javascript-jshint} to work,
;; the path  ust have been previously set correctly.


;; 20241215-0000
;; I think I probably no longer need these fixups, with all the
;; improvements in js-mode over the years. Let's try.
;; (require 'js-mode-fixups)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'aspx-mode)
(autoload  'aspx-mode "aspx-mode" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java

(when (boundp 'apheleia-formatters)
  (require 'dcjava)
  (let ((gformat-command (dcjava-gformat-command
                          (concat (getenv "HOME") "/bin"))))
    (if gformat-command
        ;; change the existing google-java-format in the builtin aphelia-formatters
        (setf (alist-get 'google-java-format apheleia-formatters)
              `,(split-string gformat-command " +")))))

(defun dino-java-mode-fn ()
  (if c-buffer-is-cc-mode
      (c-set-style "myJavaStyle"))
  (turn-on-font-lock)
  (keymap-local-set "ESC C-R" #'indent-region)
  (keymap-local-set "ESC #"   #'dino/indent-buffer)

  (modify-syntax-entry ?_ "w")

  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'c-basic-offset) 2)

  ;; 20191015-1837 - better than autopair or skeleton pair
  (electric-pair-mode)

  ;; 20230718-1235 - disabled
  ;; 20250215-1848 - I think this should work now? haven't tried it.
  (if (not (eq system-type 'windows-nt))
      (apheleia-mode))

  (eval-after-load "smarter-compile"
    '(progn
       (add-to-list
        'smart-compile-compile-command-in-comments-extension-list
        ".java")))

  ;; some of my own java-mode helpers
  (require 'dcjava)
  (keymap-local-set "C-c i"    #'dcjava-auto-add-import)
  (keymap-local-set "C-c C-i"  #'dcjava-auto-add-import)
  (keymap-local-set "C-c p"    #'dcjava-insert-inferred-package-name)
  (keymap-local-set "C-c C-l"  #'dcjava-learn-new-import)
  (keymap-local-set "C-c C-f"  #'dcjava-find-wacapps-java-source-for-class-at-point)
  (keymap-local-set "C-c C-r"  #'dcjava-reload-classlist)
  (keymap-local-set "C-c C-s"  #'dcjava-sort-import-statements)

  ;; 20241230 With apheleia-mode, the manual google-java-format is unnecessary. Apheleia does
  ;; the work every time the file is saved.  But it may be that Apheleia is turned off, so having
  ;; the ability to manually invoke google-java-format is still a good idea.
  (keymap-local-set "C-c C-g f"  #'dcjava-gformat-buffer)

  (dino-enable-delete-trailing-whitespace)
  (display-line-numbers-mode)
  )

(add-hook 'java-mode-hook 'dino-java-mode-fn)
;;(remove-hook 'java-ts-mode-hook 'dino-java-mode-fn)

;; 20230828-1658
;; treesitter mode for Java with emacs 29.x.
;; It provides better, more reliable syntax analysis, and better highlighting.
;;
;; As a one-time setup thing, need to do this:
;;   M-x treesit-install-language-grammar RET java
;; to build the language grammar. This requires a compiler.
;; See: https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html
;;
;; Some changes: the indent is no longer based on c-basic-offset.
;; It is `java-ts-mode-indent-offset'.  So we need a different mode hook.
;; For some reason, the java-ts-mode is no longer a cc-mode, so
;; calling `c-set-style' will result in an error.  So I needed to change that
;; in the java-mode hook, to check whether the mode is a cc-mode or not.
;;
;; You can try M-x load-library RET treesit-fold
;; and then  treesit-fold-toggle  to expand/collapse blocks.
;;
(defun dino-java-ts-mode-fn ()
  (setq java-ts-mode-indent-offset 2)
  (dino-java-mode-fn)
  )

(add-hook 'java-ts-mode-hook #'dino-java-ts-mode-fn)


(c-add-style "myJavaStyle"
             '("Java"  ; this must be defined elsewhere - it is in cc-modes.el
               (c-basic-offset . 2)
               (c-echo-syntactic-information-p . t)
               (c-comment-only-line-offset . (0 . 0))
               (c-offsets-alist . (
                                   (c                     . c-lineup-C-comments)
                                   (statement-case-open   . 0)
                                   (case-label            . +)
                                   (substatement-open     . 0)
                                   ))
               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; proselint

(eval-after-load "flycheck"
  '(progn
     (flycheck-define-checker proselint
       "A linter for prose."
       :command ("proselint" source-inplace)
       :error-patterns
       ((warning line-start (file-name) ":" line ":" column ": "
                 (id (one-or-more (not (any " "))))
                 (message (one-or-more not-newline)
                          (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                 line-end))
       :modes (text-mode markdown-mode gfm-mode))
     (add-to-list 'flycheck-checkers 'proselint)
     ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile stuff

(use-package smarter-compile
  :defer t
  :config
  (progn
    (setq smart-compile-alist
          (append
           '(
             ("\\.go\\'"      . "go build %f")
             ("\\.txt\\'"      . "proselint %f")
             ("\\.wxs\\'"      . "%M %n.msi")
             ("\\.css\\'"      . "~/js/csslint.node.js %f")
             ("\\.js\\'"       . "~/js/jshint.node.js %f")
             ) smart-compile-alist ))
    ))

(eval-after-load "compile"
  '(progn
     (setq compilation-scroll-output "first-error")
     ;;(setq-default compile-command (concat nmake.exe " "))
     (setq-default compile-command "make ")
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; salted files
(use-package salted
  :defer t
  :load-path "~/elisp"
  :commands (salted-encrypt-buffer-to-new-file salted-file-mode)
  :config
  (setq salted--salt-file-utility
        (dino/find-executable-in-paths
         (if (eq system-type 'windows-nt)
             "salt_file.exe"
           "salt_file")
         '("~/bin" "~/go/src/github.com/DinoChiesa/salted"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github GraphQL mode
(autoload 'gh-graphql-mode "gh-graphql")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restclient - invoke REST API calls from within Emacs

(use-package restclient
  :defer t
  :load-path "~/elisp"
  ;; autoloads work only for modules published via elpa.
  ;; use :commands to get this effect for locally-sourced modules.
  :commands (restclient-mode)
  :config
  (progn
    (require 'dino-netrc)
    (defun dpc--around-restclient-http-do (orig-fn &rest args)
      "make emacs be less chatty when sending requests"
      (let (url-mime-charset-string url-user-agent url-extensions-header)
        (apply orig-fn args)))
    (advice-add 'restclient-http-do :around #'dpc--around-restclient-http-do)

    (if (not (fboundp 'json-pretty-print-buffer))
        (defun json-pretty-print-buffer ()
          (json-prettify-buffer)))

    (eval-after-load "url"
      '(progn
         (defun dino-url-http-cleaner-request (old-function &rest arguments)
           "make url-http be less chatty when sending requests"
           (let (url-mime-charset-string
                 url-extensions-header
                 (url-user-agent "User-Agent: emacs/url-http.el\r\n"))
             (apply old-function arguments)))
         ;; to disable at runtime:
         ;; (ad-disable-advice 'url-http-create-request 'around 'dino-url-eliminate-giant-useless-header)
         (advice-add #'url-http-create-request :around #'dino-url-http-cleaner-request)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl mode
;;
;; I don't often use perl these days.

(defalias 'perl-mode 'cperl-mode)

(use-package cperl-mode
  ;;:ensure t
  :defer t
  :init
  :mode ("\\.pl\\'" . cperl-mode)
  :config
  (progn
    (defconst my-cperl-style
      '( ("MyPerl"
          (cperl-indent-level               .  0)
          (cperl-brace-offset               .  2)
          (cperl-continued-statement-offset .  2)
          (cperl-continued-brace-offset     . -2)
          (cperl-label-offset               . -2)
          (cperl-close-paren-offset         . -1))))

    (defun cperl-mode-hook-fn ()
      "My hook for perl mode"
      (set-variable 'c-indent-level 0)      ; required for c-outline
      (turn-on-font-lock)
      (set (make-local-variable 'cperl-style-alist) 'my-cperl-style))

    (add-hook 'cperl-mode-hook 'cperl-mode-hook-fn)

    (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'ediff)
(autoload 'ediff-buffers "ediff" nil t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun dino-ediff-buffer-against-file (file)
  "diff the current [edited] buffer and the file of the same name"
  (interactive
   (list (ediff-read-file-name
          "File to compare:" default-directory buffer-file-name)))
  (let ((buf-buf-name (buffer-name))
        (file-buf-name (create-file-buffer file)))
    (with-current-buffer file-buf-name
      (insert-file-contents file t nil nil t))
    (ediff-buffers buf-buf-name file-buf-name)))



;; (defun diff (old new &optional switches no-async)
;;   "Find and display the differences between OLD and NEW files.
;; Interactively the current buffer's file name is the default for NEW
;; and a backup file for NEW is the default for OLD.
;; If NO-ASYNC is non-nil, call diff synchronously.
;; With prefix arg, prompt for diff switches."
;;   (interactive
;;    (let (oldf newf)
;;      (setq newf (buffer-file-name)
;;         newf (if (and newf (file-exists-p newf))
;;                  (read-file-name
;;                   (concat "Diff new file (default "
;;                           (file-name-nondirectory newf) "): ")
;;                   nil newf t)
;;                (read-file-name "Diff new file: " nil nil t)))
;;      (setq oldf (file-newest-backup newf)
;;         oldf (if (and oldf (file-exists-p oldf))
;;                  (read-file-name
;;                   (concat "Diff original file (default "
;;                           (file-name-nondirectory oldf) "): ")
;;                   (file-name-directory oldf) oldf t)
;;                (read-file-name "Diff original file: "
;;                                (file-name-directory newf) nil t)))
;;      (list oldf newf (diff-switches))))
;;   (setq new (expand-file-name new)
;;      old (expand-file-name old))
;;   (or switches (setq switches diff-switches)) ; If not specified, use default.
;;   (let* ((old-alt (file-local-copy old))
;;      (new-alt (file-local-copy new))
;;       (command
;;        (mapconcat 'identity
;;                   `(,diff-command
;;                     ;; Use explicitly specified switches
;;                     ,@(if (listp switches) switches (list switches))
;;                     ,@(if (or old-alt new-alt)
;;                           (list "-L" old "-L" new))
;;                     ,(shell-quote-argument (or old-alt old))
;;                     ,(shell-quote-argument (or new-alt new)))
;;                   " "))
;;       (buf (get-buffer-create "*Diff*"))
;;       (thisdir default-directory)
;;       proc)
;;     (save-excursion
;;       (display-buffer buf)
;;       (set-buffer buf)
;;       (setq buffer-read-only nil)
;;       (buffer-disable-undo (current-buffer))
;;       (let ((inhibit-read-only t))
;;      (erase-buffer))
;;       (buffer-enable-undo (current-buffer))
;;       (diff-mode)
;;       (set (make-local-variable 'revert-buffer-function)
;;         `(lambda (ignore-auto noconfirm)
;;            (diff ',old ',new ',switches ',no-async)))
;;       (set (make-local-variable 'diff-old-temp-file) old-alt)
;;       (set (make-local-variable 'diff-new-temp-file) new-alt)
;;       (setq default-directory thisdir)
;;       (let ((inhibit-read-only t))
;;      (insert command "\n"))
;;       (if (and (not no-async) (fboundp 'start-process))
;;        (progn
;;          (setq proc (start-process "Diff" buf shell-file-name
;;                                    shell-command-switch command))
;;          (set-process-filter proc 'diff-process-filter)
;;          (set-process-sentinel
;;           proc (lambda (proc msg)
;;                  (with-current-buffer (process-buffer proc)
;;                    (diff-sentinel (process-exit-status proc))))))
;;      ;; Async processes aren't available.
;;      (let ((inhibit-read-only t))
;;        (diff-sentinel
;;         (call-process shell-file-name nil buf nil
;;                       shell-command-switch command)))))
;;     buf))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random stuff
;;

;; The following function (scarfed off an emacs bboard) will allow you to
;; see what emacs is seeing when you press any key.
(defun dino-see-chars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
        (inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
            quit-flag nil))         ; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lorem
  :defer t
  :load-path "~/elisp"
  :ensure nil
  :commands (lorem-ipsum))

;;08.04.2003: Kai Großjohann
(defun increment-number-at-point (amount)
  "Increment number at point by given AMOUNT."
  (interactive "NIncrement by: ")
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (old-num (number-at-point)))
    (unless old-num
      (error "No number at point"))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "%d" (+ old-num amount)))))

(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((totals '())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test 'equal)))
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                     total-buffers (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 20241116-0108 - trying to reduce touches, when using
;; the chromebook as client and opening tramp connections.
;;
;; This stuff helped _a little_, but in the end it was still way too klunky
;; a UX to be constantly touching the gnubby key, and waiting for things to
;; timeout, and so on.  So now I ssh to the remote, and run emacs there in a terminal.
;;
;; As a result I think all of the below is unnecessary now.

;;  go/corpssh-faq#fewer-touches
(defun dino-tramp-setup ()
  "This function tries to set some things for tramp."
  ;; https://www.gnu.org/software/tramp/#Using-ssh-connection-sharing-1
  (setq tramp-use-connection-share nil)

  ;; ;; 20241229-0343
  ;; ;; despite the above, which I think is supposed to tell emacs to use ~/.ssh/config ,
  ;; ;; emacs/tramp uses ControlPersist=no. Not sure why.
  ;; ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Ssh-setup.html
  ;; ;;
  ;; ;; In theory I could try to over-ride that behavior with this incantation:
  ;; (setq tramp-ssh-controlmaster-options
  ;;       (concat
  ;;        "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
  ;;        "-o ControlMaster=auto -o ControlPersist=yes"))
  ;;
  ;; ;; BTW, in emacs 30.1,  'tramp-use-ssh-controlmaster-options' was renamed to 'tramp-use-connection-share'.

  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp)
   'remote-direct-async-process)

  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-shell-setup.html
  (connection-local-set-profile-variables
   'remote-bash
   ;; I am not sure how many of the following I need.
   ;; I really just want compilation-start to use bash
   '((shell-file-name . "/bin/bash")
     (tramp-direct-async-process . t)
     (tramp-default-remote-shell . "/bin/bash")
     (tramp-encoding-shell . "/bin/bash")
     (shell-command-switch . "-c")
     (shell-interactive-switch . "-i")
     (shell-login-switch . "-l")
     (compilation-shell-name . "/bin/bash")
     ))

  ;; 20250218-0244
  ;; ;
  ;; I'm commenting these next 2 sections out.  I upgraded tramp today, and
  ;; suddenly tramp is starting up, even when I am not using tramp
  ;; or opening a buffer over tramp. WTF Tramp?

  ;; (connection-local-set-profiles
  ;;  '(:application tramp :protocol "ssh" :machine "cloudtop")
  ;;  'remote-bash)

  ;; (connection-local-set-profiles
  ;;  '(:application tramp :protocol "ssh" :machine "dpchiesa.c.googlers.com")
  ;;  'remote-bash)

  ;; tell tramp to FRICKING USE MY PATH on the remote machine, why would it do otherwise?
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; For error messages in local compilation buffers, generated by running a
  ;; compile "remotely"(over tramp), tell emacs to resolve the paths to the
  ;; tramp filespecs. This assumes /usr/local/google/home is always cloudtop.
  ;; https://emacs.stackexchange.com/a/40582/3856
  (setq directory-abbrev-alist '(("^/usr/local/google/home" . "/ssh:cloudtop:/usr/local/google/home")))

  ;; If this isn't set, Tramp will always prompt for shell on M-x shell (which i do not often use)
  (customize-set-variable 'explicit-shell-file-name "/bin/bash"))

;; 20250218-0244 - suddenly tramp is starting up. I hate tramp sometimes.
;;(eval-after-load "tramp" '(dino-tramp-setup))

;; also for tramp
(defun dino-vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))
(add-hook 'find-file-hook 'dino-vc-off-if-remote)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; url
;;
;; ;; If you want emacs to use the system proxy, you need to do
;; ;; this:
;; ;;; set up the proxy
;; (setq url-using-proxy t)
;; (setq url-proxy-services
;; '(("http" . "localhost:8888")))
;;
;; To make this happen automatically, read the registry before
;; each URL retrieval, and set the proxy appropriately.
;;
;;(if (eq system-type 'windows-nt)
;;    (eval-after-load "url"
;;      '(progn
;;         (require 'w32-registry)
;;         (defadvice url-http-create-request (before
;;                                             dino-set-proxy-dynamically
;;                                             activate)
;;          "Before retrieving a URL, query the IE Proxy settings, and use them."
;;           (let ((proxy (w32reg-get-ie-proxy-config)))
;;             (setq url-using-proxy proxy
;;                   url-proxy-services proxy))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ------------------------------------------------------------------
;; recentf - to open recent files.
;; Near the bottom there is a key binding for `recentf-open'.
;;
;; ------------------------------------------------------------------
(require 'recentf)
(recentf-mode 1)

;; ;; I think I created this to get completion when opening recent files.
;; ;; But with icomplete-vertical, `recentf-open' offers completion without this.
;; (defun dino-recentf-open-files-compl ()
;;   (interactive)
;;   (let* ((all-files recentf-list)
;;          (tocpl (mapcar (function
;;                          (lambda (x) (cons (file-name-nondirectory x) x)))
;;                         all-files))
;;          (prompt (append '("File name: ") tocpl))
;;          (fname (completing-read (car prompt) (cdr prompt) nil nil)))
;;     (find-file (cdr (assoc-string fname tocpl)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timestamp
;;
;; put this in a file in the first N lines to get an auto-updated timestamp:
;;  Last Saved: <>
;;
;; I use a custom pattern to allow a bunch of variations on how the
;; timestamp appears.
;;

(setq
 time-stamp-active t          ; do enable time-stamps
 ;; time-stamp-line-limit 34     ; check first N buffer lines for Time-stamp: <>
 ;; example: Tuesday, July 15, 2008  10:59:09  (by dinoch)
 ;;time-stamp-format "%:a, %:b %02d, %Y  %02H:%02M:%02S %Z (by %u)") ; date format
 ;;time-stamp-format "%Y-%:b-%02d %02H:%02M:%02S" ; date format
 time-stamp-pattern "34/\\(\\(L\\|l\\)ast\\( \\|-\\)\\(\\(S\\|s\\)aved\\|\\(M\\|m\\)odified\\|\\(U\\|u\\)pdated\\)\\|Time-stamp\\) *: <%Y-%:b-%02d %02H:%02M:%02S>")

;; can also add this to source code: // (set-variable time-stamp-format "%Y-%:b-%02d %02H:%02M:%02S")

(add-hook 'before-save-hook 'time-stamp)  ; update time stamps when saving



;; 20250508-1705
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; server
;; ;;
;; (require 'server)
;; ;; see https://mina86.com/2021/emacs-remote/
;; ;; if using server / daemon
;; (setq server-port 17687
;;       server-use-tcp t) ;; nil means use socket, not port (server-port is ignored)
;; ;; If using server-port, tell emacsclient to find it there.
;; ;;   alias e='emacsclient -f ~/.emacs.d/server/server -t -a ""'
;; ;; Otherwise, emacsclient will look in /tmp/emacsSOMETHING for the socket.
;;
;; (if (not (eq t (server-running-p server-name)))
;;     (server-start)
;;   (if (not server-use-tcp)
;;       (message "server socket: %s" (expand-file-name server-name server-socket-dir)
;;                #'external-debugging-output)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline things

(setq line-move-visual t    ;; ??
      line-number-mode t    ;; modeline
      column-number-mode t) ;; modeline

;; do I want this to be disabled?
(put 'narrow-to-region 'disabled nil)


;; auto-revert for all files.
(add-hook 'find-file-hook
          (lambda () (turn-on-auto-revert-mode)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; google things
;; (add-to-list 'load-path "~/elisp/site-lisp/emacs-google-config/devtools/editors/emacs")
;; ;; (load-file
;; ;;  "/usr/share/emacs/site-lisp/emacs-google-config/devtools/editors/emacs/google.el")
;; (require 'google-java-format)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings that apply for all modes

;; To change the coding system of a visited file,
;; `C-x RET r utf-8-with-signature RET'.
;;
;; Try  M-x list-coding-systems   ... to see a list
;;
(if (boundp 'utf-8-auto)
    (prefer-coding-system 'utf-8-auto)) ;; unicode

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings

;; To unbind/ remove unwanted bindings?
;; (keymap-global-unset (kbd "C-;") t) ;; for some reason this didn't work; (kbd "C-;") is no good
;;
;; ;; (keymap-global-unset [?\C-\;] t) ;; also no
;; ;; (key-valid-p (kbd "C-;")) ;; says those key things are invalid. Not keys.  WTF emacs.
;; ;;(keymap-global-unset [67108923] 1) ;; also did not work
;;
;; (keymap-global-unset (kbd "C-;") 1) ;; not a valid key definition. so frustrating.
;; (keymap-global-set (kbd "C-;") 'embark-collect 1) ;; works. gawd emacs you suck.
;;
;; (keymap-set global-map (kbd "C-;") nil) ;; gemini suggested, does not work.
;;
;; (define-key global-map (kbd "C-;") nil) ;; works to unset a key

(keymap-global-set "C-x 7"       #'dino-toggle-frame-split )
(keymap-global-set "C-x C-r"     #'recentf-open)
(keymap-global-set "C-x C-b"     #'ibuffer) ; instead of buffer-list
(keymap-global-set "C-x |"       #'align-regexp)
(keymap-global-set "C-x ?"       #'describe-text-properties)
(keymap-global-set "C-x w"       #'dino-fixup-linefeeds)
;;(keymap-global-set "C-c g"       #'httpget)
(keymap-global-set "C-c u"       #'dino-insert-uuid)
(keymap-global-set "C-c f"       #'dino-insert-filename)
(keymap-global-set "C-c C-l"     #'lorem-ipsum)
(keymap-global-set "C-c b"       #'dino-base64-insert-file)
(keymap-global-set "C-c 1"       #'just-one-space)
(keymap-global-set "C-c s"       #'search-forward-regexp)
(keymap-global-set "C-c y"       #'display-line-numbers-mode)
(keymap-global-set "C-c q"       #'query-replace)
(keymap-global-set "C-c c"       #'goto-char)
(keymap-global-set "C-c r"       #'replace-regexp)
(keymap-global-set "C-x t"       #'dino-insert-timeofday)
(keymap-global-set "C-x C-d"     #'delete-window)
(keymap-global-set "C-x x"       #'copy-to-register)
(keymap-global-set "C-x g"       #'insert-register)
(keymap-global-set "C-x p"       #'previous-window)
(keymap-global-set "C-x C-p"     #'previous-window)
(keymap-global-set "C-x n"       #'other-window)
(keymap-global-set "C-c w"       #'where-is)
(keymap-global-set "C-c C-w"     #'compare-windows)
(keymap-global-set "C-c ~"       #'revert-buffer-unconditionally)
(keymap-global-set "C-x ~"       #'dino-toggle-buffer-modified)
(keymap-global-set "C-x C-g"     #'auto-fill-mode)
(keymap-global-set "C-x C-e"     #'smarter-compile)
(keymap-global-set "C-x E"       #'smarter-compile-run)
(keymap-global-set "C-x e"       #'kmacro-end-and-call-macro)
(keymap-global-set "ESC g"       #'goto-line)
(keymap-global-set "ESC C-y"     #'yank-pop)
(keymap-global-set "ESC C-h"     #'backward-kill-word)
(keymap-global-set "C-x C-n"     #'next-error)
(keymap-global-set "ESC SPC"     #'set-mark-command)
(keymap-global-set "C-c k"       #'keymap-global-set)
(keymap-global-set "C-x d"       #'dino-ediff-buffer-against-file)
(keymap-global-set "C-x &"       #'dino-encode-uri-component-in-region)
(keymap-global-set "C-<"         #'beginning-of-defun)
(keymap-global-set "C->"         #'end-of-defun)
(keymap-global-set "C-c C-x C-c" #'calendar)
(keymap-global-set "ESC C-\\"    #'help-for-help)
(keymap-global-set "C-c C-d"     #'delete-trailing-whitespace)


(define-key prog-mode-map (kbd "C-c d")   #'chatgpt-shell-describe-code)
(define-key prog-mode-map (kbd "C-c C-c") #'comment-region)


;; unicode helpers
;; C-x 8 RET to get prompted for the code point in hex.
(define-key key-translation-map (kbd "\C-x 8 i") (kbd "∞")) ;; infinity - 221E
(define-key key-translation-map (kbd "\C-x 8 y") (kbd "λ")) ;; lambda - 03BB
(define-key key-translation-map (kbd "\C-x 8 a") (kbd "α")) ;; alpha - 03B1
(define-key key-translation-map (kbd "\C-x 8 b") (kbd "β")) ;; beta - 03B2
(define-key key-translation-map (kbd "\C-x 8 d") (kbd "δ")) ;; delta - 03B4
(define-key key-translation-map (kbd "\C-x 8 m") (kbd "µ")) ;; mu / micro - 00B5 / 03BC
(define-key key-translation-map (kbd "\C-x 8 e") (kbd "ε")) ;; epsilon - 03B5
(define-key key-translation-map (kbd "\C-x 8 p") (kbd "π")) ;; pi - 03C0



(message "Done with emacs.el...")
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
