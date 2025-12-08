;;; emacs.el -- Dino's .emacs setup file.   -*- coding: utf-8; lexical-binding: t;  -*-

;;
;;
;; Works with v30.1+ of emacs.
;;

;;; Commentary:

;;; Code:
(message "Running emacs.el...")
(require 'cl-seq)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-splash-screen t)
(setq initial-scratch-message ";;;  -*- lexical-binding: t; -*-\n;; scratch buffer\n")


;; 20251011-1123
;; On Windows
;; when using `install-package'
;; Failed to verify signature archive-contents.sig:
;; gpg: keyblock resource '/c/users/dpchi/c:/users/dpchi/.emacs.d/elpa/gnupg/pubring.kbx': No such file or directory
;;
;; Pay attention: that path is mangled.
;; https://www.reddit.com/r/emacs/comments/ymzw78/windows_elpa_gnupg_and_keys_problems/
;;
;; Setting package-gnupghome-dir to a "WSL" type form, seems to solve it.
;; eg "/c/users/dpchi/.emacs.d/elpa/gnupg/"
;;
;; Separately, there was an update from Stephen Monnier to the
;; gnu-elpa-keyring-update package (v2025.10.1 from 2025-Oct-09).
;; There may have been a problem that was more general. In any case, my tests
;; subsequent to that, shows that I still need this, so for now it's staying.
;;
;; TODO: (maybe) consolidate all the Windows-specific stuff into one section.
(if (eq system-type 'windows-nt)
    (setopt package-gnupghome-dir
            (concat
             (replace-regexp-in-string "c:/" "/c/"
                                       (replace-regexp-in-string "\\\\" "/" (getenv "HOME")))
             "/.emacs.d/elpa/gnupg")))


;; (concat
;;  (replace-regexp-in-string "\\\\" "/" (getenv "HOME")) "/.emacs.d/elpa/gnupg")

;; To make sure unicode chars are retained in this file.
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)

;; 20250406 This needs to be system-wide. Setting it in a mode hook is too late,
;; as the file local vars will have already been evaluated and set.
(setq enable-local-variables t)
(setq tty-menu-open-use-tmm t) ;; 20250717-0619 - F10 will invoke `tmm-menubar'
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
(setq xterm-max-cut-length 262144)

;; 20250610-0852 - split windows ??? sensibly?
;;
;; I'm not sure what changed, but lately windows have been splitting into much
;; smaller sizes; I'm trying to avoid that with these settings.
(setq split-height-threshold nil split-width-threshold nil)

(setq truncate-partial-width-windows nil)
(setq auto-save-interval 500)
(setq case-fold-search nil)
(setq comment-empty-lines t)
(setq python-shell-interpreter "python") ;; not python3
(setq-default show-trailing-whitespace t)
(setq-default fill-column 80)

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
;; my own various unorganized utility functions
;;
(use-package dino-utility
  :load-path "~/elisp"
  :pin manual
  :commands (dino/camel-to-snakecase-word-at-point
             dino/snake-to-camelcase-word-at-point
             dino/indent-buffer
             dino/indent-line-to-current-column
             dino/shfmt-buffer)
  :autoload (dino/insert-or-modify-alist-entry
             dino/maybe-add-to-exec-path
             dino/find-latest-nvm-version-bin-dir
             dino/setup-shmode-for-apheleia
             dino/find-executable-in-paths)
  :config
  (add-hook 'before-save-hook 'dino/untabify-maybe))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timestamp insertion functions
;;
(use-package dino-timestamp
  :load-path "~/elisp"
  :defer t
  :pin manual
  :commands (dts/insert-currentTimeMillis dts/insert-timeofday))

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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; To make sure we transmit kill to clipboard, and yank from clipboard.
;; ;; This does not work unless I have an X-server enabled!
;; (use-package xclip
;;   :if (not (display-graphic-p))
;;   :ensure t
;;   :config (xclip-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the path correctly on MacOS, based on /etc/paths.d .
;; I am unsure whether this helps on linux or Windows, I've never
;; tested it. I have my own dino/maybe-add-to-exec-path that
;; seems to work for me, see below.
(use-package path-helper
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (path-helper-setenv "PATH"))

;; 20241122-1947 - various tools and packages - apheleia, csslint, magit,
;; csharpier, shfmt, aider and more - need exec-path AND/or environment PATH to be set.
;; Any nodejs tool installed via "npm i -g" (eg ) should be on the path already.
(dino/maybe-add-to-exec-path
 (let ((home-dir (replace-regexp-in-string "\\\\" "/" (getenv "HOME"))))
   (list
    "c:/Program Files/Git/usr/bin"     ;; lots of unix utilities here for various purposes
    (dino/find-latest-nvm-version-bin-dir)
    (concat home-dir "/.dotnet/tools") ;; csharpier
    (concat home-dir "/bin")
    (concat home-dir "/.local/bin")    ;; aider
    (concat home-dir "/.fzf/bin")      ;; fzf, though I have never gotten this to work with shell-command
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
;; disparate diff.exe tools available for Windows typically. apheleia depends
;; on the --rcs option for diff, which is not always supported.  The solution is
;; to ensure that the right gnu diff.exe is available. Git ships a set of unix utilities
;; including diff; putting c:\program files\Git\usr\bin at the top of `exec-path' allows
;; apheleia to work. This is done just above. Separately, we need the
;; appropriate apheleia formatter to be configured and available.
;;

(use-package apheleia
  :ensure t
  :defer t
  :config (progn
            (require 'apheleia-log)
            (require 'cl-seq)
            (setq apheleia-log-debug-info t)

            (if (eq system-type 'windows-nt)
                (cl-dolist (item apheleia-formatters)
                  (when (and (consp (cdr item)) (equal "apheleia-npx" (cadr item)))
                    (setf (cadr item) "npx.ps1"))))

            ;; 20251025-0909
            ;; Was having trouble with apheleia + prettier causing copyright symbols
            ;; and other unicode chars to be converted to ASCII, specifically on Windows.
            ;; It turns out my powershell defaults for text encoding in the shell were
            ;; the culprit.  In npx.ps1, which is delivered by nvm?, this command:
            ;;   `$input | & node.exe "$basedir/node_modules/npm/bin/npx-cli.js" $args`
            ;; ...specifically the piped input, converts © to ┬⌐ .
            ;; Modifying that file to do
            ;;   $OutputEncoding = [System.Text.Encoding]::UTF8
            ;;   [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
            ;; before running the piped input to node.exe, avoids the problem.
            ;; See also npx.ps1.txt here in this directory.  If I ever upgrade nvm,
            ;; I will probably need to re-apply these changes.

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

(use-package company
  ;; COMPlete-ANYthing.
  :defer 31
  :config
  (setq company-idle-delay 0.4)
  ;; Not sure why, or if, we need both "TAB" and "<tab>".
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") (lambda () (interactive) (company-complete-common-or-cycle -1))))

(use-package company-box
  ;; front-end for company, for presentation. Adds features like icons for
  ;; different completion candidates, better color highlighting, and the ability
  ;; to display documentation for functions and variables.

  :defer t
  :after (company)
  :hook (company-mode . company-box-mode))

(use-package indent-bars
  ;; helpful for yaml and json and etc.
  :defer 19
  :ensure t )

(use-package x509-mode
  ;; View certs, CRLs, & related, in emacs. Relies on the
  ;; openssl utility. Untested on Windows as yet.
  :defer 36
  :ensure t)

(use-package lazy-revert
  ;; A layer on top of auto-revert-mode, It reverts only those buffers that are
  ;; currently _displayed_. For all others, flag them and revert when they get
  ;; displayed. Performs better when there are 00's of buffers.
  :ensure nil
  :vc (:url "https://github.com/yilin-zhang/lazy-revert"
       :rev :newest
       :main-file "lazy-revert.el"
       :branch "master")
  :hook (after-init . lazy-revert-mode)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil ; use polling, not inotify
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list ".")))

(use-package uniline
  ;; easily make line drawings with unicode symbols.
  :defer t)

(if ;;(or (not (eq system-type 'windows-nt))
    (version< emacs-version "30.1")
    (use-package fzf
      ;; fuzzy find in emacs, fast way to open files deep in a tree
      :defer t
      :commands (fzf fzf-find-file)
      :bind (("C-x j" . fzf));; or should I use fzf-find-file? not sure.
      :config (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
                    fzf/executable "fzf"
                    fzf/git-grep-args "-i --line-number %s"
                    ;; command used for `fzf-grep-*` functions
                    fzf/grep-command "rg --no-heading -nH"
                    ;; fzf/grep-command "grep -nrH")
                    ))
  ;; fzf-johnc-updated depends on something first made available in 30.1.
  (require 'fzf-johnc-updated))

(defun dino-rego-mode-fn ()
  (display-line-numbers-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
  (setq tab-width 4
        standard-indent 2
        indent-tabs-mode t) ;; opa fmt uses tabs, euuhhh.
  (when (and (boundp 'apheleia-formatters)
             (alist-get 'rego-mode apheleia-mode-alist))
    (apheleia-mode)))

(use-package rego-mode
  ;; OPA configuration language. As of 20250312-0217,
  ;; rego-mode on MELPA is out of date and unmaintained.
  :if (file-exists-p "~/elisp/rego-mode.el")
  :load-path "~/elisp"
  ;;:init
  :hook dino-rego-mode-fn
  :pin manual
  :defer t
  :commands (rego-repl-show rego-mode)
  :config
  (add-hook 'rego-mode-hook #'dino-rego-mode-fn)
  (when (boundp 'apheleia-formatters)
    (when (not (alist-get 'opa-fmt apheleia-formatters))
      (push '(opa-fmt . ("opa" "fmt"))
            apheleia-formatters))
    (when (not (alist-get 'rego-mode apheleia-mode-alist))
      (push '(rego-mode . opa-fmt) apheleia-mode-alist))))


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

(use-package dpc-sane-sorting
  ;; helpers that make icomplete-vertical sort sanely
  :load-path "~/elisp"
  :pin manual )

(use-package icomplete
  ;; as of emacs 28, icomplete-vertical is a feature of the builtin icomplete-mode.
  :demand t
  :requires (dpc-sane-sorting dino-utility)
  ;; Gemini says the  order of evaluation is :custom, then :init, then :config.
  :custom
  ;; For info: C-h v completion-styles-alist
  (completion-styles '(flex partial-completion substring)) ;; flex initials basic
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (max-mini-window-height 0.3)
  (icomplete-prospects-height 15)
  (icomplete-show-matches-on-no-input t)

  :config
  (icomplete-mode)
  (icomplete-vertical-mode)

  ;; Fixup the categories for a few things.
  ;;
  ;; `completion-category-overrides' is a list of category-specific user overrides
  ;; for completion metadata.  Each override has the shape (CATEGORY . ALIST)
  ;; where ALIST is an association list that can specify properties to givern sort
  ;; order, filtering, cycle size, and more.
  ;;
  ;; The way it works: The caller of completing-read designates the category by
  ;; dynamically binding the special variable `completion-category' before calling
  ;; completing-read.
  ;;
  ;; I tried modifying the variable in a dolist, but it was not satisfactory.
  ;;
  ;; NB: magit does use `completing-read' but does not use `completion-category'.
  ;; So there is no way to affect the sort order of various selections. For example
  ;; when selecting a branch, the candidates are sorted by length. (headslap)
  (setq completion-category-overrides
        (dino/insert-or-modify-alist-entry
         completion-category-overrides
         'buffer
         `((styles  . (initials flex)) (cycle . 10))))

  ;; For M-x command
  (setq completion-category-overrides
        (dino/insert-or-modify-alist-entry
         completion-category-overrides
         'command
         `((styles . (substring))
           (cycle-sort-function . ,#'dpc-ss-sort-alpha-exact-or-starts-with-first))))

  ;; For describe-function or describe-variable, use `symbol-help'
  (setq completion-category-overrides
        (dino/insert-or-modify-alist-entry
         completion-category-overrides
         'symbol-help
         `((styles . (substring))
           (cycle-sort-function . ,#'dpc-ss-sort-alpha-exact-or-starts-with-first))))

  ;; not sure _when_ this is used, or even if it is needed rather than `symbol-help'
  (setq completion-category-overrides
        (dino/insert-or-modify-alist-entry
         completion-category-overrides
         'symbol
         `((styles . (basic shorthand substring))) ))

  ;; For M-x find-file
  (setq completion-category-overrides
        (dino/insert-or-modify-alist-entry
         completion-category-overrides
         'file
         `((styles . (basic substring))
           (cycle-sort-function . ,#'dpc-ss-sort-files)
           (cycle . 10))))
  (add-to-list 'completion-ignored-extensions "#")
  (add-to-list 'completion-ignored-extensions "~")

  ;; ;; 20251020-1243 magit stuff - this needs to be sorted out.
  ;; ;; 1. Define a category just for branches
  ;; (setq completion-category-overrides
  ;;       (dino/insert-or-modify-alist-entry
  ;;        completion-category-overrides
  ;;        'magit-branch
  ;;        `((cycle-sort-function . ,#'dpc-ss-sort-alpha-exact-first))))

  ;; ;; 2. Define the advice function
  ;; (defun dino-magit-branch-checkout-advice (orig-fun &rest args)
  ;;   "Wrap `magit-branch-checkout' to set a custom category for its completer."
  ;;   (let ((completion-category 'magit-branch))
  ;;     (apply orig-fun args)))

  ;; ;; 3. Add the advice to the specific command
  ;; (advice-add 'magit-branch-checkout :around #'dino-magit-branch-checkout-advice)

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
         ;; toggle - I usually don't want this when it happens. (by mistake)
         ("C-v"       . icomplete-vertical-mode)

         ;; ;; I installed & enabled embark, but I never began using it. (shrug)
         ;; ("C-c ,"     . embark-act)
         ;; ("C-x"       . embark-export) ;; temporarily in the minibuffer
         ;; ("C-c ;"     . embark-collect)
         )
  )

(use-package marginalia
  ;; A complement to icomplete-vertical-mode. Displays annotations adjacent to
  ;; completions in the minibuffer.  For commands, it displays the docstring and
  ;; key binding if any. For files, it displays the mode and last modified
  ;; time. For symbols, docstring.
  :ensure t
  ;; `maginalia-cycle' cycles through different annotations available for a
  ;; particular completion category.  So let's bind this command to a key
  ;; sequence specifically for use in the minibuffer.  To make a binding in the
  ;; *Completions* buffer, add it to the `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :config
  ;; Enable the mode right away. This forces loading the package.
  (marginalia-mode))

(use-package electric-operator
  ;; for smart insertion of ++ and == and += etc, replaces smart-op.
  :defer 8
  :ensure t)

(use-package wsd-mode
  ;; fontifications for editing websequence diagrams
  :defer 13
  :ensure t)

(use-package magit
  :ensure t
  :defer 24
  :config
  ;; On a machine with no git installed, an obscure error occurs at runtime.
  ;; This check attempts to clarify the problem. If an error does get flagged,
  ;; the fix is to install git! And make sure it is on the path.
  (if (not (boundp 'magit-git-executable))
      (error "variable 'magit-git-executable' is not bound")
    (if (not magit-git-executable)
        (error "variable 'magit-git-executable' is not set"))
    (if (file-exists-p (executable-find magit-git-executable))
        (message "found git at %s" magit-git-executable)
      (error (format "git executable (%s) cannot be found" magit-git-executable)))
    )

  ;; Override some magit things so that branches get sorted
  ;; alphabetically. This works only because I am _redefining
  ;; some functions from magit-base.el !
  ;; See https://github.com/magit/magit/discussions/5390
  (require 'dpc-sane-sorting)
  (setq completion-category-overrides
        (dino/insert-or-modify-alist-entry
         completion-category-overrides
         'magit
         `((styles . (substring))
           (cycle-sort-function . ,#'dpc-ss-sort-alpha))))

  ;; from magit-base.el
  (defun magit--completion-table (collection)
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (category . 'magit))
        (complete-with-action action collection string pred))))

  ;; from magit-base.el
  (defun magit-builtin-completing-read
      (prompt choices &optional predicate require-match initial-input hist def)
    "Magit wrapper for standard `completing-read' function."
    (unless (or (bound-and-true-p helm-mode)
                (bound-and-true-p ivy-mode))
      (setq choices (magit--completion-table choices)))
    (let ((ivy-sort-functions-alist nil))
      (completing-read prompt
                       choices
                       predicate require-match
                       initial-input hist def)))

  )

(use-package flycheck
  :ensure t
  :defer 23
  :config (progn
            ;;(add-hook 'after-init-hook #'global-flycheck-mode)
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            ))

;; ;; for diagnosing flycheck checks (any mode)
;; (defun dpc/flycheck-command-logger (lst)
;;   "logs the LST, then returns the LST"
;;   (message "FLYCHECK check: %s" (prin1-to-string lst))
;;   lst)
;; (setq flycheck-command-wrapper-function #'dpc/flycheck-command-logger)

;; To reset the wrapper fn to the default (identity), do this:
;; (setq flycheck-command-wrapper-function #'identity)

(defun dino-start-eglot-unless-remote ()
  "Start eglot unless file is remote."
  (unless (file-remote-p default-directory)
    (eglot-ensure)))

(use-package eglot
  :defer t
  :demand t
  :commands (eglot eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c e a" . eglot-code-actions)
         ("C-c e o" . eglot-code-actions-organize-imports)
         ("C-c e r" . eglot-rename)
         ("C-c e f" . eglot-format)
         ("C-c e s" . eglot-shutdown)
         ("C-c e C-s" . eglot-shutdown-all)
         ("C-c e r" . eglot-reconnect)
         ("C-c e c" . company-capf)
         )
  ;;:hook (csharp-mode . dino-start-eglot-unless-remote)
  :config  (setq flymake-show-diagnostics-at-end-of-line t)

  ;; disable eglot, specifically for json-mode
  (setq eglot-server-programs
        (cl-remove-if
         (lambda (association)
           (let ((key (car association)))
             (if (listp key)
                 (memq 'json-mode key)
               (eq 'json-mode key))))
         eglot-server-programs))

  ;; NOTE: you still must invoke M-x eglot to start the server. or `eglot-ensure'
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
    (define-key jsonnet-mode-map (kbd "C-<tab>") #'company-complete)

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

  (define-key jsonnet-mode-map (kbd "C-c C-e") #'jsonnet-eval-buffer)
  (define-key jsonnet-mode-map (kbd "C-c C-b") #'browse-url))

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
                   `(jsonnet-mode dpc/jsonnet-lsp))))

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


(use-package jsonnet-mode
  ;; NB: the requires keyword is the same as :if - it does not load dependencies!
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

(use-package expand-region
  ;; Tips from https://www.youtube.com/watch?v=p3Te_a-AGqM
  ;; for marking ever-larger regions iteratively. Used rarely.
  :defer t
  ;; TODO: fix this keymap binding; it conflicts with the font resize key binding.
  :config (define-key global-map (kbd "C-=") 'er/expand-region))

(use-package multiple-cursors
  ;; For visually editing similar things with one key sequence.
  ;; I use this rarely and my fingers don't remember the bindings. But it's neat.
  ;; Helpful in wgrep mode!
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

(use-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config (keymap-set grep-mode-map "C-c C-p" #'wgrep-change-to-wgrep-mode))

(use-package apigee
  :if (file-exists-p "~/elisp/apigee/apigee.el")
  :load-path "~/elisp/apigee"
  :defer t
  :commands
  (apigee-new-proxy apigee-deploy-proxy apigee-import-and-deploy-proxy
                    apigee-lint-asset apigee-validate-xml-for-asset
                    apigee-add-policy apigee-add-target
                    apigee-inject-proxy-revision-logic)

  :bind (("C-c a i" . apigee-import-and-deploy-proxy)
         ("C-c a v" . apigee-validate-xml-for-asset)
         ("C-c a l" . apigee-lint-asset))

  :config
  (progn
    (if-let* ((candidate-dir (expand-file-name "~/newdev/apigee-schema-inference/dist"))
              (schema-dir (file-name-concat candidate-dir "schemas" ))
              (_ (file-directory-p schema-dir)))
        (setq apigee-xmlschema-validator-home candidate-dir))

    (let ((apigeecli-path
           (or
            (when (file-exists-p (expand-file-name "~/.apigeecli/bin/apigeecli"))
              (expand-file-name "~/.apigeecli/bin/apigeecli"))
            (executable-find "apigeecli"))))
      (if apigeecli-path
          (setf (alist-get 'apigeecli apigee-programs-alist) apigeecli-path)
        (message "Cannot find apigeecli")))

    (let* ((gcloud-cmd "gcloud")
           (found-gcloud (executable-find gcloud-cmd)))
      (if (not found-gcloud)
          (error "cannot find gcloud")
        (setf (alist-get 'gcloud apigee-programs-alist)
              found-gcloud)))

    (let* ((apigeelint-candidate-paths
            '("~/apigeelint/cli.js"
              "~/dev/apigeelint/cli.js"
              "~/a/dev/apigeelint/cli.js"))
           (found-apigeelint-path nil)
           (paths-to-check  apigeelint-candidate-paths))
      (while (and (not found-apigeelint-path) paths-to-check)
        (let* ((current-path (car paths-to-check))
               (expanded-path (expand-file-name current-path)))
          (when (file-exists-p expanded-path)
            (setq found-apigeelint-path expanded-path)))
        (setq paths-to-check (cdr paths-to-check))) ;; Move to the next path

      (if found-apigeelint-path
          (setf (alist-get 'apigeelint apigee-programs-alist)
                (format "node %s" found-apigeelint-path))
        (message "cannot find apigeelint-cli.js in any of the candidate paths: %s"
                 (mapcar #'expand-file-name apigeelint-candidate-paths))))

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
  (hs-minor-mode) ;; show: C-c @ C-s  hide: C-c @ C-h
  (dino/setup-shmode-for-apheleia)
  ;; probably the keymap is sh-mode-map, but there might be a ts in there.
  (define-key (current-local-map) (kbd "C-c C-g")  #'dino/shfmt-buffer)
  (define-key (current-local-map) (kbd "C-c C-c")  #'comment-region))

(add-hook 'sh-mode-hook 'dino-sh-mode-fn)

(defun dino-conf-toml-mode-fn ()
  (display-line-numbers-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local) )

(add-hook 'conf-toml-mode-hook #'dino-conf-toml-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang
;;

(defun dino-go-mode-fn ()
  ;; 20250310-2243
  ;; Not sure, but this does not appear to get executed with go-mode or go-ts-mode
  ;;(setq-default)
  (setq tab-width 2
        standard-indent 2
        indent-tabs-mode t) ;; golang prefers tabs, wow, ugh

  (let ((local-map (current-local-map))) ;; go-mode-map or go-ts-mode-map
    (when local-map
      (mapc (lambda (binding)
              (define-key local-map (kbd (car binding)) (cdr binding)))
            '(("ESC C-R" . indent-region)
              ("ESC #"   . dino/indent-buffer)
              ("C-c C-w" . compare-windows)
              ("C-c C-c" . comment-region)
              ("C-c C-d" . delete-trailing-whitespace)
              ))))

  (eval-after-load "smarter-compile"
    '(progn
       (add-to-list
        'smart-compile-compile-command-in-comments-extension-list
        ".go")))

  (eval-after-load "flycheck"
    '(progn
       (add-to-list
        'flycheck-disabled-checkers 'go-build))) ;; go-gofmt?

  ;; 20230918-1015
  (when (boundp 'apheleia-formatters)
    (apheleia-mode)
    (setq apheleia-log-debug-info t)
    (setq apheleia-remote-algorithm 'local))

  ;; still need this? 20241203-0215
  (require 'goflycheck)
  (flycheck-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local) )

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


(use-package org
  :defer t
  :config (org-babel-do-load-languages
           'org-babel-load-languages
           '(
             (shell . t)
             (python . t)
             (perl . t)
             ))
  (add-hook 'org-mode-hook #'org-babel-result-hide-all)
  (defun dpc-org-confirm-babel-evaluate (lang _body)
    (not  ; don't ask for any of the following languages
     (or
      (string= lang "sh")
      (string= lang "bash")
      (string= lang "shell")
      )))
  (setq org-confirm-babel-evaluate 'dpc-org-confirm-babel-evaluate))


(use-package yasnippet
  ;; Expanding snippets for any mode.
  ;; I'm pretty sure that if I use `auto-complete-config', I must load yasnippet first,
  ;; because the former appends some things to yasnippet.
  :defer 13
  :ensure t
  :defines (yas-snippet-dirs yas-prompt-functions)
  :functions (yas-global-mode yas-expand-snippet)
  :config
  (setq yas-snippet-dirs (list "~/elisp/yasnippets"))
  (yas-global-mode 1)
  ;; set prompt functions as appropriate with text vs graphical emacs
  (setq yas-prompt-functions
        (if (display-graphic-p)
            '(yas-x-prompt yas-dropdown-prompt yas-completing-prompt)
          '(yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt)))

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
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web
;;
(defun dino-web-mode-fn ()
  "My hook for web mode"
  (turn-on-font-lock)
  (display-line-numbers-mode)
  ;; why I have to re-set this key is baffling to me.
  ;; and this does not seem to work...
  (define-key web-mode-map (kbd "ESC C-R")  #'indent-region)
  (auto-fill-mode -1))

(use-package web-mode
  :defer t
  :hook (web-mode . dino-web-mode-fn))


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
  (display-line-numbers-mode)
  (define-key markdown-mode-map (kbd "C-c m s") #'dpc-markdown-standalone)
  (define-key markdown-mode-map (kbd "C-c m d") #'delete-trailing-whitespace)
  (define-key markdown-mode-map (kbd "C-c m |") #'markdown-table-align) ;; my favorite feature
  )

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  :hook (markdown-mode . dino-markdown-mode-fn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(global-hl-line-mode)


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
        (height . 32) (width . 320)
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
            '( (top . 80) (left . 400)
               (height . 54) (width . 212)
               ))
      (set-frame-font "Consolas 14" nil t))
  (progn
    ;; glinux
    (setq initial-frame-alist
          '( (top . 10) (left . 10)
             (height . 42) (width . 148)
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
         ("\\.md\\'"                            . markdown-mode) ;; fyi: there is a markdown-ts-mode
         ("\\.cs\\'"                            . csharp-ts-mode)
         ("\\.asp\\'"                           . html-mode)
         ;;("\\.aspx\\'"                        . html-helper-mode)
         ("\\.aspx\\'"                          . aspx-mode)
         ("\\.ashx\\'"                          . csharp-mode)
         ("\\.ascx\\'"                          . csharp-mode)
         ;;("\\.s?html?\\'"                       . html-mode)
         ("\\.html\\'"                          . html-ts-mode)
         ("\\.htm\\'"                           . html-ts-mode)
         ("\\.md\\'"                            . markdown-mode)
         ("\\.py\\'"                            . python-ts-mode)
         ;;("\\.dart\\'"                          . dart-mode)
         ("\\.el\\'"                            . emacs-lisp-mode)
         ;;("\\.js$"                            . js-mode)
         ;;("\\.gs$"                            . js-mode)             ;; google apps-script
         ("\\.\\(js\\|gs\\|jsi\\)\\'"           . js-ts-mode)
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
;; igist
;;
;; Reads a Personal Access Token from .gitconfig.  Get one at
;; https://github.com/settings/tokens . Give it gist rights.
;;
;; To set the token into the .gitconfig:
;;    git config --global github.gist-access-token VALUE_HERE
;;

(defun dpc-config-igist ()
  (let ((default-directory user-emacs-directory))
    (condition-case nil
        (progn (setq igist-current-user-name
                     (car-safe
                      (process-lines "git"
                                     "config"
                                     "user.name")))
               (setq igist-auth-marker
                     (or (ignore-errors
                           (car-safe (process-lines "git" "config"
                                                    "github.gist-access-token")))
                         igist-auth-marker)))
      (error (message "igist-current-user-name cannot be set")))))

(use-package igist
  :ensure t
  :defer t
  :autoload (igist-list-gists igist-explore-public-gists)
  :config
  (dpc-config-igist))


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

(use-package yaml-mode
  :defer t
  :init (defun dino-yaml-mode-fn ()
          "My hook for YAML mode"
          (interactive)
          (turn-on-font-lock)
          (turn-on-auto-revert-mode)
          (display-line-numbers-mode)
          (yaml-pretty-mode)
          (define-key yaml-mode-map (kbd "C-c C-c")  #'comment-region)
          ;;(make-local-variable 'indent-tabs-mode)
          (setq indent-tabs-mode nil))
  :hook  dino-yaml-mode-fn )

(use-package yaml-pretty-mode
  :defer t
  :load-path "~/elisp"
  :commands (yaml-pretty-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript
;;

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(defun dino-typescript-mode-fn ()
  (turn-on-font-lock)
  (apheleia-mode)
  (setq typescript-indent-level 2)
  (define-key (current-local-map) (kbd "ESC C-R") #'indent-region)
  (define-key (current-local-map) (kbd "C-c C-c")  #'comment-region)
  (display-line-numbers-mode)
  (auto-fill-mode -1))

(add-hook 'typescript-mode-hook 'dino-typescript-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powershell
;;
;; There is a powershell package on MELPA, but there's nothing new
;; there, I think. One day I will convert.
;;


(use-package powershell-mode
  :defer t
  :if (file-exists-p "~/elisp/powershell-mode.el")
  :load-path "~/elisp"
  :pin manual
  :init (defun dino-powershell-mode-fn ()
          (electric-pair-mode 1)
          (hs-minor-mode t)
          (display-line-numbers-mode) ;; powershell-mode is not derived from prog-mode
          (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
          (let ((local-map (current-local-map))) ;; powershell-mode-map / powershell-ts-mode-map
            (when local-map
              (mapc (lambda (binding)
                      (define-key local-map (kbd (car binding)) (cdr binding)))
                    '(("ESC C-R" . indent-region)
                      ("ESC #"   . dino/indent-buffer)
                      ("ESC ."   . company-complete)
                      ("C-c C-c" . comment-region)
                      ("C-c C-d" . delete-trailing-whitespace)
                      ("C-c C-w" . compare-windows)
                      ("ESC C-i" . company-capf)
                      ("C-c C-g" . powershell-format-buffer)
                      ))))
          )
  :commands (powershell-mode)
  :hook (powershell-mode . dino-powershell-mode-fn)
  :config
  (add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
  (add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))
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

(use-package treesit-fold
  :ensure t)

(defun dino-html-mode-fn ()
  (keymap-local-set "C-c 1" #'just-one-space)
  (keymap-local-set "<f7>"  #'find-file-at-point)
  (auto-fill-mode -1)
  (apheleia-mode)
  (display-line-numbers-mode)
  (if (and (fboundp 'treesit-parser-list)
           (treesit-parser-list)
           (fboundp 'treesit-fold-mode))
      (progn
        (treesit-fold-mode)
        (keymap-local-set "C-c >"  #'treesit-fold-close)
        (keymap-local-set "C-c <"  #'treesit-fold-open)))

  ;; Block highlights use `show-paren-match', which is
  ;; a steelblue3 background, tooooo much.
  ;;  (make-face-buffer-local 'show-paren-match) ;; no

  (face-remap-add-relative 'show-paren-match '(:background "color-238")))

(if (boundp 'eglot-server-programs)
    (dino/maybe-register-vscode-server-for-eglot 'html-mode))

;; 20250708-1805
;; Fixup treesit-fold .
;;
;; For html-mode, it folds from the END (closing angle bracket) of the opening
;; element to the beginning (opening angle bracket) of the close element.  It
;; SHOULD fold from the END of the opening element name to the END (closing
;; angle bracket) of the closing element.  This includes attributes in the fold.
;; This fixes that up.
;; I filed a PR for this: https://github.com/emacs-tree-sitter/treesit-fold/pull/33

(defun dpc-treesit-fold-range-html-element (node offset)
  "Define fold range for tag in HTML.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."

  ;; (let* ((beg (treesit-node-end (treesit-node-child node 0)))
  ;;        (end-node (treesit-fold-last-child node))
  ;;        (end (treesit-node-start end-node)))
  ;;   (treesit-fold--cons-add (cons beg end) offset)))
  (let* (;; beg = after element name that follows open angle bracket
         (beg (treesit-node-end (treesit-node-child (treesit-node-child node 0) 1)))
         ;; end-node is the closing angle bracket
         (end-node (treesit-node-child (treesit-fold-last-child node) 2))
         (end (treesit-node-start end-node)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun dpc-treesit-fold-parsers-html ()
  "Rule set for HTML."
  '((element . dpc-treesit-fold-range-html-element)
    (comment . (treesit-fold-range-seq 1 -1))))

(defun dpc-fixup-treesit-fold-for-html ()
  "fixup treesit-fold for HTML"
  ;; first remove the old entries:
  (setq treesit-fold-range-alist (assoc-delete-all 'html-ts-mode treesit-fold-range-alist ))
  (setq treesit-fold-range-alist (assoc-delete-all 'html-mode treesit-fold-range-alist ))

  ;; add new entries to the alist
  (add-to-list 'treesit-fold-range-alist
               `(html-ts-mode . ,(dpc-treesit-fold-parsers-html)))
  (add-to-list 'treesit-fold-range-alist
               `(html-mode . ,(dpc-treesit-fold-parsers-html))))

(dpc-fixup-treesit-fold-for-html)
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

;; one-time thing
(dino/maybe-register-vscode-server-for-eglot '(css-mode css-ts-mode))

(use-package css-mode
  :defer t
  :ensure nil
  :init (defun dino-css-mode-fn ()
          "My hook for CSS mode"
          (interactive)
          (setq css-indent-offset 2
                completion-auto-help 'always)
          (let ((local-map (current-local-map))) ;; css-mode-map probably
            (when local-map
              (mapc (lambda (binding)
                      (define-key local-map (kbd (car binding)) (cdr binding)))
                    '(("ESC C-R" . indent-region)
                      ("ESC #"   . dino/indent-buffer)
                      ("C-c C-c" . comment-region)
                      ("C-c C-d" . delete-trailing-whitespace)
                      ("C-c C-w" . compare-windows)
                      ("ESC ."   . company-complete)
                      ("ESC C-i" . company-capf)
                      ))))

          (turn-on-auto-revert-mode)
          (electric-pair-mode)
          (company-mode)
          (apheleia-mode)
          (display-line-numbers-mode)

          (if (and (fboundp 'treesit-parser-list)
                   (treesit-parser-list)
                   (fboundp 'treesit-fold-mode))
              (progn
                (treesit-fold-mode)
                (define-key (current-local-map) (kbd "C-c >")  #'treesit-fold-close)
                (define-key (current-local-map) (kbd "C-c <")  #'treesit-fold-open)))

          ;;(eglot-ensure) ;; maybe I will want this?

          ;; 20250524-1624
          ;; Microsoft produces a CSS language server as part of (VS)Code OSS.  But they do not
          ;; *package it* as an independent executable.  It's packaged only as part of VSCode. There
          ;; have been attempts to extract the CSS Lang server from VSCode and re-package it, for
          ;; example https://github.com/hrsh7th/vscode-langservers-extracted .  The way it works:
          ;; the build script clones the VSCode repo and then tries building just what's necessary I
          ;; guess, for the CSS lang server (and maybe a few others).  but (a) that repo hasn't been
          ;; updated in a while, and (b) the build doesn't work.
          ;;
          ;; There is a relevant repo that is being maintained:
          ;; https://github.com/microsoft/vscode-css-languageservice But that is a
          ;; library, service? not a tool or LSP.  There is no "npm run" script for it.
          ;; I asked Gemini how I could use it, and it advised me run a thing that is
          ;; based on an extraction that was done 7 years ago.
          ;; https://www.npmjs.com/package/vscode-css-languageserver-bin .  If I say I
          ;; don't want that, it advises me to build my own server that packages this
          ;; library. I tried that, see ~/dev/my-css-language-server , it also didn't
          ;; work.  I don't recall what the obstacle was there.
          ;;
          ;; Finally, I resorted to just running the CSS lang server that is packaged in
          ;; the VSCode installation. This works on Windows and Linux. Of course I have
          ;; to install vscode to get it.
          ;;
          ;; I am not sure what specifically I am getting out of the LSP here; tree-sitter
          ;; gives me completions and syntax highlighting. Is there refactoring or other stuff
          ;; the LSP gives me? (shrug)

          ;; (let* ((vscode-server-program
          ;;         (file-name-concat (if (eq system-type 'windows-nt)
          ;;                               (file-name-concat (getenv "HOME")
          ;;                                                 "AppData/Local/Programs/Microsoft VS Code")
          ;;                             "/usr/share/code")
          ;;                           "resources/app/extensions/css-language-features/server/dist/node/cssServerMain.js")))
          ;;   (if (file-exists-p vscode-server-program)
          ;;       (add-to-list 'eglot-server-programs
          ;;                    `((css-mode css-ts-mode)
          ;;                      . ,(list "node" vscode-server-program "--stdio")))
          ;;     ))

          ;;
          ;; To fix if I have broken things:
          ;; (setq eglot-server-programs (assoc-delete-all '(css-mode css-ts-mode) eglot-server-programs ))

          ;; To install the external flycheck checkers:
          ;;
          ;; npm install -g csslint  # not sure needed any longer!
          ;; npm install -g stylelint stylelint-config-standard stylelint-scss
          ;;
          ;; check what you have installed globally:
          ;;   npm ls -g

          (flycheck-mode)
          (flycheck-select-checker
           (if (string= mode-name "SCSS") 'scss-stylelint 'css-stylelint))

          (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)
          (setq indent-tabs-mode nil) )

  :hook ((css-mode css-ts-mode) . dino-css-mode-fn)
  :config
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
    (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON
(defun json-prettify-buffer ()
  "prettifies a json buffer."
  (interactive)
  (save-excursion
    (json-prettify-region (point-min) (point-max))))

;; function alias
(defalias 'json-prettify-region 'json-reformat-region)


(use-package json-mode
  :defer t
  :init (defun dino-json-mode-fn ()
          ;; hierarchy: (prog-mode js-base-mode js-mode javascript-mode json-mode)
          ;;(turn-on-font-lock)
          ;;(flycheck-mode 0)
          ;; 20250111-1538
          ;;
          ;; The default checker is json-python-json, which has a command "python3".
          ;; On my version of windows,
          ;;
          ;; (1) there is no "python3", the executable is just called "python" and it resides in c:\Python313 .
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
          (if (and (fboundp 'treesit-parser-list)
                   (treesit-parser-list)
                   (fboundp 'treesit-fold-mode))
              (progn
                (treesit-fold-mode)
                (define-key (current-local-map) (kbd "C-c >")  #'treesit-fold-close)
                (define-key (current-local-map) (kbd "C-c <")  #'treesit-fold-open)))
          )

  :hook ((json-mode json-ts-mode) . dino-json-mode-fn)
  :config (require 'json-reformat) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-reformat
;;
;; Brilliant for reformatting JSON when embedded within another document (eg markdown.)
;;
(use-package json-reformat
  :defer t
  :load-path "~/elisp"
  :pin manual
  :commands (json-reformat-region) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thesaurus
;; 20241227 - API key is still working
;;
(use-package thesaurus
  :defer t
  :config
  (thesaurus-set-bhl-api-key-from-file "~/elisp/.BigHugeLabs.apikey.txt")
  (define-key global-map (kbd "C-c C-t") #'thesaurus-choose-synonym-and-replace))


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
  ;; the following conflicts with somea few local maps.
  :config (define-key global-map (kbd "C-c C-d")  #'dictionary-lookup-definition)
  (setq dictionary-use-single-buffer t)
  (setq dictionary-server "dict.org")
  (custom-set-faces
   '(dictionary-word-definition-face ((t (:family "Sans Serif"))))
   '(dictionary-button-button        ((t (:family "Sans Serif"))))
   '(dictionary-button-face          ((t (:background "gray11" :foreground "LightSteelBlue")))))
  )


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
  :autoload (dpc-gemini/get-config-property)
  :commands (dpc-gemini/ask-gemini dpc-gemini/select-model)
  :bind (("C-c g a" . dpc-gemini/ask-gemini)
         ("C-c g ?" . dpc-gemini/ask-gemini)
         ("C-c g l" . dpc-gemini/list-models)
         ("C-c g s" . dpc-gemini/select-model))
  :config
  (setq dpc-gemini-properties-file "~/.google-gemini-properties"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gptel
;;
;; A multi-LLM porcelain. Uses transient menu.  See also: chatgpt-shell .  To
;; contrast chatgpt-shell and gptel: the intent of gptel is to be available in
;; any buffer, at any moment, via gptel-send. You don't need a dedicated chat
;; buffer. chatgpt-shell uses a chat buffer.

(defun dpc-gptel-setup ()
  "Invoked when gptel is loaded."
  (setq gptel-backend (gptel-make-gemini "Gemini"
                        :key (dpc-gemini/get-config-property "apikey")
                        :stream t))
  (setq gptel-model 'gemini-flash-latest)
  ;; for transient menu different view?
  ;;  (setq gptel-expert-commands t)

  (require 'cl-seq)

  ;; load my prompts
  (let ((prompt-file (expand-file-name "~/elisp/.gptel-prompts")))
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

  ;; remove the builtin chat directive. (can't remember why)
  (setq gptel-directives (cl-remove-if (lambda (pair)
                                         (and (consp pair) (equal (car pair) 'chat)))
                                       gptel-directives)))


(use-package gptel
  :bind (("C-c C-g s" . #'gptel-send))
  :ensure t
  :commands (gptel-send gptel-rewrite)
  :config (dpc-gptel-setup) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chatgpt-shell
;;
;; A chatbot interface to various LLMs, including Gemini, OpenAI/ChatGPT,
;; Anthropic Claude, DeepSeek Chat, and others via comint in emacs.
;;

;; 20250405-2235
;;
;; NB. In `use-package', The :requires keyword specifies a dependency, but does
;; not force load it as in nodejs require.  Instead, within use-package,
;; requires prevents loading of the package unless the required feature is
;; already loaded.

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
  :hook dino-chatgpt-shell-mode-fn
  :bind (("C-c t" . chatgpt-shell-google-toggle-grounding-with-google-search)
         ("C-c l" . chatgpt-shell-google-load-models)
         ("C-c p" . chatgpt-shell-prompt-compose)
         ("C-c r" . chatgpt-shell-refactor-code)
         ("C-c d" . chatgpt-shell-describe-code)
         ("C-c f" . chatgpt-shell-proofread-region))

  :init
  ;; avoid flycheck warning about the following functions? This seems dumb.
  (declare-function chatgpt-shell-google-toggle-grounding-with-google-search "ext:chatgpt-shell-google")

  (defun dino-chatgpt-shell-mode-fn ()
    "this used to have keybindings, now it is empty."
    t)

  :config
  (require 'dpc-sane-sorting)
  (setq chatgpt-shell-google-key (dpc-gemini/get-config-property "apikey"))
  ;; 20250823-1212 - chatgpt-shell currently has builtin support for latest Gemini models.
  ;; Consider re-adding this when gemini-3.0 is released.
  ;; (chatgpt-shell-google-load-models) ;; CGS does not include complete list of Google models
  (setq chatgpt-shell-model-version (dpc-gemini/get-config-property "default-model"))

  (setq chatgpt-shell-swap-model-selector
        (lambda (candidates)
          (completing-read "Swap to: "
                           (dpc-ss-completion-fn candidates 'sorted-sanely) nil t)))

  :catch
  (lambda (_keyword err)
    (message (format "chatgpt-shell use: %s" (error-message-string err)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time zones
;;
;; ui in emacs to translate timezones
;;
(use-package time-zones
  :vc (:url "https://github.com/xenodium/time-zones"
       :rev :newest
       :main-file "time-zones.el"
       :branch "main"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agent-shell - 20250927-1558
;;
;; On Windows, to make this work, with a patched version of gemini cli, I had to :
;;
;; (setq agent-shell-google-gemini-command '("node"
;;       "c:\\users\\dpchi\\dev\\gemini-cli\\bundle\\gemini.js" "--experimental-acp"))
;;
;; As of v0.9.0, this will be no longer necessary as the fix for
;; handling EOL vs \n was merged into that release of the CLI.
;;

(use-package shell-maker
  :ensure t)
;; With :vc, these get loaded from github; not sure if they are ever updated
;; after initial clone. Doc seems to suggest so.
(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el"
       :rev :newest
       :main-file "acp.el"
       :branch "main"))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell" :rev :newest)

  :config
  (setq acp-logging-enabled t) ;; helpful while it's new / evolving

  ;; Auth via API key
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :api-key
                                                (dpc-gemini/get-config-property "apikey")))
  (setq agent-shell-google-gemini-command
        (if (eq system-type 'windows-nt)
            ;; 20251011-1613 - patched version of gemini-cli
            (let ((patched-geminicli "c:\\users\\dpchi\\dev\\gemini-cli\\bundle\\gemini.js"))
              (if (file-exists-p patched-geminicli)
                  '("node" patched-geminicli "--experimental-acp")
                '("gemini" "--experimental-acp")))
          ;; There is a gcli bug (#7549) that flushes cached creds when --experimental-acp
          ;; is enabled. As a result the user must ALWAYS re-authenticate. On a text
          ;; terminal that causes a hang.  This forcibly unsets the project, and uses API
          ;; key, to allow prompt-free startup.
          `("env" "GOOGLE_CLOUD_PROJECT=\"\"" "gemini" "--experimental-acp")))

  ;; re-define this to trim the noise...
  (defun agent-shell-google--gemini-welcome-message (_config)
    "Return Gemini CLI ASCII art as per own repo using `shell-maker' CONFIG."
    (let ((art (agent-shell--indent-string 4 (agent-shell-google--gemini-ascii-art))))
      (concat "\n\n\n" art "\n\n      Welcome to Gemini within Emacs...\n\n")))

  ;; API Key auth is necessary, at least at the moment.
  ;; What I found: using the `:login t' approach starts Gemini, which then
  ;; shows a URL in stderr, telling the user to login. In a graphical
  ;; environment, the gemini client would just open up a URL in the browser, but
  ;; that doesn't work with my tty.  So gemini-cli waits forever for the paste
  ;; of an authz-code.
  ;;
  ;; This doesn't work; need to use API key auth or a graphical emacs.
  ;;
  ;; In theory the agent-shell could look at stderr and propagate the message to
  ;; "open this URL and paste in the code".  But that's not happening right now.
  ;;
  ;; Diagnosing this required
  ;;    M-x agent-shell-toggle-logging or (setq acp-logging-enabled t)
  ;;
  ;; Then you get buffers like this:
  ;;  *acp-(gemini)-10 log*                  165 Fundamental
  ;;  *acp-(gemini)-10 traffic*               39 ACP-traffic
  ;;
  ;; And in the log buffer you can see gemini cli I/O.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-insert - 20241206-0142
;;
;; builtin to emaccs
(use-package autoinsert
  :defer t
  :config
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
           ) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode

(defun dino-dired-mode-hook-fn ()
  ;;(hl-line-mode 1)
  (define-key dired-mode-map (kbd "C-c C-g") #'dino-dired-kill-new-file-contents)
  (define-key dired-mode-map (kbd "C-c C-c") #'dino-dired-copy-file-to-dir-in-other-window)
  (define-key dired-mode-map (kbd "C-c C-m") #'dino-dired-move-file-to-dir-in-other-window)
  (define-key dired-mode-map (kbd "C-c m")   #'magit-status)
  (define-key dired-mode-map (kbd "C-x m")   #'magit-status)
  ;; converse of i (dired-maybe-insert-subdir)
  (define-key dired-mode-map (kbd "K")  #'dired-kill-subdir)
  (define-key dired-mode-map (kbd "F")  #'dino-dired-do-find)
  (define-key dired-mode-map (kbd "s")  #'dino-dired-sort-cycle)
  (dino-dired-sort-cycle "t") ;; by default, sort by time
  ;;(turn-on-auto-revert-mode) ;; in favor of lazy-revert globally
  )

(use-package dired
  :ensure nil
  :hook (dired-mode . dino-dired-mode-hook-fn)
  :config
  (require 'dino-dired-fixups)
  ;; eliminate the gid in dired when using ls-lisp (eg, on windows)
  (setq ls-lisp-verbosity '(links uid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prog mode - general
;;
;; Q: why is this not just loaded from ~/.emacs.d/abbrev_defs ?
;; Not sure. Maybe it is?

(defun dino-define-global-abbrev-table ()
  "Define a custom global abbrev table. Really these are
just auto-corrects on common mis-spellings by me."

  (define-abbrev-table 'global-abbrev-table
    '(
      ("teh" "the" nil 0)
      ("somehting" "something" nil 0)
      ("deprectaed" "deprecated" nil 0)
      ("APigee" "Apigee" nil 0)
      ("Gmeini" "Gemini" nil 0)
      ("hting" "thing" nil 0)
      ("rigueur" "rigeuer" nil 0)
      ("riguer" "rigeuer" nil 0)
      ("submint" "submit" nil 0)
      ("rwquest" "request" nil 0)
      ("requets" "request" nil 0)
      ("hygeine" "hygiene" nil 0)
      ("laucnhed" "launched" nil 0)
      ("supproted" "supported" nil 0)
      ("comittee" "committee" nil 0)
      ("machien" "machine" nil 0)
      ("machiens" "machines" nil 0)
      ("siilar" "similar" nil 0)
      ("cusotmer" "customer" nil 0)
      ("accommplish" "accomplish" nil 0)
      ("accomodate" "accommodate" nil 0)
      ("recieve" "receive" nil 0)
      ("vairous" "various" nil 0)
      ("multipel" "multiple" nil 0)
      ("acheive" "achieve" nil 0)
      ("acheived" "achieved" nil 0)
      ("becasue" "because" nil 0)
      ("btw" "by the way" nil 0)
      ("omw" "on my way" nil 0)
      )
    ))

(dino-define-global-abbrev-table)

(defun dino-prog-mode-hook-fn ()
  (abbrev-mode 1)
  ;;(turn-on-auto-revert-mode) ;; in favor of lazy-revert globally
  (display-line-numbers-mode)
  (setq show-trailing-whitespace t)
  )

(add-hook 'prog-mode-hook 'dino-prog-mode-hook-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode

(defun dino-text-mode-hook-fn ()
  (auto-fill-mode 1)
  (abbrev-mode 1)
  (dino-define-global-abbrev-table)
  (display-line-numbers-mode)
  (define-key text-mode-map (kbd "C-c C-c")    #'center-paragraph)
  (define-key text-mode-map (kbd "C-c i")      #'dino/indent-line-to-current-column)
  (define-key text-mode-map (kbd "C-<insert>") #'uniline-mode)
  (define-key text-mode-map (kbd "C-c m d")    #'delete-trailing-whitespace)
  ;;(variable-pitch-mode)
  ;;
  ;; (require 'refill) ;; automatically refill paragraphs
  ;; (refill-mode 1)
  )

(add-hook 'text-mode-hook 'dino-text-mode-hook-fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C mode  (common)

(require 'dtrt-indent)
(defun dino-c-mode-common-hook-fn ()
  (cond
   (window-system

    ;; RETURN means newline-and-indent in any c-mode buffer
    (local-set-key (kbd "RET") 'newline-and-indent)

    ;; for re-tabbing a region or buffer of code:
    (keymap-local-set "ESC C-R"  #'indent-region)
    (keymap-local-set "ESC #"    #'dino/indent-buffer)

    ;; set this key binding on for c-mode, in lieu of c-subword mode,
    ;; which overwrites my preference
    (keymap-local-set "C-c C-w"  #'compare-windows)

    ;;(hl-line-mode 1)
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
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

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
         (yas-minor-mode)

         ;; for hide/show support
         (hs-minor-mode 1)
         (setq hs-isearch-open t)

         ;; with point inside the block, use these keys to hide/show
         (keymap-local-set "C-c >"  #'hs-hide-block)
         (keymap-local-set "C-c <"  #'hs-show-block)

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
            (defun dino-protobuf-mode-fn ()
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

;; 20250629-1039 - I am not sure, but I think csharpier and eglot
;; kinda solve this formatting thing, and I probably don't need this.
;; Need to test it.
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

         (yas-minor-mode)
         (show-paren-mode 1)
         ;;(hl-line-mode 1)

         (require 'flycheck)
         (flycheck-mode)
         ;;         (flycheck-select-checker 'csharp)

         ;; for hide/show support
         (hs-minor-mode 1)
         (setq hs-isearch-open t)

         ;; with point inside the block, use these keys to hide/show
         (keymap-local-set "C-c >"  #'hs-hide-block)
         (keymap-local-set "C-c <"  #'hs-show-block)

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
         (message "dino-csharp-mode-fn: done.")
         )))

(eval-after-load "csharp-mode"
  '(progn
     (require 'compile)
     (add-hook 'csharp-mode-hook 'dino-csharp-mode-fn 0 t)

     ;; 20250126-1013 - adjustments for indentation.
     ;; Discover these with treesit-explore-mode .
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
  (keymap-local-set "C-x C-e"  #'compile)

  (require 'dccsharp)
  (keymap-local-set "C-c i"    #'dccsharp-auto-add-using)
  (keymap-local-set "C-c e"    #'eglot-code-actions)
  (keymap-local-set "C-c C-s"  #'dccsharp-sort-using-statements)

  ;; 20241229-1756
  (electric-pair-local-mode 1)

  (require 'yasnippet)
  (yas-minor-mode)
  (show-paren-mode 1)
  ;;(hl-line-mode 1)
  (company-mode)

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

  ;; never convert leading spaces to tabs:
  ;; (setting this variable automatically makes it local)
  (setq indent-tabs-mode nil)
  ;; the imenu stuff doesn't perform well; impractical
  (setq csharp-want-imenu nil)

  (message "dino-csharp-ts-mode-fn: done.")

  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  )

(eval-after-load "csharp-mode"
  '(progn
     (require 'compile)
     (add-hook 'csharp-ts-mode-hook 'dino-csharp-ts-mode-fn 0 t)
     ;; 20250223-1328 - this may not be necessary in emacs 30.1; bears further testing.
     ;; (if (fboundp 'apheleia-mode)
     ;;     (add-hook 'apheleia-post-format-hook #'dino-maybe-eglot-reconnect))
     ))

(use-package csharp-ts-mode-navigation
  :defer t
  :after csharp-mode
  :config (add-hook 'csharp-ts-mode-hook 'ctsn/setup))

(defun dino-maybe-eglot-reconnect ()
  "At least in emacs 29.4, when apheleia reformats a C# buffer it seems to
  confuse the language server. This function will force a reconnect in
  that case. This is overkill and slow, but I have not yet figured out a way
  around it."
  (when (and
         (or (eq major-mode 'csharp-mode) (eq major-mode 'csharp-ts-mode))
         (eglot-managed-p))
    (eglot-reconnect (eglot--current-server-or-lose))
    ))

;; 20250702-1535 ? unsure if I need this. I think this is for completing on snippets.
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (add-to-list 'company-backends
                         '(company-capf :with company-yasnippet))))

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
(add-hook 'php-mode-hook 'dino-php-mode-fn 0 t)

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
  ;;(turn-on-auto-revert-mode) ;; in favor of lazy-revert globally
  (hs-minor-mode 1)
  (setq hs-isearch-open t)
  ;; 20250702-2204
  ;;
  ;; The python xmllsp works for files with .xsd schema, but for files with .rnc
  ;; schema, it may be better to just use the nxml builtin capability, and not
  ;; the LSP MSBuild project files are one with good .rnc schema.
  (unless (and buffer-file-name
               (or
                (string-suffix-p ".xsd" buffer-file-name)
                (string-suffix-p ".csproj" buffer-file-name)))
    (eglot-ensure))

  ;; M-C-i to get completion popups, whether from nxml or eglot
  (company-mode)
  (display-line-numbers-mode)
  (setq yas-indent-line 'nil)
  (keymap-local-set "ESC C-R" #'indent-region)
  (keymap-local-set "C-c n"   #'sgml-name-char) ;; inserts entity ref of pressed char
  (keymap-local-set "M-#"     #'dino-xml-pretty-print-buffer)
  (keymap-local-set "C-c f"   #'dino-replace-filename-no-extension)
  (keymap-local-set "ESC C-i" #'company-capf)
  (keymap-local-set "C-<"     #'nxml-backward-element)
  (keymap-local-set "C->"     #'nxml-forward-element)
  (keymap-local-set "C-c C-c" #'dino-xml-comment-region)

  (setq nxml-sexp-element-flag t
        nxml-child-indent 2
        nxml-slash-auto-complete-flag t
        completion-auto-help 'always
        indent-tabs-mode nil)

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

  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  )

(add-hook 'sgml-mode-hook 'dino-xml-mode-fn)
(add-hook 'nxml-mode-hook 'dino-xml-mode-fn)

(defun dpc-xmllsp-init-options (_server)
  "options for my own custom XML LSP, built in python."
  (let* ((home-dir (getenv "HOME") )
         (xsd-cachedir (file-name-concat home-dir "/xml-schema-cache")))
    `(:schemaLocators
      [
       (:rootElement t
        :searchPaths
        [
         ,(file-name-concat home-dir "/newdev/apigee-schema-inference/dist/schema")
         ])
       (:locationHint ,(concat xsd-cachedir "/schema_map.json"))
       (:patterns [(:pattern "*.csproj"
                    ;;:path ,(concat xsd-cachedir "/Microsoft.Build.CommonTypes.xsd")
                    :path ,(file-name-concat xsd-cachedir "/Microsoft.Build.Core.xsd")
                    :useDefaultNamespace t
                    )])
       ]
      )))


(let ((xmllsp-loc (expand-file-name "~/newdev/xmllsp")))
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     `(nxml-mode .
       (,(file-name-concat xmllsp-loc ".venv/bin/python")
        ,(file-name-concat xmllsp-loc "xml_language_server/xmllsp.py")
        "--log-level"
        "INFO"
        :initializationOptions dpc-xmllsp-init-options)))))

;; For fixing up if I've messed up the above:
;;
;; (setq eglot-server-programs (assoc-delete-all 'nxml-mode eglot-server-programs ))
;;
;;
;; (setf
;;  (alist-get 'nxml-mode eglot-server-programs)
;;  `("/usr/local/google/home/dchiesa/newdev/xmllsp/.venv/bin/python"
;;    ,(concat (getenv "HOME")
;;             "/newdev/xmllsp/xml_language_server/xmllsp.py")
;;    :initializationOptions dpc-xmllsp-init-options))



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
  ;;(hl-line-mode 1)
  (if (fboundp 'indent-bars-mode) ;; sometimes it's not pre-installed
      (indent-bars-mode))
  (company-mode)
  (apheleia-mode)
  (flycheck-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace 0 t)
  (require 'elisp-fix-indent)
  (advice-add #'calculate-lisp-indent :override #'efi/calculate-lisp-indent)
  ;;(advice-remove 'calculate-lisp-indent #'efi~calculate-lisp-indent)
  )


;; add (emacs-lisp-mode . lisp-indent) to apheleia-mode-alist
(add-hook 'emacs-lisp-mode-hook 'dino-elisp-mode-fn)

;; for the scratch buffer
(add-hook 'lisp-interaction-mode-hook 'dino-elisp-mode-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(use-package flymake-ruff
  :ensure t)

(defun dino-python-mode-fn ()
  ;; python-mode is not a prog-mode
  ;;(eglot) ;; not sure about this

  (let ((local-map (current-local-map))) ;; python-mode-map or python-ts-mode-map
    (when local-map
      (mapc (lambda (binding)
              (define-key local-map (kbd (car binding)) (cdr binding)))
            '(("ESC C-R" . indent-region)
              ("ESC #"   . dino/indent-buffer)
              ("C-c C-c" . comment-region)
              ("C-c C-d" . delete-trailing-whitespace)
              ;; python-mode resets \C-c\C-w to  `python-check'.  Silly.
              ("C-c C-w" . compare-windows)
              ))))

  (set (make-local-variable 'indent-tabs-mode) nil)
  (apheleia-mode)
  (display-line-numbers-mode)
  (electric-pair-mode)
  (yas-minor-mode)
  (show-paren-mode 1)
  ;; 20251025-1311
  (flymake-mode)
  (flymake-ruff-load)
  ;; python-mode forcibly sets python-check-command to be pyflakes.
  ;; This will reset it.
  (setq python-check-command "ruff")
  (add-hook 'before-save-hook 'delete-trailing-whitespace 0 t) )

(add-hook 'python-ts-mode-hook 'dino-python-mode-fn)
(add-hook 'python-mode-hook 'dino-python-mode-fn)

(with-eval-after-load 'apheleia
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        ;; 20251025-1309
        ;; ruff is a replacement for longtime formatter black
        ;; On Windows: powershell -c "irm https://astral.sh/ruff/install.ps1 | iex" .
        ;; It gets installed to ~\.local\bin\ruff.exe
        '(ruff-isort ruff)))

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

(define-key global-map (kbd "<f5>") #'init-macro-counter-default)
(define-key global-map (kbd "<f6>") #'kmacro-insert-counter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 20251003-1933
;; I think this is no longer needed. Also I use eglot, not lsp,
;; so, it may be irrelevant.
;;
;; (defun dino-posframe-swap-background (str)
;;   "HACK 20230627 the posframe package exposes an emacs bug, I
;; think. After displaying a frame, as with the information from an
;; LSP server showing the function definition, the background and
;; foreground colors for the frame can ... change.  Even while the
;; frame is still displayed. The background can sometimes revert to the
;; default background, which is black. Used with a dark foreground, the
;; signature definition can be unreadable.
;;
;; You'd think just displaying a new frame would solve it but
;; posframe caches the old frame and checks the frame params for new
;; frames against the cached one. I guess for performance. Anyway
;; the result is, once the bg color is munged, it stays that way.
;;
;; The documented way to override the fg
;; and bg for `lsp-signature-posframe' is to set the fg and bg
;; properties on the `lsp-signature-posframe' face. Eg
;;
;; (set-face-attribute 'lsp-signature-posframe nil :background \"lightyellow\") .
;;
;; This should directly affect the cached posframe, but because of the bug,
;; somehow it does not.
;;
;; This function swaps between two similar bg colors, to prevent
;; posframe from caching the frame, which then... allows the frames
;; to display properly. It swaps only when the input str is blank,
;; which happens when the help is to disappear. That makes the face
;; color ready for next time.
;; "
;;   (if (not str)
;;       (let ((cur-bg (face-attribute 'lsp-signature-posframe :background nil t)))
;;         (set-face-attribute 'lsp-signature-posframe nil :background
;;                             (if (string= "LightSteelBlue1" cur-bg)
;;                                 "SlateGray1" "LightSteelBlue1")))))
;; (eval-after-load "lsp"
;;   '(progn
;;      ;; I think this might help avoid a frame bug?
;;      (plist-put lsp-signature-posframe-params :font "Menlo")
;;      (advice-add 'lsp-signature-posframe :before #'dino-posframe-swap-background)))
;;
;; ;;    (set-face-attribute 'lsp-signature-posframe nil :background "LightSteelBlue1" )
;; ;;
;; ;;     (advice-add 'lsp-signature-posframe :before #'dino-posframe-swap-background)
;; ;;     (advice-remove 'lsp-signature-posframe #'dino-posframe-swap-background)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript - js-mode (everything old is new again)

(when (boundp 'apheleia-mode-alist)
  (push '(js-mode . prettier-javascript) apheleia-mode-alist)
  (push '(js-ts-mode . prettier-javascript) apheleia-mode-alist))

(defun dino--maybe-space (&optional arg)
  "Conditionally insert a space based on the preceding text.
If the cursor is immediately following '// ', this function will
do nothing. However, if the previous command was also this one
(i.e., you press SPACE a second time), it will insert a space.
In all other contexts, it behaves like a normal space.
Maybe electric-operator-mode is postfixing a space when i
type // , not sure. But anyway i'd rather it didn't  So this
counteracts that. "
  (interactive "P")
  (if (and (looking-back "// " (- (point) 3))
           (not (eq last-command this-command)))
      nil
    (self-insert-command (or arg 1))))

(defun dino-js-mode-fn ()
  ;; https://stackoverflow.com/a/15239704/48082
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'js-indent-level) 2)

  ;; (add-hook 'font-lock-extend-region-functions
  ;;           'js-fixup-font-lock-extend-region)

  (modify-syntax-entry ?_ "w")

  (keymap-local-unset "C-c C-k")
  (let ((local-map (current-local-map))) ;; js-mode-map or js-ts-mode-map
    (when local-map
      (mapc (lambda (binding)
              (define-key local-map (kbd (car binding)) (cdr binding)))
            '(("ESC C-R" . indent-region)
              ("ESC #"   . dino/indent-buffer)
              ("C-c C-c" . comment-region)
              ("C-c C-d" . delete-trailing-whitespace)
              ("C-c C-w" . compare-windows)
              ("C-c C-k s" . dino/camel-to-snakecase-word-at-point)
              ("C-c C-k c" . dino/snake-to-camelcase-word-at-point)
              ("<TAB>" . js-indent-line)
              ("C-<TAB>" . yas-expand)
              ("SPC" . dino--maybe-space)
              ))))

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


  ;; Cleanup trailing whitespace.
  ;; I forget why I wanted this to be conditional.  Which js- derived
  ;; modes should NOT get the `delete-trailing-whitespace' treatment?
  ;; I dunno.
  (when (memq major-mode '(js-mode javascript-mode json-mode))
    (add-hook 'before-save-hook 'delete-trailing-whitespace 0 t))

  ;;
  ;; eglot by default uses typescript-language-server as the server.
  ;; Examine `eglot-server-programs' to discover this.
  ;; To get this program:
  ;;   npm install -g typescript-language-server typescript
  ;;
  ;; As of July 2025, people say vtsls is a better alternative:
  ;;    npm install -g @vtsls/language-server
  ;;
  ;; ...and then modify the `eglot-server-programs' entry appropriately to
  ;; run vtsls --stdio
  ;;
  ;; Completions via Company from Eglot just work. Enable both company-mode and
  ;; eglot in the buffer, and completions should work automatically.

  (yas-minor-mode)
  (company-mode)

  ;; 20250718-1351
  ;; when running eglot in json-mode, I get all sorts of unhelpful
  ;; tips. json-mode is derivative of js-mode, so I need to not
  ;; use eglot when it's json-mode. For now anyway!
  ;; (unless (derived-mode-p 'json-mode)
  ;;   (eglot-ensure))

  (apheleia-mode)
  (electric-pair-mode)
  (electric-operator-mode)

  ;; Avoid the error seen with vtsls:
  ;; "Request textDocument/onTypeFormatting failed with message: Cannot find provider for onTypeFormatting, the feature is possibly not supported by the current TypeScript version or disabled by settings."
  ;; Will apply only to the current buffer and not affect other buffers.
  (setopt eglot-ignored-server-capabilities (list :documentOnTypeFormattingProvider))

  (if (fboundp 'indent-bars-mode)
      (indent-bars-mode))
  (setq apheleia-remote-algorithm 'local)
  (setq apheleia-log-debug-info t)
  (if (and (fboundp 'treesit-parser-list)
           (treesit-parser-list)
           (fboundp 'treesit-fold-mode))
      (progn
        (treesit-fold-mode)
        (keymap-local-set "C-c >"  #'treesit-fold-close)
        (keymap-local-set "C-c <"  #'treesit-fold-open)))
  )

(add-hook 'js-mode-hook   'dino-js-mode-fn)
(add-hook 'js-ts-mode-hook   'dino-js-mode-fn)

;; supplant the default typescript-language-server
(if (boundp 'eglot-server-programs)
    (add-to-list 'eglot-server-programs
                 `(((js-mode :language-id "javascript")
                    (js-ts-mode :language-id "javascript")
                    (tsx-ts-mode :language-id "typescriptreact")
                    (typescript-ts-mode :language-id "typescript")
                    (typescript-mode :language-id "typescript"))
                   . ("vtsls" "--stdio"))))


;; fix when broken
;; (setq eglot-server-programs (assoc-delete-all '(js-mode js-ts-mode typescript-ts-mode) eglot-server-programs ))
;;
;; (setq eglot-server-programs (assoc-delete-all '((js-mode :language-id "javascript")
;;                                                 (js-ts-mode :language-id "javascript")
;;                                                 (tsx-ts-mode :language-id "typescriptreact")
;;                                                 (typescript-ts-mode :language-id "typescript")
;;                                                 (typescript-mode :language-id "typescript"))
;;                                               eglot-server-programs ))
;;
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
        ;; change the existing google-java-format in the builtin apheleia-formatters
        (setf (alist-get 'google-java-format apheleia-formatters)
              `,(split-string gformat-command " +")))))

(defun dino-java-mode-fn ()
  ;; the following two are unnecessary if using java-ts-mode
  (if c-buffer-is-cc-mode
      (progn
        (c-set-style "myJavaStyle")
        (turn-on-font-lock)))

  (keymap-local-set "ESC C-R" #'indent-region)
  (keymap-local-set "ESC #"   #'dino/indent-buffer)

  (modify-syntax-entry ?_ "w")

  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'c-basic-offset) 2)

  (electric-pair-mode)

  (if (and (fboundp 'treesit-parser-list)
           (treesit-parser-list)
           (fboundp 'treesit-fold-mode))
      (progn
        (treesit-fold-mode)
        (define-key (current-local-map) (kbd "C-c >")  #'treesit-fold-close)
        (define-key (current-local-map) (kbd "C-c <")  #'treesit-fold-open)))

  ;; 20250215-1848 - I think this should work now on Windows? haven't tried it.
  (if (not (eq system-type 'windows-nt))
      (apheleia-mode))

  (eval-after-load "smarter-compile"
    '(progn
       (add-to-list
        'smart-compile-compile-command-in-comments-extension-list
        ".java")))

  ;; some of my own java-mode helpers
  (require 'dcjava)

  (let ((local-map (current-local-map))) ;; python-mode-map or python-ts-mode-map
    (when local-map
      (mapc (lambda (binding)
              (define-key local-map (kbd (car binding)) (cdr binding)))
            '(("C-c i" . dcjava-auto-add-import)
              ("C-c C-i" . dcjava-auto-add-import)
              ("C-c p" . dcjava-insert-inferred-package-name)
              ("C-c C-l" . dcjava-learn-new-import)
              ("C-c C-f" . dcjava-find-wacapps-java-source-for-class-at-point)
              ("C-c C-r" . dcjava-reload-classlist)
              ("C-c C-s" . dcjava-sort-import-statements) ;; obsolete with gformat
              ("C-c C-g f" . dcjava-gformat-buffer)
              ))))


  ;; 20241230 With apheleia-mode, the manual google-java-format is unnecessary. Apheleia does
  ;; the work every time the file is saved.  But it may be that Apheleia is turned off, so having
  ;; the ability to manually invoke google-java-format is still a good idea.

  ;; remove trailing whitespace in C files
  ;; http://stackoverflow.com/questions/1931784
  (add-hook 'before-save-hook 'delete-trailing-whitespace 0 t))


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
;; rfc-mode
;;
;; Search & Complete on titles for IETF RFCs in the minibuffer,
;; and retrieve with a keystroke.

(use-package rfc-mode
  :defer 38
  :commands (rfc-mode-browse)
  :config
  (setq rfc-mode-directory (expand-file-name "~/ietf-rfcs/"))

  (defun dpc/rfc-mode-browse-advice (orig-fn &rest args)
    "Advice for `rfc-mode-browse' to temporarily set `max-mini-window-height'."
    (let ((old-max-mini-window-height max-mini-window-height))
      (unless (file-directory-p rfc-mode-directory)
        (message "Directory '%s' does not exist, creating it..." rfc-mode-directory)
        (make-directory rfc-mode-directory t)       )
      (setq max-mini-window-height 0.8) ; e.g., 80% of frame height
      (unwind-protect
          (apply orig-fn args)
        ;; Restore the original value after the original function completes
        (setq max-mini-window-height old-max-mini-window-height))))

  (advice-add 'rfc-mode-browse :around #'dpc/rfc-mode-browse-advice))


(use-package vdiff
  :defer t
  :config
  (defun dino-vdiff-mode-fn ()
    (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))

  (add-hook 'vdiff-mode-hook 'dino-vdiff-mode-fn))

(defun dino-nginx-mode-fn()
  (electric-pair-mode)
  (display-line-numbers-mode))

(add-hook 'nginx-mode-hook 'dino-nginx-mode-fn)


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

    (defun dino-restclient-mode-fn ()
      (display-line-numbers-mode))
    (add-hook 'restclient-mode-hook #'dino-restclient-mode-fn)

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
;; recentf - to open recent files.
;;
;; The list, `recentf-list', is sorted by «date opened in emacs», not
;; most recently modified. So if I open a file Tuesday and save it
;; intermittently through the week, it won't bubble up in the `recentf-list'.
;;

(use-package recentf
  :defer t
  :commands (recentf-mode recentf-open)
  :bind (( "C-x C-r" . recentf-open))
  :config
  (require 'dpc-sane-sorting)
  (recentf-mode 1)

  ;; Redefine `recentf-open' to use a completion function which means I can
  ;; disable sorting the recent files list, while using sorting for other
  ;; categories.
  (defun recentf-open (file)
    "Prompt for FILE in `recentf-list' and visit it.
Enable `recentf-mode' if it isn't already."
    (interactive
     (list
      (progn (unless recentf-mode (recentf-mode 1))
             (completing-read (format-prompt "Open recent?" nil)
                              (dpc-ss-completion-fn recentf-list 'unsorted)
                              nil t))))
    (when file
      (funcall recentf-menu-action file)))
  )


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


;; 20251012-0900
;; Trying out lazy-revert
;; ;; auto-revert for all files.
;; (add-hook 'find-file-hook
;;           (lambda () (turn-on-auto-revert-mode)))


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

(use-package ibuffer-preview
  :if (and (file-exists-p "~/elisp/ibuffer-preview.el")
           (boundp 'ibuffer-mode-hook) )
  :load-path "~/elisp"
  :config (defun ibpm-set-map()
            (keymap-set ibuffer-mode-map "v" #'ibuffer-preview-mode))
  (add-hook 'ibuffer-mode-hook #'ibpm-set-map))


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

(define-key global-map (kbd "C-x 7")       #'dino-toggle-frame-split)
(define-key global-map (kbd "C-x C-b")     #'ibuffer) ; instead of buffer-list
(define-key global-map (kbd "C-x |")       #'align-regexp)
(define-key global-map (kbd "C-x ?")       #'describe-text-properties)
(define-key global-map (kbd "C-x w")       #'dino-fixup-linefeeds)
(define-key global-map (kbd "C-c u")       #'dino-insert-uuid)
(define-key global-map (kbd "C-c f")       #'dino-insert-filename)
(define-key global-map (kbd "C-c C-l")     #'lorem-ipsum)
(define-key global-map (kbd "C-c b")       #'dino-base64-insert-file)
(define-key global-map (kbd "C-c 1")       #'just-one-space)
(define-key global-map (kbd "C-c s")       #'search-forward-regexp)
(define-key global-map (kbd "C-c y")       #'display-line-numbers-mode)
(define-key global-map (kbd "C-c q")       #'query-replace)
(define-key global-map (kbd "C-c c")       #'goto-char)
(define-key global-map (kbd "C-c r")       #'replace-regexp)
(define-key global-map (kbd "C-x t")       #'dts/insert-timeofday)
(define-key global-map (kbd "C-x C-d")     #'delete-window)
(define-key global-map (kbd "C-x x")       #'copy-to-register)
(define-key global-map (kbd "C-x g")       #'insert-register)
(define-key global-map (kbd "C-x p")       #'previous-window)
(define-key global-map (kbd "C-x C-p")     #'previous-window)
(define-key global-map (kbd "C-x n")       #'other-window)
(define-key global-map (kbd "C-c w")       #'where-is)
(define-key global-map (kbd "C-c C-w")     #'compare-windows)
(define-key global-map (kbd "C-c C-y")     #'dcjava-wacapps-intelligently-open-file)
(define-key global-map (kbd "C-c ~")       #'revert-buffer-unconditionally)
(define-key global-map (kbd "C-x ~")       #'dino/toggle-buffer-modified)
(define-key global-map (kbd "C-x C-g")     #'auto-fill-mode)
(define-key global-map (kbd "C-x C-e")     #'smarter-compile)
(define-key global-map (kbd "C-x E")       #'smarter-compile-run)
(define-key global-map (kbd "C-x e")       #'kmacro-end-and-call-macro)
(define-key global-map (kbd "ESC g")       #'goto-line)
(define-key global-map (kbd "ESC C-y")     #'yank-pop)
(define-key global-map (kbd "ESC C-h")     #'backward-kill-word)
(define-key global-map (kbd "C-x C-n")     #'next-error)
(define-key global-map (kbd "ESC SPC")     #'set-mark-command)
(define-key global-map (kbd "C-c k")       #'keymap-global-set)
(define-key global-map (kbd "C-x d")       #'dino-ediff-buffer-against-file)
(define-key global-map (kbd "C-x &")       #'dino/urlencode-region)
(define-key global-map (kbd "C-<")         #'beginning-of-defun)
(define-key global-map (kbd "C->")         #'end-of-defun)
(define-key global-map (kbd "C-c C-x C-c") #'calendar)
(define-key global-map (kbd "ESC C-\\")    #'help-for-help)
(define-key global-map (kbd "C-c C-d")     #'delete-trailing-whitespace)
(define-key global-map (kbd "C-c j f")     #'dino/json-format-region)


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
