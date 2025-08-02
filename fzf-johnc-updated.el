;;; fzf-johnc-updated.el  -*- coding: utf-8; lexical-binding: t;  -*-

(use-package fzf
  :defer t
  :commands (fzf fzf-find-file)
  :vc (:url "https://github.com/JohnC32/fzf.el" ;; JohnC32 has updates for windows OS
            :branch "master"
            :rev :newest)
  :bind (("C-x j" . fzf));; or should I use fzf-find-file? not sure.
  :config (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
                fzf/executable "fzf"
                fzf/git-grep-args "-i --line-number %s"
                ;; command used for `fzf-grep-*` functions
                fzf/grep-command "rg --no-heading -nH"
                ;; fzf/grep-command "grep -nrH")
                ))

(provide 'fzf-johnc-updated)
