;;; dpc-sane-sorting.el  -*- coding: utf-8; lexical-binding: t;  -*-

;; Copyright (c) 2025 Google LLC
;;
;; Author: Dino Chiesa
;; Version: 20250608
;;

;;; Commentary:

;; Logic to help make icomplete-vertical-mode sort sanely in minibuffer.
;;
;;
;; The default behavior of `icomplete-vertical-mode' when used with `find-file',
;; `execute-extended-command', `recentf-open' and others, is to show the list of
;; candidates sorted first by length of the filename and then
;; alphabetically. The implementation for that is in
;; `completion-all-sorted-completions' which is defined in minibuffer.el; it
;; uses a function called `minibuffer--sort-by-length-alpha' which applies when
;; there is no completion function used with `completiing-read' (eg, no
;; metadata, just a list of candidates), or when there is a completion function
;; but there is no `cycle-sort-function' defined.  I am not sure when this
;; behavior originally became available, but it exists in emacs v29-31, in my
;; experience.  This default behavior seems counter-intuitive and user-hostile
;; to me. It results in a geometrically pleasing presentation, but
;; informationnally, the default sort order seems irrelevant.

;; This module adds two things that help:
;;
;; - several sort functions that can be used for the `cycle-sort-function' in
;;   `completion-category-overrides' for categories like `file', `command', as
;;   well as any custom categories you or your modules may define. This will work
;;   for any use of `completing-read' that uses a category.
;;
;;   For example:
;;     (setq completion-category-overrides
;;       (cons `(file
;;               (styles . (basic substring))
;;               (cycle-sort-function . ,#'dpc-ss-files)
;;               (cycle . 10))
;;           completion-category-overrides))
;;
;; - Several new categories (`unsorted', `sorted-sanely') that you can use
;;   in metadata with completion functions with `completing-read'
;;
;;   For example:
;;     (completing-read
;;       "New model: "
;;       (dpc-ss-completion-fn candidates 'sorted-sanely)
;;       nil t)))
;;
;;   The unsorted category is handy in `recentf-open'. The list of candidates
;;   presented there is already sorted by the most recently opened
;;   time. Therefore there should be no additional sort performed. Therefore:
;;
;;     (defun recentf-open (file)
;;       "Prompt for FILE in `recentf-list' and visit it.
;;     Enable `recentf-mode' if it isn't already."
;;       (interactive
;;        (list
;;         (progn (unless recentf-mode (recentf-mode 1))
;;                (completing-read (format-prompt "Open recent?" nil)
;;                                 (dpc-ss-completion-fn recentf-list 'unsorted)
;;                                 nil t))))
;;       (when file
;;         (funcall recentf-menu-action file)))

;; TODO: add key bindings to the `icomplete-vertical-mode-minibuffer-map'
;; that invoke commands to alter this sorting. Eg, by file mtime, or by
;; filename extension.



;;; Code:


(require 'dino-utility)
(require 'cl-seq) ;; cl-remove-if

;; ==== sorters
(defun dpc-ss-alpha (candidates)
  "Sorts the model CANDIDATE by name"
  (sort candidates :lessp #'string<))

(defun dpc-ss-alpha-exactfirst (candidates)
  "Sort CANDIDATES to place an exact match for the minibuffer
content first. The rest of the candidates are sorted alphabetically.
This corrects the problem in which I type «M-x grep» and the top
candidate is not grep.

This function is suitable for use as a `cycle-sort-function' for
the `command' category in `completion-category-overrides', like so:

  (setq completion-category-overrides
    (cons `(command
            (styles . (basic substring))
            (cycle-sort-function . ,#'dpc-ss-alpha-exactfirst)
            (cycle . 10))
        completion-category-overrides))

See also `dpc-ss-alphaexact-startswith-first' which
is probably better.
"
  (let ((cur-input (minibuffer-contents-no-properties)))
    (let ((exact-match (car (member cur-input candidates))))
      (if exact-match
          (cons exact-match
                (sort (cl-remove-if
                       (lambda (c) (equal c exact-match)) candidates)
                      #'string-lessp))
        (sort candidates #'string-lessp)))))

(defun dpc-ss-alphaexact-startswith-first (candidates)
  "Sort CANDIDATES placing:
  1. An exact match for the minibuffer content first.
  2. Candidates that start with the input, sorted alphabetically.
  3. The remaining candidates, sorted alphabetically.

  This function is suitable for use as a `cycle-sort-function' for
  the `command' category in `completion-category-overrides', like so:

  (setq completion-category-overrides
    (cons `(command
            (styles . (basic substring))
            (cycle-sort-function . ,#'dpc-ss-alphaexact-startswith-first)
            (cycle . 10))
        completion-category-overrides))
"
  (let ((cur-input (minibuffer-contents-no-properties))
        (exact-matches nil)
        (starts-with-matches nil)
        (remaining-candidates nil))

    ;; There is a more memory-efficient way to do this sort in a single pass,
    ;; with a more complex predictae, but it's a little more complicated to
    ;; read. I prefer the readability of this approach, creating multiple
    ;; independent temporary lists.

    ;; 1. Identify and categorize candidates
    (dolist (candidate candidates)
      (cond
       ((string= candidate cur-input)
        (push candidate exact-matches))
       ((string-prefix-p cur-input candidate)
        (push candidate starts-with-matches))
       (t
        (push candidate remaining-candidates))))

    ;; 2. Sort within the relevant groups alphabetically
    (setq starts-with-matches (sort starts-with-matches #'string-lessp))
    (setq remaining-candidates (sort remaining-candidates #'string-lessp))

    ;; 3. Combine and return the results in the specified order
    (nconc exact-matches starts-with-matches remaining-candidates)))


(defun dpc-ss-files (candidates)
  "Sort a list of CANDIDATES alphabetically, except that  \"./\"
always appears at the end, and an exact match of what the user
typed is presented at the top of the list.

This is intended for use as a `cycle-sort-function' for
the `file' category in `completion-category-overrides', like so:

  (setq completion-category-overrides
    (cons `(file
            (styles . (basic substring))
            (cycle-sort-function . ,#'dpc-ss-files)
            (cycle . 10))
        completion-category-overrides))
"
  (let ((cur-input (minibuffer-contents-no-properties)))
    ;;(let ((cur-input "~/foo")) ;; usable for testing
    ;; The cur-input will include (abbreviate-file-name default-directory) as a
    ;; prefix, for example ~/ when find-file is invoked in the home directory,
    ;; or ~/elisp/ if find-file is invoked from within the elisp directory.
    ;; But the candidates will not include that prefix. So this logic gets the
    ;; last segment of what is in the minibuffer and matches against that.
    ;;

    ;;(message "cur-input (%s)" cur-input)
    (let* ((last-segment (file-name-nondirectory cur-input))
           (exact-match (and (not (s-blank? last-segment))
                             (or
                              (car
                               (member last-segment candidates))
                              (cl-find-if
                               (lambda (s) (or
                                            (s-ends-with? last-segment s)
                                            (s-ends-with? (concat last-segment "/") s)))
                               candidates)))))
      ;;(message "last (%s) exact(%s)" last-segment exact-match)

      (let* ((strip-trailing-slash
              (lambda (s)
                (if (string-suffix-p "/" s)
                    (substring s 0 -1)
                  s)))
             (sort-sensibly-with-dot-slash-last
              (lambda (a b)
                (cond
                 ((string= a "./") nil)
                 ((string= b "./") t)
                 ;; If neither is the special "./" name, then sort without
                 ;; concern for any trailing slash.  The slash is added by
                 ;; find-file to candidates if they are directories, even though
                 ;; it is not part of the name. But that trailing slash distorts
                 ;; the sorting if some of the candidates have dashes in their
                 ;; actual names, which comes _before_ slash in sort order.  So
                 ;; the strip-trailing-slash fn sorts without considering
                 ;; any possible trailing slash.
                 (t (string<
                     (funcall strip-trailing-slash a)
                     (funcall strip-trailing-slash b)))))))
        (if exact-match
            ;; Exact match found: place it first, then sort the rest
            (let ((remaining-candidates (cl-remove-if (lambda (c) (equal c exact-match)) candidates)))
              (cons exact-match
                    (sort remaining-candidates sort-sensibly-with-dot-slash-last)))
          ;; No exact match: just sort with "./" last
          (sort candidates sort-sensibly-with-dot-slash-last))))))

(defvar dpc-ss-supported-categories '(sorted-sanely unsorted)
  "List of supported completion categories for `dpc-ss-completion-fn'.")

(defun dpc-ss-completion-fn (candidates category-symbol)
  "Returns a function to be used as the COMPLETIONS parameter in
`completing-read'.

CANDIDATES is a list of strings, the completion candidates.
CATEGORY-SYMBOL is a symbol indicating the completion category, which
influences sorting behavior. It must be a member of
`dpc-ss-supported-categories'.

This allows you to control sort order for that call to `completing-read'.

Without this function:

   (completing-read \"Choose: \" candidates nil t)

The list is shown with the shortest candidate first, the longest
candidate last?  With this function:

   (completing-read
      \"Choose: \"
      (dpc-ss-completion-fn candidates \\'sorted-sanely) nil t)

The list is shown with the items sorted alphabetically.

And here is a concrete example to set the model selector for ChatGPT to
be sorted:

  (setq chatgpt-shell-swap-model-selector
        (lambda (candidates)
          (completing-read
            \"New model: \"
            (dpc-ss-completion-fn candidates \\'sorted-sanely)
            nil t)))"
  (unless (memq category-symbol dpc-ss-supported-categories)
    (error "Invalid category symbol: %s. Supported categories are: %s"
           category-symbol dpc-ss-supported-categories))
  (let ((candidates candidates))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . ,category-symbol))
        (complete-with-action action candidates string pred)))))

;; Define two new categories.
;;
;; The first category, `sorted-sanely', is for sorting of anything... LLM models in the
;; swap-model chooser, candidates in a yasnippet expansion, etc. Anything that needs a sane
;; sort order.  Without an overrides, minibuffer sorts first by length, then alphabetically,
;; which seems insane. With an override, the user (me) can pre-empt that default sorting.
;; Providing a cycle-sort-function in the metadata works for initial display, but does not work
;; when filtering happens.  Sorting with an active filter (like we've typed a few characters)
;; happens correctly only with a category override which specifies a cycle-sort-function, which
;; means to make this work, the existing call of `completing-read' must specify a category. As
;; an example, `find-file' uses a category, so I can use this `completion-category-overrides'
;; trick to modify sorting for it.  Conversely, the completing-read in magit does not specify a
;; category when reading a branch name, so I cannot use the overrides trick to modify sorting
;; for magit purposes.  As an example of using a category, see above `dpc-ss-completion-fn'.
;; To specify the sorting for a category, add the category, in this case `sorted-sanely', to
;; `completion-category-overrides', and the right sorting will happen.
;;
;; In some cases we want no sorting; particularly with recentf.  Even here, without an
;; overrides, minibuffer will apply its weird sort. So we override to say "no sort" with the
;; `unsorted' category. This also requires a change in `recentf-open' to use
;; `dpc-ss-completion-fn' or a similar completions fn that provides a category.
;;

(dolist (entry
         `((sorted-sanely . ((styles . (substring))
                             (cycle-sort-function . ,#'dpc-ss-alpha)))
           (unsorted      . ((styles . (substring))
                             (cycle-sort-function . ,#'identity)))))
  (setf (alist-get (car entry) completion-category-overrides) (cdr entry)))


(provide 'dpc-sane-sorting)

;;; dpc-sane-sorting.el ends here
