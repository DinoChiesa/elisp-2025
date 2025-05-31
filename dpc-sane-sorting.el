;;; -*- coding: utf-8; lexical-binding: t;  -*-

;;; dpc-sane-sorting.el --- logic to help make icomplete-vertical-model sort sanely in minibuffer
;;


;; ==== sorters
(defun dpc-ss-sort-alpha (candidates)
  "Sorts the model candidate by name"
  (sort candidates :lessp #'string<))

(defun dpc-ss-sort-alpha-but-dot-slash-last (candidates)
  "Sort a list of CANDIDATES alphabetically, except that  \"./\"
always appears at the end.

This is intended for use as a `cycle-sort-function' for
the `file' category in `completion-category-overrides', like so:

   (setq completion-category-overrides
         `((buffer
            (styles . (initials flex))
            (cycle . 10))
           (file
            (styles . (basic substring))
            (cycle-sort-function . ,#'dpc-ss-sort-alpha-but-dot-slash-last)
            (cycle . 10))
           (symbol
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

(defvar dpc-ss-supported-categories '(sorted-sanely unsorted)
  "List of supported completion categories for `dpc-ss-alphasort-completion-fn'.")

(defun dpc-ss-alphasort-completion-fn (candidates category-symbol)
  "Returns a function to be used as the COMPLETIONS parameter in
`completing-read'.
When using icomplete or icomplete-vertical, `completing-read' uses a
default of «sort first by length and then alphabetically». That is
inappropriate mostly. Actually this is done in minibuffer.el

To override this, users of chatgpt-shell can set
`chatgpt-shell-swap-model-selector' to provide a different experience.

Use this function this way:
  (setq chatgpt-shell-swap-model-selector
        (lambda (candidates)
          (completing-read
            \"New model: \"
            (dpc-ss--completion-fn candidates) nil t)))"
  (unless (memq category-symbol dpc-ss-supported-categories)
    (error "Invalid category symbol: %s. Supported categories are: %s"
           category-symbol dpc-ss-supported-categories))
  (let ((candidates candidates))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . ,category-symbol))
        (complete-with-action action candidates string pred)))))


;; This category, `sorted-sanely', is for sorting of anything... LLM models in the
;; swap-model chooser, commands, etc. Anything that needs a sane sort order.  Without an
;; overrides, minibuffer sorts first by length, then alphabetically, which seems
;; insane. With an override, the user (me) can pre-empt that default sorting.  Providing
;; a cycle-sort-function in the metadata works for initial display, but does not work
;; when filtering happens.  Sorting with an active filter (like we've typed a few
;; characters) happens correctly only with a category override which specifies a
;; cycle-sort-function, which means I need a completion function that specifies a
;; category. Eg, see above `dpc-ss-alphasort-completion-fn'.  Then add the category, in this case
;; `sorted-sanely', to `completion-category-overrides', and the right sorting will
;; happen.
(add-to-list 'completion-category-overrides
             `(sorted-sanely
               (styles . (substring))
               (cycle-sort-function . ,#'dpc-ss-sort-alpha)))

;; In some cases we want no sorting; particularly recentf.  Even here,
;; without an overrides, minibuffer will apply its weird sort. So
;; we override to say "no sort" with the `unsorted' category.
(add-to-list 'completion-category-overrides
             `(unsorted
               (styles . (substring))
               (cycle-sort-function . ,#'identity)))


(provide 'dpc-sane-sorting)

;;; dpc-sane-sorting.el ends here
