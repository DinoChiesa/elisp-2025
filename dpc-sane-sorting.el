;;; -*- coding: utf-8; lexical-binding: t;  -*-

;;; dpc-sane-sorting.el --- logic to help make icomplete-vertical-model sort sanely in minibuffer
;;

(defun dpc-ss--sort (candidates)
  "Sorts the model candidate by name"
  (sort candidates :lessp #'string<))

(defun dpc-ss-completion-fn (candidates)
  "Returns a function to be used as the COMPLETIONS parameter in
`completing-read'.
This is a closure around CANDIDATES.
When using icomplete or icomplete-vertical, `completing-read' uses a
default of «sort first by length and then alphabetically». That is
inappropriate mostly.

To override this, users of chatgpt-shell can set
`chatgpt-shell-swap-model-selector' to provide a different experience.

Use this function this way:
  (setq chatgpt-shell-swap-model-selector
        (lambda (candidates)
          (completing-read
            \"New model: \"
            (dpc-ss--completion-fn candidates) nil t)))"
  (let ((candidates candidates))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (category . sorted-sanely) )
        (complete-with-action action candidates string pred)))))

;; For sorting of models in the swap-model chooser.  Sorting after
;; filtering happens correctly only with a category override, which means I
;; need a completion function, (See `dpc--cgs--model-completion-fn')
;; which specifies a category. Then add that category, `llm-model', to
;; `completion-category-overrides', and the right sorting will happen.
(add-to-list 'completion-category-overrides
             `(sorted-sanely
               (styles . (substring))
               (cycle-sort-function . ,#'dpc-ss--sort)))


(provide 'dpc-sane-sorting)

;;; dpc-sane-sorting.el ends here
