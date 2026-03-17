;;; el-xeger.el --- Generate strings from regular expressions -*- lexical-binding: t; -*-

;; Author: Gemini CLI
;; Version: 0.1
;; Keywords: lisp, regex, testing

;;; Commentary:

;; This library provides functions to generate random strings that match
;; a given regular expression, specifically using the `rx` S-expression
;; syntax.

;;; Code:

(require 'pcase)
(require 'xr)
(require 'cl-lib)

(defgroup el-xeger nil
  "Random string generation from regex."
  :group 'lisp)

(defcustom el-xeger-max-repeat 10
  "Maximum number of repetitions for unbounded operators like `*` and `+`."
  :type 'integer
  :group 'el-xeger)

(defconst el-xeger--named-classes
  '((digit  . ((?0 ?9)))
    (alnum  . ((?0 ?9) (?a ?z) (?A ?Z)))
    (alpha  . ((?a ?z) (?A ?Z)))
    (blank  . (?\s ?\t))
    (cntrl  . ((0 31) 127))
    (graph  . ((33 126)))
    (lower  . ((?a ?z)))
    (print  . ((32 126)))
    (punct  . ("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))
    (space  . (" \t\n\r\v\f"))
    (upper  . ((?A ?Z)))
    (word   . ((?0 ?9) (?a ?z) (?A ?Z) ?_))
    (xdigit . ((?0 ?9) (?a ?f) (?A ?F))))
  "Mapping of rx named classes to character class specifications.")

;;;###autoload
(defun el-xeger-from-exp (exp)
  "Generate a random string matching the regular expression in EXP.

Example: (el-xeger-from-exp \"[A-Z]\{4\}\") "
  (el-xeger-from-rx (xr exp)))


;;;###autoload
(defun el-xeger-from-rx (rx-form)
  "Generate a random string matching RX-FORM."
  (cond
   ((stringp rx-form) rx-form)
   ((characterp rx-form) (char-to-string rx-form))
   ((symbolp rx-form)
    (let ((named (assoc rx-form el-xeger--named-classes)))
      (if named
          (el-xeger--any (cdr named))
        (error "Unknown rx symbol: %s" rx-form))))
   ((listp rx-form)
    (pcase rx-form
      (`(,(or 'seq 'and 'submatch 'group 'group-n) . ,rest)
       (mapconcat #'el-xeger-from-rx rest ""))
      (`(or . ,rest)
       (el-xeger-from-rx (nth (random (length rest)) rest)))
      (`(,(or 'zero-or-more '*) . ,rest)
       (el-xeger--repeat 0 el-xeger-max-repeat rest))
      (`(,(or 'one-or-more '+) . ,rest)
       (el-xeger--repeat 1 el-xeger-max-repeat rest))
      (`(,(or 'optional '?\?) . ,rest)
       (el-xeger--repeat 0 1 rest))
      (`(repeat ,n . ,rest)
       (if (integerp (car rest))
           (el-xeger--repeat n (car rest) (cdr rest))
         (el-xeger--repeat n n rest)))
      (`(= ,n . ,rest)
       (el-xeger--repeat n n rest))
      (`(>= ,n . ,rest)
       (el-xeger--repeat n (+ n el-xeger-max-repeat) rest))
      (`(** ,n ,m . ,rest)
       (el-xeger--repeat n m rest))
      (`(any . ,args)
       (el-xeger--any args))
      (`(char . ,args)
       (el-xeger--any args))
      (`(not (any . ,args))
       (error "Negated character classes not yet supported"))
      (_ (error "Unsupported rx form: %S" rx-form))))
   (t (error "Invalid rx form type: %S" rx-form))))

(defun el-xeger--repeat (min max rest)
  "Generate a string by repeating REST between MIN and MAX times."
  (let* ((count (+ min (random (1+ (- (or max min) min)))))
         (inner (if (= (length rest) 1) (car rest) `(seq ,@rest))))
    (let ((result ""))
      (dotimes (_ count)
        (setq result (concat result (el-xeger-from-rx inner))))
      result)))

(defun el-xeger--any (args)
  "Generate a random character from ARGS specification."
  (let ((pool nil))
    (dolist (arg args)
      (cond
       ((characterp arg) (push arg pool))
       ((stringp arg)
        (let ((len (length arg))
              (i 0))
          (while (< i len)
            (if (and (< (+ i 2) len)
                     (= (aref arg (1+ i)) ?-))
                (let ((start (aref arg i))
                      (end (aref arg (+ i 2))))
                  (cl-loop for c from start to end do (push c pool))
                  (setq i (+ i 3)))
              (push (aref arg i) pool)
              (setq i (1+ i))))))
       ((and (listp arg) (= (length arg) 2))
        (let ((start (car arg))
              (end (cadr arg)))
          (cl-loop for i from start to end do (push i pool))))
       ((symbolp arg)
        (let ((named (assoc arg el-xeger--named-classes)))
          (if named
              (let ((inner-pool (string-to-list (el-xeger--any (cdr named)))))
                (setq pool (append inner-pool pool)))
            (error "Unknown named class in 'any': %s" arg))))))
    (unless pool (error "Empty character class"))
    (char-to-string (nth (random (length pool)) pool))))

(provide 'el-xeger)
;;; el-xeger.el ends here
