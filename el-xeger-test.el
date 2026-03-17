;;; el-xeger-test.el --- Tests for el-xeger.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'el-xeger)
(require 'rx)

(ert-deftest el-xeger-test-literals ()
  (should (string= (el-xeger-from-rx "abc") "abc"))
  (should (string= (el-xeger-from-rx ?a) "a")))

(ert-deftest el-xeger-test-seq ()
  (should (string= (el-xeger-from-rx '(seq "a" "b" "c")) "abc"))
  (should (string= (el-xeger-from-rx '(and "x" ?y)) "xy")))

(ert-deftest el-xeger-test-or ()
  (let ((res (el-xeger-from-rx '(or "a" "b"))))
    (should (member res '("a" "b")))))

(ert-deftest el-xeger-test-repeat ()
  (let ((res (el-xeger-from-rx '(repeat 3 "a"))))
    (should (string= res "aaa")))
  (let ((res (el-xeger-from-rx '(repeat 2 4 "b"))))
    (should (member (length res) '(2 3 4)))
    (should (string-match-p "^b+$" res))))

(ert-deftest el-xeger-test-any ()
  (let ((res (el-xeger-from-rx '(any "abc"))))
    (should (string-match-p "^[abc]$" res)))
  (let ((res (el-xeger-from-rx '(any "a-z"))))
    (should (string-match-p "^[a-z]$" res)))
  (let ((res (el-xeger-from-rx '(any (?a ?z)))))
    (should (string-match-p "^[a-z]$" res))))

(ert-deftest el-xeger-test-named-classes ()
  (let ((res (el-xeger-from-rx 'digit)))
    (should (string-match-p "^[0-9]$" res)))
  (let ((res (el-xeger-from-rx '(seq digit digit digit))))
    (should (string-match-p "^[0-9]\\{3\\}$" res))))

(ert-deftest el-xeger-test-complex ()
  (let* ((rx-form '(seq "ID-" (repeat 3 digit) (or "A" "B")))
         (regex (rx-to-string rx-form))
         (res (el-xeger-from-rx rx-form)))
    (should (string-match-p regex res))))

(ert-deftest el-xeger-test-zero-or-more ()
  (let* ((rx-form '(zero-or-more "a"))
         (regex (rx-to-string rx-form))
         (res (el-xeger-from-rx rx-form)))
    (should (string-match-p regex res))))

(ert-deftest el-xeger-test-one-or-more ()
  (let* ((rx-form '(one-or-more "a"))
         (regex (rx-to-string rx-form))
         (res (el-xeger-from-rx rx-form)))
    (should (string-match-p regex res))
    (should (> (length res) 0))))
