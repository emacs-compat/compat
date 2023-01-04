;;; compat-tests.el --- Tests for compat.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for compatibility functions from compat.el.
;;
;; Note that not all functions have tests yet.  Additions are very welcome.
;; If you intend to use a function, which doesn't have tests yet, please
;; contribute tests here.  NO GUARANTEES ARE MADE FOR FUNCTIONS WITHOUT
;; TESTS.

;; The tests are written in a simple, explicit style.  Please inspect the
;; tests in order to find out the supported calling conventions.  In
;; particular, note the use of `compat-call' to call functions, where the
;; calling convention changed between Emacs versions.

;; The functions tested here are guaranteed to work on the Emacs versions
;; tested by continuous integration.  This includes 24.4, 24.5, 25.1, 25.2,
;; 25.3, 26.1, 26.2, 26.3, 27.1, 27.2, 28.1 and the current Emacs master
;; branch.

;;; Code:

(require 'ert)
(require 'compat)
(require 'subr-x)
(require 'text-property-search nil t)
(setq ert-quiet t)

(ert-deftest compat-call ()
  (let (list)
    (setq list (compat-call plist-put list "first" 1 #'string=))
    (setq list (compat-call plist-put list "second" 2 #'string=))
    (setq list (compat-call plist-put list "first" 10 #'string=))
    (should (eq (compat-call plist-get list "first" #'string=) 10))
    (should (eq (compat-call plist-get list "second" #'string=) 2))))

(ert-deftest mapcan ()
  (should (equal nil (mapcan #'identity nil)))
  (should (equal (list 1)
                 (mapcan #'identity
                         (list (list 1)))))
  (should (equal (list 1 2 3 4)
                 (mapcan #'identity
                         (list (list 1) (list 2 3) (list 4)))))
  (should (equal (list (list 1) (list 2 3) (list 4))
                 (mapcan #'list
                         (list (list 1) (list 2 3) (list 4)))))
  (should (equal (list 1 2 3 4)
                 (mapcan #'identity
                         (list (list 1) (list) (list 2 3) (list 4)))))
  (should (equal (list (list 1) (list) (list 2 3) (list 4))
                 (mapcan #'list
                         (list (list 1) (list) (list 2 3) (list 4)))))
  (should (equal (list)
                 (mapcan #'identity
                         (list (list) (list) (list) (list))))))

(ert-deftest xor ()
  (should (equal t (xor t nil)))
  (should (equal t (xor nil t)))
  (should (equal nil (xor nil nil)))
  (should (equal nil (xor t t))))

(ert-deftest length= ()
  (should (equal t (length= '() 0)))                  ;empty list
  (should (equal t (length= '(1) 1)))                 ;single element
  (should (equal t (length= '(1 2 3) 3)))             ;multiple elements
  (should (equal nil (length= '(1 2 3) 2)))           ;less than
  (should (equal nil (length= '(1) 0)))
  (should (equal nil (length= '(1 2 3) 4)))           ;more than
  (should (equal nil (length= '(1) 2)))
  (should (equal nil (length= '() 1)))
  (should (equal t (length= [] 0)))                   ;empty vector
  (should (equal t (length= [1] 1)))                  ;single element vector
  (should (equal t (length= [1 2 3] 3)))              ;multiple element vector
  (should (equal nil (length= [1 2 3] 2)))            ;less than
  (should (equal nil (length= [1 2 3] 4)))            ;more than
  (should-error (length= 3 nil) :type 'wrong-type-argument))

(ert-deftest length< ()
  (should (equal nil (length< '(1) 0)))               ;single element
  (should (equal nil (length< '(1 2 3) 2)))           ;multiple elements
  (should (equal nil (length< '(1 2 3) 3)))           ;equal length
  (should (equal nil (length< '(1) 1)))
  (should (equal t (length< '(1 2 3) 4)))             ;more than
  (should (equal t (length< '(1) 2)))
  (should (equal t (length< '() 1)))
  (should (equal nil (length< [1] 0)))                ;single element vector
  (should (equal nil (length< [1 2 3] 2)))            ;multiple element vector
  (should (equal nil (length< [1 2 3] 3)))            ;equal length
  (should (equal t (length< [1 2 3] 4)))              ;more than
  (should-error (length< 3 nil) :type 'wrong-type-argument))

(ert-deftest length> ()
  (should (equal t (length> '(1) 0)))                 ;single element
  (should (equal t (length> '(1 2 3) 2)))             ;multiple elements
  (should (equal nil (length> '(1 2 3) 3)))           ;equal length
  (should (equal nil (length> '(1) 1)))
  (should (equal nil (length> '(1 2 3) 4)))           ;more than
  (should (equal nil (length> '(1) 2)))
  (should (equal nil (length> '() 1)))
  (should (equal t (length> [1] 0)))                  ;single element vector
  (should (equal t (length> [1 2 3] 2)))              ;multiple element vector
  (should (equal nil (length> [1 2 3] 3)))            ;equal length
  (should (equal nil (length> [1 2 3] 4)))            ;more than
  (should-error (length< 3 nil) :type 'wrong-type-argument))

(ert-deftest ensure-list ()
  (should (equal nil (ensure-list nil)))           ;; empty list
  (should (equal '(1) (ensure-list '(1))))         ;; single element list
  (should (equal '(1 2 3) (ensure-list '(1 2 3)))) ;; multiple element list
  (should (equal '(1) (ensure-list 1))))           ;; atom

(ert-deftest proper-list-p ()
  (should (equal 0 (proper-list-p ())))            ;; empty list
  (should (equal 1 (proper-list-p '(1))))          ;; single element
  (should (equal 3 (proper-list-p '(1 2 3))))      ;; multiple elements
  (should (equal nil (proper-list-p '(1 . 2))))    ;; cons
  (should (equal nil (proper-list-p '(1 2 . 3))))  ;; dotted
  (should (equal nil (let ((l (list 1 2 3)))       ;; circular
                       (setf (nthcdr 3 l) l)
                       (proper-list-p l))))
  (should (equal nil (proper-list-p 1)))           ;; non-lists
  (should (equal nil (proper-list-p "")))
  (should (equal nil (proper-list-p "abc")))
  (should (equal nil (proper-list-p [])))
  (should (equal nil (proper-list-p [1 2 3]))))

(ert-deftest always ()
  (should (equal t (always)))                      ;; no arguments
  (should (equal t (always 1)))                    ;; single argument
  (should (equal t (always 1 2 3 4))))             ;; multiple arguments

(ert-deftest string-width ()
  (should (equal 0 (compat-string-width "")))                         ;; Obsolete
  (should (equal 0 (compat-call string-width "")))
  (should (equal 3 (compat-call string-width "abc")))                 ;; no argument
  (should (equal 5 (compat-call string-width "abcあ")))
  (should (equal (1+ tab-width) (compat-call string-width "a	")))
  (should (equal 2 (compat-call string-width "abc" 1)))               ;; with from
  (should (equal 4 (compat-call string-width "abcあ" 1)))
  (should (equal tab-width (compat-call string-width "a	" 1)))
  (should (equal 2 (compat-call string-width "abc" 0 2)))             ;; with to
  (should (equal 3 (compat-call string-width "abcあ" 0 3)))
  (should (equal 1 (compat-call string-width "a	" 0 1)))
  (should (equal 1 (compat-call string-width "abc" 1 2)))             ;; with from and to
  (should (equal 2 (compat-call string-width "abcあ" 3 4)))
  (should (equal 0 (compat-call string-width "a	" 1 1))))

(ert-deftest string-trim-left ()
  (should (equal "a" (compat-string-trim-left " a"))) ;; Obsolete
  (should (equal "a" (compat-call string-trim-left "---a" "-+"))) ;; Additional regexp
  (should (equal "" (compat-call string-trim-left "")))                          ;empty string
  (should (equal "a" (compat-call string-trim-left "a")))                        ;"full" string
  (should (equal "aaa" (compat-call string-trim-left "aaa")))
  (should (equal "へっろ" (compat-call string-trim-left "へっろ")))
  (should (equal "hello world" (compat-call string-trim-left "hello world")))
  (should (equal "a " (compat-call string-trim-left "a ")))                        ;right trailing
  (should (equal "aaa " (compat-call string-trim-left "aaa ")))
  (should (equal "a    " (compat-call string-trim-left "a    ")))
  (should (equal "a\t\t" (compat-call string-trim-left "a\t\t")))
  (should (equal "a\n  \t" (compat-call string-trim-left "a\n  \t")))
  (should (equal "a" (compat-call string-trim-left " a")))                        ;left trailing
  (should (equal "aaa" (compat-call string-trim-left " aaa")))
  (should (equal "a" (compat-call string-trim-left "a")))
  (should (equal "a" (compat-call string-trim-left "\t\ta")))
  (should (equal "a" (compat-call string-trim-left "\n  \ta")))
  (should (equal "a " (compat-call string-trim-left " a ")))                        ;both trailing
  (should (equal "aaa  " (compat-call string-trim-left " aaa  ")))
  (should (equal "a\t\n" (compat-call string-trim-left "\t\ta\t\n")))
  (should (equal "a  \n" (compat-call string-trim-left "\n  \ta  \n"))))

(ert-deftest string-trim-right ()
  (should (equal "a" (compat-string-trim-right "a    "))) ;; Obsolete
  (should (equal "a" (compat-call string-trim-right "a---" "-+"))) ;; Additional regexp
  (should (equal "" (compat-call string-trim-right "")))                          ;empty string
  (should (equal "a" (compat-call string-trim-right "a")))                        ;"full" string
  (should (equal "aaa" (compat-call string-trim-right "aaa")))
  (should (equal "へっろ" (compat-call string-trim-right "へっろ")))
  (should (equal "hello world" (compat-call string-trim-right "hello world")))
  (should (equal "a" (compat-call string-trim-right "a")))                      ;right trailing
  (should (equal "aaa" (compat-call string-trim-right "aaa")))
  (should (equal "a" (compat-call string-trim-right "a    ")))
  (should (equal "a" (compat-call string-trim-right "a\t\t")))
  (should (equal "a" (compat-call string-trim-right "a\n  \t")))
  (should (equal " a" (compat-call string-trim-right " a")))                       ;left trailing
  (should (equal " aaa" (compat-call string-trim-right " aaa")))
  (should (equal "a" (compat-call string-trim-right "a")))
  (should (equal "\t\ta" (compat-call string-trim-right "\t\ta")))
  (should (equal "\n  \ta" (compat-call string-trim-right "\n  \ta")))
  (should (equal " a" (compat-call string-trim-right " a ")))                        ;both trailing
  (should (equal " aaa" (compat-call string-trim-right " aaa")))
  (should (equal "\t\ta" (compat-call string-trim-right "\t\ta\t\n")))
  (should (equal "\n  \ta" (compat-call string-trim-right "\n  \ta  \n"))))

(ert-deftest string-trim ()
  (should (equal "aaa" (compat-string-trim " aaa  "))) ;; Obsolete
  (should (equal "aaa" (compat-call string-trim "--aaa__" "-+" "_+"))) ;; Additional regexp
  (should (equal "" (compat-call string-trim "")))                          ;empty string
  (should (equal "a" (compat-call string-trim "a")))                        ;"full" string
  (should (equal "aaa" (compat-call string-trim "aaa")))
  (should (equal "へっろ" (compat-call string-trim "へっろ")))
  (should (equal "hello world" (compat-call string-trim "hello world")))
  (should (equal "a" (compat-call string-trim "a ")))                       ;right trailing
  (should (equal "aaa" (compat-call string-trim "aaa ")))
  (should (equal "a" (compat-call string-trim "a    ")))
  (should (equal "a" (compat-call string-trim "a\t\t")))
  (should (equal "a" (compat-call string-trim "a\n  \t")))
  (should (equal "a" (compat-call string-trim " a")))                       ;left trailing
  (should (equal "aaa" (compat-call string-trim " aaa")))
  (should (equal "a" (compat-call string-trim "a")))
  (should (equal "a" (compat-call string-trim "\t\ta")))
  (should (equal "a" (compat-call string-trim "\n  \ta")))
  (should (equal "a" (compat-call string-trim " a ")))                      ;both trailing
  (should (equal "aaa" (compat-call string-trim " aaa  ")))
  (should (equal "t\ta" (compat-call string-trim "t\ta\t\n")))
  (should (equal "a" (compat-call string-trim "\n  \ta  \n"))))

(ert-deftest hash-table-keys ()
  (let ((ht (make-hash-table)))
    (should (null (hash-table-keys ht)))
    (puthash 1 'one ht)
    (should (equal '(1) (hash-table-keys ht)))
    (puthash 1 'one ht)
    (should (equal '(1) (hash-table-keys ht)))
    (puthash 2 'two ht)
    (should (memq 1 (hash-table-keys ht)))
    (should (memq 2 (hash-table-keys ht)))
    (should (= 2 (length (hash-table-keys ht))))
    (remhash 1 ht)
    (should (equal '(2) (hash-table-keys ht)))))

(ert-deftest hash-table-values ()
  (let ((ht (make-hash-table)))
    (should (null (hash-table-values ht)))
    (puthash 1 'one ht)
    (should (equal '(one) (hash-table-values ht)))
    (puthash 1 'one ht)
    (should (equal '(one) (hash-table-values ht)))
    (puthash 2 'two ht)
    (should (memq 'one (hash-table-values ht)))
    (should (memq 'two (hash-table-values ht)))
    (should (= 2 (length (hash-table-values ht))))
    (remhash 1 ht)
    (should (equal '(two) (hash-table-values ht)))))

(ert-deftest named-let ()
  "Check if `named-let' was implemented properly."
  (should (= (named-let l ((i 0)) (if (= i 8) i (l (1+ i))))
             8))
  (should (= (named-let l ((i 0)) (if (= i 100000) i (l (1+ i))))
             100000))
  (should (= (named-let l ((i 0))
               (cond
                ((= i 100000) i)
                ((= (mod i 2) 0)
                 (l (+ i 2)))
                ((l (+ i 3)))))
             100000))
  (should (= (named-let l ((i 0) (x 1)) (if (= i 8) x (l (1+ i) (* x 2))))
             (expt 2 8)))
  (should (eq (named-let lop ((x 1))
                (if (> x 0)
                    (condition-case nil
                        (lop (1- x))
                      (arith-error 'ok))
                  (/ 1 x)))
              'ok))
  (should (eq (named-let lop ((n 10000))
                (if (> n 0)
                    (condition-case nil
                        (/ n 0)
                      (arith-error (lop (1- n))))
                  'ok))
              'ok))
  (should (eq (named-let lop ((x nil))
                (cond (x)
                      (t 'ok)))
              'ok))
  (should (eq (named-let lop ((x 100000))
                (cond ((= x 0) 'ok)
                      ((lop (1- x)))))
              'ok))
  (should (eq (named-let lop ((x 100000))
                (cond
                 ((= x -1) nil)
                 ((= x 0) 'ok)
                 ((lop -1))
                 ((lop (1- x)))))
              'ok))
  (should (eq (named-let lop ((x 10000))
                (cond ((= x 0) 'ok)
                      ((and t (lop (1- x))))))
              'ok))
  (should (eq (let ((b t))
                (named-let lop ((i 0))
                  (cond ((null i) nil) ((= i 10000) 'ok)
                        ((lop (and (setq b (not b)) (1+ i))))
                        ((lop (and (setq b (not b)) (1+ i)))))))
              'ok)))

(ert-deftest compat-call-alist-get-gv ()
  "Test if the `compat-call alist-get' can be used as a generalised variable."
  (let ((alist-1 (list (cons 1 "one")
                       (cons 2 "two")
                       (cons 3 "three")))
        (alist-2 (list (cons "one" 1)
                       (cons "two" 2)
                       (cons "three" 3))))
    (setf (compat-call alist-get 1 alist-1) "eins")
    (should (equal (compat-call alist-get 1 alist-1) "eins"))
    (setf (compat-call alist-get 2 alist-1 nil 'remove) nil)
    (should (equal alist-1 '((1 . "eins") (3 . "three"))))
    (setf (compat-call alist-get "one" alist-2 nil nil #'string=) "eins")
    (should (equal (compat-call alist-get "one" alist-2 nil nil #'string=)
                   "eins"))))

(ert-deftest alist-get-gv ()
  "Test if the `compat-alist-get' can be used as a generalised variable."
  (let ((alist-1 (list (cons 1 "one")
                       (cons 2 "two")
                       (cons 3 "three")))
        (alist-2 (list (cons "one" 1)
                       (cons "two" 2)
                       (cons "three" 3))))
    (setf (compat-alist-get 1 alist-1) "eins")
    (should (equal (compat-alist-get 1 alist-1) "eins"))
    (setf (compat-alist-get 2 alist-1 nil 'remove) nil)
    (should (equal alist-1 '((1 . "eins") (3 . "three"))))
    (setf (compat-alist-get "one" alist-2 nil nil #'string=) "eins")
    (should (equal (compat-alist-get "one" alist-2 nil nil #'string=)
                   "eins"))))

(ert-deftest json-serialize ()
  "Check if `json-serialize' was implemented properly."
  (let ((input-1 '((:key . ["abc" 2]) (yek . t)))
        (input-2 '(:key ["abc" 2] yek t))
        (input-3 (let ((ht (make-hash-table)))
                   (puthash "key" ["abc" 2] ht)
                   (puthash "yek" t ht)
                   ht)))
    (should (equal (json-serialize input-1)
                   "{\":key\":[\"abc\",2],\"yek\":true}"))
    (should (equal (json-serialize input-2)
                   "{\"key\":[\"abc\",2],\"yek\":true}"))
    (should (member (json-serialize input-2)
                    '("{\"key\":[\"abc\",2],\"yek\":true}"
                      "{\"yek\":true,\"key\":[\"abc\",2]}")))
    ;; TODO fix broken test
    ;; (should (equal (json-serialize input-3)
    ;;                "{\"key\":[\"abc\",2],\"yek\":true}"))
    (should-error (json-serialize '(("a" . 1)))
                  :type '(wrong-type-argument symbolp "a"))
    (should-error (json-serialize '("a" 1))
                  :type '(wrong-type-argument symbolp "a"))
    (should-error (json-serialize '("a" 1 2))
                  :type '(wrong-type-argument symbolp "a"))
    (should-error (json-serialize '(:a 1 2))
                  :type '(wrong-type-argument consp nil))
    (should-error (json-serialize
                   (let ((ht (make-hash-table)))
                     (puthash 'a 1 ht)
                     ht))
                  :type '(wrong-type-argument stringp a))))

(ert-deftest json-parse-string ()
  "Check if `compat--t-json-parse-string' was implemented properly."
  (let ((input "{\"key\":[\"abc\", 2], \"yek\": null}"))
    (let ((obj (json-parse-string input :object-type 'alist)))
      (should (equal (cdr (assq 'key obj)) ["abc" 2]))
      (should (equal (cdr (assq 'yek obj)) :null)))
    (let ((obj (json-parse-string input :object-type 'plist)))
      (should (equal (plist-get obj :key) ["abc" 2]))
      (should (equal (plist-get obj :yek) :null)))
    (let ((obj (json-parse-string input)))
      (should (equal (gethash "key" obj) ["abc" 2]))
      (should (equal (gethash "yek" obj) :null)))))

(ert-deftest json-insert ()
  (with-temp-buffer
    (json-insert '((:key . ["abc" 2]) (yek . t)))
    (should (equal (buffer-string) "{\":key\":[\"abc\",2],\"yek\":true}"))))

(ert-deftest text-property-search-forward ()
  (with-temp-buffer
    (insert "one "
            (propertize "two " 'prop 'val)
            "three "
            (propertize "four " 'prop 'wert)
            "five ")
    (goto-char (point-min))
    (let ((match (text-property-search-forward 'prop)))
      (should (eq (prop-match-beginning match) 5))
      (should (eq (prop-match-end match) 9))
      (should (eq (prop-match-value match) 'val)))
    (let ((match (text-property-search-forward 'prop)))
      (should (eq (prop-match-beginning match) 15))
      (should (eq (prop-match-end match) 20))
      (should (eq (prop-match-value match) 'wert)))
    (should (null (text-property-search-forward 'prop)))
    (goto-char (point-min))
    (should (null (text-property-search-forward 'non-existant)))))

(ert-deftest text-property-search-backward ()
  (with-temp-buffer
    (insert "one "
            (propertize "two " 'prop 'val)
            "three "
            (propertize "four " 'prop 'wert)
            "five ")
    (goto-char (point-max))
    (let ((match (text-property-search-backward 'prop)))
      (should (eq (prop-match-beginning match) 15))
      (should (eq (prop-match-end match) 20))
      (should (eq (prop-match-value match) 'wert)))
    (let ((match (text-property-search-backward 'prop)))
      (should (eq (prop-match-beginning match) 5))
      (should (eq (prop-match-end match) 9))
      (should (eq (prop-match-value match) 'val)))
    (should (null (text-property-search-backward 'prop)))
    (goto-char (point-max))
    (should (null (text-property-search-backward 'non-existant)))))

;; (ert-deftest string-search
;;   ;; Find needle at the beginning of a haystack:
;;   (ought 0 "a" "abb")
;;   ;; Find needle at the begining of a haystack, with more potential
;;   ;; needles that could be found:
;;   (ought 0 "a" "abba")
;;   ;; Find needle with more than one charachter at the beginning of
;;   ;; a line:
;;   (ought 0 "aa" "aabbb")
;;   ;; Find a needle midstring:
;;   (ought 1 "a" "bab")
;;   ;; Find a needle at the end:
;;   (ought 2 "a" "bba")
;;   ;; Find a longer needle midstring:
;;   (ought 1 "aa" "baab")
;;   ;; Find a longer needle at the end:
;;   (ought 2 "aa" "bbaa")
;;   ;; Find a case-sensitive needle:
;;   (ought 2 "a" "AAa")
;;   ;; Find another case-sensitive needle:
;;   (ought 2 "aa" "AAaa")
;;   ;; Test regular expression quoting (1):
;;   (ought 5 "." "abbbb.b")
;;   ;; Test regular expression quoting (2):
;;   (ought 5 ".*" "abbbb.*b")
;;   ;; Attempt to find non-existent needle:
;;   (ought nil "a" "bbb")
;;   ;; Attempt to find non-existent needle that has the form of a
;;   ;; regular expression:
;;   (ought nil "." "bbb")
;;   ;; Handle empty string as needle:
;;   (ought 0 "" "abc")
;;   ;; Handle empty string as haystack:
;;   (ought nil "a" "")
;;   ;; Handle empty string as needle and haystack:
;;   (ought 0 "" "")
;;   ;; Handle START argument:
;;   (ought 3 "a" "abba" 1)
;;   ;; Additional test copied from:
;;   (ought 6 "zot" "foobarzot")
;;   (ought 0 "foo" "foobarzot")
;;   (ought nil "fooz" "foobarzot")
;;   (ought nil "zot" "foobarzo")
;;   (ought 0 "ab" "ab")
;;   (ought nil "ab\0" "ab")
;;   (ought 4 "ab" "abababab" 3)
;;   (ought nil "ab" "ababac" 3)
;;   (ought nil "aaa" "aa")
;;   ;; The `make-string' calls with three arguments have been replaced
;;   ;; here with the result of their evaluation, to avoid issues with
;;   ;; older versions of Emacs that only support two arguments.
;;   (ought 5
;;                   (make-string 2 130)
;;                   ;; Per (concat "helló" (make-string 5 130 t) "bár")
;;                   "hellóbár")
;;   (ought 5
;;                   (make-string 2 127)
;;                   ;; Per (concat "helló" (make-string 5 127 t) "bár")
;;                   "hellóbár")
;;   (ought 1 "\377" "a\377ø")
;;   (ought 1 "\377" "a\377a")
;;   (ought nil (make-string 1 255) "a\377ø")
;;   (ought nil (make-string 1 255) "a\377a")
;;   (ought 3 "fóo" "zotfóo")
;;   (ought nil "\303" "aøb")
;;   (ought nil "\270" "aøb")
;;   (ought nil "ø" "\303\270")
;;   (ought nil "ø" (make-string 32 ?a))
;;   (ought nil "ø" (string-to-multibyte (make-string 32 ?a)))
;;   (ought 14 "o" (string-to-multibyte
;;                           (apply #'string (number-sequence ?a ?z))))
;;   (ought 2 "a\U00010f98z" "a\U00010f98a\U00010f98z")
;;   (expect (args-out-of-range -1) "a" "abc" -1)
;;   (expect (args-out-of-range 4) "a" "abc" 4)
;;   (expect (args-out-of-range 100000000000)
;;                  "a" "abc" 100000000000)
;;   (ought nil "a" "aaa" 3)
;;   (ought nil "aa" "aa" 1)
;;   (ought nil "\0" "")
;;   (ought 0 "" "")
;;   (expect (args-out-of-range 1) "" "" 1)
;;   (ought 0 "" "abc")
;;   (ought 2 "" "abc" 2)
;;   (ought 3 "" "abc" 3)
;;   (expect (args-out-of-range 4) "" "abc" 4)
;;   (expect (args-out-of-range -1) "" "abc" -1)
;;   (ought nil "ø" "foo\303\270")
;;   (ought nil "\303\270" "ø")
;;   (ought nil "\370" "ø")
;;   (ought nil (string-to-multibyte "\370") "ø")
;;   (ought nil "ø" "\370")
;;   (ought nil "ø" (string-to-multibyte "\370"))
;;   (ought nil "\303\270" "\370")
;;   (ought nil (string-to-multibyte "\303\270") "\370")
;;   (ought nil "\303\270" (string-to-multibyte "\370"))
;;   (ought nil
;;                   (string-to-multibyte "\303\270")
;;                   (string-to-multibyte "\370"))
;;   (ought nil "\370" "\303\270")
;;   (ought nil (string-to-multibyte "\370") "\303\270")
;;   (ought nil "\370" (string-to-multibyte "\303\270"))
;;   (ought nil
;;                   (string-to-multibyte "\370")
;;                   (string-to-multibyte "\303\270"))
;;   (ought 3 "\303\270" "foo\303\270")
;;   (when (version<= "27" emacs-version)
;;     ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1 in
;;     ;; emacs.git fixes the behaviour of regular expressions matching
;;     ;; raw bytes.  The compatibility functions should updated to
;;     ;; backport this behaviour.
;;     (ought 2 (string-to-multibyte "\377") "ab\377c")
;;     (ought 2
;;                     (string-to-multibyte "o\303\270")
;;                     "foo\303\270")))

;; (ert-deftest string-replace
;;   (ought "bba" "aa" "bb" "aaa")
;;   (ought "AAA" "aa" "bb" "AAA")
;;   ;; Additional test copied from subr-tests.el:
;;   (ought "zot" "foo" "bar" "zot")
;;   (ought "barzot" "foo" "bar" "foozot")
;;   (ought "barbarzot" "foo" "bar" "barfoozot")
;;   (ought "barfoobar" "zot" "bar" "barfoozot")
;;   (ought "barfoobarot" "z" "bar" "barfoozot")
;;   (ought "zat" "zot" "bar" "zat")
;;   (ought "zat" "azot" "bar" "zat")
;;   (ought "bar" "azot" "bar" "azot")
;;   (ought "foozotbar" "azot" "bar" "foozotbar")
;;   (ought "labarbarbarzot" "fo" "bar" "lafofofozot")
;;   (ought "axb" "\377" "x" "a\377b")
;;   (ought "axø" "\377" "x" "a\377ø")
;;   (when (version<= "27" emacs-version)
;;     ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1
;;     ;; in emacs.git fixes the behaviour of regular
;;     ;; expressions matching raw bytes.  The compatibility
;;     ;; functions should updated to backport this
;;     ;; behaviour.
;;     (ought "axb" (string-to-multibyte "\377") "x" "a\377b")
;;     (ought "axø" (string-to-multibyte "\377") "x" "a\377ø"))
;;   (ought "ANAnas" "ana" "ANA" "ananas")
;;   (ought "" "a" "" "")
;;   (ought "" "a" "" "aaaaa")
;;   (ought "" "ab" "" "ababab")
;;   (ought "ccc" "ab" "" "abcabcabc")
;;   (ought "aaaaaa" "a" "aa" "aaa")
;;   (ought "defg" "abc" "defg" "abc")
;;   (when (version<= "24.4" emacs-version)
;;     ;; FIXME: Emacs 24.3 do not know of `wrong-length-argument' and
;;     ;; therefore fail this test, even if the right symbol is being
;;     ;; thrown.
;;     (expect wrong-length-argument "" "x" "abc")))

;; (ert-deftest compat-insert-into-buffer ()
;;   "Check if `insert-into-buffer' was implemented correctly."
;;   ;; Without optional compat--arguments
;;   (with-temp-buffer
;;     (let ((other (current-buffer)))
;;       (insert "abc")
;;       (with-temp-buffer
;;         (insert "def")
;;         (compat--t-insert-into-buffer other))
;;       (should (string= (buffer-string) "abcdef"))))
;;   (when (fboundp 'insert-into-buffer)
;;     (with-temp-buffer
;;       (let ((other (current-buffer)))
;;         (insert "abc")
;;         (with-temp-buffer
;;           (insert "def")
;;           (insert-into-buffer other))
;;         (should (string= (buffer-string) "abcdef")))))
;;   ;; With one optional argument
;;   (with-temp-buffer
;;     (let ((other (current-buffer)))
;;       (insert "abc")
;;       (with-temp-buffer
;;         (insert "def")
;;         (compat--t-insert-into-buffer other 2))
;;       (should (string= (buffer-string) "abcef"))))
;;   (when (fboundp 'insert-into-buffer)
;;     (with-temp-buffer
;;       (let ((other (current-buffer)))
;;         (insert "abc")
;;         (with-temp-buffer
;;           (insert "def")
;;           (insert-into-buffer other 2))
;;         (should (string= (buffer-string) "abcef")))))
;;   ;; With two optional arguments
;;   (with-temp-buffer
;;     (let ((other (current-buffer)))
;;       (insert "abc")
;;       (with-temp-buffer
;;         (insert "def")
;;         (compat--t-insert-into-buffer other 2 3))
;;       (should (string= (buffer-string) "abce"))))
;;   (when (fboundp 'insert-into-buffer)
;;     (with-temp-buffer
;;       (let ((other (current-buffer)))
;;         (insert "abc")
;;         (with-temp-buffer
;;           (insert "def")
;;           (insert-into-buffer other 2 3))
;;         (should (string= (buffer-string) "abce"))))))

;; (ert-deftest file-name-with-extension
;;   (ought "file.ext" "file" "ext")
;;   (ought "file.ext" "file" ".ext")
;;   (ought "file.ext" "file." ".ext")
;;   (ought "file..ext" "file.." ".ext")
;;   (ought "file..ext" "file." "..ext")
;;   (ought "file...ext" "file.." "..ext")
;;   (ought "/abs/file.ext" "/abs/file" "ext")
;;   (ought "/abs/file.ext" "/abs/file" ".ext")
;;   (ought "/abs/file.ext" "/abs/file." ".ext")
;;   (ought "/abs/file..ext" "/abs/file.." ".ext")
;;   (ought "/abs/file..ext" "/abs/file." "..ext")
;;   (ought "/abs/file...ext" "/abs/file.." "..ext")
;;   (expect error "file" "")
;;   (expect error "" "ext")
;;   (expect error "file" "")
;;   (expect error "rel/" "ext")
;;   (expect error "/abs/" "ext"))

;; (ert-deftest flatten-tree
;;   ;; Example from docstring:
;;   (ought '(1 2 3 4 5 6 7) '(1 (2 . 3) nil (4 5 (6)) 7))
;;   ;; Trivial example
;;   (ought nil ())
;;   ;; Simple examples
;;   (ought '(1) '(1))
;;   (ought '(1 2) '(1 2))
;;   (ought '(1 2 3) '(1 2 3))
;;   ;; Regular sublists
;;   (ought '(1) '((1)))
;;   (ought '(1 2) '((1) (2)))
;;   (ought '(1 2 3) '((1) (2) (3)))
;;   ;; Complex examples
;;   (ought '(1) '(((((1))))))
;;   (ought '(1 2 3 4) '((1) nil 2 ((3 4))))
;;   (ought '(1 2 3 4) '(((1 nil)) 2 (((3 nil nil) 4)))))

;; (ert-deftest string-distance
;;   (ought 3 "kitten" "sitting")     ;from wikipedia
;;   (if (version<= "28" emacs-version) ;trivial examples
;;       (ought 0 "" "")
;;     ;; Up until Emacs 28, `string-distance' had a bug
;;     ;; when comparing two empty strings. This was fixed
;;     ;; in the following commit:
;;     ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=c44190c
;;     ;;
;;     ;; Therefore, we must make sure, that the test
;;     ;; doesn't fail because of this bug:
;;     (should (= (compat--t-string-distance "" "") 0)))
;;   (ought 0 "a" "a")
;;   (ought 1 "" "a")
;;   (ought 1 "b" "a")
;;   (ought 2 "aa" "bb")
;;   (ought 2 "aa" "bba")
;;   (ought 2 "aaa" "bba")
;;   (ought 3 "a" "あ" t)             ;byte example
;;   (ought 1 "a" "あ"))

;; (ert-deftest compat-regexp-unmatchable ()
;;   "Check if `compat--string-distance' was implemented correctly."
;;   (dolist (str '(""                     ;empty string
;;                  "a"                    ;simple string
;;                  "aaa"                  ;longer string
;;                  ))
;;     (should-not (string-match-p (with-no-warnings compat--t-regexp-unmatchable) str))
;;     (when (boundp 'regexp-unmatchable)
;;       (should-not (string-match-p regexp-unmatchable str)))))

;; (ert-deftest compat-regexp-opt
;;   ;; Ensure `compat--regexp-opt' doesn't change the existing
;;   ;; behaviour:
;;   (ought (regexp-opt '("a" "b" "c")) '("a" "b" "c"))
;;   (ought (regexp-opt '("abc" "def" "ghe")) '("abc" "def" "ghe"))
;;   (ought (regexp-opt '("a" "b" "c") 'words) '("a" "b" "c") 'words)
;;   ;; Test empty list:
;;   (ought "\\(?:\\`a\\`\\)" '())
;;   (ought "\\<\\(\\`a\\`\\)\\>" '() 'words))

;; (ert-deftest compat-regexp-opt ()
;;   "Check if `compat--regexp-opt' advice was defined correctly."
;;   (let ((unmatchable "\\(?:\\`a\\`\\)"))
;;     (dolist (str '(""                   ;empty string
;;                    "a"                  ;simple string
;;                    "aaa"                ;longer string
;;                    ))
;;       (should-not (string-match-p unmatchable str)))))

;; (ert-deftest compat-assoc
;;   ;; Fallback behaviour:
;;   (ought nil 1 nil)               ;empty list
;;   (ought '(1) 1 '((1)))            ;single element list
;;   (ought nil 1 '(1))
;;   (ought '(2) 2 '((1) (2) (3)))    ;multiple element list
;;   (ought nil 2 '(1 2 3))
;;   (ought '(2) 2 '(1 (2) 3))
;;   (ought nil 2 '((1) 2 (3)))
;;   (ought '(1) 1 '((3) (2) (1)))
;;   (ought '("a") "a" '(("a") ("b") ("c")))  ;non-primitive elements
;;   (ought '("a" 0) "a" '(("c" . "a") "b" ("a" 0)))
;;   ;; With testfn (advised behaviour):
;;   (ought '(1) 3 '((10) (4) (1) (9)) #'<)
;;   (ought '("a") "b" '(("c") ("a") ("b")) #'string-lessp)
;;   (ought '("b") "a" '(("a") ("a") ("b"))
;;          (lambda (s1 s2) (not (string= s1 s2))))
;;   (ought
;;    '("\\.el\\'" . emacs-lisp-mode)
;;    "file.el"
;;    '(("\\.c\\'" . c-mode)
;;      ("\\.p\\'" . pascal-mode)
;;      ("\\.el\\'" . emacs-lisp-mode)
;;      ("\\.awk\\'" . awk-mode))
;;    #'string-match-p))

;; ;; (when (fboundp 'alist-get)
;; ;;   (ert-deftest compat-alist-get-1 ()
;; ;;     "Check if `compat--alist-get' was advised correctly."
;; ;;     (ert-deftest compat-alist-get
;; ;;       ;; Fallback behaviour:
;; ;;       (ought nil 1 nil)                      ;empty list
;; ;;       (ought 'a 1 '((1 . a)))                  ;single element list
;; ;;       (ought nil 1 '(1))
;; ;;       (ought 'b 2 '((1 . a) (2 . b) (3 . c)))  ;multiple element list
;; ;;       (ought nil 2 '(1 2 3))
;; ;;       (ought 'b 2 '(1 (2 . b) 3))
;; ;;       (ought nil 2 '((1 . a) 2 (3 . c)))
;; ;;       (ought 'a 1 '((3 . c) (2 . b) (1 . a)))
;; ;;       (ought nil "a" '(("a" . 1) ("b" . 2) ("c" . 3)))  ;non-primitive elements

;; ;;       ;; With testfn (advised behaviour):
;; ;;       (ought 1 "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal)
;; ;;       (ought 1 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<)
;; ;;       (ought '(a) "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp)
;; ;;       (ought 'c "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
;; ;;                        (lambda (s1 s2) (not (string= s1 s2))))
;; ;;       (ought 'emacs-lisp-mode
;; ;;                        "file.el"
;; ;;                        '(("\\.c\\'" . c-mode)
;; ;;                          ("\\.p\\'" . pascal-mode)
;; ;;                          ("\\.el\\'" . emacs-lisp-mode)
;; ;;                          ("\\.awk\\'" . awk-mode))
;; ;;                        nil nil #'string-match-p)
;; ;;       (ought 'd 0 '((1 . a) (2 . b) (3 . c)) 'd) ;default value
;; ;;       (ought 'd 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore))))

;; (ert-deftest (alist-get compat--alist-get-full-elisp)
;;   ;; Fallback behaviour:
;;   (ought nil 1 nil)                      ;empty list
;;   (ought 'a 1 '((1 . a)))                  ;single element list
;;   (ought nil 1 '(1))
;;   (ought 'b 2 '((1 . a) (2 . b) (3 . c)))  ;multiple element list
;;   (ought nil 2 '(1 2 3))
;;   (ought 'b 2 '(1 (2 . b) 3))
;;   (ought nil 2 '((1 . a) 2 (3 . c)))
;;   (ought 'a 1 '((3 . c) (2 . b) (1 . a)))
;;   (ought nil "a" '(("a" . 1) ("b" . 2) ("c" . 3))) ;non-primitive elements
;;   ;; With testfn (advised behaviour):
;;   (ought 1 "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal)
;;   (ought 1 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<)
;;   (ought '(a) "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp)
;;   (ought 'c "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
;;          (lambda (s1 s2) (not (string= s1 s2))))
;;   (ought 'emacs-lisp-mode
;;          "file.el"
;;          '(("\\.c\\'" . c-mode)
;;            ("\\.p\\'" . pascal-mode)
;;            ("\\.el\\'" . emacs-lisp-mode)
;;            ("\\.awk\\'" . awk-mode))
;;          nil nil #'string-match-p)
;;   (ought 'd 0 '((1 . a) (2 . b) (3 . c)) 'd) ;default value
;;   (ought 'd 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore))

;; ;; Note: as the cXXX+r implementations are relatively trivial, their
;; ;; tests are not as extensive.

;; (defvar compat-cXXXr-test
;;   '(((a . b) . (c . d)) . ((e . f) . (g . h)))
;;   "Testcase for cXXXr functions.")

;; (defvar compat-cXXXXr-test
;;   '((((a . b) . (c . d)) . ((e . f) . (g . h))) .
;;     (((i . j) . (k . l)) . ((m . j) . (o . p))))
;;   "Testcase for cXXXXr functions.")

;; (ert-deftest caaar
;;   (ought nil ())
;;   (ought 'a compat-cXXXr-test))

;; (ert-deftest caadr
;;   (ought nil ())
;;   (ought 'e compat-cXXXr-test))

;; (ert-deftest cadar
;;   (ought nil ())
;;   (ought 'c compat-cXXXr-test))

;; (ert-deftest caddr
;;   (ought nil ())
;;   (ought 'g compat-cXXXr-test))

;; (ert-deftest cdaar
;;   (ought nil ())
;;   (ought 'b compat-cXXXr-test))

;; (ert-deftest cdadr
;;   (ought nil ())
;;   (ought 'f compat-cXXXr-test))

;; (ert-deftest cddar
;;   (ought nil ())
;;   (ought 'd compat-cXXXr-test))

;; (ert-deftest cdddr
;;   (ought nil ())
;;   (ought 'h compat-cXXXr-test)
;;   #'cdddr)

;; (ert-deftest caaaar
;;   (ought nil ())
;;   (ought 'a compat-cXXXXr-test))

;; (ert-deftest caaadr
;;   (ought nil ())
;;   (ought 'i compat-cXXXXr-test))

;; (ert-deftest caadar
;;   (ought nil ())
;;   (ought 'e compat-cXXXXr-test))

;; (ert-deftest caaddr
;;   (ought nil ())
;;   (ought 'm compat-cXXXXr-test))

;; (ert-deftest cadaar
;;   (ought nil ())
;;   (ought 'c compat-cXXXXr-test))

;; (ert-deftest cadadr
;;   (ought nil ())
;;   (ought 'k compat-cXXXXr-test))

;; (ert-deftest caddar
;;   (ought nil ())
;;   (ought 'g compat-cXXXXr-test))

;; (ert-deftest cadddr
;;   (ought nil ())
;;   (ought 'o compat-cXXXXr-test))

;; (ert-deftest cdaaar
;;   (ought nil ())
;;   (ought 'b compat-cXXXXr-test))

;; (ert-deftest cdaadr
;;   (ought nil ())
;;   (ought 'j compat-cXXXXr-test))

;; (ert-deftest cdadar
;;   (ought nil ())
;;   (ought 'f compat-cXXXXr-test))

;; (ert-deftest cdaddr
;;   (ought nil ())
;;   (ought 'j compat-cXXXXr-test))

;; (ert-deftest cddaar
;;   (ought nil ())
;;   (ought 'd compat-cXXXXr-test))

;; (ert-deftest cddadr
;;   (ought nil ())
;;   (ought 'l compat-cXXXXr-test))

;; (ert-deftest cdddar
;;   (ought nil ())
;;   (ought 'h compat-cXXXXr-test))

;; (ert-deftest string-greaterp
;;   (ought t "b" "a")
;;   (ought nil "a" "b")
;;   (ought t "aaab" "aaaa")
;;   (ought nil "aaaa" "aaab"))

;; (ert-deftest compat-sort
;;   (ought (list 1 2 3) (list 1 2 3) #'<)
;;   (ought (list 1 2 3) (list 3 2 1) #'<)
;;   (ought '[1 2 3] '[1 2 3] #'<)
;;   (ought '[1 2 3] '[3 2 1] #'<))

;; (ert-deftest compat-=
;;   (ought t 0 0)
;;   (ought t 0 0 0)
;;   (ought t 0 0 0 0)
;;   (ought t 0 0 0 0 0)
;;   (ought t 0.0 0.0)
;;   (ought t +0.0 -0.0)
;;   (ought t 0.0 0.0 0.0)
;;   (ought t 0.0 0.0 0.0 0.0)
;;   (ought nil 0 1)
;;   (ought nil 0 0 1)
;;   (ought nil 0 0 0 0 1)
;;   (expect wrong-type-argument 0 0 'a)
;;   (ought nil 0 1 'a)
;;   (ought nil 0.0 0.0 0.0 0.1))

;; (ert-deftest compat-<
;;   (ought nil 0 0)
;;   (ought nil 0 0 0)
;;   (ought nil 0 0 0 0)
;;   (ought nil 0 0 0 0 0)
;;   (ought nil 0.0 0.0)
;;   (ought nil +0.0 -0.0)
;;   (ought nil 0.0 0.0 0.0)
;;   (ought nil 0.0 0.0 0.0 0.0)
;;   (ought t 0 1)
;;   (ought nil 1 0)
;;   (ought nil 0 0 1)
;;   (ought t 0 1 2)
;;   (ought nil 2 1 0)
;;   (ought nil 0 0 0 0 1)
;;   (ought t 0 1 2 3 4)
;;   (expect wrong-type-argument 0 1 'a)
;;   (ought nil 0 0 'a)
;;   (ought nil 0.0 0.0 0.0 0.1)
;;   (ought t -0.1 0.0 0.2 0.4)
;;   (ought t -0.1 0 0.2 0.4))

;; (ert-deftest compat->
;;   (ought nil 0 0)
;;   (ought nil 0 0 0)
;;   (ought nil 0 0 0 0)
;;   (ought nil 0 0 0 0 0)
;;   (ought nil 0.0 0.0)
;;   (ought nil +0.0 -0.0)
;;   (ought nil 0.0 0.0 0.0)
;;   (ought nil 0.0 0.0 0.0 0.0)
;;   (ought t 1 0)
;;   (ought nil 1 0 0)
;;   (ought nil 0 1 2)
;;   (ought t 2 1 0)
;;   (ought nil 1 0 0 0 0)
;;   (ought t 4 3 2 1 0)
;;   (ought nil 4 3 2 1 1)
;;   (expect wrong-type-argument 1 0 'a)
;;   (ought nil 0 0 'a)
;;   (ought nil 0.1 0.0 0.0 0.0)
;;   (ought t 0.4 0.2 0.0 -0.1)
;;   (ought t 0.4 0.2 0 -0.1))

;; (ert-deftest compat-<=
;;   (ought t 0 0)
;;   (ought t 0 0 0)
;;   (ought t 0 0 0 0)
;;   (ought t 0 0 0 0 0)
;;   (ought t 0.0 0.0)
;;   (ought t +0.0 -0.0)
;;   (ought t 0.0 0.0 0.0)
;;   (ought t 0.0 0.0 0.0 0.0)
;;   (ought nil 1 0)
;;   (ought nil 1 0 0)
;;   (ought t 0 1 2)
;;   (ought nil 2 1 0)
;;   (ought nil 1 0 0 0 0)
;;   (ought nil 4 3 2 1 0)
;;   (ought nil 4 3 2 1 1)
;;   (ought t 0 1 2 3 4)
;;   (ought t 1 1 2 3 4)
;;   (expect wrong-type-argument 0 0 'a)
;;   (expect wrong-type-argument 0 1 'a)
;;   (ought nil 1 0 'a)
;;   (ought nil 0.1 0.0 0.0 0.0)
;;   (ought t 0.0 0.0 0.0 0.1)
;;   (ought t -0.1 0.0 0.2 0.4)
;;   (ought t -0.1 0.0 0.0 0.2 0.4)
;;   (ought t -0.1 0.0 0 0.2 0.4)
;;   (ought t -0.1 0 0.2 0.4)
;;   (ought nil 0.4 0.2 0.0 -0.1)
;;   (ought nil 0.4 0.2 0.0 0.0 -0.1)
;;   (ought nil 0.4 0.2 0 0.0 0.0 -0.1)
;;   (ought nil 0.4 0.2 0 -0.1))

;; (ert-deftest compat->=
;;   (ought t 0 0)
;;   (ought t 0 0 0)
;;   (ought t 0 0 0 0)
;;   (ought t 0 0 0 0 0)
;;   (ought t 0.0 0.0)
;;   (ought t +0.0 -0.0)
;;   (ought t 0.0 0.0 0.0)
;;   (ought t 0.0 0.0 0.0 0.0)
;;   (ought t 1 0)
;;   (ought t 1 0 0)
;;   (ought nil 0 1 2)
;;   (ought t 2 1 0)
;;   (ought t 1 0 0 0 0)
;;   (ought t 4 3 2 1 0)
;;   (ought t 4 3 2 1 1)
;;   (expect wrong-type-argument 0 0 'a)
;;   (expect wrong-type-argument 1 0 'a)
;;   (ought nil 0 1 'a)
;;   (ought t 0.1 0.0 0.0 0.0)
;;   (ought nil 0.0 0.0 0.0 0.1)
;;   (ought nil -0.1 0.0 0.2 0.4)
;;   (ought nil -0.1 0.0 0.0 0.2 0.4)
;;   (ought nil -0.1 0.0 0 0.2 0.4)
;;   (ought nil -0.1 0 0.2 0.4)
;;   (ought t 0.4 0.2 0.0 -0.1)
;;   (ought t 0.4 0.2 0.0 0.0 -0.1)
;;   (ought t 0.4 0.2 0 0.0 0.0 -0.1)
;;   (ought t 0.4 0.2 0 -0.1))

;; (ert-deftest special-form-p
;;   (ought t 'if)
;;   (ought t 'cond)
;;   (ought nil 'when)
;;   (ought nil 'defun)
;;   (ought nil '+)
;;   (ought nil nil)
;;   (ought nil "macro")
;;   (ought nil '(macro . +)))

;; (ert-deftest macrop
;;   (ought t 'lambda)
;;   (ought t 'defun)
;;   (ought t 'defmacro)
;;   (ought nil 'defalias)
;;   (ought nil 'foobar)
;;   (ought nil 'if)
;;   (ought nil '+)
;;   (ought nil 1)
;;   (ought nil nil)
;;   (ought nil "macro")
;;   (ought t '(macro . +)))

;; (ert-deftest string-suffix-p
;;   (ought t "a" "abba")
;;   (ought t "ba" "abba")
;;   (ought t "abba" "abba")
;;   (ought nil "a" "ABBA")
;;   (ought nil "bA" "ABBA")
;;   (ought nil "aBBA" "ABBA")
;;   (ought nil "c" "ABBA")
;;   (ought nil "c" "abba")
;;   (ought nil "cddc" "abba")
;;   (ought nil "aabba" "abba"))

;; (ert-deftest compat-split-string
;;   (ought '("a" "b" "c") "a b c")
;;   (ought '("..a.." "..b.." "..c..") "..a.. ..b.. ..c..")
;;   (ought '("a" "b" "c") "..a.. ..b.. ..c.." nil nil "\\.+"))

;; (ert-deftest delete-consecutive-dups
;;   (ought '(1 2 3 4) '(1 2 3 4))
;;   (ought '(1 2 3 4) '(1 2 2 3 4 4))
;;   (ought '(1 2 3 2 4) '(1 2 2 3 2 4 4)))

;; (ert-deftest string-clean-whitespace
;;   (ought "a b c" "a b c")
;;   (ought "a b c" "   a b c")
;;   (ought "a b c" "a b c   ")
;;   (ought "a b c" "a    b c")
;;   (ought "a b c" "a b    c")
;;   (ought "a b c" "a    b    c")
;;   (ought "a b c" "   a    b    c")
;;   (ought "a b c" "a    b    c    ")
;;   (ought "a b c" "   a    b    c    ")
;;   (ought "aa bb cc" "aa bb cc")
;;   (ought "aa bb cc" "   aa bb cc")
;;   (ought "aa bb cc" "aa bb cc   ")
;;   (ought "aa bb cc" "aa    bb cc")
;;   (ought "aa bb cc" "aa bb    cc")
;;   (ought "aa bb cc" "aa    bb    cc")
;;   (ought "aa bb cc" "   aa    bb    cc")
;;   (ought "aa bb cc" "aa    bb    cc    ")
;;   (ought "aa bb cc" "   aa    bb    cc    "))

;; (ert-deftest string-fill
;;   (ought "a a a a a" "a a a a a" 9)
;;   (ought "a a a a a" "a a a a a" 10)
;;   (ought "a a a a\na" "a a a a a" 8)
;;   (ought "a a a a\na" "a  a  a  a  a" 8)
;;   (ought "a a\na a\na" "a a a a a" 4)
;;   (ought "a\na\na\na\na" "a a a a a" 2)
;;   (ought "a\na\na\na\na" "a a a a a" 1))

;; (ert-deftest string-lines
;;   (ought '("a" "b" "c") "a\nb\nc")
;;   (ought '("a" "b" "c" "") "a\nb\nc\n")
;;   (ought '("a" "b" "c") "a\nb\nc\n" t)
;;   (ought '("abc" "bcd" "cde") "abc\nbcd\ncde")
;;   (ought '(" abc" " bcd " "cde ") " abc\n bcd \ncde "))

;; (ert-deftest string-pad
;;   (ought "a   " "a" 4)
;;   (ought "aaaa" "aaaa" 4)
;;   (ought "aaaaaa" "aaaaaa" 4)
;;   (ought "a..." "a" 4 ?.)
;;   (ought "   a" "a" 4 nil t)
;;   (ought "...a" "a" 4 ?. t))

;; (ert-deftest string-chop-newline
;;   (ought "" "")
;;   (ought "" "\n")
;;   (ought "aaa" "aaa")
;;   (ought "aaa" "aaa\n")
;;   (ought "aaa\n" "aaa\n\n"))

;; (ert-deftest macroexpand-1
;;   (ought '(if a b c) '(if a b c))
;;   (ought '(if a (progn b)) '(when a b))
;;   (ought '(if a (progn (unless b c))) '(when a (unless b c))))

;; (ert-deftest compat-file-size-human-readable
;;     (ought "1000" 1000)
;;     (ought "1k" 1024)
;;     (ought "1M" (* 1024 1024))
;;     (ought "1G" (expt 1024 3))
;;     (ought "1T" (expt 1024 4))
;;     (ought "1k" 1000 'si)
;;     (ought "1KiB" 1024 'iec)
;;     (ought "1KiB" 1024 'iec)
;;     (ought "1 KiB" 1024 'iec " ")
;;     (ought "1KiA" 1024 'iec nil "A")
;;     (ought "1 KiA" 1024 'iec " " "A")
;;     (ought "1kA" 1000 'si nil "A")
;;     (ought "1 k" 1000 'si " ")
;;     (ought "1 kA" 1000 'si " " "A"))

;; (ert-deftest format-prompt
;;   (ought "Prompt: " "Prompt" nil)
;;   (ought "Prompt: " "Prompt" "")
;;   (ought "Prompt (default  ): " "Prompt" " ")
;;   (ought "Prompt (default 3): " "Prompt" 3)
;;   (ought "Prompt (default abc): " "Prompt" "abc")
;;   (ought "Prompt (default abc def): " "Prompt" "abc def")
;;   (ought "Prompt 10: " "Prompt %d" nil 10)
;;   (ought "Prompt \"abc\" (default 3): " "Prompt %S" 3 "abc"))

;; ;; TODO fix broken test
;; ;;(ert-deftest directory-files-recursively
;; ;;  (should (equal
;; ;;           (compat-sort (directory-files-recursively "." "make\\|copying") #'string<)
;; ;;           '("./.github/workflows/makefile.yml" "./COPYING" "./Makefile"))))

;; (ert-deftest directory-name-p
;;   (ought t "/")
;;   (ought nil "/file")
;;   (ought nil "/dir/file")
;;   (ought t "/dir/")
;;   (ought nil "/dir")
;;   (ought t "/dir/subdir/")
;;   (ought nil "/dir/subdir")
;;   (ought t "dir/")
;;   (ought nil "file")
;;   (ought nil "dir/file")
;;   (ought t "dir/subdir/")
;;   (ought nil "dir/subdir"))

;; (ert-deftest compat-if-let* ()
;;   "Check if `compat--t-if-let*' was implemented properly."
;;   (should
;;    (compat--t-if-let*
;;     ((x 3)
;;      (y 2)
;;      (z (+ x y))
;;      ((= z 5))
;;      (true t))
;;     true nil))
;;   (should-not
;;    (compat--t-if-let* (((= 5 6))) t nil)))

;; (ert-deftest compat-if-let ()
;;   "Check if `compat--t-if-let' was implemented properly."
;;   (should (compat--t-if-let ((e (memq 0 '(1 2 3 0 5 6))))
;;               e))
;;   (should-not (compat--t-if-let ((e (memq 0 '(1 2 3 5 6)))
;;                                (d (memq 0 '(1 2 3 0 5 6))))
;;                   t))
;;   (should-not (compat--t-if-let ((d (memq 0 '(1 2 3 0 5 6)))
;;                                (e (memq 0 '(1 2 3 5 6))))
;;                   t))
;;   (should-not
;;    (compat--t-if-let (((= 5 6))) t nil)))

;; (ert-deftest compat-and-let* ()
;;   "Check if `compat--t-and-let*' was implemented properly."
;;   (should                               ;trivial body
;;    (compat--t-and-let*
;;     ((x 3)
;;      (y 2)
;;      (z (+ x y))
;;      ((= z 5))
;;      (true t))
;;     true))
;;   (should                               ;no body
;;    (compat--t-and-let*
;;     ((x 3)
;;      (y 2)
;;      (z (+ x y))
;;      ((= z 5))
;;      (true t))))
;;   (should-not
;;    (compat--t-and-let* (((= 5 6))) t)))

;; (ert-deftest compat-json-parse-string
;;   (ought 0 "0")
;;   (ought 1 "1")
;;   (ought 0.5 "0.5")
;;   (ought [1 2 3] "[1,2,3]")
;;   (ought ["a" 2 3] "[\"a\",2,3]")
;;   (ought [["a" 2] 3] "[[\"a\",2],3]")
;;   (ought '(("a" 2) 3) "[[\"a\",2],3]" :array-type 'list)
;;   (ought 'foo "null" :null-object 'foo)
;;   (ought ["false" t] "[false, true]" :false-object "false"))

;; (ert-deftest compat-lookup-key
;;   (let ((a-map (make-sparse-keymap))
;;         (b-map (make-sparse-keymap)))
;;     (define-key a-map "x" 'foo)
;;     (define-key b-map "x" 'bar)
;;     (ought 'foo a-map "x")
;;     (ought 'bar b-map "x")
;;     (ought 'foo (list a-map b-map) "x")
;;     (ought 'bar (list b-map a-map) "x")))

;; (ert-deftest string-empty-p
;;   (ought t "")
;;   (ought nil " ")
;;   (ought t (make-string 0 ?x))
;;   (ought nil (make-string 1 ?x)))

;; (ert-deftest string-join
;;   (ought "" '(""))
;;   (ought "" '("") " ")
;;   (ought "a" '("a"))
;;   (ought "a" '("a") " ")
;;   (ought "abc" '("a" "b" "c"))
;;   (ought "a b c" '("a" "b" "c") " "))

;; (ert-deftest string-blank-p
;;   (ought 0 "")
;;   (ought 0 " ")
;;   (ought 0 (make-string 0 ?x))
;;   (ought nil (make-string 1 ?x)))

;; (ert-deftest string-remove-prefix
;;   (ought "" "" "")
;;   (ought "a" "" "a")
;;   (ought "" "a" "")
;;   (ought "bc" "a" "abc")
;;   (ought "abc" "c" "abc")
;;   (ought "bbcc" "aa" "aabbcc")
;;   (ought "aabbcc" "bb" "aabbcc")
;;   (ought "aabbcc" "cc" "aabbcc")
;;   (ought "aabbcc" "dd" "aabbcc"))

;; (ert-deftest string-remove-suffix
;;   (ought "" "" "")
;;   (ought "a" "" "a")
;;   (ought "" "a" "")
;;   (ought "abc" "a" "abc")
;;   (ought "ab" "c" "abc")
;;   (ought "aabbcc" "aa" "aabbcc")
;;   (ought "aabbcc" "bb" "aabbcc")
;;   (ought "aabb" "cc" "aabbcc")
;;   (ought "aabbcc" "dd" "aabbcc"))

;; (let ((a (bool-vector t t nil nil))
;;       (b (bool-vector t nil t nil)))
;;   (ert-deftest bool-vector-exclusive-or
;;     (ought (bool-vector nil t t nil) a b)
;;     (ought (bool-vector nil t t nil) b a)
;;     (ert-deftest compat-bool-vector-exclusive-or-sideeffect ()
;;       (let ((c (make-bool-vector 4 nil)))
;;         (compat--t-bool-vector-exclusive-or a b c)
;;         (should (equal (bool-vector nil t t nil) c))
;;         (should (equal (bool-vector nil t t nil) c))))
;;     (when (version<= "24.4" emacs-version)
;;       (expect wrong-length-argument a (bool-vector))
;;       (expect wrong-length-argument a b (bool-vector)))
;;     (expect wrong-type-argument (bool-vector) (vector))
;;     (expect wrong-type-argument (vector) (bool-vector))
;;     (expect wrong-type-argument (vector) (vector))
;;     (expect wrong-type-argument (bool-vector) (bool-vector) (vector))
;;     (expect wrong-type-argument (bool-vector) (vector) (vector))
;;     (expect wrong-type-argument (vector) (bool-vector) (vector))
;;     (expect wrong-type-argument (vector) (vector) (vector))))

;; (let ((a (bool-vector t t nil nil))
;;       (b (bool-vector t nil t nil)))
;;   (ert-deftest bool-vector-union
;;     (ought (bool-vector t t t nil) a b)
;;     (ought (bool-vector t t t nil) b a)
;;     (ert-deftest compat-bool-vector-union-sideeffect ()
;;       (let ((c (make-bool-vector 4 nil)))
;;         (compat--t-bool-vector-union a b c)
;;         (should (equal (bool-vector t t t nil) c))))
;;     (when (version<= "24.4" emacs-version)
;;       (expect wrong-length-argument a (bool-vector))
;;       (expect wrong-length-argument a b (bool-vector)))
;;     (expect wrong-type-argument (bool-vector) (vector))
;;     (expect wrong-type-argument (vector) (bool-vector))
;;     (expect wrong-type-argument (vector) (vector))
;;     (expect wrong-type-argument (bool-vector) (bool-vector) (vector))
;;     (expect wrong-type-argument (bool-vector) (vector) (vector))
;;     (expect wrong-type-argument (vector) (bool-vector) (vector))
;;     (expect wrong-type-argument (vector) (vector) (vector))))

;; (let ((a (bool-vector t t nil nil))
;;       (b (bool-vector t nil t nil)))
;;   (ert-deftest bool-vector-intersection
;;     (ought (bool-vector t nil nil nil) a b)
;;     (ought (bool-vector t nil nil nil) b a)
;;     (ert-deftest compat-bool-vector-intersection-sideeffect ()
;;       (let ((c (make-bool-vector 4 nil)))
;;         (compat--t-bool-vector-intersection a b c)
;;         (should (equal (bool-vector t nil nil nil) c))))
;;     (when (version<= "24.4" emacs-version)
;;       (expect wrong-length-argument a (bool-vector))
;;       (expect wrong-length-argument a b (bool-vector)))
;;     (expect wrong-type-argument (bool-vector) (vector))
;;     (expect wrong-type-argument (vector) (bool-vector))
;;     (expect wrong-type-argument (vector) (vector))
;;     (expect wrong-type-argument (bool-vector) (bool-vector) (vector))
;;     (expect wrong-type-argument (bool-vector) (vector) (vector))
;;     (expect wrong-type-argument (vector) (bool-vector) (vector))
;;     (expect wrong-type-argument (vector) (vector) (vector))))

;; (let ((a (bool-vector t t nil nil))
;;       (b (bool-vector t nil t nil)))
;;   (ert-deftest bool-vector-set-difference
;;     (ought (bool-vector nil t nil nil) a b)
;;     (ought (bool-vector nil nil t nil) b a)
;;     (ert-deftest compat-bool-vector-set-difference-sideeffect ()
;;       (let ((c (make-bool-vector 4 nil)))
;;         (compat--t-bool-vector-set-difference a b c)
;;         (should (equal (bool-vector nil t nil nil) c)))
;;       (let ((c (make-bool-vector 4 nil)))
;;         (compat--t-bool-vector-set-difference b a c)
;;         (should (equal (bool-vector nil nil t nil) c))))
;;     (when (version<= "24.4" emacs-version)
;;       (expect wrong-length-argument a (bool-vector))
;;       (expect wrong-length-argument a b (bool-vector)))
;;     (expect wrong-type-argument (bool-vector) (vector))
;;     (expect wrong-type-argument (vector) (bool-vector))
;;     (expect wrong-type-argument (vector) (vector))
;;     (expect wrong-type-argument (bool-vector) (bool-vector) (vector))
;;     (expect wrong-type-argument (bool-vector) (vector) (vector))
;;     (expect wrong-type-argument (vector) (bool-vector) (vector))
;;     (expect wrong-type-argument (vector) (vector) (vector))))

;; (ert-deftest bool-vector-not
;;   (ought (bool-vector) (bool-vector))
;;   (ought (bool-vector t) (bool-vector nil))
;;   (ought (bool-vector nil) (bool-vector t))
;;   (ought (bool-vector t t) (bool-vector nil nil))
;;   (ought (bool-vector t nil) (bool-vector nil t))
;;   (ought (bool-vector nil t) (bool-vector t nil))
;;   (ought (bool-vector nil nil) (bool-vector t t))
;;   (expect wrong-type-argument (vector))
;;   (expect wrong-type-argument (vector) (vector)))

;; (ert-deftest bool-vector-subsetp
;;   (ought t (bool-vector) (bool-vector))
;;   (ought t (bool-vector t) (bool-vector t))
;;   (ought t (bool-vector nil) (bool-vector t))
;;   (ought nil (bool-vector t) (bool-vector nil))
;;   (ought t (bool-vector nil) (bool-vector nil))
;;   (ought t (bool-vector t t) (bool-vector t t))
;;   (ought t (bool-vector nil nil) (bool-vector t t))
;;   (ought t (bool-vector nil nil) (bool-vector t nil))
;;   (ought t (bool-vector nil nil) (bool-vector nil t))
;;   (ought nil (bool-vector t nil) (bool-vector nil nil))
;;   (ought nil (bool-vector nil t) (bool-vector nil nil))
;;   (when (version<= "24.4" emacs-version)
;;     (expect wrong-length-argument (bool-vector nil) (bool-vector nil nil)))
;;   (expect wrong-type-argument (bool-vector) (vector))
;;   (expect wrong-type-argument (vector) (bool-vector))
;;   (expect wrong-type-argument (vector) (vector)))

;; (ert-deftest bool-vector-count-consecutive
;;   (ought 0 (bool-vector nil) (bool-vector nil) 0)
;;   (ought 0 (make-bool-vector 10 nil) t 0)
;;   (ought 10 (make-bool-vector 10 nil) nil 0)
;;   (ought 0 (make-bool-vector 10 nil) t 1)
;;   (ought 9 (make-bool-vector 10 nil) nil 1)
;;   (ought 0 (make-bool-vector 10 nil) t 1)
;;   (ought 9 (make-bool-vector 10 t) t 1)
;;   (ought 0 (make-bool-vector 10 nil) t 8)
;;   (ought 2 (make-bool-vector 10 nil) nil 8)
;;   (ought 2 (make-bool-vector 10 t) t 8)
;;   (ought 10 (make-bool-vector 10 t) (make-bool-vector 10 t) 0)
;;   (ought 4 (bool-vector t t t t nil t t t t t) t 0)
;;   (ought 0 (bool-vector t t t t nil t t t t t) t 4)
;;   (ought 5 (bool-vector t t t t nil t t t t t) t 5)
;;   (expect wrong-type-argument (vector) nil 0))

;; (ert-deftest bool-vector-count-population
;;   (ought  0 (bool-vector))
;;   (ought  0 (make-bool-vector 10 nil))
;;   (ought 10 (make-bool-vector 10 t))
;;   (ought  1 (bool-vector nil nil t nil))
;;   (ought  1 (bool-vector nil nil nil t))
;;   (ought  1 (bool-vector t nil nil nil))
;;   (ought  2 (bool-vector t nil nil t))
;;   (ought  2 (bool-vector t nil t nil))
;;   (ought  3 (bool-vector t nil t t))
;;   (expect wrong-type-argument (vector)))

;; (ert-deftest compat-assoc-delete-all
;;   (ought (list) 0 (list))
;;   ;; Test `eq'
;;   (ought '((1 . one)) 0 (list (cons 1 'one)))
;;   (ought '((1 . one) a) 0 (list (cons 1 'one) 'a))
;;   (ought '((1 . one)) 0 (list (cons 0 'zero) (cons 1 'one)))
;;   (ought '((1 . one)) 0 (list (cons 0 'zero) (cons 0 'zero) (cons 1 'one)))
;;   (ought '((1 . one)) 0 (list (cons 0 'zero) (cons 1 'one) (cons 0 'zero)))
;;   (ought '((1 . one) a) 0 (list (cons 0 'zero) (cons 1 'one) 'a  (cons 0 'zero)))
;;   (ought '(a (1 . one)) 0 (list 'a (cons 0 'zero) (cons 1 'one) (cons 0 'zero)))
;;   ;; Test `equal'
;;   (ought '(("one" . one)) "zero" (list (cons "one" 'one)))
;;   (ought '(("one" . one) a) "zero" (list (cons "one" 'one) 'a))
;;   (ought '(("one" . one)) "zero" (list (cons "zero" 'zero) (cons "one" 'one)))
;;   (ought '(("one" . one)) "zero" (list (cons "zero" 'zero) (cons "zero" 'zero) (cons "one" 'one)))
;;   (ought '(("one" . one)) "zero" (list (cons "zero" 'zero) (cons "one" 'one) (cons "zero" 'zero)))
;;   (ought '(("one" . one) a) "zero" (list (cons "zero" 'zero) (cons "one" 'one) 'a  (cons "zero" 'zero)))
;;   (ought '(a ("one" . one)) "zero" (list 'a (cons "zero" 'zero) (cons "one" 'one) (cons "zero" 'zero)))
;;   ;; Test custom predicate
;;   (ought '() 0 (list (cons 1 'one)) #'/=)
;;   (ought '(a) 0 (list (cons 1 'one) 'a) #'/=)
;;   (ought '((0 . zero)) 0 (list (cons 0 'zero) (cons 1 'one)) #'/=)
;;   (ought '((0 . zero) (0 . zero)) 0 (list (cons 0 'zero) (cons 0 'zero) (cons 1 'one)) #'/=)
;;   (ought '((0 . zero) (0 . zero)) 0 (list (cons 0 'zero) (cons 1 'one) (cons 0 'zero)) #'/=)
;;   (ought '((0 . zero) a (0 . zero)) 0 (list (cons 0 'zero) (cons 1 'one) 'a  (cons 0 'zero)) #'/=)
;;   (ought '(a (0 . zero) (0 . zero)) 0 (list 'a (cons 0 'zero) (cons 1 'one) (cons 0 'zero)) #'/=))

;; (ert-deftest color-values-from-color-spec
;;   ;; #RGB notation
;;   (ought '(0 0 0) "#000")
;;   (ought '(0 0 0) "#000000")
;;   (ought '(0 0 0) "#000000000")
;;   (ought '(0 0 0) "#000000000000")
;;   (ought '(0 0 65535) "#00F")
;;   (ought '(0 0 65535) "#0000FF")
;;   (ought '(0 0 65535) "#000000FFF")
;;   (ought '(0 0 65535) "#00000000FFFF")
;;   (ought '(0 0 65535) "#00f")
;;   (ought '(0 0 65535) "#0000ff")
;;   (ought '(0 0 65535) "#000000fff")
;;   (ought '(0 0 65535) "#00000000ffff")
;;   (ought '(0 0 65535) "#00000000ffFF")
;;   (ought '(#xffff #x0000 #x5555) "#f05")
;;   (ought '(#x1f1f #xb0b0 #xc5c5) "#1fb0C5")
;;   (ought '(#x1f83 #xb0ad #xc5e2) "#1f83b0ADC5e2")
;;   (ought nil "")
;;   (ought nil "#")
;;   (ought nil "#0")
;;   (ought nil "#00")
;;   (ought nil "#0000FG")
;;   (ought nil "#0000FFF")
;;   (ought nil "#0000FFFF")
;;   (ought '(0 4080 65535) "#0000FFFFF")
;;   (ought nil "#000FF")
;;   (ought nil "#0000F")
;;   (ought nil " #000000")
;;   (ought nil "#000000 ")
;;   (ought nil " #000000 ")
;;   (ought nil "#1f83b0ADC5e2g")
;;   (ought nil "#1f83b0ADC5e20")
;;   (ought nil "#12345")
;;   ;; rgb: notation
;;   (ought '(0 0 0) "rgb:0/0/0")
;;   (ought '(0 0 0) "rgb:0/0/00")
;;   (ought '(0 0 0) "rgb:0/00/000")
;;   (ought '(0 0 0) "rgb:0/000/0000")
;;   (ought '(0 0 0) "rgb:000/0000/0")
;;   (ought '(0 0 65535) "rgb:000/0000/F")
;;   (ought '(65535 0 65535) "rgb:FFF/0000/F")
;;   (ought '(65535 0 65535) "rgb:FFFF/0000/FFFF")
;;   (ought '(0 255 65535) "rgb:0/00FF/FFFF")
;;   (ought '(#xffff #x2323 #x28a2) "rgb:f/23/28a")
;;   (ought '(#x1234 #x5678 #x09ab) "rgb:1234/5678/09ab")
;;   (ought nil "rgb:/0000/FFFF")
;;   (ought nil "rgb:0000/0000/FFFG")
;;   (ought nil "rgb:0000/0000/FFFFF")
;;   (ought nil "rgb:0000/0000")
;;   (ought nil "rg:0000/0000/0000")
;;   (ought nil "rgb: 0000/0000/0000")
;;   (ought nil "rgbb:0000/0000/0000")
;;   (ought nil "rgb:0000/0000/0000   ")
;;   (ought nil " rgb:0000/0000/0000  ")
;;   (ought nil "  rgb:0000/0000/0000")
;;   (ought nil "rgb:0000/ 0000 /0000")
;;   (ought nil "rgb: 0000 /0000 /0000")
;;   (ought nil "rgb:0//0")
;;   ;; rgbi: notation
;;   (ought '(0 0 0) "rgbi:0/0/0")
;;   (ought '(0 0 0) "rgbi:0.0/0.0/0.0")
;;   (ought '(0 0 0) "rgbi:0.0/0/0")
;;   (ought '(0 0 0) "rgbi:0.0/0/0")
;;   (ought '(0 0 0) "rgbi:0/0/0.")
;;   (ought '(0 0 0) "rgbi:0/0/0.0000")
;;   (ought '(0 0 0) "rgbi:0/0/.0")
;;   (ought '(0 0 0) "rgbi:0/0/.0000")
;;   (ought '(65535 0 0) "rgbi:1/0/0.0000")
;;   (ought '(65535 0 0) "rgbi:1./0/0.0000")
;;   (ought '(65535 0 0) "rgbi:1.0/0/0.0000")
;;   (ought '(65535 32768 0) "rgbi:1.0/0.5/0.0000")
;;   (ought '(6554 21843 65469) "rgbi:0.1/0.3333/0.999")
;;   (ought '(0 32768 6554) "rgbi:0/0.5/0.1")
;;   (ought '(66 655 65535) "rgbi:1e-3/1.0e-2/1e0")
;;   (ought '(6554 21843 65469) "rgbi:1e-1/+0.3333/0.00999e2")
;;   (ought nil "rgbi:1.0001/0/0")
;;   (ought nil "rgbi:2/0/0")
;;   (ought nil "rgbi:0.a/0/0")
;;   (ought nil "rgbi:./0/0")
;;   (ought nil "rgbi:./0/0")
;;   (ought nil " rgbi:0/0/0")
;;   (ought nil "rgbi:0/0/0 ")
;;   (ought nil "	rgbi:0/0/0 ")
;;   (ought nil "rgbi:0 /0/ 0")
;;   (ought nil "rgbi:0/ 0 /0")
;;   (ought nil "rgbii:0/0/0")
;;   (ought nil "rgbi :0/0/0")
;;   ;; strtod ignores leading whitespace, making these legal colour
;;   ;; specifications:
;;   ;;
;;   ;; (ought nil "rgbi: 0/0/0")
;;   ;; (ought nil "rgbi: 0/ 0/ 0")
;;   (ought nil "rgbi : 0/0/0")
;;   (ought nil "rgbi:0/0.5/10"))

;; (ert-deftest file-modes-number-to-symbolic
;;   (ought "-rwx------" #o700)
;;   (ought "-rwxrwx---" #o770)
;;   (ought "-rwx---rwx" #o707)
;;   (ought "-rw-r-xr--" #o654)
;;   (ought "--wx-w---x" #o321)
;;   (ought "drwx------" #o700 ?d)
;;   (ought "?rwx------" #o700 ??)
;;   (ought "lrwx------" #o120700)
;;   (ought "prwx------" #o10700)
;;   (ought "-rwx------" #o30700))

;; (ert-deftest file-local-name
;;   (ought "" "")
;;   (ought "foo" "foo")
;;   (ought "/bar/foo" "/bar/foo")
;;   ;; These tests fails prior to Emacs 26, because /ssh:foo was a valid
;;   ;; TRAMP path back then.
;;   ;;
;;   ;; (ought "/ssh:foo" "/ssh:foo")
;;   ;; (ought "/ssh:/bar/foo" "/ssh:/bar/foo")
;;   (ought "foo" "/ssh::foo")
;;   (ought "/bar/foo" "/ssh::/bar/foo")
;;   (ought ":foo" "/ssh:::foo")
;;   (ought ":/bar/foo" "/ssh:::/bar/foo"))

;; (ert-deftest file-name-quoted-p
;;   (ought nil "")
;;   (ought t "/:")
;;   (ought nil "//:")
;;   (ought t "/::")
;;   (ought nil "/ssh::")
;;   (ought nil "/ssh::a")
;;   (ought t "/ssh::/:a")
;;   ;; These tests fails prior to Emacs 26, because /ssh:foo was a valid
;;   ;; TRAMP path back then.
;;   ;;
;;   ;; (ought nil "/ssh:/:a")
;;   )

;; (ert-deftest file-name-quote
;;   (ought "/:" "")
;;   (ought "/::" ":")
;;   (ought "/:/" "/")
;;   (ought "/:" "/:")
;;   (ought "/:a" "a")
;;   (ought "/::a" ":a")
;;   (ought "/:/a" "/a")
;;   (ought "/:a" "/:a")
;;   (ought (concat "/ssh:" (system-name) ":/:a") "/ssh::a"))

;; (ert-deftest make-lock-file-name
;;   (ought (expand-file-name ".#") "")
;;   (ought (expand-file-name ".#a") "a")
;;   (ought (expand-file-name ".#foo") "foo")
;;   (ought (expand-file-name ".#.") ".")
;;   (ought (expand-file-name ".#.#") ".#")
;;   (ought (expand-file-name ".#.a") ".a")
;;   (ought (expand-file-name ".#.#") ".#")
;;   (ought (expand-file-name "a/.#") "a/")
;;   (ought (expand-file-name "a/.#b") "a/b")
;;   (ought (expand-file-name "a/.#.#") "a/.#")
;;   (ought (expand-file-name "a/.#.") "a/.")
;;   (ought (expand-file-name "a/.#.b") "a/.b")
;;   (ought (expand-file-name "a/.#foo") "a/foo")
;;   (ought (expand-file-name "bar/.#b") "bar/b")
;;   (ought (expand-file-name "bar/.#foo") "bar/foo"))

;; (ert-deftest time-equal-p
;;   (ought t nil nil)

;;   ;; FIXME: Testing these values can be tricky, because the timestamp
;;   ;; might change between evaluating (current-time) and evaluating
;;   ;; `time-equal-p', especially in the interpreted compatibility
;;   ;; version.

;;   ;; (ought t (current-time) nil)
;;   ;; (ought t nil (current-time))

;;   ;; While `sleep-for' returns nil, indicating the current time, this
;;   ;; behaviour seems to be undefined.  Relying on it is therefore not
;;   ;; advised.
;;   (ought nil (current-time) (ignore (sleep-for 0.01)))
;;   (ought nil (current-time) (progn
;;                               (sleep-for 0.01)
;;                               (current-time)))
;;   (ought t '(1 2 3 4) '(1 2 3 4))
;;   (ought nil '(1 2 3 4) '(1 2 3 5))
;;   (ought nil '(1 2 3 5) '(1 2 3 4))
;;   (ought nil '(1 2 3 4) '(1 2 4 4))
;;   (ought nil '(1 2 4 4) '(1 2 3 4))
;;   (ought nil '(1 2 3 4) '(1 3 3 4))
;;   (ought nil '(1 3 3 4) '(1 2 3 4))
;;   (ought nil '(1 2 3 4) '(2 2 3 4))
;;   (ought nil '(2 2 3 4) '(1 2 3 4)))

;; (ert-deftest date-days-in-month
;;   (ought 31 2020 1)
;;   (ought 30 2020 4)
;;   (ought 29 2020 2)
;;   (ought 28 2021 2))

;; (ert-deftest decoded-time-period
;;   (ought 0 '())
;;   (ought 0 '(0))
;;   (ought 1 '(1))
;;   (ought 0.125 '((1 . 8)))

;;   (ought 60 '(0 1))
;;   (ought 61 '(1 1))
;;   (ought -59 '(1 -1))

;;   (ought (* 60 60) '(0 0 1))
;;   (ought (+ (* 60 60) 60) '(0 1 1))
;;   (ought (+ (* 60 60) 120 1) '(1 2 1))

;;   (ought (* 60 60 24) '(0 0 0 1))
;;   (ought (+ (* 60 60 24) 1) '(1 0 0 1))
;;   (ought (+ (* 60 60 24) (* 60 60) 60 1) '(1 1 1 1))
;;   (ought (+ (* 60 60 24) (* 60 60) 120 1) '(1 2 1 1))

;;   (ought (* 60 60 24 30) '(0 0 0 0 1))
;;   (ought (+ (* 60 60 24 30) 1) '(1 0 0 0 1))
;;   (ought (+ (* 60 60 24 30) 60 1) '(1 1 0 0 1))
;;   (ought (+ (* 60 60 24 30) (* 60 60) 60 1)
;;          '(1 1 1 0 1))
;;   (ought (+ (* 60 60 24 30) (* 60 60 24) (* 60 60) 120 1)
;;          '(1 2 1 1 1))

;;   (ought (* 60 60 24 365) '(0 0 0 0 0 1))
;;   (ought (+ (* 60 60 24 365) 1)
;;          '(1 0 0 0 0 1))
;;   (ought (+ (* 60 60 24 365) 60 1)
;;          '(1 1 0 0 0 1))
;;   (ought (+ (* 60 60 24 365) (* 60 60) 60 1)
;;          '(1 1 1 0 0 1))
;;   (ought (+ (* 60 60 24 365) (* 60 60 24) (* 60 60) 60 1)
;;          '(1 1 1 1 0 1))
;;   (ought (+ (* 60 60 24 365)
;;             (* 60 60 24 30)
;;             (* 60 60 24)
;;             (* 60 60)
;;             120 1)
;;          '(1 2 1 1 1 1))

;;   (expect wrong-type-argument 'a)
;;   (expect wrong-type-argument '(0 a))
;;   (expect wrong-type-argument '(0 0 a))
;;   (expect wrong-type-argument '(0 0 0 a))
;;   (expect wrong-type-argument '(0 0 0 0 a))
;;   (expect wrong-type-argument '(0 0 0 0 0 a)))

;; ;; TODO func-arity seems broken
;; ;; (ert-deftest func-arity
;; ;;   (should (equal '(0 . 0) (func-arity (lambda ()))))
;; ;;   (should (equal '(1 . 1) (func-arity (lambda (x) x))))
;; ;;   (should (equal '(1 . 2) (func-arity (lambda (x &optional _) x))))
;; ;;   (should (equal '(0 . many) (func-arity (lambda (&rest _)))))
;; ;;   (ought '(1 . 1) 'identity)
;; ;;   (ought '(0 . many) 'ignore)
;; ;;   (ought '(2 . many) 'defun)
;; ;;   (ought '(2 . 3) 'defalias)
;; ;;   (ought '(1 . unevalled) 'defvar))

;; (ert-deftest subr-primitive-p
;;   (ought t (symbol-function 'identity))       ;function from fns.c
;;   (unless (fboundp 'subr-native-elisp-p)
;;     (ought nil (symbol-function 'match-string))) ;function from subr.el
;;   (ought nil (symbol-function 'defun))        ;macro from subr.el
;;   (ought nil nil))

;; (ert-deftest file-name-absolute-p   ;assuming unix
;;   (ought t "/")
;;   (ought t "/a")
;;   (ought nil "a")
;;   (ought nil "a/b")
;;   (ought nil "a/b/")
;;   (ought t "~")
;;   (when (version< "27.1" emacs-version)
;;     (ought t "~/foo")
;;     (ought nil "~foo")
;;     (ought nil "~foo/"))
;;   (ought t "~root")
;;   (ought t "~root/")
;;   (ought t "~root/file"))

;; (let ((one (make-symbol "1"))
;;       (two (make-symbol "2"))
;;       (three (make-symbol "3"))
;;       (one.5 (make-symbol "1.5"))
;;       (eins (make-symbol "𝟙")))
;;   (put two 'derived-mode-parent one)
;;   (put one.5 'derived-mode-parent one)
;;   (put three 'derived-mode-parent two)
;;   (ert-deftest provided-mode-derived-p
;;     (ought one one one)
;;     (ought one two one)
;;     (ought one three one)
;;     (ought nil one eins)
;;     (ought nil two eins)
;;     (ought nil two one.5)
;;     (ought one two one.5 one)
;;     (ought two two one.5 two)
;;     (ought one three one.5 one)
;;     (ought two three one.5 one two)
;;     (ought two three one.5 two one)
;;     (ought three three one.5 two one three)
;;     (ought three three one.5 three two one)))

;; (unless (fboundp 'make-prop-match)
;;   (defalias 'make-prop-match
;;     (if (version< emacs-version "26.1")
;;         #'compat--make-prop-match-with-vector
;;       #'compat--make-prop-match-with-record)))

(provide 'compat-tests)
;;; compat-tests.el ends here
