;;; compat-tests.el --- Tests for compat.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Package-Requires: ((emacs "28.1"))

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
;; Note that not all functions have tests (yet), for the most part
;; because it is difficult to create a reproducible environment to
;; test these in or their tests are trivial -- or a combination of
;; both.
;;
;; Tests are welcome, but until then anyone working on these functions
;; should take care to test these manually.

;;; Code:

(require 'ert)

(require 'compat-macs)
(defvar compat-testing)
(setq compat--generate-function #'compat--generate-verbose)
(let ((compat-testing t))
  (load "compat.el"))

(defvar compat-test-counter)

(defun compat--ought (name compat)
  "Implementation for the `ought' macro for NAME.
COMPAT is the name of the compatibility function the behaviour is
being compared against."
  (lambda (result &rest args)
    (let ((real-test (intern (format "%s-%04d-actual/ought" compat compat-test-counter)))
          (comp-test (intern (format "%s-%04d-compat/ought" compat compat-test-counter))))
      (setq compat-test-counter (1+ compat-test-counter))
      (macroexp-progn
       (list (and (fboundp name)
                  (or (not (get compat 'compat-version))
                      (version<= emacs-version (get compat 'compat-version)))
                  `(ert-set-test
                    ',real-test
                    (make-ert-test
                     :name ',real-test
                     :tags '(,name)
                     :body (lambda () (should (equal ,result (,name ,@args)))))))
             (and (fboundp compat)
                  `(ert-set-test
                    ',comp-test
                    (make-ert-test
                     :name ',comp-test
                     :tags '(,name)
                     :body (lambda () (should (equal ,result (,compat ,@args))))))))))))

(defun compat--expect (name compat)
  "Implementation for the `expect' macro for NAME.
COMPAT is the name of the compatibility function the behaviour is
being compared against."
  (lambda (error-spec &rest args)
    (let ((real-test (intern (format "%s-%04d-actual/expect" compat compat-test-counter)))
          (comp-test (intern (format "%s-%04d-compat/expect" compat compat-test-counter)))
          (error-type (if (consp error-spec) (car error-spec) error-spec)))
      (setq compat-test-counter (1+ compat-test-counter))
      (macroexp-progn
       (list (and (fboundp name)
                  (or (not (get compat 'compat-version))
                      (version<= emacs-version (get compat 'compat-version)))
                  `(ert-set-test
                    ',real-test
                    (make-ert-test
                     :name ',real-test
                     :tags '(,name)
                     :body (lambda ()
                             (should
                              (let ((res (should-error (,name ,@args) :type ',error-type)))
                                (should
                                 ,(if (consp error-spec)
                                      `(equal res ',error-spec)
                                    `(eq (car res) ',error-spec)))))))))
             (and (fboundp compat)
                  `(ert-set-test
                    ',comp-test
                    (make-ert-test
                     :name ',comp-test
                     :tags '(,name)
                     :body (lambda ()
                             (should
                              (let ((res (should-error (,name ,@args) :type ',error-type)))
                                (should
                                 ,(if (consp error-spec)
                                      `(equal res ',error-spec)
                                    `(eq (car res) ',error-spec))))))))))))))

(defmacro compat-deftests (name &rest body)
  "Test NAME in BODY."
  (declare (debug (sexp &rest body))
           (indent 1))
  (let* ((compat-test-counter 0)
         (real-name (if (consp name) (car name) name))
         (compat-name (if (consp name)
                          (cadr name)
                        (intern (format "compat--%s" real-name))))
         (env (list
               (cons 'ought (compat--ought real-name compat-name))
               (cons 'expect (compat--expect real-name compat-name)))))
    (and (or (not (get compat-name 'compat-min-version))
             (version< (get compat-name 'compat-min-version) emacs-version))
         (or (not (get compat-name 'compat-max-version))
             (version< emacs-version (get compat-name 'compat-max-version)))
         (macroexpand-all
          (macroexp-progn body)
          (append env macroexpand-all-environment)))))



(compat-deftests string-search
  ;; Find needle at the beginning of a haystack:
  (ought 0 "a" "abb")
  ;; Find needle at the begining of a haystack, with more potential
  ;; needles that could be found:
  (ought 0 "a" "abba")
  ;; Find needle with more than one charachter at the beginning of
  ;; a line:
  (ought 0 "aa" "aabbb")
  ;; Find a needle midstring:
  (ought 1 "a" "bab")
  ;; Find a needle at the end:
  (ought 2 "a" "bba")
  ;; Find a longer needle midstring:
  (ought 1 "aa" "baab")
  ;; Find a longer needle at the end:
  (ought 2 "aa" "bbaa")
  ;; Find a case-sensitive needle:
  (ought 2 "a" "AAa")
  ;; Find another case-sensitive needle:
  (ought 2 "aa" "AAaa")
  ;; Test regular expression quoting (1):
  (ought 5 "." "abbbb.b")
  ;; Test regular expression quoting (2):
  (ought 5 ".*" "abbbb.*b")
  ;; Attempt to find non-existent needle:
  (ought nil "a" "bbb")
  ;; Attempt to find non-existent needle that has the form of a
  ;; regular expression:
  (ought nil "." "bbb")
  ;; Handle empty string as needle:
  (ought 0 "" "abc")
  ;; Handle empty string as haystack:
  (ought nil "a" "")
  ;; Handle empty string as needle and haystack:
  (ought 0 "" "")
  ;; Handle START argument:
  (ought 3 "a" "abba" 1)
  ;; Additional test copied from:
  (ought 6 "zot" "foobarzot")
  (ought 0 "foo" "foobarzot")
  (ought nil "fooz" "foobarzot")
  (ought nil "zot" "foobarzo")
  (ought 0 "ab" "ab")
  (ought nil "ab\0" "ab")
  (ought 4 "ab" "abababab" 3)
  (ought nil "ab" "ababac" 3)
  (ought nil "aaa" "aa")
  ;; The `make-string' calls with three arguments have been replaced
  ;; here with the result of their evaluation, to avoid issues with
  ;; older versions of Emacs that only support two arguments.
  (ought 5
                  (make-string 2 130)
                  ;; Per (concat "helló" (make-string 5 130 t) "bár")
                  "hellóbár")
  (ought 5
                  (make-string 2 127)
                  ;; Per (concat "helló" (make-string 5 127 t) "bár")
                  "hellóbár")
  (ought 1 "\377" "a\377ø")
  (ought 1 "\377" "a\377a")
  (ought nil (make-string 1 255) "a\377ø")
  (ought nil (make-string 1 255) "a\377a")
  (ought 3 "fóo" "zotfóo")
  (ought nil "\303" "aøb")
  (ought nil "\270" "aøb")
  (ought nil "ø" "\303\270")
  (ought nil "ø" (make-string 32 ?a))
  (ought nil "ø" (string-to-multibyte (make-string 32 ?a)))
  (ought 14 "o" (string-to-multibyte
                          (apply #'string (number-sequence ?a ?z))))
  (ought 2 "a\U00010f98z" "a\U00010f98a\U00010f98z")
  (expect (args-out-of-range -1) "a" "abc" -1)
  (expect (args-out-of-range 4) "a" "abc" 4)
  (expect (args-out-of-range 100000000000)
                 "a" "abc" 100000000000)
  (ought nil "a" "aaa" 3)
  (ought nil "aa" "aa" 1)
  (ought nil "\0" "")
  (ought 0 "" "")
  (expect (args-out-of-range 1) "" "" 1)
  (ought 0 "" "abc")
  (ought 2 "" "abc" 2)
  (ought 3 "" "abc" 3)
  (expect (args-out-of-range 4) "" "abc" 4)
  (expect (args-out-of-range -1) "" "abc" -1)
  (ought nil "ø" "foo\303\270")
  (ought nil "\303\270" "ø")
  (ought nil "\370" "ø")
  (ought nil (string-to-multibyte "\370") "ø")
  (ought nil "ø" "\370")
  (ought nil "ø" (string-to-multibyte "\370"))
  (ought nil "\303\270" "\370")
  (ought nil (string-to-multibyte "\303\270") "\370")
  (ought nil "\303\270" (string-to-multibyte "\370"))
  (ought nil
                  (string-to-multibyte "\303\270")
                  (string-to-multibyte "\370"))
  (ought nil "\370" "\303\270")
  (ought nil (string-to-multibyte "\370") "\303\270")
  (ought nil "\370" (string-to-multibyte "\303\270"))
  (ought nil
                  (string-to-multibyte "\370")
                  (string-to-multibyte "\303\270"))
  (ought 3 "\303\270" "foo\303\270")
  (when (version<= "27" emacs-version)
    ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1 in
    ;; emacs.git fixes the behaviour of regular expressions matching
    ;; raw bytes.  The compatibility functions should updated to
    ;; backport this behaviour.
    (ought 2 (string-to-multibyte "\377") "ab\377c")
    (ought 2
                    (string-to-multibyte "o\303\270")
                    "foo\303\270")))

(compat-deftests string-replace
  (ought "bba" "aa" "bb" "aaa")
  (ought "AAA" "aa" "bb" "AAA")
  ;; Additional test copied from subr-tests.el:
  (ought "zot" "foo" "bar" "zot")
  (ought "barzot" "foo" "bar" "foozot")
  (ought "barbarzot" "foo" "bar" "barfoozot")
  (ought "barfoobar" "zot" "bar" "barfoozot")
  (ought "barfoobarot" "z" "bar" "barfoozot")
  (ought "zat" "zot" "bar" "zat")
  (ought "zat" "azot" "bar" "zat")
  (ought "bar" "azot" "bar" "azot")
  (ought "foozotbar" "azot" "bar" "foozotbar")
  (ought "labarbarbarzot" "fo" "bar" "lafofofozot")
  (ought "axb" "\377" "x" "a\377b")
  (ought "axø" "\377" "x" "a\377ø")
  (when (version<= "27" emacs-version)
    ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1
    ;; in emacs.git fixes the behaviour of regular
    ;; expressions matching raw bytes.  The compatibility
    ;; functions should updated to backport this
    ;; behaviour.
    (ought "axb" (string-to-multibyte "\377") "x" "a\377b")
    (ought "axø" (string-to-multibyte "\377") "x" "a\377ø"))
  (ought "ANAnas" "ana" "ANA" "ananas")
  (ought "" "a" "" "")
  (ought "" "a" "" "aaaaa")
  (ought "" "ab" "" "ababab")
  (ought "ccc" "ab" "" "abcabcabc")
  (ought "aaaaaa" "a" "aa" "aaa")
  (ought "defg" "abc" "defg" "abc")
  (when (version<= "24.4" emacs-version)
    ;; FIXME: Emacs 24.3 do not know of `wrong-length-argument' and
    ;; therefore fail this test, even if the right symbol is being
    ;; thrown.
    (expect wrong-length-argument "" "x" "abc")))

(compat-deftests length=
  (ought t '() 0)                  ;empty list
  (ought t '(1) 1)			;single element
  (ought t '(1 2 3) 3)             ;multiple elements
  (ought nil '(1 2 3) 2)           ;less than
  (ought nil '(1) 0)
  (ought nil '(1 2 3) 4)           ;more than
  (ought nil '(1) 2)
  (ought nil '() 1)
  (ought t [] 0)                   ;empty vector
  (ought t [1] 1)			;single element vector
  (ought t [1 2 3] 3)              ;multiple element vector
  (ought nil [1 2 3] 2)            ;less than
  (ought nil [1 2 3] 4)            ;more than
  (expect wrong-type-argument 3 nil))

(compat-deftests length<
  (ought nil '(1) 0)               ;single element
  (ought nil '(1 2 3) 2)           ;multiple elements
  (ought nil '(1 2 3) 3)           ;equal length
  (ought nil '(1) 1)
  (ought t '(1 2 3) 4)             ;more than
  (ought t '(1) 2)
  (ought t '() 1)
  (ought nil [1] 0)                ;single element vector
  (ought nil [1 2 3] 2)            ;multiple element vector
  (ought nil [1 2 3] 3)            ;equal length
  (ought t [1 2 3] 4)              ;more than
  (expect wrong-type-argument 3 nil))

(compat-deftests length>
  (ought t '(1) 0)			;single element
  (ought t '(1 2 3) 2)             ;multiple elements
  (ought nil '(1 2 3) 3)           ;equal length
  (ought nil '(1) 1)
  (ought nil '(1 2 3) 4)           ;more than
  (ought nil '(1) 2)
  (ought nil '() 1)
  (ought t [1] 0)			;single element vector
  (ought t [1 2 3] 2)              ;multiple element vector
  (ought nil [1 2 3] 3)            ;equal length
  (ought nil [1 2 3] 4)            ;more than
  (expect wrong-type-argument 3 nil))

(compat-deftests always
  (ought t)                        ;no arguments
  (ought t 1)                      ;single argument
  (ought t 1 2 3 4))              ;multiple arguments

(ert-deftest compat-insert-into-buffer ()
  "Check if `insert-into-buffer' was implemented correctly."
  ;; Without optional compat--arguments
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
	(insert "def")
	(compat--insert-into-buffer other))
      (should (string= (buffer-string) "abcdef"))))
  (when (fboundp 'insert-into-buffer)
    (with-temp-buffer
      (let ((other (current-buffer)))
	(insert "abc")
	(with-temp-buffer
	  (insert "def")
	  (insert-into-buffer other))
	(should (string= (buffer-string) "abcdef")))))
  ;; With one optional argument
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
	(insert "def")
	(compat--insert-into-buffer other 2))
      (should (string= (buffer-string) "abcef"))))
  (when (fboundp 'insert-into-buffer)
    (with-temp-buffer
      (let ((other (current-buffer)))
	(insert "abc")
	(with-temp-buffer
	  (insert "def")
	  (insert-into-buffer other 2))
	(should (string= (buffer-string) "abcef")))))
  ;; With two optional arguments
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
	(insert "def")
	(compat--insert-into-buffer other 2 3))
      (should (string= (buffer-string) "abce"))))
  (when (fboundp 'insert-into-buffer)
    (with-temp-buffer
      (let ((other (current-buffer)))
	(insert "abc")
	(with-temp-buffer
	  (insert "def")
	  (insert-into-buffer other 2 3))
	(should (string= (buffer-string) "abce"))))))

(compat-deftests file-name-with-extension
  (ought "file.ext" "file" "ext")
  (ought "file.ext" "file" ".ext")
  (ought "file.ext" "file." ".ext")
  (ought "file..ext" "file.." ".ext")
  (ought "file..ext" "file." "..ext")
  (ought "file...ext" "file.." "..ext")
  (ought "/abs/file.ext" "/abs/file" "ext")
  (ought "/abs/file.ext" "/abs/file" ".ext")
  (ought "/abs/file.ext" "/abs/file." ".ext")
  (ought "/abs/file..ext" "/abs/file.." ".ext")
  (ought "/abs/file..ext" "/abs/file." "..ext")
  (ought "/abs/file...ext" "/abs/file.." "..ext")
  (expect error "file" "")
  (expect error "" "ext")
  (expect error "file" "")
  (expect error "rel/" "ext")
  (expect error "/abs/" "ext"))

(compat-deftests compat-string-width
  (ought 0 "")
  (ought 3 "abc")			;no argument
  (ought 5 "abcあ")
  (ought (1+ tab-width) "a	")
  (ought 2 "abc" 1)               ;with from
  (ought 4 "abcあ" 1)
  (ought tab-width "a	" 1)
  (ought 2 "abc" 0 2)             ;with to
  (ought 3 "abcあ" 0 3)
  (ought 1 "a	" 0 1)
  (ought 1 "abc" 1 2)             ;with from and to
  (ought 2 "abcあ" 3 4)
  (ought 0 "a	" 1 1))

(compat-deftests ensure-list
  (ought nil nil)                        ;empty list
  (ought '(1) '(1))                        ;single element list
  (ought '(1 2 3) '(1 2 3))                ;multiple element list
  (ought '(1) 1))                          ;atom

(compat-deftests (proper-list-p compat--proper-list-p-length-signal)
  (ought 0 ())				;empty list
  (ought 1 '(1))				;single element
  (ought 3 '(1 2 3))			;multiple elements
  (ought nil '(1 . 2))			;cons
  (ought nil '(1 2 . 3))			;dotted
  (ought nil (let ((l (list 1 2 3)))		;circular
               (setf (nthcdr 3 l) l)
               l))
  (ought nil 1)                       ;non-lists
  (ought nil "")
  (ought nil "abc")
  (ought nil [])
  (ought nil [1 2 3]))

(compat-deftests (proper-list-p compat--proper-list-p-tortoise-hare)
  (ought 0 ())				;empty list
  (ought 1 '(1))                        ;single element
  (ought 3 '(1 2 3))			;multiple elements
  (ought nil '(1 . 2))			;cons
  (ought nil '(1 2 . 3))                ;dotted
  (ought nil (let ((l (list 1 2 3)))    ;circular
               (setf (nthcdr 3 l) l)
               l))
  (ought nil 1)                         ;non-lists
  (ought nil "")
  (ought nil "abc")
  (ought nil [])
  (ought nil [1 2 3]))

(compat-deftests flatten-tree
  ;; Example from docstring:
  (ought '(1 2 3 4 5 6 7) '(1 (2 . 3) nil (4 5 (6)) 7))
  ;; Trivial example
  (ought nil ())
  ;; Simple examples
  (ought '(1) '(1))
  (ought '(1 2) '(1 2))
  (ought '(1 2 3) '(1 2 3))
  ;; Regular sublists
  (ought '(1) '((1)))
  (ought '(1 2) '((1) (2)))
  (ought '(1 2 3) '((1) (2) (3)))
  ;; Complex examples
  (ought '(1) '(((((1))))))
  (ought '(1 2 3 4) '((1) nil 2 ((3 4))))
  (ought '(1 2 3 4) '(((1 nil)) 2 (((3 nil nil) 4)))))

(compat-deftests xor
  (ought t t nil)
  (ought t nil t)
  (ought nil nil nil)
  (ought nil t t))

(compat-deftests string-distance
  (ought 3 "kitten" "sitting")     ;from wikipedia
  (if (version<= "28" emacs-version) ;trivial examples
      (ought 0 "" "")
    ;; Up until Emacs 28, `string-distance' had a bug
    ;; when comparing two empty strings. This was fixed
    ;; in the following commit:
    ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=c44190c
    ;;
    ;; Therefore, we must make sure, that the test
    ;; doesn't fail because of this bug:
    (should (= (compat--string-distance "" "") 0)))
  (ought 0 "a" "a")
  (ought 1 "" "a")
  (ought 1 "b" "a")
  (ought 2 "aa" "bb")
  (ought 2 "aa" "bba")
  (ought 2 "aaa" "bba")
  (ought 3 "a" "あ" t)             ;byte example
  (ought 1 "a" "あ"))

(ert-deftest compat-regexp-unmatchable ()
  "Check if `compat--string-distance' was implemented correctly."
  (dolist (str '(""                     ;empty string
                 "a"                    ;simple string
                 "aaa"                  ;longer string
                 ))
    (should-not (string-match-p (with-no-warnings compat--regexp-unmatchable) str))
    (when (boundp 'regexp-unmatchable)
      (should-not (string-match-p regexp-unmatchable str)))))

(compat-deftests compat-regexp-opt
  ;; Ensure `compat--regexp-opt' doesn't change the existing
  ;; behaviour:
  (ought (regexp-opt '("a" "b" "c")) '("a" "b" "c"))
  (ought (regexp-opt '("abc" "def" "ghe")) '("abc" "def" "ghe"))
  (ought (regexp-opt '("a" "b" "c") 'words) '("a" "b" "c") 'words)
  ;; Test empty list:
  (ought "\\(?:\\`a\\`\\)" '())
  (ought "\\<\\(\\`a\\`\\)\\>" '() 'words))

(ert-deftest compat-regexp-opt ()
  "Check if `compat--regexp-opt' advice was defined correctly."
  (let ((unmatchable "\\(?:\\`a\\`\\)"))
    (dolist (str '(""                   ;empty string
                   "a"                  ;simple string
                   "aaa"                ;longer string
                   ))
      (should-not (string-match-p unmatchable str)))))

(compat-deftests compat-assoc
  ;; Fallback behaviour:
  (ought nil 1 nil)               ;empty list
  (ought '(1) 1 '((1)))            ;single element list
  (ought nil 1 '(1))
  (ought '(2) 2 '((1) (2) (3)))    ;multiple element list
  (ought nil 2 '(1 2 3))
  (ought '(2) 2 '(1 (2) 3))
  (ought nil 2 '((1) 2 (3)))
  (ought '(1) 1 '((3) (2) (1)))
  (ought '("a") "a" '(("a") ("b") ("c")))  ;non-primitive elements
  (ought '("a" 0) "a" '(("c" . "a") "b" ("a" 0)))
  ;; With testfn (advised behaviour):
  (ought '(1) 3 '((10) (4) (1) (9)) #'<)
  (ought '("a") "b" '(("c") ("a") ("b")) #'string-lessp)
  (ought '("b") "a" '(("a") ("a") ("b"))
         (lambda (s1 s2) (not (string= s1 s2))))
  (ought
   '("\\.el\\'" . emacs-lisp-mode)
   "file.el"
   '(("\\.c\\'" . c-mode)
     ("\\.p\\'" . pascal-mode)
     ("\\.el\\'" . emacs-lisp-mode)
     ("\\.awk\\'" . awk-mode))
   #'string-match-p))

;; (when (fboundp 'alist-get)
;;   (ert-deftest compat-alist-get-1 ()
;;     "Check if `compat--alist-get' was advised correctly."
;;     (compat-deftests compat-alist-get
;;       ;; Fallback behaviour:
;;       (ought nil 1 nil)                      ;empty list
;;       (ought 'a 1 '((1 . a)))                  ;single element list
;;       (ought nil 1 '(1))
;;       (ought 'b 2 '((1 . a) (2 . b) (3 . c)))  ;multiple element list
;;       (ought nil 2 '(1 2 3))
;;       (ought 'b 2 '(1 (2 . b) 3))
;;       (ought nil 2 '((1 . a) 2 (3 . c)))
;;       (ought 'a 1 '((3 . c) (2 . b) (1 . a)))
;;       (ought nil "a" '(("a" . 1) ("b" . 2) ("c" . 3)))  ;non-primitive elements

;;       ;; With testfn (advised behaviour):
;;       (ought 1 "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal)
;;       (ought 1 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<)
;;       (ought '(a) "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp)
;;       (ought 'c "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
;;                        (lambda (s1 s2) (not (string= s1 s2))))
;;       (ought 'emacs-lisp-mode
;;                        "file.el"
;;                        '(("\\.c\\'" . c-mode)
;;                          ("\\.p\\'" . pascal-mode)
;;                          ("\\.el\\'" . emacs-lisp-mode)
;;                          ("\\.awk\\'" . awk-mode))
;;                        nil nil #'string-match-p)
;;       (ought 'd 0 '((1 . a) (2 . b) (3 . c)) 'd) ;default value
;;       (ought 'd 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore))))

(compat-deftests (alist-get compat--alist-get-full-elisp)
  ;; Fallback behaviour:
  (ought nil 1 nil)                      ;empty list
  (ought 'a 1 '((1 . a)))                  ;single element list
  (ought nil 1 '(1))
  (ought 'b 2 '((1 . a) (2 . b) (3 . c)))  ;multiple element list
  (ought nil 2 '(1 2 3))
  (ought 'b 2 '(1 (2 . b) 3))
  (ought nil 2 '((1 . a) 2 (3 . c)))
  (ought 'a 1 '((3 . c) (2 . b) (1 . a)))
  (ought nil "a" '(("a" . 1) ("b" . 2) ("c" . 3))) ;non-primitive elements
  ;; With testfn (advised behaviour):
  (ought 1 "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal)
  (ought 1 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<)
  (ought '(a) "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp)
  (ought 'c "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
         (lambda (s1 s2) (not (string= s1 s2))))
  (ought 'emacs-lisp-mode
         "file.el"
         '(("\\.c\\'" . c-mode)
           ("\\.p\\'" . pascal-mode)
           ("\\.el\\'" . emacs-lisp-mode)
           ("\\.awk\\'" . awk-mode))
         nil nil #'string-match-p)
  (ought 'd 0 '((1 . a) (2 . b) (3 . c)) 'd) ;default value
  (ought 'd 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore))

(ert-deftest compat-alist-get-gv ()
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

(compat-deftests string-trim-left
  (ought "" "")                          ;empty string
  (ought "a" "a")                        ;"full" string
  (ought "aaa" "aaa")
  (ought "へっろ" "へっろ")
  (ought "hello world" "hello world")
  (ought "a " "a ")                        ;right trailing
  (ought "aaa " "aaa ")
  (ought "a    " "a    ")
  (ought "a\t\t" "a\t\t")
  (ought "a\n  \t" "a\n  \t")
  (ought "a" " a")                        ;left trailing
  (ought "aaa" " aaa")
  (ought "a" "a")
  (ought "a" "\t\ta")
  (ought "a" "\n  \ta")
  (ought "a " " a ")                        ;both trailing
  (ought "aaa  " " aaa  ")
  (ought "a\t\n" "\t\ta\t\n")
  (ought "a  \n" "\n  \ta  \n"))

(compat-deftests string-trim-right
  (ought "" "")                          ;empty string
  (ought "a" "a")                        ;"full" string
  (ought "aaa" "aaa")
  (ought "へっろ" "へっろ")
  (ought "hello world" "hello world")
  (ought "a" "a")                      ;right trailing
  (ought "aaa" "aaa")
  (ought "a" "a    ")
  (ought "a" "a\t\t")
  (ought "a" "a\n  \t")
  (ought " a" " a")                       ;left trailing
  (ought " aaa" " aaa")
  (ought "a" "a")
  (ought "\t\ta" "\t\ta")
  (ought "\n  \ta" "\n  \ta")
  (ought " a" " a ")                        ;both trailing
  (ought " aaa" " aaa")
  (ought "\t\ta" "\t\ta\t\n")
  (ought "\n  \ta" "\n  \ta  \n"))

(compat-deftests string-trim
  (ought "" "")                          ;empty string
  (ought "a" "a")                        ;"full" string
  (ought "aaa" "aaa")
  (ought "へっろ" "へっろ")
  (ought "hello world" "hello world")
  (ought "a" "a ")                       ;right trailing
  (ought "aaa" "aaa ")
  (ought "a" "a    ")
  (ought "a" "a\t\t")
  (ought "a" "a\n  \t")
  (ought "a" " a")                       ;left trailing
  (ought "aaa" " aaa")
  (ought "a" "a")
  (ought "a" "\t\ta")
  (ought "a" "\n  \ta")
  (ought "a" " a ")                      ;both trailing
  (ought "aaa" " aaa  ")
  (ought "t\ta" "t\ta\t\n")
  (ought "a" "\n  \ta  \n"))

(compat-deftests mapcan
  (ought nil #'identity nil)
  (ought (list 1)
         #'identity
         (list (list 1)))
  (ought (list 1 2 3 4)
         #'identity
         (list (list 1) (list 2 3) (list 4)))
  (ought (list (list 1) (list 2 3) (list 4))
         #'list
         (list (list 1) (list 2 3) (list 4)))
  (ought (list 1 2 3 4)
         #'identity
         (list (list 1) (list) (list 2 3) (list 4)))
  (ought (list (list 1) (list) (list 2 3) (list 4))
         #'list
         (list (list 1) (list) (list 2 3) (list 4)))
  (ought (list)
         #'identity
         (list (list) (list) (list) (list))))

;; Note: as the cXXX+r implementations are relatively trivial, their
;; tests are not as extensive.

(defvar compat-cXXXr-test
  '(((a . b) . (c . d)) . ((e . f) . (g . h)))
  "Testcase for cXXXr functions.")

(defvar compat-cXXXXr-test
  '((((a . b) . (c . d)) . ((e . f) . (g . h))) .
    (((i . j) . (k . l)) . ((m . j) . (o . p))))
  "Testcase for cXXXXr functions.")

(compat-deftests caaar
  (ought nil ())
  (ought 'a compat-cXXXr-test))

(compat-deftests caadr
  (ought nil ())
  (ought 'e compat-cXXXr-test))

(compat-deftests cadar
  (ought nil ())
  (ought 'c compat-cXXXr-test))

(compat-deftests caddr
  (ought nil ())
  (ought 'g compat-cXXXr-test))

(compat-deftests cdaar
  (ought nil ())
  (ought 'b compat-cXXXr-test))

(compat-deftests cdadr
  (ought nil ())
  (ought 'f compat-cXXXr-test))

(compat-deftests cddar
  (ought nil ())
  (ought 'd compat-cXXXr-test))

(compat-deftests cdddr
  (ought nil ())
  (ought 'h compat-cXXXr-test)
  #'cdddr)

(compat-deftests caaaar
  (ought nil ())
  (ought 'a compat-cXXXXr-test))

(compat-deftests caaadr
  (ought nil ())
  (ought 'i compat-cXXXXr-test))

(compat-deftests caadar
  (ought nil ())
  (ought 'e compat-cXXXXr-test))

(compat-deftests caaddr
  (ought nil ())
  (ought 'm compat-cXXXXr-test))

(compat-deftests cadaar
  (ought nil ())
  (ought 'c compat-cXXXXr-test))

(compat-deftests cadadr
  (ought nil ())
  (ought 'k compat-cXXXXr-test))

(compat-deftests caddar
  (ought nil ())
  (ought 'g compat-cXXXXr-test))

(compat-deftests cadddr
  (ought nil ())
  (ought 'o compat-cXXXXr-test))

(compat-deftests cdaaar
  (ought nil ())
  (ought 'b compat-cXXXXr-test))

(compat-deftests cdaadr
  (ought nil ())
  (ought 'j compat-cXXXXr-test))

(compat-deftests cdadar
  (ought nil ())
  (ought 'f compat-cXXXXr-test))

(compat-deftests cdaddr
  (ought nil ())
  (ought 'j compat-cXXXXr-test))

(compat-deftests cddaar
  (ought nil ())
  (ought 'd compat-cXXXXr-test))

(compat-deftests cddadr
  (ought nil ())
  (ought 'l compat-cXXXXr-test))

(compat-deftests cdddar
  (ought nil ())
  (ought 'h compat-cXXXXr-test))

(compat-deftests string-greaterp
  (ought t "b" "a")
  (ought nil "a" "b")
  (ought t "aaab" "aaaa")
  (ought nil "aaaa" "aaab"))

(compat-deftests compat-sort
  (ought (list 1 2 3) (list 1 2 3) #'<)
  (ought (list 1 2 3) (list 3 2 1) #'<)
  (ought '[1 2 3] '[1 2 3] #'<)
  (ought '[1 2 3] '[3 2 1] #'<))

(compat-deftests compat-=
  (ought t 0 0)
  (ought t 0 0 0)
  (ought t 0 0 0 0)
  (ought t 0 0 0 0 0)
  (ought t 0.0 0.0)
  (ought t +0.0 -0.0)
  (ought t 0.0 0.0 0.0)
  (ought t 0.0 0.0 0.0 0.0)
  (ought nil 0 1)
  (ought nil 0 0 1)
  (ought nil 0 0 0 0 1)
  (expect wrong-type-argument 0 0 'a)
  (ought nil 0 1 'a)
  (ought nil 0.0 0.0 0.0 0.1))

(compat-deftests compat-<
  (ought nil 0 0)
  (ought nil 0 0 0)
  (ought nil 0 0 0 0)
  (ought nil 0 0 0 0 0)
  (ought nil 0.0 0.0)
  (ought nil +0.0 -0.0)
  (ought nil 0.0 0.0 0.0)
  (ought nil 0.0 0.0 0.0 0.0)
  (ought t 0 1)
  (ought nil 1 0)
  (ought nil 0 0 1)
  (ought t 0 1 2)
  (ought nil 2 1 0)
  (ought nil 0 0 0 0 1)
  (ought t 0 1 2 3 4)
  (expect wrong-type-argument 0 1 'a)
  (ought nil 0 0 'a)
  (ought nil 0.0 0.0 0.0 0.1)
  (ought t -0.1 0.0 0.2 0.4)
  (ought t -0.1 0 0.2 0.4))

(compat-deftests compat->
  (ought nil 0 0)
  (ought nil 0 0 0)
  (ought nil 0 0 0 0)
  (ought nil 0 0 0 0 0)
  (ought nil 0.0 0.0)
  (ought nil +0.0 -0.0)
  (ought nil 0.0 0.0 0.0)
  (ought nil 0.0 0.0 0.0 0.0)
  (ought t 1 0)
  (ought nil 1 0 0)
  (ought nil 0 1 2)
  (ought t 2 1 0)
  (ought nil 1 0 0 0 0)
  (ought t 4 3 2 1 0)
  (ought nil 4 3 2 1 1)
  (expect wrong-type-argument 1 0 'a)
  (ought nil 0 0 'a)
  (ought nil 0.1 0.0 0.0 0.0)
  (ought t 0.4 0.2 0.0 -0.1)
  (ought t 0.4 0.2 0 -0.1))

(compat-deftests compat-<=
  (ought t 0 0)
  (ought t 0 0 0)
  (ought t 0 0 0 0)
  (ought t 0 0 0 0 0)
  (ought t 0.0 0.0)
  (ought t +0.0 -0.0)
  (ought t 0.0 0.0 0.0)
  (ought t 0.0 0.0 0.0 0.0)
  (ought nil 1 0)
  (ought nil 1 0 0)
  (ought t 0 1 2)
  (ought nil 2 1 0)
  (ought nil 1 0 0 0 0)
  (ought nil 4 3 2 1 0)
  (ought nil 4 3 2 1 1)
  (ought t 0 1 2 3 4)
  (ought t 1 1 2 3 4)
  (expect wrong-type-argument 0 0 'a)
  (expect wrong-type-argument 0 1 'a)
  (ought nil 1 0 'a)
  (ought nil 0.1 0.0 0.0 0.0)
  (ought t 0.0 0.0 0.0 0.1)
  (ought t -0.1 0.0 0.2 0.4)
  (ought t -0.1 0.0 0.0 0.2 0.4)
  (ought t -0.1 0.0 0 0.2 0.4)
  (ought t -0.1 0 0.2 0.4)
  (ought nil 0.4 0.2 0.0 -0.1)
  (ought nil 0.4 0.2 0.0 0.0 -0.1)
  (ought nil 0.4 0.2 0 0.0 0.0 -0.1)
  (ought nil 0.4 0.2 0 -0.1))

(compat-deftests compat->=
  (ought t 0 0)
  (ought t 0 0 0)
  (ought t 0 0 0 0)
  (ought t 0 0 0 0 0)
  (ought t 0.0 0.0)
  (ought t +0.0 -0.0)
  (ought t 0.0 0.0 0.0)
  (ought t 0.0 0.0 0.0 0.0)
  (ought t 1 0)
  (ought t 1 0 0)
  (ought nil 0 1 2)
  (ought t 2 1 0)
  (ought t 1 0 0 0 0)
  (ought t 4 3 2 1 0)
  (ought t 4 3 2 1 1)
  (expect wrong-type-argument 0 0 'a)
  (expect wrong-type-argument 1 0 'a)
  (ought nil 0 1 'a)
  (ought t 0.1 0.0 0.0 0.0)
  (ought nil 0.0 0.0 0.0 0.1)
  (ought nil -0.1 0.0 0.2 0.4)
  (ought nil -0.1 0.0 0.0 0.2 0.4)
  (ought nil -0.1 0.0 0 0.2 0.4)
  (ought nil -0.1 0 0.2 0.4)
  (ought t 0.4 0.2 0.0 -0.1)
  (ought t 0.4 0.2 0.0 0.0 -0.1)
  (ought t 0.4 0.2 0 0.0 0.0 -0.1)
  (ought t 0.4 0.2 0 -0.1))

(compat-deftests special-form-p
  (ought t 'if)
  (ought t 'cond)
  (ought nil 'when)
  (ought nil 'defun)
  (ought nil '+)
  (ought nil nil)
  (ought nil "macro")
  (ought nil '(macro . +)))

(compat-deftests macrop
  (ought t 'lambda)
  (ought t 'defun)
  (ought t 'defmacro)
  (ought nil 'defalias)
  (ought nil 'foobar)
  (ought nil 'if)
  (ought nil '+)
  (ought nil 1)
  (ought nil nil)
  (ought nil "macro")
  (ought t '(macro . +)))

(compat-deftests string-suffix-p
  (ought t "a" "abba")
  (ought t "ba" "abba")
  (ought t "abba" "abba")
  (ought nil "a" "ABBA")
  (ought nil "bA" "ABBA")
  (ought nil "aBBA" "ABBA")
  (ought nil "c" "ABBA")
  (ought nil "c" "abba")
  (ought nil "cddc" "abba")
  (ought nil "aabba" "abba"))

(compat-deftests compat-split-string
  (ought '("a" "b" "c") "a b c")
  (ought '("..a.." "..b.." "..c..") "..a.. ..b.. ..c..")
  (ought '("a" "b" "c") "..a.. ..b.. ..c.." nil nil "\\.+"))

(compat-deftests delete-consecutive-dups
  (ought '(1 2 3 4) '(1 2 3 4))
  (ought '(1 2 3 4) '(1 2 2 3 4 4))
  (ought '(1 2 3 2 4) '(1 2 2 3 2 4 4)))

(compat-deftests string-clean-whitespace
  (ought "a b c" "a b c")
  (ought "a b c" "   a b c")
  (ought "a b c" "a b c   ")
  (ought "a b c" "a    b c")
  (ought "a b c" "a b    c")
  (ought "a b c" "a    b    c")
  (ought "a b c" "   a    b    c")
  (ought "a b c" "a    b    c    ")
  (ought "a b c" "   a    b    c    ")
  (ought "aa bb cc" "aa bb cc")
  (ought "aa bb cc" "   aa bb cc")
  (ought "aa bb cc" "aa bb cc   ")
  (ought "aa bb cc" "aa    bb cc")
  (ought "aa bb cc" "aa bb    cc")
  (ought "aa bb cc" "aa    bb    cc")
  (ought "aa bb cc" "   aa    bb    cc")
  (ought "aa bb cc" "aa    bb    cc    ")
  (ought "aa bb cc" "   aa    bb    cc    "))

(compat-deftests string-fill
  (ought "a a a a a" "a a a a a" 9)
  (ought "a a a a a" "a a a a a" 10)
  (ought "a a a a\na" "a a a a a" 8)
  (ought "a a a a\na" "a  a  a  a  a" 8)
  (ought "a a\na a\na" "a a a a a" 4)
  (ought "a\na\na\na\na" "a a a a a" 2)
  (ought "a\na\na\na\na" "a a a a a" 1))

(compat-deftests string-lines
  (ought '("a" "b" "c") "a\nb\nc")
  (ought '("a" "b" "c" "") "a\nb\nc\n")
  (ought '("a" "b" "c") "a\nb\nc\n" t)
  (ought '("abc" "bcd" "cde") "abc\nbcd\ncde")
  (ought '(" abc" " bcd " "cde ") " abc\n bcd \ncde "))

(compat-deftests string-pad
  (ought "a   " "a" 4)
  (ought "aaaa" "aaaa" 4)
  (ought "aaaaaa" "aaaaaa" 4)
  (ought "a..." "a" 4 ?.)
  (ought "   a" "a" 4 nil t)
  (ought "...a" "a" 4 ?. t))

(compat-deftests string-chop-newline
  (ought "" "")
  (ought "" "\n")
  (ought "aaa" "aaa")
  (ought "aaa" "aaa\n")
  (ought "aaa\n" "aaa\n\n"))

(compat-deftests macroexpand-1
  (ought '(if a b c) '(if a b c))
  (ought '(if a (progn b)) '(when a b))
  (ought '(if a (progn (unless b c))) '(when a (unless b c))))

(compat-deftests compat-file-size-human-readable
    (ought "1000" 1000)
    (ought "1k" 1024)
    (ought "1M" (* 1024 1024))
    (ought "1G" (expt 1024 3))
    (ought "1T" (expt 1024 4))
    (ought "1k" 1000 'si)
    (ought "1KiB" 1024 'iec)
    (ought "1KiB" 1024 'iec)
    (ought "1 KiB" 1024 'iec " ")
    (ought "1KiA" 1024 'iec nil "A")
    (ought "1 KiA" 1024 'iec " " "A")
    (ought "1kA" 1000 'si nil "A")
    (ought "1 k" 1000 'si " ")
    (ought "1 kA" 1000 'si " " "A"))

(compat-deftests format-prompt
  (ought "Prompt: " "Prompt" nil)
  (ought "Prompt: " "Prompt" "")
  (ought "Prompt (default  ): " "Prompt" " ")
  (ought "Prompt (default 3): " "Prompt" 3)
  (ought "Prompt (default abc): " "Prompt" "abc")
  (ought "Prompt (default abc def): " "Prompt" "abc def")
  (ought "Prompt 10: " "Prompt %d" nil 10)
  (ought "Prompt \"abc\" (default 3): " "Prompt %S" 3 "abc"))

(ert-deftest compat-named-let ()
  "Check if `compat--named-let' was implemented properly."
  (should (= (compat--named-let l ((i 0)) (if (= i 8) i (l (1+ i))))
             8))
  (should (= (compat--named-let l ((i 0)) (if (= i 100000) i (l (1+ i))))
             100000))
  (should (= (compat--named-let l ((i 0))
               (cond
                ((= i 100000) i)
                ((= (mod i 2) 0)
                 (l (+ i 2)))
                ((l (+ i 3)))))
             100000))
  (should (= (compat--named-let l ((i 0) (x 1)) (if (= i 8) x (l (1+ i) (* x 2))))
             (expt 2 8)))
  (should (eq (compat--named-let lop ((x 1))
                (if (> x 0)
                    (condition-case nil
                        (lop (1- x))
                      (arith-error 'ok))
                  (/ 1 x)))
              'ok))
  (should (eq (compat--named-let lop ((n 10000))
                (if (> n 0)
                    (condition-case nil
                        (/ n 0)
                      (arith-error (lop (1- n))))
                  'ok))
              'ok))
  (should (eq (compat--named-let lop ((x nil))
                (cond (x)
                      (t 'ok)))
              'ok))
  (should (eq (compat--named-let lop ((x 100000))
                (cond ((= x 0) 'ok)
                      ((lop (1- x)))))
              'ok))
  (should (eq (compat--named-let lop ((x 100000))
                (cond
                 ((= x -1) nil)
                 ((= x 0) 'ok)
                 ((lop -1))
                 ((lop (1- x)))))
              'ok))
  (should (eq (compat--named-let lop ((x 10000))
                (cond ((= x 0) 'ok)
                      ((and t (lop (1- x))))))
              'ok))
  (should (eq (let ((b t))
                (compat--named-let lop ((i 0))
                  (cond ((null i) nil) ((= i 10000) 'ok)
                        ((lop (and (setq b (not b)) (1+ i))))
                        ((lop (and (setq b (not b)) (1+ i)))))))
              'ok)))

(compat-deftests directory-name-p
  (ought t "/")
  (ought nil "/file")
  (ought nil "/dir/file")
  (ought t "/dir/")
  (ought nil "/dir")
  (ought t "/dir/subdir/")
  (ought nil "/dir/subdir")
  (ought t "dir/")
  (ought nil "file")
  (ought nil "dir/file")
  (ought t "dir/subdir/")
  (ought nil "dir/subdir"))

(ert-deftest compat-if-let* ()
  "Check if `compat--if-let*' was implemented properly."
  (should
   (compat--if-let*
    ((x 3)
     (y 2)
     (z (+ x y))
     ((= z 5))
     (true t))
    true nil))
  (should-not
   (compat--if-let* (((= 5 6))) t nil)))

(ert-deftest compat-if-let ()
  "Check if `compat--if-let' was implemented properly."
  (should (compat--if-let ((e (memq 0 '(1 2 3 0 5 6))))
              e))
  (should-not (compat--if-let ((e (memq 0 '(1 2 3 5 6)))
                               (d (memq 0 '(1 2 3 0 5 6))))
                  t))
  (should-not (compat--if-let ((d (memq 0 '(1 2 3 0 5 6)))
                               (e (memq 0 '(1 2 3 5 6))))
                  t))
  (should-not
   (compat--if-let (((= 5 6))) t nil)))

(ert-deftest compat-and-let* ()
  "Check if `compat--and-let*' was implemented properly."
  (should                               ;trivial body
   (compat--and-let*
    ((x 3)
     (y 2)
     (z (+ x y))
     ((= z 5))
     (true t))
    true))
  (should                               ;no body
   (compat--and-let*
    ((x 3)
     (y 2)
     (z (+ x y))
     ((= z 5))
     (true t))))
  (should-not
   (compat--and-let* (((= 5 6))) t)))

(compat-deftests compat-json-parse-string
  (ought 0 "0")
  (ought 1 "1")
  (ought 0.5 "0.5")
  (ought [1 2 3] "[1,2,3]")
  (ought ["a" 2 3] "[\"a\",2,3]")
  (ought [["a" 2] 3] "[[\"a\",2],3]")
  (ought '(("a" 2) 3) "[[\"a\",2],3]" :array-type 'list)
  (ought 'foo "null" :null-object 'foo)
  (ought ["false" t] "[false, true]" :false-object "false"))

(ert-deftest compat-json-parse-string ()
  "Check if `compat--json-parse-string' was implemented properly."
  (let ((input "{\"key\":[\"abc\", 2], \"yek\": null}"))
    (let ((obj (compat--json-parse-string input)))
      (should (equal (gethash "key" obj) ["abc" 2]))
      (should (equal (gethash "yek" obj) :null)))
    (let ((obj (compat--json-parse-string input :object-type 'alist)))
      (should (equal (cdr (assq 'key obj)) ["abc" 2]))
      (should (equal (cdr (assq 'yek obj)) :null)))
    (let ((obj (compat--json-parse-string input :object-type 'plist)))
      (should (equal (plist-get obj :key) ["abc" 2]))
      (should (equal (plist-get obj :yek) :null)))
    (when (fboundp 'json-parse-string)
      (let ((obj (json-parse-string input :object-type 'alist)))
        (should (equal (cdr (assq 'key obj)) ["abc" 2]))
        (should (equal (cdr (assq 'yek obj)) :null)))
      (let ((obj (json-parse-string input :object-type 'plist)))
        (should (equal (plist-get obj :key) ["abc" 2]))
        (should (equal (plist-get obj :yek) :null)))
      (let ((obj (json-parse-string input)))
        (should (equal (gethash "key" obj) ["abc" 2]))
        (should (equal (gethash "yek" obj) :null))))))

(ert-deftest compat-json-serialize ()
  "Check if `compat--json-serialize' was implemented properly."
  (let ((input-1 '((:key . ["abc" 2]) (yek . t)))
        (input-2 '(:key ["abc" 2] yek t))
        (input-3 (let ((ht (make-hash-table)))
                   (puthash "key" ["abc" 2] ht)
                   (puthash "yek" t ht)
                   ht)))
    (should (equal (compat--json-serialize input-1)
                   "{\":key\":[\"abc\",2],\"yek\":true}"))
    (should (equal (compat--json-serialize input-2)
                   "{\"key\":[\"abc\",2],\"yek\":true}"))
    (should (member (compat--json-serialize input-2)
                    '("{\"key\":[\"abc\",2],\"yek\":true}"
                      "{\"yek\":true,\"key\":[\"abc\",2]}")))
    (should-error (compat--json-serialize '(("a" . 1)))
                  :type '(wrong-type-argument symbolp "a"))
    (should-error (compat--json-serialize '("a" 1))
                  :type '(wrong-type-argument symbolp "a"))
    (should-error (compat--json-serialize '("a" 1 2))
                  :type '(wrong-type-argument symbolp "a"))
    (should-error (compat--json-serialize '(:a 1 2))
                  :type '(wrong-type-argument consp nil))
    (should-error (compat--json-serialize
                   (let ((ht (make-hash-table)))
                     (puthash 'a 1 ht)
                     ht))
                  :type '(wrong-type-argument stringp a))
    (when (fboundp 'json-serialize)
      (should (equal (json-serialize input-1)
                     "{\":key\":[\"abc\",2],\"yek\":true}"))
      (should (equal (json-serialize input-2)
                     "{\"key\":[\"abc\",2],\"yek\":true}"))
      (should (member (json-serialize input-2)
                      '("{\"key\":[\"abc\",2],\"yek\":true}"
                        "{\"yek\":true,\"key\":[\"abc\",2]}")))
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
                    :type '(wrong-type-argument stringp a)))))

(compat-deftests compat-lookup-key
  (let ((a-map (make-sparse-keymap))
        (b-map (make-sparse-keymap)))
    (define-key a-map "x" 'foo)
    (define-key b-map "x" 'bar)
    (ought 'foo a-map "x")
    (ought 'bar b-map "x")
    (ought 'foo (list a-map b-map) "x")
    (ought 'bar (list b-map a-map) "x")))

(ert-deftest compat-hash-table-keys ()
  (let ((ht (make-hash-table)))
    (should (null (compat--hash-table-keys ht)))
    (puthash 1 'one ht)
    (should (equal '(1) (compat--hash-table-keys ht)))
    (puthash 1 'one ht)
    (should (equal '(1) (compat--hash-table-keys ht)))
    (puthash 2 'two ht)
    (should (memq 1 (compat--hash-table-keys ht)))
    (should (memq 2 (compat--hash-table-keys ht)))
    (should (= 2 (length (compat--hash-table-keys ht))))
    (remhash 1 ht)
    (should (equal '(2) (compat--hash-table-keys ht)))))

(ert-deftest compat-hash-table-values ()
  (let ((ht (make-hash-table)))
    (should (null (compat--hash-table-values ht)))
    (puthash 1 'one ht)
    (should (equal '(one) (compat--hash-table-values ht)))
    (puthash 1 'one ht)
    (should (equal '(one) (compat--hash-table-values ht)))
    (puthash 2 'two ht)
    (should (memq 'one (compat--hash-table-values ht)))
    (should (memq 'two (compat--hash-table-values ht)))
    (should (= 2 (length (compat--hash-table-values ht))))
    (remhash 1 ht)
    (should (equal '(two) (compat--hash-table-values ht)))))

(compat-deftests string-empty-p
  (ought t "")
  (ought nil " ")
  (ought t (make-string 0 ?x))
  (ought nil (make-string 1 ?x)))

(compat-deftests string-join
  (ought "" '(""))
  (ought "" '("") " ")
  (ought "a" '("a"))
  (ought "a" '("a") " ")
  (ought "abc" '("a" "b" "c"))
  (ought "a b c" '("a" "b" "c") " "))

(compat-deftests string-blank-p
  (ought 0 "")
  (ought 0 " ")
  (ought 0 (make-string 0 ?x))
  (ought nil (make-string 1 ?x)))

(compat-deftests string-remove-prefix
  (ought "" "" "")
  (ought "a" "" "a")
  (ought "" "a" "")
  (ought "bc" "a" "abc")
  (ought "abc" "c" "abc")
  (ought "bbcc" "aa" "aabbcc")
  (ought "aabbcc" "bb" "aabbcc")
  (ought "aabbcc" "cc" "aabbcc")
  (ought "aabbcc" "dd" "aabbcc"))

(compat-deftests string-remove-suffix
  (ought "" "" "")
  (ought "a" "" "a")
  (ought "" "a" "")
  (ought "abc" "a" "abc")
  (ought "ab" "c" "abc")
  (ought "aabbcc" "aa" "aabbcc")
  (ought "aabbcc" "bb" "aabbcc")
  (ought "aabb" "cc" "aabbcc")
  (ought "aabbcc" "dd" "aabbcc"))

(let ((a (bool-vector t t nil nil))
      (b (bool-vector t nil t nil)))
  (compat-deftests bool-vector-exclusive-or
    (ought (bool-vector nil t t nil) a b)
    (ought (bool-vector nil t t nil) b a)
    (ert-deftest compat-bool-vector-exclusive-or-sideeffect ()
      (let ((c (make-bool-vector 4 nil)))
        (compat--bool-vector-exclusive-or a b c)
        (should (equal (bool-vector nil t t nil) c))
        (should (equal (bool-vector nil t t nil) c))))
    (when (version<= "24.4" emacs-version)
      (expect wrong-length-argument a (bool-vector))
      (expect wrong-length-argument a b (bool-vector)))
    (expect wrong-type-argument (bool-vector) (vector))
    (expect wrong-type-argument (vector) (bool-vector))
    (expect wrong-type-argument (vector) (vector))
    (expect wrong-type-argument (bool-vector) (bool-vector) (vector))
    (expect wrong-type-argument (bool-vector) (vector) (vector))
    (expect wrong-type-argument (vector) (bool-vector) (vector))
    (expect wrong-type-argument (vector) (vector) (vector))))

(let ((a (bool-vector t t nil nil))
      (b (bool-vector t nil t nil)))
  (compat-deftests bool-vector-union
    (ought (bool-vector t t t nil) a b)
    (ought (bool-vector t t t nil) b a)
    (ert-deftest compat-bool-vector-union-sideeffect ()
      (let ((c (make-bool-vector 4 nil)))
        (compat--bool-vector-union a b c)
        (should (equal (bool-vector t t t nil) c))))
    (when (version<= "24.4" emacs-version)
      (expect wrong-length-argument a (bool-vector))
      (expect wrong-length-argument a b (bool-vector)))
    (expect wrong-type-argument (bool-vector) (vector))
    (expect wrong-type-argument (vector) (bool-vector))
    (expect wrong-type-argument (vector) (vector))
    (expect wrong-type-argument (bool-vector) (bool-vector) (vector))
    (expect wrong-type-argument (bool-vector) (vector) (vector))
    (expect wrong-type-argument (vector) (bool-vector) (vector))
    (expect wrong-type-argument (vector) (vector) (vector))))

(let ((a (bool-vector t t nil nil))
      (b (bool-vector t nil t nil)))
  (compat-deftests bool-vector-intersection
    (ought (bool-vector t nil nil nil) a b)
    (ought (bool-vector t nil nil nil) b a)
    (ert-deftest compat-bool-vector-intersection-sideeffect ()
      (let ((c (make-bool-vector 4 nil)))
        (compat--bool-vector-intersection a b c)
        (should (equal (bool-vector t nil nil nil) c))))
    (when (version<= "24.4" emacs-version)
      (expect wrong-length-argument a (bool-vector))
      (expect wrong-length-argument a b (bool-vector)))
    (expect wrong-type-argument (bool-vector) (vector))
    (expect wrong-type-argument (vector) (bool-vector))
    (expect wrong-type-argument (vector) (vector))
    (expect wrong-type-argument (bool-vector) (bool-vector) (vector))
    (expect wrong-type-argument (bool-vector) (vector) (vector))
    (expect wrong-type-argument (vector) (bool-vector) (vector))
    (expect wrong-type-argument (vector) (vector) (vector))))

(let ((a (bool-vector t t nil nil))
      (b (bool-vector t nil t nil)))
  (compat-deftests bool-vector-set-difference
    (ought (bool-vector nil t nil nil) a b)
    (ought (bool-vector nil nil t nil) b a)
    (ert-deftest compat-bool-vector-set-difference-sideeffect ()
      (let ((c (make-bool-vector 4 nil)))
        (compat--bool-vector-set-difference a b c)
        (should (equal (bool-vector nil t nil nil) c)))
      (let ((c (make-bool-vector 4 nil)))
        (compat--bool-vector-set-difference b a c)
        (should (equal (bool-vector nil nil t nil) c))))
    (when (version<= "24.4" emacs-version)
      (expect wrong-length-argument a (bool-vector))
      (expect wrong-length-argument a b (bool-vector)))
    (expect wrong-type-argument (bool-vector) (vector))
    (expect wrong-type-argument (vector) (bool-vector))
    (expect wrong-type-argument (vector) (vector))
    (expect wrong-type-argument (bool-vector) (bool-vector) (vector))
    (expect wrong-type-argument (bool-vector) (vector) (vector))
    (expect wrong-type-argument (vector) (bool-vector) (vector))
    (expect wrong-type-argument (vector) (vector) (vector))))

(compat-deftests bool-vector-not
  (ought (bool-vector) (bool-vector))
  (ought (bool-vector t) (bool-vector nil))
  (ought (bool-vector nil) (bool-vector t))
  (ought (bool-vector t t) (bool-vector nil nil))
  (ought (bool-vector t nil) (bool-vector nil t))
  (ought (bool-vector nil t) (bool-vector t nil))
  (ought (bool-vector nil nil) (bool-vector t t))
  (expect wrong-type-argument (vector))
  (expect wrong-type-argument (vector) (vector)))

(compat-deftests bool-vector-subsetp
  (ought t (bool-vector) (bool-vector))
  (ought t (bool-vector t) (bool-vector t))
  (ought t (bool-vector nil) (bool-vector t))
  (ought nil (bool-vector t) (bool-vector nil))
  (ought t (bool-vector nil) (bool-vector nil))
  (ought t (bool-vector t t) (bool-vector t t))
  (ought t (bool-vector nil nil) (bool-vector t t))
  (ought t (bool-vector nil nil) (bool-vector t nil))
  (ought t (bool-vector nil nil) (bool-vector nil t))
  (ought nil (bool-vector t nil) (bool-vector nil nil))
  (ought nil (bool-vector nil t) (bool-vector nil nil))
  (when (version<= "24.4" emacs-version)
    (expect wrong-length-argument (bool-vector nil) (bool-vector nil nil)))
  (expect wrong-type-argument (bool-vector) (vector))
  (expect wrong-type-argument (vector) (bool-vector))
  (expect wrong-type-argument (vector) (vector)))

(compat-deftests bool-vector-count-consecutive
  (ought 0 (bool-vector nil) (bool-vector nil) 0)
  (ought 0 (make-bool-vector 10 nil) t 0)
  (ought 10 (make-bool-vector 10 nil) nil 0)
  (ought 0 (make-bool-vector 10 nil) t 1)
  (ought 9 (make-bool-vector 10 nil) nil 1)
  (ought 0 (make-bool-vector 10 nil) t 1)
  (ought 9 (make-bool-vector 10 t) t 1)
  (ought 0 (make-bool-vector 10 nil) t 8)
  (ought 2 (make-bool-vector 10 nil) nil 8)
  (ought 2 (make-bool-vector 10 t) t 8)
  (ought 10 (make-bool-vector 10 t) (make-bool-vector 10 t) 0)
  (ought 4 (bool-vector t t t t nil t t t t t) t 0)
  (ought 0 (bool-vector t t t t nil t t t t t) t 4)
  (ought 5 (bool-vector t t t t nil t t t t t) t 5)
  (expect wrong-type-argument (vector) nil 0))

(compat-deftests bool-vector-count-population
  (ought  0 (bool-vector))
  (ought  0 (make-bool-vector 10 nil))
  (ought 10 (make-bool-vector 10 t))
  (ought  1 (bool-vector nil nil t nil))
  (ought  1 (bool-vector nil nil nil t))
  (ought  1 (bool-vector t nil nil nil))
  (ought  2 (bool-vector t nil nil t))
  (ought  2 (bool-vector t nil t nil))
  (ought  3 (bool-vector t nil t t))
  (expect wrong-type-argument (vector)))

(compat-deftests compat-assoc-delete-all
  (ought (list) 0 (list))
  ;; Test `eq'
  (ought '((1 . one)) 0 (list (cons 1 'one)))
  (ought '((1 . one) a) 0 (list (cons 1 'one) 'a))
  (ought '((1 . one)) 0 (list (cons 0 'zero) (cons 1 'one)))
  (ought '((1 . one)) 0 (list (cons 0 'zero) (cons 0 'zero) (cons 1 'one)))
  (ought '((1 . one)) 0 (list (cons 0 'zero) (cons 1 'one) (cons 0 'zero)))
  (ought '((1 . one) a) 0 (list (cons 0 'zero) (cons 1 'one) 'a  (cons 0 'zero)))
  (ought '(a (1 . one)) 0 (list 'a (cons 0 'zero) (cons 1 'one) (cons 0 'zero)))
  ;; Test `equal'
  (ought '(("one" . one)) "zero" (list (cons "one" 'one)))
  (ought '(("one" . one) a) "zero" (list (cons "one" 'one) 'a))
  (ought '(("one" . one)) "zero" (list (cons "zero" 'zero) (cons "one" 'one)))
  (ought '(("one" . one)) "zero" (list (cons "zero" 'zero) (cons "zero" 'zero) (cons "one" 'one)))
  (ought '(("one" . one)) "zero" (list (cons "zero" 'zero) (cons "one" 'one) (cons "zero" 'zero)))
  (ought '(("one" . one) a) "zero" (list (cons "zero" 'zero) (cons "one" 'one) 'a  (cons "zero" 'zero)))
  (ought '(a ("one" . one)) "zero" (list 'a (cons "zero" 'zero) (cons "one" 'one) (cons "zero" 'zero)))
  ;; Test custom predicate
  (ought '() 0 (list (cons 1 'one)) #'/=)
  (ought '(a) 0 (list (cons 1 'one) 'a) #'/=)
  (ought '((0 . zero)) 0 (list (cons 0 'zero) (cons 1 'one)) #'/=)
  (ought '((0 . zero) (0 . zero)) 0 (list (cons 0 'zero) (cons 0 'zero) (cons 1 'one)) #'/=)
  (ought '((0 . zero) (0 . zero)) 0 (list (cons 0 'zero) (cons 1 'one) (cons 0 'zero)) #'/=)
  (ought '((0 . zero) a (0 . zero)) 0 (list (cons 0 'zero) (cons 1 'one) 'a  (cons 0 'zero)) #'/=)
  (ought '(a (0 . zero) (0 . zero)) 0 (list 'a (cons 0 'zero) (cons 1 'one) (cons 0 'zero)) #'/=))

(compat-deftests color-values-from-color-spec
  ;; #RGB notation
  (ought '(0 0 0) "#000")
  (ought '(0 0 0) "#000000")
  (ought '(0 0 0) "#000000000")
  (ought '(0 0 0) "#000000000000")
  (ought '(0 0 65535) "#00F")
  (ought '(0 0 65535) "#0000FF")
  (ought '(0 0 65535) "#000000FFF")
  (ought '(0 0 65535) "#00000000FFFF")
  (ought '(0 0 65535) "#00f")
  (ought '(0 0 65535) "#0000ff")
  (ought '(0 0 65535) "#000000fff")
  (ought '(0 0 65535) "#00000000ffff")
  (ought '(0 0 65535) "#00000000ffFF")
  (ought '(#xffff #x0000 #x5555) "#f05")
  (ought '(#x1f1f #xb0b0 #xc5c5) "#1fb0C5")
  (ought '(#x1f83 #xb0ad #xc5e2) "#1f83b0ADC5e2")
  (ought nil "")
  (ought nil "#")
  (ought nil "#0")
  (ought nil "#00")
  (ought nil "#0000FG")
  (ought nil "#0000FFF")
  (ought nil "#0000FFFF")
  (ought '(0 4080 65535) "#0000FFFFF")
  (ought nil "#000FF")
  (ought nil "#0000F")
  (ought nil " #000000")
  (ought nil "#000000 ")
  (ought nil " #000000 ")
  (ought nil "#1f83b0ADC5e2g")
  (ought nil "#1f83b0ADC5e20")
  (ought nil "#12345")
  ;; rgb: notation
  (ought '(0 0 0) "rgb:0/0/0")
  (ought '(0 0 0) "rgb:0/0/00")
  (ought '(0 0 0) "rgb:0/00/000")
  (ought '(0 0 0) "rgb:0/000/0000")
  (ought '(0 0 0) "rgb:000/0000/0")
  (ought '(0 0 65535) "rgb:000/0000/F")
  (ought '(65535 0 65535) "rgb:FFF/0000/F")
  (ought '(65535 0 65535) "rgb:FFFF/0000/FFFF")
  (ought '(0 255 65535) "rgb:0/00FF/FFFF")
  (ought '(#xffff #x2323 #x28a2) "rgb:f/23/28a")
  (ought '(#x1234 #x5678 #x09ab) "rgb:1234/5678/09ab")
  (ought nil "rgb:/0000/FFFF")
  (ought nil "rgb:0000/0000/FFFG")
  (ought nil "rgb:0000/0000/FFFFF")
  (ought nil "rgb:0000/0000")
  (ought nil "rg:0000/0000/0000")
  (ought nil "rgb: 0000/0000/0000")
  (ought nil "rgbb:0000/0000/0000")
  (ought nil "rgb:0000/0000/0000   ")
  (ought nil " rgb:0000/0000/0000  ")
  (ought nil "  rgb:0000/0000/0000")
  (ought nil "rgb:0000/ 0000 /0000")
  (ought nil "rgb: 0000 /0000 /0000")
  (ought nil "rgb:0//0")
  ;; rgbi: notation
  (ought '(0 0 0) "rgbi:0/0/0")
  (ought '(0 0 0) "rgbi:0.0/0.0/0.0")
  (ought '(0 0 0) "rgbi:0.0/0/0")
  (ought '(0 0 0) "rgbi:0.0/0/0")
  (ought '(0 0 0) "rgbi:0/0/0.")
  (ought '(0 0 0) "rgbi:0/0/0.0000")
  (ought '(0 0 0) "rgbi:0/0/.0")
  (ought '(0 0 0) "rgbi:0/0/.0000")
  (ought '(65535 0 0) "rgbi:1/0/0.0000")
  (ought '(65535 0 0) "rgbi:1./0/0.0000")
  (ought '(65535 0 0) "rgbi:1.0/0/0.0000")
  (ought '(65535 32768 0) "rgbi:1.0/0.5/0.0000")
  (ought '(6554 21843 65469) "rgbi:0.1/0.3333/0.999")
  (ought '(0 32768 6554) "rgbi:0/0.5/0.1")
  (ought '(66 655 65535) "rgbi:1e-3/1.0e-2/1e0")
  (ought '(6554 21843 65469) "rgbi:1e-1/+0.3333/0.00999e2")
  (ought nil "rgbi:1.0001/0/0")
  (ought nil "rgbi:2/0/0")
  (ought nil "rgbi:0.a/0/0")
  (ought nil "rgbi:./0/0")
  (ought nil "rgbi:./0/0")
  (ought nil " rgbi:0/0/0")
  (ought nil "rgbi:0/0/0 ")
  (ought nil "	rgbi:0/0/0 ")
  (ought nil "rgbi:0 /0/ 0")
  (ought nil "rgbi:0/ 0 /0")
  (ought nil "rgbii:0/0/0")
  (ought nil "rgbi :0/0/0")
  ;; strtod ignores leading whitespace, making these legal colour
  ;; specifications:
  ;;
  ;; (ought nil "rgbi: 0/0/0")
  ;; (ought nil "rgbi: 0/ 0/ 0")
  (ought nil "rgbi : 0/0/0")
  (ought nil "rgbi:0/0.5/10"))

(compat-deftests file-modes-number-to-symbolic
  (ought "-rwx------" #o700)
  (ought "-rwxrwx---" #o770)
  (ought "-rwx---rwx" #o707)
  (ought "-rw-r-xr--" #o654)
  (ought "--wx-w---x" #o321)
  (ought "drwx------" #o700 ?d)
  (ought "?rwx------" #o700 ??)
  (ought "lrwx------" #o120700)
  (ought "prwx------" #o10700)
  (ought "-rwx------" #o30700))

(compat-deftests file-local-name
  (ought "" "")
  (ought "foo" "foo")
  (ought "/bar/foo" "/bar/foo")
  ;; These tests fails prior to Emacs 26, because /ssh:foo was a valid
  ;; TRAMP path back then.
  ;;
  ;; (ought "/ssh:foo" "/ssh:foo")
  ;; (ought "/ssh:/bar/foo" "/ssh:/bar/foo")
  (ought "foo" "/ssh::foo")
  (ought "/bar/foo" "/ssh::/bar/foo")
  (ought ":foo" "/ssh:::foo")
  (ought ":/bar/foo" "/ssh:::/bar/foo"))

(compat-deftests file-name-quoted-p
  (ought nil "")
  (ought t "/:")
  (ought nil "//:")
  (ought t "/::")
  (ought nil "/ssh::")
  (ought nil "/ssh::a")
  (ought t "/ssh::/:a")
  ;; These tests fails prior to Emacs 26, because /ssh:foo was a valid
  ;; TRAMP path back then.
  ;;
  ;; (ought nil "/ssh:/:a")
  )

(compat-deftests file-name-quote
  (ought "/:" "")
  (ought "/::" ":")
  (ought "/:/" "/")
  (ought "/:" "/:")
  (ought "/:a" "a")
  (ought "/::a" ":a")
  (ought "/:/a" "/a")
  (ought "/:a" "/:a")
  (ought (concat "/ssh:" (system-name) ":/:a") "/ssh::a"))

(compat-deftests make-lock-file-name
  (ought (expand-file-name ".#") "")
  (ought (expand-file-name ".#a") "a")
  (ought (expand-file-name ".#foo") "foo")
  (ought (expand-file-name ".#.") ".")
  (ought (expand-file-name ".#.#") ".#")
  (ought (expand-file-name ".#.a") ".a")
  (ought (expand-file-name ".#.#") ".#")
  (ought (expand-file-name "a/.#") "a/")
  (ought (expand-file-name "a/.#b") "a/b")
  (ought (expand-file-name "a/.#.#") "a/.#")
  (ought (expand-file-name "a/.#.") "a/.")
  (ought (expand-file-name "a/.#.b") "a/.b")
  (ought (expand-file-name "a/.#foo") "a/foo")
  (ought (expand-file-name "bar/.#b") "bar/b")
  (ought (expand-file-name "bar/.#foo") "bar/foo"))

(compat-deftests time-equal-p
  (ought t nil nil)

  ;; FIXME: Testing these values can be tricky, because the timestamp
  ;; might change between evaluating (current-time) and evaluating
  ;; `time-equal-p', especially in the interpreted compatibility
  ;; version.

  ;; (ought t (current-time) nil)
  ;; (ought t nil (current-time))

  ;; While `sleep-for' returns nil, indicating the current time, this
  ;; behaviour seems to be undefined.  Relying on it is therefore not
  ;; advised.
  (ought nil (current-time) (ignore (sleep-for 0.01)))
  (ought nil (current-time) (progn
                              (sleep-for 0.01)
                              (current-time)))
  (ought t '(1 2 3 4) '(1 2 3 4))
  (ought nil '(1 2 3 4) '(1 2 3 5))
  (ought nil '(1 2 3 5) '(1 2 3 4))
  (ought nil '(1 2 3 4) '(1 2 4 4))
  (ought nil '(1 2 4 4) '(1 2 3 4))
  (ought nil '(1 2 3 4) '(1 3 3 4))
  (ought nil '(1 3 3 4) '(1 2 3 4))
  (ought nil '(1 2 3 4) '(2 2 3 4))
  (ought nil '(2 2 3 4) '(1 2 3 4)))

(compat-deftests date-days-in-month
  (ought 31 2020 1)
  (ought 30 2020 4)
  (ought 29 2020 2)
  (ought 28 2021 2))

(compat-deftests decoded-time-period
  (ought 0 '())
  (ought 0 '(0))
  (ought 1 '(1))
  (ought 0.125 '((1 . 8)))

  (ought 60 '(0 1))
  (ought 61 '(1 1))
  (ought -59 '(1 -1))

  (ought (* 60 60) '(0 0 1))
  (ought (+ (* 60 60) 60) '(0 1 1))
  (ought (+ (* 60 60) 120 1) '(1 2 1))

  (ought (* 60 60 24) '(0 0 0 1))
  (ought (+ (* 60 60 24) 1) '(1 0 0 1))
  (ought (+ (* 60 60 24) (* 60 60) 60 1) '(1 1 1 1))
  (ought (+ (* 60 60 24) (* 60 60) 120 1) '(1 2 1 1))

  (ought (* 60 60 24 30) '(0 0 0 0 1))
  (ought (+ (* 60 60 24 30) 1) '(1 0 0 0 1))
  (ought (+ (* 60 60 24 30) 60 1) '(1 1 0 0 1))
  (ought (+ (* 60 60 24 30) (* 60 60) 60 1)
         '(1 1 1 0 1))
  (ought (+ (* 60 60 24 30) (* 60 60 24) (* 60 60) 120 1)
         '(1 2 1 1 1))

  (ought (* 60 60 24 365) '(0 0 0 0 0 1))
  (ought (+ (* 60 60 24 365) 1)
         '(1 0 0 0 0 1))
  (ought (+ (* 60 60 24 365) 60 1)
         '(1 1 0 0 0 1))
  (ought (+ (* 60 60 24 365) (* 60 60) 60 1)
         '(1 1 1 0 0 1))
  (ought (+ (* 60 60 24 365) (* 60 60 24) (* 60 60) 60 1)
         '(1 1 1 1 0 1))
  (ought (+ (* 60 60 24 365)
            (* 60 60 24 30)
            (* 60 60 24)
            (* 60 60)
            120 1)
         '(1 2 1 1 1 1))

  (expect wrong-type-argument 'a)
  (expect wrong-type-argument '(0 a))
  (expect wrong-type-argument '(0 0 a))
  (expect wrong-type-argument '(0 0 0 a))
  (expect wrong-type-argument '(0 0 0 0 a))
  (expect wrong-type-argument '(0 0 0 0 0 a)))

(compat-deftests subr-primitive-p
  (ought t (symbol-function 'identity))       ;function from fns.c
  (ought nil (symbol-function 'match-string)) ;function from subr.el
  (ought nil (symbol-function 'defun))        ;macro from subr.el
  (ought nil nil))

(provide 'compat-tests)
;;; compat-tests.el ends here
