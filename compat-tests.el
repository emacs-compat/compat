;;; compat-tests.el --- Tests for compat.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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
;; both.  The current list of untested functions, macros, etc. are:
;;
;; - and-let* (`compat--and-let*')
;; - buffer-local-boundp (`buffer-local-boundp')
;; - count-windows (`compat--count-windows')
;; - defmacro (`compat--defmacro')
;; - directory-empty-p (`compat--directory-empty-p')
;; - directory-files (`compat--directory-files')
;; - dlet (`compat--dlet')
;; - garbage-collect-maybe (`compat--garbage-collect-maybe')
;; - if-let (`compat--if-let')
;; - if-let* (`compat--if-let*')
;; - ignore-error (`compat--ignore-error')
;; - insert-into-buffer (`compat--insert-into-buffer')
;; - line-number-at-pos (`compat--line-number-at-pos')
;; - macroexp-file-name (`compat--macroexp-file-name')
;; - make-nearby-temp-file (`compat--make-nearby-temp-file')
;; - named-let (`compat--named-let')
;; - package-get-version (`compat--package-get-version')
;; - recenter (`compat--recenter')
;; - replace-regexp-in-region (`compat--replace-regexp-in-region')
;; - setq-local (`compat--setq-local')
;; - temporary-file-directory (`compat--temporary-file-directory')
;; - thing-at-mouse (`compat--thing-at-mouse')
;; - thread-first (`compat--thread-first')
;; - thread-last (`compat--thread-last')
;; - unlock-buffer (`compat--unlock-buffer')
;; - when-let (`compat--when-let')
;; - when-let* (`compat--when-let*')
;; - with-environment-variables (`compat--with-environment-variables')
;; - with-existing-directory (`compat--with-existing-directory')
;; - with-file-modes (`compat--with-file-modes')
;;
;; Tests are welcome, but until then anyone working on these functions
;; should take care to test these manually.

;;; Code:

(require 'ert)
(eval-and-compile
  (require 'compat-macs)

  (let ((compat--disable-defer t))
    (load "compat-24.2.el" nil nil nil t)
    (load "compat-24.3.el" nil nil nil t)
    (load "compat-24.4.el" nil nil nil t)
    (load "compat-25.1.el" nil nil nil t)
    (load "compat-26.1.el" nil nil nil t)
    (load "compat-27.1.el" nil nil nil t)
    (load "compat-28.1.el" nil nil nil t))

  (defvar compat--current-fn nil)
  (defvar compat--compat-fn nil)

  (defmacro compat--should (result &rest input)
    "Generate code for test with INPUT evaluating to RESULT."
    (let ((cfn (or compat--compat-fn
                   (intern (format "compat--%s" compat--current-fn))))
          (rfn compat--current-fn))
      `(progn
         (should (equal (,cfn ,@input) ,result))
         (should (equal (,rfn ,@input) ,result)))))

  (defmacro compat--should* (result &rest input)
    "Generate code for test with INPUT evaluating to RESULT."
    (let ((cfn (or compat--compat-fn
                   (intern (format "compat--%s" compat--current-fn))))
          (rfn compat--current-fn))
      `(progn
         (should (equal (funcall (apply-partially #',cfn #',rfn) ,@input) ,result))
         (should (equal (,rfn ,@input) ,result)))))

  (defmacro compat--mshould (result &rest input)
    "Generate code for test with INPUT evaluating to RESULT."
    (let ((cfn (or compat--compat-fn
                   (intern (format "compat--%s" compat--current-fn))))
          (rfn compat--current-fn))
      `(progn
         (should (equal (macroexpand-all `(,',cfn ,,@input)) ,result))
         (should (equal (macroexpand-all `(,',rfn ,,@input)) ,result)))))

  (defmacro compat--error (error &rest input)
    "Generate code for test FN with INPUT to signal ERROR."
    (let ((cfn (or compat--compat-fn
                   (intern (format "compat--%s" compat--current-fn))))
          (rfn compat--current-fn))
      `(progn
         (should-error (,cfn ,@input) :type ',error)
         (should-error (,rfn ,@input) :type ',error))))

  (defmacro compat--error* (error &rest input)
    "Generate code for test FN with INPUT to signal ERROR."
    (let ((cfn (or compat--compat-fn
                   (intern (format "compat--%s" compat--current-fn))))
          (rfn compat--current-fn))
      `(progn
         (should-error (funcall (apply-partially #',cfn #',rfn) ,@input) :type ',error)
         (should-error (,rfn ,@input) :type ',error))))

  ;; FIXME: extract the name of the test out of the ERT-test, instead
  ;;        of having to re-declare the name of the test redundantly.
  (defmacro compat-test (fn &rest body)
    "Set `compat--current-fn' to FN in BODY.
If FN is a list, the car should be the actual function, and cadr
the compatibility function."
    (declare (indent 1))
    (if (consp fn)
        (setq compat--current-fn (car fn)
              compat--compat-fn (cadr fn))
      (setq compat--current-fn fn
            compat--compat-fn nil))
    (macroexp-progn body)))



(ert-deftest compat-string-search ()
  "Check if `string-search' was implemented correctly."
  (compat-test string-search
    ;; Find needle at the beginning of a haystack:
    (c-should 0 "a" "abb")
    ;; Find needle at the begining of a haystack, with more potential
    ;; needles that could be found:
    (c-should 0 "a" "abba")
    ;; Find needle with more than one charachter at the beginning of
    ;; a line:
    (c-should 0 "aa" "aabbb")
    ;; Find a needle midstring:
    (c-should 1 "a" "bab")
    ;; Find a needle at the end:
    (c-should 2 "a" "bba")
    ;; Find a longer needle midstring:
    (c-should 1 "aa" "baab")
    ;; Find a longer needle at the end:
    (c-should 2 "aa" "bbaa")
    ;; Find a case-sensitive needle:
    (c-should 2 "a" "AAa")
    ;; Find another case-sensitive needle:
    (c-should 2 "aa" "AAaa")
    ;; Test regular expression quoting (1):
    (c-should 5 "." "abbbb.b")
    ;; Test regular expression quoting (2):
    (c-should 5 ".*" "abbbb.*b")
    ;; Attempt to find non-existent needle:
    (c-should nil "a" "bbb")
    ;; Attempt to find non-existent needle that has the form of a
    ;; regular expression:
    (c-should nil "." "bbb")
    ;; Handle empty string as needle:
    (c-should 0 "" "abc")
    ;; Handle empty string as haystack:
    (c-should nil "a" "")
    ;; Handle empty string as needle and haystack:
    (c-should 0 "" "")
    ;; Handle START argument:
    (c-should 3 "a" "abba" 1)
    ;; Additional test copied from:
    (c-should 6 "zot" "foobarzot")
    (c-should 0 "foo" "foobarzot")
    (c-should nil "fooz" "foobarzot")
    (c-should nil "zot" "foobarzo")
    (c-should 0 "ab" "ab")
    (c-should nil "ab\0" "ab")
    (c-should 4 "ab" "abababab" 3)
    (c-should nil "ab" "ababac" 3)
    (c-should nil "aaa" "aa")
    (c-should 5
              (make-string 2 130)
              (concat "helló" (make-string 5 130 t) "bár"))
    (c-should 5
              (make-string 2 127)
              (concat "helló" (make-string 5 127 t) "bár"))
    (c-should 1 "\377" "a\377ø")
    (c-should 1 "\377" "a\377a")
    (c-should nil (make-string 1 255) "a\377ø")
    (c-should nil (make-string 1 255) "a\377a")
    (c-should 3 "fóo" "zotfóo")
    (c-should 2 (string-to-multibyte "\377") "ab\377c")
    (c-should nil "\303" "aøb")
    (c-should nil "\270" "aøb")
    (c-should nil "ø" "\303\270")
    (c-should nil "ø" (make-string 32 ?a))
    (c-should nil "ø" (string-to-multibyte (make-string 32 ?a)))
    (c-should 14 "o" (string-to-multibyte
                      (apply #'string (number-sequence ?a ?z))))
    (c-should 2 "a\U00010f98z" "a\U00010f98a\U00010f98z")
    (c-error (args-out-of-range -1) "a" "abc" -1)
    (c-error (args-out-of-range 4) "a" "abc" 4)
    (c-error (args-out-of-range 100000000000)
             "a" "abc" 100000000000)
    (c-should nil "a" "aaa" 3)
    (c-should nil "aa" "aa" 1)
    (c-should nil "\0" "")
    (c-should 0 "" "")
    (c-error (args-out-of-range 1) "" "" 1)
    (c-should 0 "" "abc")
    (c-should 2 "" "abc" 2)
    (c-should 3 "" "abc" 3)
    (c-error (args-out-of-range 4) "" "abc" 4)
    (c-error (args-out-of-range -1) "" "abc" -1)
    (c-should nil "ø" "foo\303\270")
    (c-should nil "\303\270" "ø")
    (c-should nil "\370" "ø")
    (c-should nil (string-to-multibyte "\370") "ø")
    (c-should nil "ø" "\370")
    (c-should nil "ø" (string-to-multibyte "\370"))
    (c-should nil "\303\270" "\370")
    (c-should nil (string-to-multibyte "\303\270") "\370")
    (c-should nil "\303\270" (string-to-multibyte "\370"))
    (c-should nil
              (string-to-multibyte "\303\270")
              (string-to-multibyte "\370"))
    (c-should nil "\370" "\303\270")
    (c-should nil (string-to-multibyte "\370") "\303\270")
    (c-should nil "\370" (string-to-multibyte "\303\270"))
    (c-should nil
              (string-to-multibyte "\370")
              (string-to-multibyte "\303\270"))
    (c-should 2
              (string-to-multibyte "o\303\270")
              "foo\303\270")
    (c-should 3 "\303\270" "foo\303\270")))

(ert-deftest compat-string-replace ()
  "Check if `string-replace' was implemented correctly."
  (compat-test string-replace
    (c-should "bba" "aa" "bb" "aaa")
    (c-should "AAA" "aa" "bb" "AAA")
    ;; Additional test copied from subr-tests.el:
    (c-should "zot" "foo" "bar" "zot")
    (c-should "barzot" "foo" "bar" "foozot")
    (c-should "barbarzot" "foo" "bar" "barfoozot")
    (c-should "barfoobar" "zot" "bar" "barfoozot")
    (c-should "barfoobarot" "z" "bar" "barfoozot")
    (c-should "zat" "zot" "bar" "zat")
    (c-should "zat" "azot" "bar" "zat")
    (c-should "bar" "azot" "bar" "azot")
    (c-should "foozotbar" "azot" "bar" "foozotbar")
    (c-should "labarbarbarzot" "fo" "bar" "lafofofozot")
    (c-should "axb" "\377" "x" "a\377b")
    (c-should "axø" "\377" "x" "a\377ø")
    (c-should "axb" (string-to-multibyte "\377") "x" "a\377b")
    (c-should "axø" (string-to-multibyte "\377") "x" "a\377ø")
    (c-should "ANAnas" "ana" "ANA" "ananas")
    (c-should "" "a" "" "")
    (c-should "" "a" "" "aaaaa")
    (c-should "" "ab" "" "ababab")
    (c-should "ccc" "ab" "" "abcabcabc")
    (c-should "aaaaaa" "a" "aa" "aaa")
    (c-should "defg" "abc" "defg" "abc")
    (c-error wrong-length-argument "" "x" "abc")))

(ert-deftest compat-length= ()
  "Check if `string-length=' was implemented correctly."
  (compat-test length=
    (c-should t '() 0)                  ;empty list
    (c-should t '(1) 1)			;single element
    (c-should t '(1 2 3) 3)             ;multiple elements
    (c-should nil '(1 2 3) 2)           ;less than
    (c-should nil '(1) 0)
    (c-should nil '(1 2 3) 4)           ;more than
    (c-should nil '(1) 2)
    (c-should nil '() 1)
    (c-should t [] 0)                   ;empty vector
    (c-should t [1] 1)			;single element vector
    (c-should t [1 2 3] 3)              ;multiple element vector
    (c-should nil [1 2 3] 2)            ;less than
    (c-should nil [1 2 3] 4)            ;more than
    (c-error wrong-type-argument 3 nil)))

(ert-deftest compat-length< ()
  "Check if `length<' was implemented correctly."
  (compat-test length<
    (c-should nil '(1) 0)               ;single element
    (c-should nil '(1 2 3) 2)           ;multiple elements
    (c-should nil '(1 2 3) 3)           ;equal length
    (c-should nil '(1) 1)
    (c-should t '(1 2 3) 4)             ;more than
    (c-should t '(1) 2)
    (c-should t '() 1)
    (c-should nil [1] 0)                ;single element vector
    (c-should nil [1 2 3] 2)            ;multiple element vector
    (c-should nil [1 2 3] 3)            ;equal length
    (c-should t [1 2 3] 4)              ;more than
    (c-error wrong-type-argument 3 nil)))

(ert-deftest compat-length> ()
  "Check if `length>' was implemented correctly."
  (compat-test length>
    (c-should t '(1) 0)			;single element
    (c-should t '(1 2 3) 2)             ;multiple elements
    (c-should nil '(1 2 3) 3)           ;equal length
    (c-should nil '(1) 1)
    (c-should nil '(1 2 3) 4)           ;more than
    (c-should nil '(1) 2)
    (c-should nil '() 1)
    (c-should t [1] 0)			;single element vector
    (c-should t [1 2 3] 2)              ;multiple element vector
    (c-should nil [1 2 3] 3)            ;equal length
    (c-should nil [1 2 3] 4)            ;more than
    (c-error wrong-type-argument 3 nil)))

(ert-deftest compat-always ()
  "Check if `always' was implemented correctly."
  (compat-test always
    (c-should t)                        ;no arguments
    (c-should t 1)                      ;single argument
    (c-should t 1 2 3 4)))              ;multiple arguments

(ert-deftest compat-insert-into-buffer ()
  "Check if `insert-into-buffer' was implemented correctly."
  ;; Without optional arguments
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
	(insert "def")
	(c-insert-into-buffer other))
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
	(c-insert-into-buffer other 2))
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
	(c-insert-into-buffer other 2 3))
      (should (string= (buffer-string) "abce"))))
  (when (fboundp 'insert-into-buffer)
    (with-temp-buffer
      (let ((other (current-buffer)))
	(insert "abc")
	(with-temp-buffer
	  (insert "def")
	  (insert-into-buffer other 2 3))
	(should (string= (buffer-string) "abce"))))))

(ert-deftest compat-file-name-with-extension ()
  "Check if `file-name-with-extension' was implemented correctly."
  (compat-test file-name-with-extension
    (c-should "file.ext" "file" "ext")
    (c-should "file.ext" "file" ".ext")
    (c-should "file.ext" "file." ".ext")
    (c-should "file..ext" "file.." ".ext")
    (c-should "file..ext" "file." "..ext")
    (c-should "file...ext" "file.." "..ext")
    (c-should "/abs/file.ext" "/abs/file" "ext")
    (c-should "/abs/file.ext" "/abs/file" ".ext")
    (c-should "/abs/file.ext" "/abs/file." ".ext")
    (c-should "/abs/file..ext" "/abs/file.." ".ext")
    (c-should "/abs/file..ext" "/abs/file." "..ext")
    (c-should "/abs/file...ext" "/abs/file.." "..ext")
    (c-error error "file" "")
    (c-error error '"" "ext")
    (c-error error "file" "")
    (c-error error "rel/" "ext")
    (c-error error "/abs/" "ext")))

(ert-deftest compat-file-name-with-extension ()
  "Check if `file-name-with-extension' was implemented correctly."
  (compat-test file-name-with-extension
    (c-should "file.ext" "file" "ext")
    (c-should "file.ext" "file" ".ext")
    (c-should "file.ext" "file." ".ext")
    (c-should "file..ext" "file.." ".ext")
    (c-should "file..ext" "file." "..ext")
    (c-should "file...ext" "file.." "..ext")
    (c-should "/abs/file.ext" "/abs/file" "ext")
    (c-should "/abs/file.ext" "/abs/file" ".ext")
    (c-should "/abs/file.ext" "/abs/file." ".ext")
    (c-should "/abs/file..ext" "/abs/file.." ".ext")
    (c-should "/abs/file..ext" "/abs/file." "..ext")
    (c-should "/abs/file...ext" "/abs/file.." "..ext")
    (c-error error "file" "")
    (c-error error "" "ext")
    (c-error error "file" "")
    (c-error error "rel/" "ext")
    (c-error error "/abs/" "ext")))

(ert-deftest compat-string-width ()
  "Check if `string-width' was implemented correctly."
  (compat-test string-width
    (c-should* 0 "")
    (c-should* 3 "abc")			;no argument
    (c-should* 5 "abcあ")
    (c-should* (1+ tab-width) "a	")
    (c-should* 2 "abc" 1)               ;with from
    (c-should* 4 "abcあ" 1)
    (c-should* tab-width "a	" 1)
    (c-should* 2 "abc" 0 2)             ;with to
    (c-should* 3 "abcあ" 0 3)
    (c-should* 1 "a	" 0 1)
    (c-should* 1 "abc" 1 2)             ;with from and to
    (c-should* 2 "abcあ" 3 4)
    (c-should* 0 "a	" 1 1)))

(ert-deftest compat-ensure-list ()
  "Check if `ensure-list' was implemented correctly."
  (compat-test ensure-list
    (c-should nil nil)                        ;empty list
    (c-should '(1) '(1))                        ;single element list
    (c-should '(1 2 3) '(1 2 3))                ;multiple element list
    (c-should '(1) 1)))                          ;atom

(ert-deftest compat-proper-list-p ()
  "Check if `proper-list-p' was implemented correctly."
  (compat-test proper-list-p
    (c-should 0 ())				;empty list
    (c-should 1 '(1))				;single element
    (c-should 3 '(1 2 3))			;multiple elements
    (c-should nil '(1 . 2))			;cons
    (c-should nil '(1 2 . 3))			;dotted
    (c-should nil (let ((l (list 1 2 3)))		;circular
                    (setf (nthcdr 3 l) l)
                    l))))

(ert-deftest compat-flatten-tree ()
  "Check if `flatten-tree' was implemented correctly."
  (compat-test flatten-tree
    ;; Example from docstring:
    (c-should '(1 2 3 4 5 6 7) '(1 (2 . 3) nil (4 5 (6)) 7))
    ;; Trivial example
    (c-should nil ())
    ;; Simple examples
    (c-should '(1) '(1))
    (c-should '(1 2) '(1 2))
    (c-should '(1 2 3) '(1 2 3))
    ;; Regular sublists
    (c-should '(1) '((1)))
    (c-should '(1 2) '((1) (2)))
    (c-should '(1 2 3) '((1) (2) (3)))
    ;; Complex examples
    (c-should '(1) '(((((1))))))
    (c-should '(1 2 3 4) '((1) nil 2 ((3 4))))
    (c-should '(1 2 3 4) '(((1 nil)) 2 (((3 nil nil) 4))))))

(ert-deftest compat-xor ()
  "Check if `xor' was implemented correctly."
  (compat-test xor
    (c-should t t nil)
    (c-should t nil t)
    (c-should nil nil nil)
    (c-should nil t t)))

(ert-deftest compat- ()
  "Check if `string-distance' was implemented correctly."
  (compat-test string-distance
    (c-should 3 "kitten" "sitting")     ;from wikipedia
    (c-should 0 "" "")                  ;trivial examples
    (c-should 0 "a" "a")
    (c-should 1 "" "a")
    (c-should 1 "b" "a")
    (c-should 2 "aa" "bb")
    (c-should 2 "aa" "bba")
    (c-should 2 "aaa" "bba")
    (c-should 3 "a" "あ" t)             ;byte example
    (c-should 1 "a" "あ")))

(ert-deftest compat-regexp-unmatchable ()
  "Check if `string-distance' was implemented correctly."
  (dolist (str '(""                     ;empty string
                 "a"                    ;simple string
                 "aaa"                  ;longer string
                 ))
    (should-not (string-match-p (with-no-warnings c-regexp-unmatchable) str))
    (should-not (string-match-p regexp-unmatchable str))))

(ert-deftest compat-regexp-opt ()
  "Check if `regexp-opt' advice was defined correctly."
  (compat-test regexp-opt
    ;; Ensure `compat--regexp-opt' doesn't change the existing
    ;; behaviour:
    (c-should* (regexp-opt '("a" "b" "c")) '("a" "b" "c"))
    (c-should* (regexp-opt '("abc" "def" "ghe")) '("abc" "def" "ghe"))
    (c-should* (regexp-opt '("a" "b" "c") 'words) '("a" "b" "c") 'words)
    ;; Test empty list:
    (c-should* "\\(?:\\`a\\`\\)" '())
    (c-should* "\\<\\(\\`a\\`\\)\\>" '() 'words))
  (let ((unmatchable (regexp-opt '())))
    (dolist (str '(""                   ;empty string
                   "a"                  ;simple string
                   "aaa"                ;longer string
                   ))
      (should-not (string-match-p unmatchable str)))))

(ert-deftest compat-assoc ()
  "Check if `assoc' advice was advised correctly."
  (compat-test assoc
    ;; Fallback behaviour:
    (c-should* nil 1 nil)               ;empty list
    (c-should* '(1) 1 '((1)))            ;single element list
    (c-should* nil 1 '(1))
    (c-should* '(2) 2 '((1) (2) (3)))    ;multiple element list
    (c-should* nil 2 '(1 2 3))
    (c-should* '(2) 2 '(1 (2) 3))
    (c-should* nil 2 '((1) 2 (3)))
    (c-should* '(1) 1 '((3) (2) (1)))
    (c-should* '("a") "a" '(("a") ("b") ("c")))  ;non-primitive elements
    (c-should* '("a" 0) "a" '(("c" . "a") "b" ("a" 0)))
    ;; With testfn (advised behaviour):
    (c-should* '(1) 3 '((10) (4) (1) (9)) #'<)
    (c-should* '("a") "b" '(("c") ("a") ("b")) #'string-lessp)
    (c-should* '("b") "a" '(("a") ("a") ("b"))
               (lambda (s1 s2) (not (string= s1 s2))))
    (c-should*
     '("\\.el\\'" . emacs-lisp-mode)
     "file.el"
     '(("\\.c\\'" . c-mode)
       ("\\.p\\'" . pascal-mode)
       ("\\.el\\'" . emacs-lisp-mode)
       ("\\.awk\\'" . awk-mode))
     #'string-match-p)))

(ert-deftest compat-alist-get-1 ()
  "Check if `alist-get' was advised correctly."
  (compat-test (alist-get compat--alist-get-handle-testfn)
    ;; Fallback behaviour:
    (c-should* nil 1 nil)                      ;empty list
    (c-should* 'a 1 '((1 . a)))                  ;single element list
    (c-should* nil 1 '(1))
    (c-should* 'b 2 '((1 . a) (2 . b) (3 . c)))  ;multiple element list
    (c-should* nil 2 '(1 2 3))
    (c-should* 'b 2 '(1 (2 . b) 3))
    (c-should* nil 2 '((1 . a) 2 (3 . c)))
    (c-should* 'a 1 '((3 . c) (2 . b) (1 . a)))
    (c-should* nil "a" '(("a" . 1) ("b" . 2) ("c" . 3)))  ;non-primitive elements

    ;; With testfn (advised behaviour):
    (c-should* 1 "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal)
    (c-should* 1 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<)
    (c-should* '(a) "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp)
    (c-should* 'c "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
               (lambda (s1 s2) (not (string= s1 s2))))
    (c-should* 'emacs-lisp-mode
               "file.el"
               '(("\\.c\\'" . c-mode)
                 ("\\.p\\'" . pascal-mode)
                 ("\\.el\\'" . emacs-lisp-mode)
                 ("\\.awk\\'" . awk-mode))
               nil nil #'string-match-p)
    (c-should* 'd 0 '((1 . a) (2 . b) (3 . c)) 'd) ;default value
    (c-should* 'd 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore)))

(ert-deftest compat-alist-get-2 ()
  "Check if `alist-get' was implemented correctly."
  (compat-test (alist-get compat--alist-get-full-elisp)
    ;; Fallback behaviour:
    (c-should nil 1 nil)                      ;empty list
    (c-should 'a 1 '((1 . a)))                  ;single element list
    (c-should nil 1 '(1))
    (c-should 'b 2 '((1 . a) (2 . b) (3 . c)))  ;multiple element list
    (c-should nil 2 '(1 2 3))
    (c-should 'b 2 '(1 (2 . b) 3))
    (c-should nil 2 '((1 . a) 2 (3 . c)))
    (c-should 'a 1 '((3 . c) (2 . b) (1 . a)))
    (c-should nil "a" '(("a" . 1) ("b" . 2) ("c" . 3)))  ;non-primitive elements

    ;; With testfn (advised behaviour):
    (c-should 1 "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal)
    (c-should 1 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<)
    (c-should '(a) "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp)
    (c-should 'c "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
              (lambda (s1 s2) (not (string= s1 s2))))
    (c-should 'emacs-lisp-mode
              "file.el"
              '(("\\.c\\'" . c-mode)
                ("\\.p\\'" . pascal-mode)
                ("\\.el\\'" . emacs-lisp-mode)
                ("\\.awk\\'" . awk-mode))
              nil nil #'string-match-p)
    (c-should 'd 0 '((1 . a) (2 . b) (3 . c)) 'd) ;default value
    (c-should 'd 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore)))

(ert-deftest compat-string-trim-left ()
  "Check if `string-trim-left' was implemented correctly."
  (compat-test string-trim-left'
    (c-should "" "")                          ;empty string
    (c-should "a" "a")                        ;"full" string
    (c-should "aaa" "aaa")
    (c-should "へっろ" "へっろ")
    (c-should "hello world" "hello world")
    (c-should "a " "a ")                        ;right trailing
    (c-should "aaa " "aaa ")
    (c-should "a    " "a    ")
    (c-should "a\t\t" "a\t\t")
    (c-should "a\n  \t" "a\n  \t")
    (c-should "a" " a")                        ;left trailing
    (c-should "aaa" " aaa")
    (c-should "a" "a")
    (c-should "a" "\t\ta")
    (c-should "a" "\n  \ta")
    (c-should "a " " a ")                        ;both trailing
    (c-should "aaa  " " aaa  ")
    (c-should "a\t\n" "\t\ta\t\n")
    (c-should "a  \n" "\n  \ta  \n")))

(ert-deftest compat-string-trim-right ()
  "Check if `string-trim-right' was implemented correctly."
  (compat-test string-trim-right
    (c-should "" "")                          ;empty string
    (c-should "a" "a")                        ;"full" string
    (c-should "aaa" "aaa")
    (c-should "へっろ" "へっろ")
    (c-should "hello world" "hello world")
    (c-should "a" "a")                      ;right trailing
    (c-should "aaa" "aaa")
    (c-should "a" "a    ")
    (c-should "a" "a\t\t")
    (c-should "a" "a\n  \t")
    (c-should " a" " a")                       ;left trailing
    (c-should " aaa" " aaa")
    (c-should "a" "a")
    (c-should "\t\ta" "\t\ta")
    (c-should "\n  \ta" "\n  \ta")
    (c-should " a" " a ")                        ;both trailing
    (c-should " aaa" " aaa")
    (c-should "\t\ta" "\t\ta\t\n")
    (c-should "\n  \ta" "\n  \ta  \n")))

(ert-deftest compat-string-trim ()
  "Check if `string-trim' was implemented correctly."
  (compat-test string-trim
    (c-should "" "")                          ;empty string
    (c-should "a" "a")                        ;"full" string
    (c-should "aaa" "aaa")
    (c-should "へっろ" "へっろ")
    (c-should "hello world" "hello world")
    (c-should "a" "a ")                       ;right trailing
    (c-should "aaa" "aaa ")
    (c-should "a" "a    ")
    (c-should "a" "a\t\t")
    (c-should "a" "a\n  \t")
    (c-should "a" " a")                       ;left trailing
    (c-should "aaa" " aaa")
    (c-should "a" "a")
    (c-should "a" "\t\ta")
    (c-should "a" "\n  \ta")
    (c-should "a" " a ")                      ;both trailing
    (c-should "aaa" " aaa  ")
    (c-should "t\ta" "t\ta\t\n")
    (c-should "a" "\n  \ta  \n")))

(ert-deftest compat-mapcan ()
  "Check if `mapcan' was implemented correctly."
  (compat-test mapcan
    (c-should nil #'identity nil)
    (c-should (list 1)
              #'identity
              (list (list 1)))
    (c-should (list 1 2 3 4)
              #'identity
              (list (list 1) (list 2 3) (list 4)))
    (c-should (list (list 1) (list 2 3) (list 4))
              #'list
              (list (list 1) (list 2 3) (list 4)))
    (c-should (list 1 2 3 4)
              #'identity
              (list (list 1) (list) (list 2 3) (list 4)))
    (c-should (list (list 1) (list) (list 2 3) (list 4))
              #'list
              (list (list 1) (list) (list 2 3) (list 4)))
    (c-should (list)
              #'identity
              (list (list) (list) (list) (list)))))

;; Note: as the cXXX+r implementations are relatively trivial, their
;; tests are not as extensive.

(defvar compat-cXXXr-test
  '(((a . b) . (c . d)) . ((e . f) . (g . h)))
  "Testcase for cXXXr functions.")

(defvar compat-cXXXXr-test
  '((((a . b) . (c . d)) . ((e . f) . (g . h))) .
    (((i . j) . (k . l)) . ((m . j) . (o . p))))
  "Testcase for cXXXXr functions.")

(ert-deftest compat-caaar ()
  "Check if `caaar' was implemented correctly."
  (compat-test caaar
    (c-should nil ())
    (c-should 'a compat-cXXXr-test)))

(ert-deftest compat-caadr ()
  "Check if `caadr' was implemented correctly."
  (compat-test caadr
    (c-should nil ())
    (c-should 'e compat-cXXXr-test)))

(ert-deftest compat-cadar ()
  "Check if `cadar' was implemented correctly."
  (compat-test cadar
    (c-should nil ())
    (c-should 'c compat-cXXXr-test)))

(ert-deftest compat-caddr ()
  "Check if `caddr' was implemented correctly."
  (compat-test caddr
    (c-should nil ())
    (c-should 'g compat-cXXXr-test)))

(ert-deftest compat-cdaar ()
  "Check if `cdaar' was implemented correctly."
  (compat-test cdaar
    (c-should nil ())
    (c-should 'b compat-cXXXr-test)))

(ert-deftest compat-cdadr ()
  "Check if `cdadr' was implemented correctly."
  (compat-test cdadr
    (c-should nil ())
    (c-should 'f compat-cXXXr-test)))

(ert-deftest compat-cddar ()
  "Check if `cddar' was implemented correctly."
  (compat-test cddar
    (c-should nil ())
    (c-should 'd compat-cXXXr-test)))

(ert-deftest compat-cdddr ()
  "Check if `cdddr' was implemented correctly."
  (compat-test cdddr
    (c-should nil ())
    (c-should 'h compat-cXXXr-test)
    #'cdddr))

(ert-deftest compat-caaaar ()
  "Check if `caaaar' was implemented correctly."
  (compat-test caaaar
    (c-should nil ())
    (c-should 'a compat-cXXXXr-test)))

(ert-deftest compat-caaadr ()
  "Check if `caaadr' was implemented correctly."
  (compat-test caaadr
    (c-should nil ())
    (c-should 'i compat-cXXXXr-test)))

(ert-deftest compat-caadar ()
  "Check if `caadar' was implemented correctly."
  (compat-test caadar
    (c-should nil ())
    (c-should 'e compat-cXXXXr-test)))

(ert-deftest compat-caaddr ()
  "Check if `caaddr' was implemented correctly."
  (compat-test caaddr
    (c-should nil ())
    (c-should 'm compat-cXXXXr-test)))

(ert-deftest compat-cadaar ()
  "Check if `cadaar' was implemented correctly."
  (compat-test cadaar
    (c-should nil ())
    (c-should 'c compat-cXXXXr-test)))

(ert-deftest compat-cadadr ()
  "Check if `cadadr' was implemented correctly."
  (compat-test cadadr
    (c-should nil ())
    (c-should 'k compat-cXXXXr-test)))

(ert-deftest compat-caddar ()
  "Check if `caddar' was implemented correctly."
  (compat-test caddar
    (c-should nil ())
    (c-should 'g compat-cXXXXr-test)))

(ert-deftest compat-cadddr ()
  "Check if `cadddr' was implemented correctly."
  (compat-test cadddr
    (c-should nil ())
    (c-should 'o compat-cXXXXr-test)))

(ert-deftest compat-cdaaar ()
  "Check if `cdaaar' was implemented correctly."
  (compat-test cdaaar
    (c-should nil ())
    (c-should 'b compat-cXXXXr-test)))

(ert-deftest compat-cdaadr ()
  "Check if `cdaadr' was implemented correctly."
  (compat-test cdaadr
    (c-should nil ())
    (c-should 'j compat-cXXXXr-test)))

(ert-deftest compat-cdadar ()
  "Check if `cdadar' was implemented correctly."
  (compat-test cdadar
    (c-should nil ())
    (c-should 'f compat-cXXXXr-test)))

(ert-deftest compat-cdaddr ()
  "Check if `cdaddr' was implemented correctly."
  (compat-test cdaddr
    (c-should nil ())
    (c-should 'j compat-cXXXXr-test)))

(ert-deftest compat-cddaar ()
  "Check if `cddaar' was implemented correctly."
  (compat-test cddaar
    (c-should nil ())
    (c-should 'd compat-cXXXXr-test)))

(ert-deftest compat-cddadr ()
  "Check if `cddadr' was implemented correctly."
  (compat-test cddadr
    (c-should nil ())
    (c-should 'l compat-cXXXXr-test)))

(ert-deftest compat-cdddar ()
  "Check if `cdddar' was implemented correctly."
  (compat-test cdddar
    (c-should nil ())
    (c-should 'h compat-cXXXXr-test)))

(ert-deftest compat-string-greaterp ()
  "Check if `string-greaterp' was implemented correctly."
  (compat-test string-greaterp
    (c-should t "b" "a")
    (c-should nil "a" "b")
    (c-should t "aaab" "aaaa")
    (c-should nil "aaaa" "aaab")))

(ert-deftest compat-sort ()
  "Check if `sort' was advised correctly."
  (compat-test sort
    (c-should* (list 1 2 3) (list 1 2 3) #'<)
    (c-should* (list 1 2 3) (list 3 2 1) #'<)
    (c-should* '[1 2 3] '[1 2 3] #'<)
    (c-should* '[1 2 3] '[3 2 1] #'<)))

(ert-deftest compat-= ()
  "Check if `=' was advised correctly."
  (compat-test =
    '(((0 0) t)
      ((0 0 0) t)
      ((0 0 0 0) t)
      ((0 0 0 0 0) t)
      ((0.0 0.0) t)
      ((+0.0 -0.0) t)
      ((0.0 0.0 0.0) t)
      ((0.0 0.0 0.0 0.0) t)
      ((0 1) nil)
      ((0 0 1) nil)
      ((0 0 0 0 1) nil)
      ((0 0 a) nil wrong-type-argument)
      ((0 1 a) nil)
      ((0.0 0.0 0.0 0.1) nil)
      )
    (apply-partially #'c-= #'=)
    #'=))

(ert-deftest compat-< ()
  "Check if `<' was advised correctly."
  (compat-test <
    (c-should* nil 0 0)
    (c-should* nil 0 0 0)
    (c-should* nil 0 0 0 0)
    (c-should* nil 0 0 0 0 0)
    (c-should* nil 0.0 0.0)
    (c-should* nil +0.0 -0.0)
    (c-should* nil 0.0 0.0 0.0)
    (c-should* nil 0.0 0.0 0.0 0.0)
    (c-should* t 0 1)
    (c-should* nil 1 0)
    (c-should* nil 0 0 1)
    (c-should* t 0 1 2)
    (c-should* nil 2 1 0)
    (c-should* nil 0 0 0 0 1)
    (c-should* t 0 1 2 3 4)
    (c-error* wrong-type-argument 0 1 'a)
    (c-should* nil 0 0 'a)
    (c-should* nil 0.0 0.0 0.0 0.1)
    (c-should* t -0.1 0.0 0.2 0.4)
    (c-should* t -0.1 0 0.2 0.4)))

(ert-deftest compat-> ()
  "Check if `>' was advised correctly."
  (compat-test >
    (c-should* nil 0 0)
    (c-should* nil 0 0 0)
    (c-should* nil 0 0 0 0)
    (c-should* nil 0 0 0 0 0)
    (c-should* nil 0.0 0.0)
    (c-should* nil +0.0 -0.0)
    (c-should* nil 0.0 0.0 0.0)
    (c-should* nil 0.0 0.0 0.0 0.0)
    (c-should* t 1 0)
    (c-should* nil 1 0 0)
    (c-should* nil 0 1 2)
    (c-should* t 2 1 0)
    (c-should* nil 1 0 0 0 0)
    (c-should* t 4 3 2 1 0)
    (c-should* nil 4 3 2 1 1)
    (c-error* wrong-type-argument 1 0 'a)
    (c-should* nil 0 0 'a)
    (c-should* nil 0.1 0.0 0.0 0.0)
    (c-should* t 0.4 0.2 0.0 -0.1)
    (c-should* t 0.4 0.2 0 -0.1)))

(ert-deftest compat-<= ()
  "Check if `<=' was advised correctly."
  (compat-test <=
    (c-should* t 0 0)
    (c-should* t 0 0 0)
    (c-should* t 0 0 0 0)
    (c-should* t 0 0 0 0 0)
    (c-should* t 0.0 0.0)
    (c-should* t +0.0 -0.0)
    (c-should* t 0.0 0.0 0.0)
    (c-should* t 0.0 0.0 0.0 0.0)
    (c-should* nil 1 0)
    (c-should* nil 1 0 0)
    (c-should* t 0 1 2)
    (c-should* nil 2 1 0)
    (c-should* nil 1 0 0 0 0)
    (c-should* nil 4 3 2 1 0)
    (c-should* nil 4 3 2 1 1)
    (c-should* t 0 1 2 3 4)
    (c-should* t 1 1 2 3 4)
    (c-error* wrong-type-argument 0 0 'a)
    (c-error* wrong-type-argument 0 1 'a)
    (c-should* nil 1 0 'a)
    (c-should* nil 0.1 0.0 0.0 0.0)
    (c-should* t 0.0 0.0 0.0 0.1)
    (c-should* t -0.1 0.0 0.2 0.4)
    (c-should* t -0.1 0.0 0.0 0.2 0.4)
    (c-should* t -0.1 0.0 0 0.2 0.4)
    (c-should* t -0.1 0 0.2 0.4)
    (c-should* nil 0.4 0.2 0.0 -0.1)
    (c-should* nil 0.4 0.2 0.0 0.0 -0.1)
    (c-should* nil 0.4 0.2 0 0.0 0.0 -0.1)
    (c-should* nil 0.4 0.2 0 -0.1)))

(ert-deftest compat->= ()
  "Check if `>=' was implemented correctly."
  (compat-test >=
    (c-should* t 0 0)
    (c-should* t 0 0 0)
    (c-should* t 0 0 0 0)
    (c-should* t 0 0 0 0 0)
    (c-should* t 0.0 0.0)
    (c-should* t +0.0 -0.0)
    (c-should* t 0.0 0.0 0.0)
    (c-should* t 0.0 0.0 0.0 0.0)
    (c-should* t 1 0)
    (c-should* t 1 0 0)
    (c-should* nil 0 1 2)
    (c-should* t 2 1 0)
    (c-should* t 1 0 0 0 0)
    (c-should* t 4 3 2 1 0)
    (c-should* t 4 3 2 1 1)
    (c-error* wrong-type-argument 0 0 'a)
    (c-error* wrong-type-argument 1 0 'a)
    (c-should* nil 0 1 'a)
    (c-should* t 0.1 0.0 0.0 0.0)
    (c-should* nil 0.0 0.0 0.0 0.1)
    (c-should* nil -0.1 0.0 0.2 0.4)
    (c-should* nil -0.1 0.0 0.0 0.2 0.4)
    (c-should* nil -0.1 0.0 0 0.2 0.4)
    (c-should* nil -0.1 0 0.2 0.4)
    (c-should* t 0.4 0.2 0.0 -0.1)
    (c-should* t 0.4 0.2 0.0 0.0 -0.1)
    (c-should* t 0.4 0.2 0 0.0 0.0 -0.1)
    (c-should* t 0.4 0.2 0 -0.1)))

(ert-deftest compat-special-form-p ()
  "Check if `special-form-p' was implemented correctly."
  (compat-test special-form-p
    (c-should t 'if)
    (c-should t 'cond)
    (c-should nil 'when)
    (c-should nil 'defun)
    (c-should nil '+)
    (c-should nil nil)
    (c-should nil "macro")
    (c-should nil '(macro . +))))

(ert-deftest compat-macrop ()
  "Check if `macrop' was implemented correctly."
  (compat-test macrop
    (c-should t 'lambda)
    (c-should t 'defun)
    (c-should t 'defmacro)
    (c-should nil 'defalias)
    (c-should nil 'if)
    (c-should nil '+)
    (c-should nil 1)
    (c-should nil nil)
    (c-should nil "macro")
    (c-should t '(macro . +))))

(ert-deftest compat-string-suffix-p ()
  "Check if `string-suffix-p' was implemented correctly."
  (compat-test string-suffix-p
    (c-should t "a" "abba")
    (c-should t "ba" "abba")
    (c-should t "abba" "abba")
    (c-should nil "a" "ABBA")
    (c-should nil "bA" "ABBA")
    (c-should nil "aBBA" "ABBA")
    (c-should nil "c" "ABBA")
    (c-should nil "c" "abba")
    (c-should nil "cddc" "abba")
    (c-should nil "aabba" "abba")))

(ert-deftest compat-split-string ()
  "Check if `split-string' was advised correctly."
  (compat-test split-string
    (c-should* '("a" "b" "c") "a b c")
    (c-should* '("..a.." "..b.." "..c..") "..a.. ..b.. ..c..")
    (c-should* '("a" "b" "c") "..a.. ..b.. ..c.." nil nil "\\.+")))

(ert-deftest compat-delete-consecutive-dups ()
  "Check if `delete-consecutive-dups' was implemented correctly."
  (compat-test delete-consecutive-dups
    (c-should '(1 2 3 4) '(1 2 3 4))
    (c-should '(1 2 3 4) '(1 2 2 3 4 4))
    (c-should '(1 2 3 2 4) '(1 2 2 3 2 4 4))))

(ert-deftest compat-autoloadp ()
  "Check if `autoloadp' was implemented correctly."
  (compat-test autoloadp
    (c-should t '(autoload . anything))
    (c-should nil 'anything)))

(ert-deftest compat-file-name-base ()
  "Check if `file-name-base' was implemented correctly."
  (compat-test file-name-base
    (c-should "file" "file.txt")
    (c-should "file" "/path/to/some/file.txt")
    (c-should "file" "/path/to/some/file")))

(ert-deftest compat-posnp ()
  "Check if `posnp' was implemented correctly."
  (compat-test posnp
    ;; FIXME: return an actual posn.
    ;; (c-should t (posn-at-point))
    (c-should nil (current-buffer))
    (c-should nil (point-max))
    (c-should nil (point-min))
    (c-should nil nil)))

(ert-deftest compat-string-clean-whitespace ()
  "Check if `string-clean-whitespace' was implemented correctly."
  (compat-test string-clean-whitespace
    (c-should "a b c" "a b c")
    (c-should "a b c" "   a b c")
    (c-should "a b c" "a b c   ")
    (c-should "a b c" "a    b c")
    (c-should "a b c" "a b    c")
    (c-should "a b c" "a    b    c")
    (c-should "a b c" "   a    b    c")
    (c-should "a b c" "a    b    c    ")
    (c-should "a b c" "   a    b    c    ")
    (c-should "aa bb cc" "aa bb cc")
    (c-should "aa bb cc" "   aa bb cc")
    (c-should "aa bb cc" "aa bb cc   ")
    (c-should "aa bb cc" "aa    bb cc")
    (c-should "aa bb cc" "aa bb    cc")
    (c-should "aa bb cc" "aa    bb    cc")
    (c-should "aa bb cc" "   aa    bb    cc")
    (c-should "aa bb cc" "aa    bb    cc    ")
    (c-should "aa bb cc" "   aa    bb    cc    ")))

(ert-deftest compat-string-fill ()
  "Check if `string-fill' was implemented correctly."
  (compat-test string-fill
    (c-should "a a a a a" "a a a a a" 9)
    (c-should "a a a a a" "a a a a a" 10)
    (c-should "a a a a\na" "a a a a a" 8)
    (c-should "a a a a\na" "a  a  a  a  a" 8)
    (c-should "a a\na a\na" "a a a a a" 4)
    (c-should "a\na\na\na\na" "a a a a a" 2)
    (c-should "a\na\na\na\na" "a a a a a" 1)))

(ert-deftest compat-string-lines ()
  "Check if `string-lines' was implemented correctly."
  (compat-test string-lines
    (c-should '("a" "b" "c") "a\nb\nc")
    (c-should '("a" "b" "c" "") "a\nb\nc\n")
    (c-should '("a" "b" "c") "a\nb\nc\n" t)
    (c-should '("abc" "bcd" "cde") "abc\nbcd\ncde")
    (c-should '(" abc" " bcd " "cde ") " abc\n bcd \ncde ")))

(ert-deftest compat-string-pad ()
  "Check if `string-pad' was implemented correctly."
  (compat-test string-pad
    (c-should "a   " "a" 4)
    (c-should "aaaa" "aaaa" 4)
    (c-should "aaaaaa" "aaaaaa" 4)
    (c-should "a..." "a" 4 ?.)
    (c-should "   a" "a" 4 nil t)
    (c-should "...a" "a" 4 ?. t)))

(ert-deftest compat-string-chop-newline ()
  "Check if `string-chop-newline' was implemented correctly."
  (compat-test string-chop-newline
    (c-should "" "")
    (c-should "" "\n")
    (c-should "aaa" "aaa")
    (c-should "aaa" "aaa\n")
    (c-should "aaa\n" "aaa\n\n")))

(ert-deftest compat-macroexpand-1 ()
  "Check if `macroexpand-1' was implemented correctly."
  (compat-test macroexpand-1
    (c-should '(if a b c) '(if a b c))
    (c-should '(if a (progn b)) '(when a b))
    (c-should '(if a (progn (unless b c))) '(when a (unless b c)))))

(ert-deftest compat-file-size-human-readable ()
  "Check if `file-size-human-readable' was advised properly."
  (compat-test file-size-human-readable
    (c-should* "1000" 1000)
    (c-should* "1k" 1024)
    (c-should* "1M" (* 1024 1024))
    (c-should* "1G" (expt 1024 3))
    (c-should* "1T" (expt 1024 4))
    (c-should* "1k" 1000 'si)
    (c-should* "1KiB" 1024 'iec)
    (c-should* "1KiB" 1024 'iec)
    (c-should* "1 KiB" 1024 'iec " ")
    (c-should* "1KiA" 1024 'iec nil "A")
    (c-should* "1 KiA" 1024 'iec " " "A")
    (c-should* "1kA" 1000 'si nil "A")
    (c-should* "1 k" 1000 'si " ")
    (c-should* "1 kA" 1000 'si " " "A")))

(ert-deftest compat-format-prompt ()
  "Check if `file-size-human-readable' was implemented properly."
  (compat-test format-prompt
    (c-should "Prompt: " "Prompt" nil)
    (c-should "Prompt (default 3): " "Prompt" 3)
    (c-should "Prompt (default abc): " "Prompt" "abc")
    (c-should "Prompt (default abc def): " "Prompt" "abc def")
    (c-should "Prompt 10: " "Prompt %d" nil 10)
    (c-should "Prompt \"abc\" (default 3): " "Prompt %S" 3 "abc")))

(provide 'compat-tests)
;;; compat-tests.el ends here

;; Local Variables:
;; elisp-shorthands: (("c-" . "compat--"))
;; End:
