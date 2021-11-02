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
;; both.
;;
;; Tests are welcome, but until then anyone working on these functions
;; should take care to test these manually.

;;; Code:

(require 'ert)
(require 'compat)

(defvar compat--current-fn nil)
(defvar compat--compat-fn nil)

(defmacro compat--should (result &rest input)
  "Generate code for test with INPUT evaluating to RESULT."
  (let ((cfn (or compat--compat-fn
                 (intern (format "compat--%s" compat--current-fn))))
        (rfn compat--current-fn))
    (macroexp-progn
     (list
      `(should (equal (,cfn ,@input) ,result))
      (and (fboundp rfn)
           `(should (equal (,rfn ,@input) ,result)))))))

(defmacro compat--should* (result &rest input)
  "Generate code for test with INPUT evaluating to RESULT."
  (let ((cfn (or compat--compat-fn
                 (intern (format "compat--%s" compat--current-fn))))
        (rfn compat--current-fn))
    (macroexp-progn
     (list
      `(should (equal (funcall (apply-partially #',cfn #',rfn) ,@input) ,result))
      (and (and (fboundp rfn)
                (or (not (eq (get cfn 'compat-type) 'advice))
                    (not (get cfn 'compat-version))
                    (version<= (get cfn 'compat-version) emacs-version)))
           `(should (equal (,rfn ,@input) ,result)))))))

(defmacro compat--mshould (result &rest input)
  "Generate code for test with INPUT evaluating to RESULT."
  (let ((cfn (or compat--compat-fn
                 (intern (format "compat--%s" compat--current-fn))))
        (rfn compat--current-fn))
    (macroexp-progn
     (list
      `(should (equal (macroexpand-all `(,',cfn ,,@input)) ,result))
      (and (fboundp rfn)
           `(should (equal (macroexpand-all `(,',rfn ,,@input)) ,result)))))))

(defmacro compat--error (error &rest input)
  "Generate code for test FN with INPUT to signal ERROR."
  (let ((cfn (or compat--compat-fn
                 (intern (format "compat--%s" compat--current-fn))))
        (rfn compat--current-fn))
    (macroexp-progn
     (list
      `(should-error (,cfn ,@input) :type ',error)
      (and (fboundp rfn)
           `(should-error (,rfn ,@input) :type ',error))))))

(defmacro compat--error* (error &rest input)
  "Generate code for test FN with INPUT to signal ERROR."
  (let ((cfn (or compat--compat-fn
                 (intern (format "compat--%s" compat--current-fn))))
        (rfn compat--current-fn))
    (macroexp-progn
     (list
      `(should-error (funcall (apply-partially #',cfn #',rfn) ,@input) :type ',error)
      (and (and (fboundp rfn)
                (or (not (eq (get cfn 'compat-type) 'advice))
                    (not (get cfn 'compat-version))
                    (version<= (get cfn 'compat-version) emacs-version)))
           `(should-error (,rfn ,@input) :type ',error))))))

;; FIXME: extract the name of the test out of the ERT-test, instead
;;        of having to re-declare the name of the test redundantly.
(defmacro compat-test (fn &rest body)
  "Set `compat--current-fn' to FN in BODY.
If FN is a list, the car should be the actual function, and cadr
the compatibility function."
  (declare (indent 1))
  (if (consp fn)
      (setq compat--current-fn (if (symbolp (car fn))
                                   (car fn)
                                 ;; Handle expressions
                                 (eval (car fn) t))
            compat--compat-fn (if (symbolp (cadr fn))
                                   (cadr fn)
                                 ;; Handle expressions
                                 (eval (cadr fn) t)))
    (setq compat--current-fn fn
          compat--compat-fn nil))
  (macroexp-progn body))



(ert-deftest compat-string-search ()
  "Check if `compat--string-search' was implemented correctly."
  (compat-test string-search
    ;; Find needle at the beginning of a haystack:
    (compat--should 0 "a" "abb")
    ;; Find needle at the begining of a haystack, with more potential
    ;; needles that could be found:
    (compat--should 0 "a" "abba")
    ;; Find needle with more than one charachter at the beginning of
    ;; a line:
    (compat--should 0 "aa" "aabbb")
    ;; Find a needle midstring:
    (compat--should 1 "a" "bab")
    ;; Find a needle at the end:
    (compat--should 2 "a" "bba")
    ;; Find a longer needle midstring:
    (compat--should 1 "aa" "baab")
    ;; Find a longer needle at the end:
    (compat--should 2 "aa" "bbaa")
    ;; Find a case-sensitive needle:
    (compat--should 2 "a" "AAa")
    ;; Find another case-sensitive needle:
    (compat--should 2 "aa" "AAaa")
    ;; Test regular expression quoting (1):
    (compat--should 5 "." "abbbb.b")
    ;; Test regular expression quoting (2):
    (compat--should 5 ".*" "abbbb.*b")
    ;; Attempt to find non-existent needle:
    (compat--should nil "a" "bbb")
    ;; Attempt to find non-existent needle that has the form of a
    ;; regular expression:
    (compat--should nil "." "bbb")
    ;; Handle empty string as needle:
    (compat--should 0 "" "abc")
    ;; Handle empty string as haystack:
    (compat--should nil "a" "")
    ;; Handle empty string as needle and haystack:
    (compat--should 0 "" "")
    ;; Handle START argument:
    (compat--should 3 "a" "abba" 1)
    ;; Additional test copied from:
    (compat--should 6 "zot" "foobarzot")
    (compat--should 0 "foo" "foobarzot")
    (compat--should nil "fooz" "foobarzot")
    (compat--should nil "zot" "foobarzo")
    (compat--should 0 "ab" "ab")
    (compat--should nil "ab\0" "ab")
    (compat--should 4 "ab" "abababab" 3)
    (compat--should nil "ab" "ababac" 3)
    (compat--should nil "aaa" "aa")
    ;; The `make-string' calls with three arguments have been replaced
    ;; here with the result of their evaluation, to avoid issues with
    ;; older versions of Emacs that only support two arguments.
    (compat--should 5
		    (make-string 2 130)
		    ;; Per (concat "helló" (make-string 5 130 t) "bár")
		    "hellóbár")
    (compat--should 5
		    (make-string 2 127)
		    ;; Per (concat "helló" (make-string 5 127 t) "bár")
		    "hellóbár")
    (compat--should 1 "\377" "a\377ø")
    (compat--should 1 "\377" "a\377a")
    (compat--should nil (make-string 1 255) "a\377ø")
    (compat--should nil (make-string 1 255) "a\377a")
    (compat--should 3 "fóo" "zotfóo")
    (compat--should nil "\303" "aøb")
    (compat--should nil "\270" "aøb")
    (compat--should nil "ø" "\303\270")
    (compat--should nil "ø" (make-string 32 ?a))
    (compat--should nil "ø" (string-to-multibyte (make-string 32 ?a)))
    (compat--should 14 "o" (string-to-multibyte
                            (apply #'string (number-sequence ?a ?z))))
    (compat--should 2 "a\U00010f98z" "a\U00010f98a\U00010f98z")
    (compat--error (args-out-of-range -1) "a" "abc" -1)
    (compat--error (args-out-of-range 4) "a" "abc" 4)
    (compat--error (args-out-of-range 100000000000)
                   "a" "abc" 100000000000)
    (compat--should nil "a" "aaa" 3)
    (compat--should nil "aa" "aa" 1)
    (compat--should nil "\0" "")
    (compat--should 0 "" "")
    (compat--error (args-out-of-range 1) "" "" 1)
    (compat--should 0 "" "abc")
    (compat--should 2 "" "abc" 2)
    (compat--should 3 "" "abc" 3)
    (compat--error (args-out-of-range 4) "" "abc" 4)
    (compat--error (args-out-of-range -1) "" "abc" -1)
    (compat--should nil "ø" "foo\303\270")
    (compat--should nil "\303\270" "ø")
    (compat--should nil "\370" "ø")
    (compat--should nil (string-to-multibyte "\370") "ø")
    (compat--should nil "ø" "\370")
    (compat--should nil "ø" (string-to-multibyte "\370"))
    (compat--should nil "\303\270" "\370")
    (compat--should nil (string-to-multibyte "\303\270") "\370")
    (compat--should nil "\303\270" (string-to-multibyte "\370"))
    (compat--should nil
                    (string-to-multibyte "\303\270")
                    (string-to-multibyte "\370"))
    (compat--should nil "\370" "\303\270")
    (compat--should nil (string-to-multibyte "\370") "\303\270")
    (compat--should nil "\370" (string-to-multibyte "\303\270"))
    (compat--should nil
                    (string-to-multibyte "\370")
                    (string-to-multibyte "\303\270"))
    (compat--should 3 "\303\270" "foo\303\270")
    (when (version<= "27" emacs-version)
      ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1 in
      ;; emacs.git fixes the behaviour of regular expressions matching
      ;; raw bytes.  The compatibility functions should updated to
      ;; backport this behaviour.
      (compat--should 2 (string-to-multibyte "\377") "ab\377c")
      (compat--should 2
                      (string-to-multibyte "o\303\270")
                      "foo\303\270"))))

(ert-deftest compat-string-replace ()
  "Check if `compat--string-replace' was implemented correctly."
  (compat-test string-replace
    (compat--should "bba" "aa" "bb" "aaa")
    (compat--should "AAA" "aa" "bb" "AAA")
    ;; Additional test copied from subr-tests.el:
    (compat--should "zot" "foo" "bar" "zot")
    (compat--should "barzot" "foo" "bar" "foozot")
    (compat--should "barbarzot" "foo" "bar" "barfoozot")
    (compat--should "barfoobar" "zot" "bar" "barfoozot")
    (compat--should "barfoobarot" "z" "bar" "barfoozot")
    (compat--should "zat" "zot" "bar" "zat")
    (compat--should "zat" "azot" "bar" "zat")
    (compat--should "bar" "azot" "bar" "azot")
    (compat--should "foozotbar" "azot" "bar" "foozotbar")
    (compat--should "labarbarbarzot" "fo" "bar" "lafofofozot")
    (compat--should "axb" "\377" "x" "a\377b")
    (compat--should "axø" "\377" "x" "a\377ø")
    (when (version<= "27" emacs-version)
      ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1
      ;; in emacs.git fixes the behaviour of regular
      ;; expressions matching raw bytes.  The compatibility
      ;; functions should updated to backport this
      ;; behaviour.
      (compat--should "axb" (string-to-multibyte "\377") "x" "a\377b")
      (compat--should "axø" (string-to-multibyte "\377") "x" "a\377ø"))
    (compat--should "ANAnas" "ana" "ANA" "ananas")
    (compat--should "" "a" "" "")
    (compat--should "" "a" "" "aaaaa")
    (compat--should "" "ab" "" "ababab")
    (compat--should "ccc" "ab" "" "abcabcabc")
    (compat--should "aaaaaa" "a" "aa" "aaa")
    (compat--should "defg" "abc" "defg" "abc")
    (when (version<= "24.4" emacs-version)
      ;; FIXME: Emacs 24.3 do not know of `wrong-length-argument' and
      ;; therefore fail this test, even if the right symbol is being
      ;; thrown.
      (compat--error wrong-length-argument "" "x" "abc"))))

(ert-deftest compat-length= ()
  "Check if `compat--string-length=' was implemented correctly."
  (compat-test length=
    (compat--should t '() 0)                  ;empty list
    (compat--should t '(1) 1)			;single element
    (compat--should t '(1 2 3) 3)             ;multiple elements
    (compat--should nil '(1 2 3) 2)           ;less than
    (compat--should nil '(1) 0)
    (compat--should nil '(1 2 3) 4)           ;more than
    (compat--should nil '(1) 2)
    (compat--should nil '() 1)
    (compat--should t [] 0)                   ;empty vector
    (compat--should t [1] 1)			;single element vector
    (compat--should t [1 2 3] 3)              ;multiple element vector
    (compat--should nil [1 2 3] 2)            ;less than
    (compat--should nil [1 2 3] 4)            ;more than
    (compat--error wrong-type-argument 3 nil)))

(ert-deftest compat-length< ()
  "Check if `compat--length<' was implemented correctly."
  (compat-test length<
    (compat--should nil '(1) 0)               ;single element
    (compat--should nil '(1 2 3) 2)           ;multiple elements
    (compat--should nil '(1 2 3) 3)           ;equal length
    (compat--should nil '(1) 1)
    (compat--should t '(1 2 3) 4)             ;more than
    (compat--should t '(1) 2)
    (compat--should t '() 1)
    (compat--should nil [1] 0)                ;single element vector
    (compat--should nil [1 2 3] 2)            ;multiple element vector
    (compat--should nil [1 2 3] 3)            ;equal length
    (compat--should t [1 2 3] 4)              ;more than
    (compat--error wrong-type-argument 3 nil)))

(ert-deftest compat-length> ()
  "Check if `compat--length>' was implemented correctly."
  (compat-test length>
    (compat--should t '(1) 0)			;single element
    (compat--should t '(1 2 3) 2)             ;multiple elements
    (compat--should nil '(1 2 3) 3)           ;equal length
    (compat--should nil '(1) 1)
    (compat--should nil '(1 2 3) 4)           ;more than
    (compat--should nil '(1) 2)
    (compat--should nil '() 1)
    (compat--should t [1] 0)			;single element vector
    (compat--should t [1 2 3] 2)              ;multiple element vector
    (compat--should nil [1 2 3] 3)            ;equal length
    (compat--should nil [1 2 3] 4)            ;more than
    (compat--error wrong-type-argument 3 nil)))

(ert-deftest compat-always ()
  "Check if `compat--always' was implemented correctly."
  (compat-test always
    (compat--should t)                        ;no arguments
    (compat--should t 1)                      ;single argument
    (compat--should t 1 2 3 4)))              ;multiple arguments

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

(ert-deftest compat-file-name-with-extension ()
  "Check if `compat--file-name-with-extension' was implemented correctly."
  (compat-test file-name-with-extension
    (compat--should "file.ext" "file" "ext")
    (compat--should "file.ext" "file" ".ext")
    (compat--should "file.ext" "file." ".ext")
    (compat--should "file..ext" "file.." ".ext")
    (compat--should "file..ext" "file." "..ext")
    (compat--should "file...ext" "file.." "..ext")
    (compat--should "/abs/file.ext" "/abs/file" "ext")
    (compat--should "/abs/file.ext" "/abs/file" ".ext")
    (compat--should "/abs/file.ext" "/abs/file." ".ext")
    (compat--should "/abs/file..ext" "/abs/file.." ".ext")
    (compat--should "/abs/file..ext" "/abs/file." "..ext")
    (compat--should "/abs/file...ext" "/abs/file.." "..ext")
    (compat--error error "file" "")
    (compat--error error '"" "ext")
    (compat--error error "file" "")
    (compat--error error "rel/" "ext")
    (compat--error error "/abs/" "ext")))

(ert-deftest compat-file-name-with-extension ()
  "Check if `compat--file-name-with-extension' was implemented correctly."
  (compat-test file-name-with-extension
    (compat--should "file.ext" "file" "ext")
    (compat--should "file.ext" "file" ".ext")
    (compat--should "file.ext" "file." ".ext")
    (compat--should "file..ext" "file.." ".ext")
    (compat--should "file..ext" "file." "..ext")
    (compat--should "file...ext" "file.." "..ext")
    (compat--should "/abs/file.ext" "/abs/file" "ext")
    (compat--should "/abs/file.ext" "/abs/file" ".ext")
    (compat--should "/abs/file.ext" "/abs/file." ".ext")
    (compat--should "/abs/file..ext" "/abs/file.." ".ext")
    (compat--should "/abs/file..ext" "/abs/file." "..ext")
    (compat--should "/abs/file...ext" "/abs/file.." "..ext")
    (compat--error error "file" "")
    (compat--error error "" "ext")
    (compat--error error "file" "")
    (compat--error error "rel/" "ext")
    (compat--error error "/abs/" "ext")))

(ert-deftest compat-string-width ()
  "Check if `compat--string-width' was implemented correctly."
  (compat-test string-width
    (compat--should* 0 "")
    (compat--should* 3 "abc")			;no argument
    (compat--should* 5 "abcあ")
    (compat--should* (1+ tab-width) "a	")
    (compat--should* 2 "abc" 1)               ;with from
    (compat--should* 4 "abcあ" 1)
    (compat--should* tab-width "a	" 1)
    (compat--should* 2 "abc" 0 2)             ;with to
    (compat--should* 3 "abcあ" 0 3)
    (compat--should* 1 "a	" 0 1)
    (compat--should* 1 "abc" 1 2)             ;with from and to
    (compat--should* 2 "abcあ" 3 4)
    (compat--should* 0 "a	" 1 1)))

(ert-deftest compat-ensure-list ()
  "Check if `compat--ensure-list' was implemented correctly."
  (compat-test ensure-list
    (compat--should nil nil)                        ;empty list
    (compat--should '(1) '(1))                        ;single element list
    (compat--should '(1 2 3) '(1 2 3))                ;multiple element list
    (compat--should '(1) 1)))                          ;atom

(ert-deftest compat-proper-list-p-1 ()
  "Check if `compat--proper-list-p' was implemented correctly (>=26.1)."
  (compat-test (proper-list-p compat--proper-list-p-length-signal)
    (compat--should 0 ())				;empty list
    (compat--should 1 '(1))				;single element
    (compat--should 3 '(1 2 3))			;multiple elements
    (compat--should nil '(1 . 2))			;cons
    (compat--should nil '(1 2 . 3))			;dotted
    (compat--should nil (let ((l (list 1 2 3)))		;circular
                          (setf (nthcdr 3 l) l)
                          l))))

(ert-deftest compat-proper-list-p-1 ()
  "Check if `compat--proper-list-p' was implemented correctly (<25.3)."
  (compat-test (proper-list-p compat--proper-list-p-tortoise-hare)
    (compat--should 0 ())				;empty list
    (compat--should 1 '(1))				;single element
    (compat--should 3 '(1 2 3))			;multiple elements
    (compat--should nil '(1 . 2))			;cons
    (compat--should nil '(1 2 . 3))			;dotted
    (compat--should nil (let ((l (list 1 2 3)))		;circular
                          (setf (nthcdr 3 l) l)
                          l))))

(ert-deftest compat-flatten-tree ()
  "Check if `compat--flatten-tree' was implemented correctly."
  (compat-test flatten-tree
    ;; Example from docstring:
    (compat--should '(1 2 3 4 5 6 7) '(1 (2 . 3) nil (4 5 (6)) 7))
    ;; Trivial example
    (compat--should nil ())
    ;; Simple examples
    (compat--should '(1) '(1))
    (compat--should '(1 2) '(1 2))
    (compat--should '(1 2 3) '(1 2 3))
    ;; Regular sublists
    (compat--should '(1) '((1)))
    (compat--should '(1 2) '((1) (2)))
    (compat--should '(1 2 3) '((1) (2) (3)))
    ;; Complex examples
    (compat--should '(1) '(((((1))))))
    (compat--should '(1 2 3 4) '((1) nil 2 ((3 4))))
    (compat--should '(1 2 3 4) '(((1 nil)) 2 (((3 nil nil) 4))))))

(ert-deftest compat-xor ()
  "Check if `compat--xor' was implemented correctly."
  (compat-test xor
    (compat--should t t nil)
    (compat--should t nil t)
    (compat--should nil nil nil)
    (compat--should nil t t)))

(ert-deftest compat-string-distance ()
  "Check if `compat--string-distance' was implemented correctly."
  (compat-test string-distance
    (compat--should 3 "kitten" "sitting")     ;from wikipedia
    (if (version<= "28" emacs-version) ;trivial examples
        (compat--should 0 "" "")
      ;; Up until Emacs 28, `string-distance' had a bug
      ;; when comparing two empty strings. This was fixed
      ;; in the following commit:
      ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=c44190c
      ;;
      ;; Therefore, we must make sure, that the test
      ;; doesn't fail because of this bug:
      (should (= (compat--string-distance "" "") 0)))
    (compat--should 0 "a" "a")
    (compat--should 1 "" "a")
    (compat--should 1 "b" "a")
    (compat--should 2 "aa" "bb")
    (compat--should 2 "aa" "bba")
    (compat--should 2 "aaa" "bba")
    (compat--should 3 "a" "あ" t)             ;byte example
    (compat--should 1 "a" "あ")))

(ert-deftest compat-regexp-unmatchable ()
  "Check if `compat--string-distance' was implemented correctly."
  (dolist (str '(""                     ;empty string
                 "a"                    ;simple string
                 "aaa"                  ;longer string
                 ))
    (should-not (string-match-p (with-no-warnings compat--regexp-unmatchable) str))
    (when (boundp 'regexp-unmatchable)
      (should-not (string-match-p regexp-unmatchable str)))))

(ert-deftest compat-regexp-opt ()
  "Check if `compat--regexp-opt' advice was defined correctly."
  (compat-test regexp-opt
    ;; Ensure `compat--regexp-opt' doesn't change the existing
    ;; behaviour:
    (compat--should* (regexp-opt '("a" "b" "c")) '("a" "b" "c"))
    (compat--should* (regexp-opt '("abc" "def" "ghe")) '("abc" "def" "ghe"))
    (compat--should* (regexp-opt '("a" "b" "c") 'words) '("a" "b" "c") 'words)
    ;; Test empty list:
    (compat--should* "\\(?:\\`a\\`\\)" '())
    (compat--should* "\\<\\(\\`a\\`\\)\\>" '() 'words))
  (let ((unmatchable (compat--regexp-opt #'regexp-opt '())))
    (dolist (str '(""                   ;empty string
                   "a"                  ;simple string
                   "aaa"                ;longer string
                   ))
      (should-not (string-match-p unmatchable str)))))

(ert-deftest compat-assoc ()
  "Check if `compat--assoc' advice was advised correctly."
  (compat-test assoc
    ;; Fallback behaviour:
    (compat--should* nil 1 nil)               ;empty list
    (compat--should* '(1) 1 '((1)))            ;single element list
    (compat--should* nil 1 '(1))
    (compat--should* '(2) 2 '((1) (2) (3)))    ;multiple element list
    (compat--should* nil 2 '(1 2 3))
    (compat--should* '(2) 2 '(1 (2) 3))
    (compat--should* nil 2 '((1) 2 (3)))
    (compat--should* '(1) 1 '((3) (2) (1)))
    (compat--should* '("a") "a" '(("a") ("b") ("c")))  ;non-primitive elements
    (compat--should* '("a" 0) "a" '(("c" . "a") "b" ("a" 0)))
    ;; With testfn (advised behaviour):
    (compat--should* '(1) 3 '((10) (4) (1) (9)) #'<)
    (compat--should* '("a") "b" '(("c") ("a") ("b")) #'string-lessp)
    (compat--should* '("b") "a" '(("a") ("a") ("b"))
                     (lambda (s1 s2) (not (string= s1 s2))))
    (compat--should*
     '("\\.el\\'" . emacs-lisp-mode)
     "file.el"
     '(("\\.c\\'" . c-mode)
       ("\\.p\\'" . pascal-mode)
       ("\\.el\\'" . emacs-lisp-mode)
       ("\\.awk\\'" . awk-mode))
     #'string-match-p)))

(when (fboundp 'alist-get)
  (ert-deftest compat-alist-get-1 ()
    "Check if `compat--alist-get' was advised correctly."
    (compat-test (alist-get compat--alist-get-handle-testfn)
      ;; Fallback behaviour:
      (compat--should* nil 1 nil)                      ;empty list
      (compat--should* 'a 1 '((1 . a)))                  ;single element list
      (compat--should* nil 1 '(1))
      (compat--should* 'b 2 '((1 . a) (2 . b) (3 . c)))  ;multiple element list
      (compat--should* nil 2 '(1 2 3))
      (compat--should* 'b 2 '(1 (2 . b) 3))
      (compat--should* nil 2 '((1 . a) 2 (3 . c)))
      (compat--should* 'a 1 '((3 . c) (2 . b) (1 . a)))
      (compat--should* nil "a" '(("a" . 1) ("b" . 2) ("c" . 3)))  ;non-primitive elements

      ;; With testfn (advised behaviour):
      (compat--should* 1 "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal)
      (compat--should* 1 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<)
      (compat--should* '(a) "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp)
      (compat--should* 'c "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
                       (lambda (s1 s2) (not (string= s1 s2))))
      (compat--should* 'emacs-lisp-mode
                       "file.el"
                       '(("\\.c\\'" . c-mode)
                         ("\\.p\\'" . pascal-mode)
                         ("\\.el\\'" . emacs-lisp-mode)
                         ("\\.awk\\'" . awk-mode))
                       nil nil #'string-match-p)
      (compat--should* 'd 0 '((1 . a) (2 . b) (3 . c)) 'd) ;default value
      (compat--should* 'd 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore))))

(ert-deftest compat-alist-get-2 ()
  "Check if `compat--alist-get' was implemented correctly."
  (compat-test (alist-get compat--alist-get-full-elisp)
    ;; Fallback behaviour:
    (compat--should nil 1 nil)                      ;empty list
    (compat--should 'a 1 '((1 . a)))                  ;single element list
    (compat--should nil 1 '(1))
    (compat--should 'b 2 '((1 . a) (2 . b) (3 . c)))  ;multiple element list
    (compat--should nil 2 '(1 2 3))
    (compat--should 'b 2 '(1 (2 . b) 3))
    (compat--should nil 2 '((1 . a) 2 (3 . c)))
    (compat--should 'a 1 '((3 . c) (2 . b) (1 . a)))
    (compat--should nil "a" '(("a" . 1) ("b" . 2) ("c" . 3))))  ;non-primitive elements
  (compat-test ((and (version<= "26.1" emacs-version) #'alist-get)
                compat--alist-get-full-elisp)
    ;; With testfn (advised behaviour):
    (compat--should 1 "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal)
    (compat--should 1 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<)
    (compat--should '(a) "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp)
    (compat--should 'c "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
                    (lambda (s1 s2) (not (string= s1 s2))))
    (compat--should 'emacs-lisp-mode
                    "file.el"
                    '(("\\.c\\'" . c-mode)
                      ("\\.p\\'" . pascal-mode)
                      ("\\.el\\'" . emacs-lisp-mode)
                      ("\\.awk\\'" . awk-mode))
                    nil nil #'string-match-p)
    (compat--should 'd 0 '((1 . a) (2 . b) (3 . c)) 'd) ;default value
    (compat--should 'd 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore)))

(ert-deftest compat-string-trim-left ()
  "Check if `compat--string-trim-left' was implemented correctly."
  (compat-test string-trim-left'
    (compat--should "" "")                          ;empty string
    (compat--should "a" "a")                        ;"full" string
    (compat--should "aaa" "aaa")
    (compat--should "へっろ" "へっろ")
    (compat--should "hello world" "hello world")
    (compat--should "a " "a ")                        ;right trailing
    (compat--should "aaa " "aaa ")
    (compat--should "a    " "a    ")
    (compat--should "a\t\t" "a\t\t")
    (compat--should "a\n  \t" "a\n  \t")
    (compat--should "a" " a")                        ;left trailing
    (compat--should "aaa" " aaa")
    (compat--should "a" "a")
    (compat--should "a" "\t\ta")
    (compat--should "a" "\n  \ta")
    (compat--should "a " " a ")                        ;both trailing
    (compat--should "aaa  " " aaa  ")
    (compat--should "a\t\n" "\t\ta\t\n")
    (compat--should "a  \n" "\n  \ta  \n")))

(ert-deftest compat-string-trim-right ()
  "Check if `compat--string-trim-right' was implemented correctly."
  (compat-test string-trim-right
    (compat--should "" "")                          ;empty string
    (compat--should "a" "a")                        ;"full" string
    (compat--should "aaa" "aaa")
    (compat--should "へっろ" "へっろ")
    (compat--should "hello world" "hello world")
    (compat--should "a" "a")                      ;right trailing
    (compat--should "aaa" "aaa")
    (compat--should "a" "a    ")
    (compat--should "a" "a\t\t")
    (compat--should "a" "a\n  \t")
    (compat--should " a" " a")                       ;left trailing
    (compat--should " aaa" " aaa")
    (compat--should "a" "a")
    (compat--should "\t\ta" "\t\ta")
    (compat--should "\n  \ta" "\n  \ta")
    (compat--should " a" " a ")                        ;both trailing
    (compat--should " aaa" " aaa")
    (compat--should "\t\ta" "\t\ta\t\n")
    (compat--should "\n  \ta" "\n  \ta  \n")))

(ert-deftest compat-string-trim ()
  "Check if `compat--string-trim' was implemented correctly."
  (compat-test string-trim
    (compat--should "" "")                          ;empty string
    (compat--should "a" "a")                        ;"full" string
    (compat--should "aaa" "aaa")
    (compat--should "へっろ" "へっろ")
    (compat--should "hello world" "hello world")
    (compat--should "a" "a ")                       ;right trailing
    (compat--should "aaa" "aaa ")
    (compat--should "a" "a    ")
    (compat--should "a" "a\t\t")
    (compat--should "a" "a\n  \t")
    (compat--should "a" " a")                       ;left trailing
    (compat--should "aaa" " aaa")
    (compat--should "a" "a")
    (compat--should "a" "\t\ta")
    (compat--should "a" "\n  \ta")
    (compat--should "a" " a ")                      ;both trailing
    (compat--should "aaa" " aaa  ")
    (compat--should "t\ta" "t\ta\t\n")
    (compat--should "a" "\n  \ta  \n")))

(ert-deftest compat-mapcan ()
  "Check if `compat--mapcan' was implemented correctly."
  (compat-test mapcan
    (compat--should nil #'identity nil)
    (compat--should (list 1)
                    #'identity
                    (list (list 1)))
    (compat--should (list 1 2 3 4)
                    #'identity
                    (list (list 1) (list 2 3) (list 4)))
    (compat--should (list (list 1) (list 2 3) (list 4))
                    #'list
                    (list (list 1) (list 2 3) (list 4)))
    (compat--should (list 1 2 3 4)
                    #'identity
                    (list (list 1) (list) (list 2 3) (list 4)))
    (compat--should (list (list 1) (list) (list 2 3) (list 4))
                    #'list
                    (list (list 1) (list) (list 2 3) (list 4)))
    (compat--should (list)
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
  "Check if `compat--caaar' was implemented correctly."
  (compat-test caaar
    (compat--should nil ())
    (compat--should 'a compat-cXXXr-test)))

(ert-deftest compat-caadr ()
  "Check if `compat--caadr' was implemented correctly."
  (compat-test caadr
    (compat--should nil ())
    (compat--should 'e compat-cXXXr-test)))

(ert-deftest compat-cadar ()
  "Check if `compat--cadar' was implemented correctly."
  (compat-test cadar
    (compat--should nil ())
    (compat--should 'c compat-cXXXr-test)))

(ert-deftest compat-caddr ()
  "Check if `compat--caddr' was implemented correctly."
  (compat-test caddr
    (compat--should nil ())
    (compat--should 'g compat-cXXXr-test)))

(ert-deftest compat-cdaar ()
  "Check if `compat--cdaar' was implemented correctly."
  (compat-test cdaar
    (compat--should nil ())
    (compat--should 'b compat-cXXXr-test)))

(ert-deftest compat-cdadr ()
  "Check if `compat--cdadr' was implemented correctly."
  (compat-test cdadr
    (compat--should nil ())
    (compat--should 'f compat-cXXXr-test)))

(ert-deftest compat-cddar ()
  "Check if `compat--cddar' was implemented correctly."
  (compat-test cddar
    (compat--should nil ())
    (compat--should 'd compat-cXXXr-test)))

(ert-deftest compat-cdddr ()
  "Check if `compat--cdddr' was implemented correctly."
  (compat-test cdddr
    (compat--should nil ())
    (compat--should 'h compat-cXXXr-test)
    #'cdddr))

(ert-deftest compat-caaaar ()
  "Check if `compat--caaaar' was implemented correctly."
  (compat-test caaaar
    (compat--should nil ())
    (compat--should 'a compat-cXXXXr-test)))

(ert-deftest compat-caaadr ()
  "Check if `compat--caaadr' was implemented correctly."
  (compat-test caaadr
    (compat--should nil ())
    (compat--should 'i compat-cXXXXr-test)))

(ert-deftest compat-caadar ()
  "Check if `compat--caadar' was implemented correctly."
  (compat-test caadar
    (compat--should nil ())
    (compat--should 'e compat-cXXXXr-test)))

(ert-deftest compat-caaddr ()
  "Check if `compat--caaddr' was implemented correctly."
  (compat-test caaddr
    (compat--should nil ())
    (compat--should 'm compat-cXXXXr-test)))

(ert-deftest compat-cadaar ()
  "Check if `compat--cadaar' was implemented correctly."
  (compat-test cadaar
    (compat--should nil ())
    (compat--should 'c compat-cXXXXr-test)))

(ert-deftest compat-cadadr ()
  "Check if `compat--cadadr' was implemented correctly."
  (compat-test cadadr
    (compat--should nil ())
    (compat--should 'k compat-cXXXXr-test)))

(ert-deftest compat-caddar ()
  "Check if `compat--caddar' was implemented correctly."
  (compat-test caddar
    (compat--should nil ())
    (compat--should 'g compat-cXXXXr-test)))

(ert-deftest compat-cadddr ()
  "Check if `compat--cadddr' was implemented correctly."
  (compat-test cadddr
    (compat--should nil ())
    (compat--should 'o compat-cXXXXr-test)))

(ert-deftest compat-cdaaar ()
  "Check if `compat--cdaaar' was implemented correctly."
  (compat-test cdaaar
    (compat--should nil ())
    (compat--should 'b compat-cXXXXr-test)))

(ert-deftest compat-cdaadr ()
  "Check if `compat--cdaadr' was implemented correctly."
  (compat-test cdaadr
    (compat--should nil ())
    (compat--should 'j compat-cXXXXr-test)))

(ert-deftest compat-cdadar ()
  "Check if `compat--cdadar' was implemented correctly."
  (compat-test cdadar
    (compat--should nil ())
    (compat--should 'f compat-cXXXXr-test)))

(ert-deftest compat-cdaddr ()
  "Check if `compat--cdaddr' was implemented correctly."
  (compat-test cdaddr
    (compat--should nil ())
    (compat--should 'j compat-cXXXXr-test)))

(ert-deftest compat-cddaar ()
  "Check if `compat--cddaar' was implemented correctly."
  (compat-test cddaar
    (compat--should nil ())
    (compat--should 'd compat-cXXXXr-test)))

(ert-deftest compat-cddadr ()
  "Check if `compat--cddadr' was implemented correctly."
  (compat-test cddadr
    (compat--should nil ())
    (compat--should 'l compat-cXXXXr-test)))

(ert-deftest compat-cdddar ()
  "Check if `compat--cdddar' was implemented correctly."
  (compat-test cdddar
    (compat--should nil ())
    (compat--should 'h compat-cXXXXr-test)))

(ert-deftest compat-string-greaterp ()
  "Check if `compat--string-greaterp' was implemented correctly."
  (compat-test string-greaterp
    (compat--should t "b" "a")
    (compat--should nil "a" "b")
    (compat--should t "aaab" "aaaa")
    (compat--should nil "aaaa" "aaab")))

(ert-deftest compat-sort ()
  "Check if `compat--sort' was advised correctly."
  (compat-test sort
    (compat--should* (list 1 2 3) (list 1 2 3) #'<)
    (compat--should* (list 1 2 3) (list 3 2 1) #'<)
    (compat--should* '[1 2 3] '[1 2 3] #'<)
    (compat--should* '[1 2 3] '[3 2 1] #'<)))

(ert-deftest compat-= ()
  "Check if `compat--=' was advised correctly."
  (compat-test =
    (compat--should* t 0 0)
    (compat--should* t 0 0 0)
    (compat--should* t 0 0 0 0)
    (compat--should* t 0 0 0 0 0)
    (compat--should* t 0.0 0.0)
    (compat--should* t +0.0 -0.0)
    (compat--should* t 0.0 0.0 0.0)
    (compat--should* t 0.0 0.0 0.0 0.0)
    (compat--should* nil 0 1)
    (compat--should* nil 0 0 1)
    (compat--should* nil 0 0 0 0 1)
    (compat--error* wrong-type-argument 0 0 'a)
    (compat--should* nil 0 1 'a)
    (compat--should* nil 0.0 0.0 0.0 0.1)))

(ert-deftest compat-< ()
  "Check if `compat--<' was advised correctly."
  (compat-test <
    (compat--should* nil 0 0)
    (compat--should* nil 0 0 0)
    (compat--should* nil 0 0 0 0)
    (compat--should* nil 0 0 0 0 0)
    (compat--should* nil 0.0 0.0)
    (compat--should* nil +0.0 -0.0)
    (compat--should* nil 0.0 0.0 0.0)
    (compat--should* nil 0.0 0.0 0.0 0.0)
    (compat--should* t 0 1)
    (compat--should* nil 1 0)
    (compat--should* nil 0 0 1)
    (compat--should* t 0 1 2)
    (compat--should* nil 2 1 0)
    (compat--should* nil 0 0 0 0 1)
    (compat--should* t 0 1 2 3 4)
    (compat--error* wrong-type-argument 0 1 'a)
    (compat--should* nil 0 0 'a)
    (compat--should* nil 0.0 0.0 0.0 0.1)
    (compat--should* t -0.1 0.0 0.2 0.4)
    (compat--should* t -0.1 0 0.2 0.4)))

(ert-deftest compat-> ()
  "Check if `compat-->' was advised correctly."
  (compat-test >
    (compat--should* nil 0 0)
    (compat--should* nil 0 0 0)
    (compat--should* nil 0 0 0 0)
    (compat--should* nil 0 0 0 0 0)
    (compat--should* nil 0.0 0.0)
    (compat--should* nil +0.0 -0.0)
    (compat--should* nil 0.0 0.0 0.0)
    (compat--should* nil 0.0 0.0 0.0 0.0)
    (compat--should* t 1 0)
    (compat--should* nil 1 0 0)
    (compat--should* nil 0 1 2)
    (compat--should* t 2 1 0)
    (compat--should* nil 1 0 0 0 0)
    (compat--should* t 4 3 2 1 0)
    (compat--should* nil 4 3 2 1 1)
    (compat--error* wrong-type-argument 1 0 'a)
    (compat--should* nil 0 0 'a)
    (compat--should* nil 0.1 0.0 0.0 0.0)
    (compat--should* t 0.4 0.2 0.0 -0.1)
    (compat--should* t 0.4 0.2 0 -0.1)))

(ert-deftest compat-<= ()
  "Check if `compat--<=' was advised correctly."
  (compat-test <=
    (compat--should* t 0 0)
    (compat--should* t 0 0 0)
    (compat--should* t 0 0 0 0)
    (compat--should* t 0 0 0 0 0)
    (compat--should* t 0.0 0.0)
    (compat--should* t +0.0 -0.0)
    (compat--should* t 0.0 0.0 0.0)
    (compat--should* t 0.0 0.0 0.0 0.0)
    (compat--should* nil 1 0)
    (compat--should* nil 1 0 0)
    (compat--should* t 0 1 2)
    (compat--should* nil 2 1 0)
    (compat--should* nil 1 0 0 0 0)
    (compat--should* nil 4 3 2 1 0)
    (compat--should* nil 4 3 2 1 1)
    (compat--should* t 0 1 2 3 4)
    (compat--should* t 1 1 2 3 4)
    (compat--error* wrong-type-argument 0 0 'a)
    (compat--error* wrong-type-argument 0 1 'a)
    (compat--should* nil 1 0 'a)
    (compat--should* nil 0.1 0.0 0.0 0.0)
    (compat--should* t 0.0 0.0 0.0 0.1)
    (compat--should* t -0.1 0.0 0.2 0.4)
    (compat--should* t -0.1 0.0 0.0 0.2 0.4)
    (compat--should* t -0.1 0.0 0 0.2 0.4)
    (compat--should* t -0.1 0 0.2 0.4)
    (compat--should* nil 0.4 0.2 0.0 -0.1)
    (compat--should* nil 0.4 0.2 0.0 0.0 -0.1)
    (compat--should* nil 0.4 0.2 0 0.0 0.0 -0.1)
    (compat--should* nil 0.4 0.2 0 -0.1)))

(ert-deftest compat->= ()
  "Check if `compat-->=' was implemented correctly."
  (compat-test >=
    (compat--should* t 0 0)
    (compat--should* t 0 0 0)
    (compat--should* t 0 0 0 0)
    (compat--should* t 0 0 0 0 0)
    (compat--should* t 0.0 0.0)
    (compat--should* t +0.0 -0.0)
    (compat--should* t 0.0 0.0 0.0)
    (compat--should* t 0.0 0.0 0.0 0.0)
    (compat--should* t 1 0)
    (compat--should* t 1 0 0)
    (compat--should* nil 0 1 2)
    (compat--should* t 2 1 0)
    (compat--should* t 1 0 0 0 0)
    (compat--should* t 4 3 2 1 0)
    (compat--should* t 4 3 2 1 1)
    (compat--error* wrong-type-argument 0 0 'a)
    (compat--error* wrong-type-argument 1 0 'a)
    (compat--should* nil 0 1 'a)
    (compat--should* t 0.1 0.0 0.0 0.0)
    (compat--should* nil 0.0 0.0 0.0 0.1)
    (compat--should* nil -0.1 0.0 0.2 0.4)
    (compat--should* nil -0.1 0.0 0.0 0.2 0.4)
    (compat--should* nil -0.1 0.0 0 0.2 0.4)
    (compat--should* nil -0.1 0 0.2 0.4)
    (compat--should* t 0.4 0.2 0.0 -0.1)
    (compat--should* t 0.4 0.2 0.0 0.0 -0.1)
    (compat--should* t 0.4 0.2 0 0.0 0.0 -0.1)
    (compat--should* t 0.4 0.2 0 -0.1)))

(ert-deftest compat-special-form-p ()
  "Check if `compat--special-form-p' was implemented correctly."
  (compat-test special-form-p
    (compat--should t 'if)
    (compat--should t 'cond)
    (compat--should nil 'when)
    (compat--should nil 'defun)
    (compat--should nil '+)
    (compat--should nil nil)
    (compat--should nil "macro")
    (compat--should nil '(macro . +))))

(ert-deftest compat-macrop ()
  "Check if `compat--macrop' was implemented correctly."
  (compat-test macrop
    (compat--should t 'lambda)
    (compat--should t 'defun)
    (compat--should t 'defmacro)
    (compat--should nil 'defalias)
    (compat--should nil 'foobar)
    (compat--should nil 'if)
    (compat--should nil '+)
    (compat--should nil 1)
    (compat--should nil nil)
    (compat--should nil "macro")
    (compat--should t '(macro . +))))

(ert-deftest compat-string-suffix-p ()
  "Check if `compat--string-suffix-p' was implemented correctly."
  (compat-test string-suffix-p
    (compat--should t "a" "abba")
    (compat--should t "ba" "abba")
    (compat--should t "abba" "abba")
    (compat--should nil "a" "ABBA")
    (compat--should nil "bA" "ABBA")
    (compat--should nil "aBBA" "ABBA")
    (compat--should nil "c" "ABBA")
    (compat--should nil "c" "abba")
    (compat--should nil "cddc" "abba")
    (compat--should nil "aabba" "abba")))

(ert-deftest compat-split-string ()
  "Check if `compat--split-string' was advised correctly."
  (compat-test split-string
    (compat--should* '("a" "b" "c") "a b c")
    (compat--should* '("..a.." "..b.." "..c..") "..a.. ..b.. ..c..")
    (compat--should* '("a" "b" "c") "..a.. ..b.. ..c.." nil nil "\\.+")))

(ert-deftest compat-delete-consecutive-dups ()
  "Check if `compat--delete-consecutive-dups' was implemented correctly."
  (compat-test delete-consecutive-dups
    (compat--should '(1 2 3 4) '(1 2 3 4))
    (compat--should '(1 2 3 4) '(1 2 2 3 4 4))
    (compat--should '(1 2 3 2 4) '(1 2 2 3 2 4 4))))

(ert-deftest compat-string-clean-whitespace ()
  "Check if `compat--string-clean-whitespace' was implemented correctly."
  (compat-test string-clean-whitespace
    (compat--should "a b c" "a b c")
    (compat--should "a b c" "   a b c")
    (compat--should "a b c" "a b c   ")
    (compat--should "a b c" "a    b c")
    (compat--should "a b c" "a b    c")
    (compat--should "a b c" "a    b    c")
    (compat--should "a b c" "   a    b    c")
    (compat--should "a b c" "a    b    c    ")
    (compat--should "a b c" "   a    b    c    ")
    (compat--should "aa bb cc" "aa bb cc")
    (compat--should "aa bb cc" "   aa bb cc")
    (compat--should "aa bb cc" "aa bb cc   ")
    (compat--should "aa bb cc" "aa    bb cc")
    (compat--should "aa bb cc" "aa bb    cc")
    (compat--should "aa bb cc" "aa    bb    cc")
    (compat--should "aa bb cc" "   aa    bb    cc")
    (compat--should "aa bb cc" "aa    bb    cc    ")
    (compat--should "aa bb cc" "   aa    bb    cc    ")))

(ert-deftest compat-string-fill ()
  "Check if `compat--string-fill' was implemented correctly."
  (compat-test string-fill
    (compat--should "a a a a a" "a a a a a" 9)
    (compat--should "a a a a a" "a a a a a" 10)
    (compat--should "a a a a\na" "a a a a a" 8)
    (compat--should "a a a a\na" "a  a  a  a  a" 8)
    (compat--should "a a\na a\na" "a a a a a" 4)
    (compat--should "a\na\na\na\na" "a a a a a" 2)
    (compat--should "a\na\na\na\na" "a a a a a" 1)))

(ert-deftest compat-string-lines ()
  "Check if `compat--string-lines' was implemented correctly."
  (compat-test string-lines
    (compat--should '("a" "b" "c") "a\nb\nc")
    (compat--should '("a" "b" "c" "") "a\nb\nc\n")
    (compat--should '("a" "b" "c") "a\nb\nc\n" t)
    (compat--should '("abc" "bcd" "cde") "abc\nbcd\ncde")
    (compat--should '(" abc" " bcd " "cde ") " abc\n bcd \ncde ")))

(ert-deftest compat-string-pad ()
  "Check if `compat--string-pad' was implemented correctly."
  (compat-test string-pad
    (compat--should "a   " "a" 4)
    (compat--should "aaaa" "aaaa" 4)
    (compat--should "aaaaaa" "aaaaaa" 4)
    (compat--should "a..." "a" 4 ?.)
    (compat--should "   a" "a" 4 nil t)
    (compat--should "...a" "a" 4 ?. t)))

(ert-deftest compat-string-chop-newline ()
  "Check if `compat--string-chop-newline' was implemented correctly."
  (compat-test string-chop-newline
    (compat--should "" "")
    (compat--should "" "\n")
    (compat--should "aaa" "aaa")
    (compat--should "aaa" "aaa\n")
    (compat--should "aaa\n" "aaa\n\n")))

(ert-deftest compat-macroexpand-1 ()
  "Check if `compat--macroexpand-1' was implemented correctly."
  (compat-test macroexpand-1
    (compat--should '(if a b c) '(if a b c))
    (compat--should '(if a (progn b)) '(when a b))
    (compat--should '(if a (progn (unless b c))) '(when a (unless b c)))))

(ert-deftest compat-file-size-human-readable ()
  "Check if `compat--file-size-human-readable' was advised properly."
  (compat-test file-size-human-readable
    (compat--should* "1000" 1000)
    (compat--should* "1k" 1024)
    (compat--should* "1M" (* 1024 1024))
    (compat--should* "1G" (expt 1024 3))
    (compat--should* "1T" (expt 1024 4))
    (compat--should* "1k" 1000 'si)
    (compat--should* "1KiB" 1024 'iec)
    (compat--should* "1KiB" 1024 'iec)
    (compat--should* "1 KiB" 1024 'iec " ")
    (compat--should* "1KiA" 1024 'iec nil "A")
    (compat--should* "1 KiA" 1024 'iec " " "A")
    (compat--should* "1kA" 1000 'si nil "A")
    (compat--should* "1 k" 1000 'si " ")
    (compat--should* "1 kA" 1000 'si " " "A")))

(ert-deftest compat-format-prompt ()
  "Check if `compat--file-size-human-readable' was implemented properly."
  (compat-test format-prompt
    (compat--should "Prompt: " "Prompt" nil)
    (compat--should "Prompt (default 3): " "Prompt" 3)
    (compat--should "Prompt (default abc): " "Prompt" "abc")
    (compat--should "Prompt (default abc def): " "Prompt" "abc def")
    (compat--should "Prompt 10: " "Prompt %d" nil 10)
    (compat--should "Prompt \"abc\" (default 3): " "Prompt %S" 3 "abc")))

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
  (should (eq (compat--named-let loop ((x 1))
                (if (> x 0)
                    (condition-case nil
                        (loop (1- x))
                      (arith-error 'ok))
                  (/ 1 x)))
              'ok))
  (should (eq (compat--named-let loop ((n 10000))
                (if (> n 0)
                    (condition-case nil
                        (/ n 0)
                      (arith-error (loop (1- n))))
                  'ok))
              'ok))
  (should (eq (compat--named-let loop ((x nil))
                (cond (x)
                      (t 'ok)))
              'ok))
  (should (eq (compat--named-let loop ((x 100000))
                (cond ((= x 0) 'ok)
                      ((loop (1- x)))))
              'ok))
  (should (eq (compat--named-let loop ((x 100000))
                (cond
                 ((= x -1) nil)
                 ((= x 0) 'ok)
                 ((loop -1))
                 ((loop (1- x)))))
              'ok))
  (should (eq (compat--named-let loop ((x 10000))
                (cond ((= x 0) 'ok)
                      ((and t (loop (1- x))))))
              'ok))
  (should (eq (eval
               (let ((branch '((loop (and (setq b (not b)) (1+ i))))))
                 `(let ((b t))
                    (compat--named-let loop ((i 0))
                      (cond ((null i) nil)
                            ((= i 10000) 'ok)
                            ,branch
                            ,branch))))
               t)
              'ok)))

(ert-deftest compat-directory-name-p ()
  "Check if `compat--directory-name-p' was implemented properly."
  (compat-test directory-name-p
    (compat--should t "/")
    (compat--should nil "/file")
    (compat--should nil "/dir/file")
    (compat--should t "/dir/")
    (compat--should nil "/dir")
    (compat--should t "/dir/subdir/")
    (compat--should nil "/dir/subdir")
    (compat--should t "dir/")
    (compat--should nil "file")
    (compat--should nil "dir/file")
    (compat--should t "dir/subdir/")
    (compat--should nil "dir/subdir")))

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

(ert-deftest compat-and-let* ()
  "Check if `compat--if-let*' was implemented properly."
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

(ert-deftest compat-json-parse-string ()
  "Check if `compat--json-parse-string' was implemented properly."
  (compat-test (json-parse-string (if (version<= "28" emacs-version)
                                      (apply-partially #'compat--json-parse-string-handle-tlo
                                                       #'json-parse-string)
                                    #'compat--json-parse-string))
    (compat--should 0 "0")
    (compat--should 1 "1")
    (compat--should 0.5 "0.5")
    (compat--should [1 2 3] "[1,2,3]")
    (compat--should ["a" 2 3] "[\"a\",2,3]")
    (compat--should [["a" 2] 3] "[[\"a\",2],3]")
    (compat--should '(("a" 2) 3) "[[\"a\",2],3]" :array-type 'list)
    (compat--should 'foo "null" :null-object 'foo)
    (compat--should ["false" t] "[false, true]" :false-object "false"))
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

(ert-deftest compat-lookup-key ()
  "Check if `compat-lookup-key' was implemented properly."
  (let ((a-map (make-sparse-keymap))
        (b-map (make-sparse-keymap)))
    (define-key a-map "x" 'foo)
    (define-key b-map "x" 'bar)
    (compat-test lookup-key
      (compat--should* 'foo a-map "x")
      (compat--should* 'bar b-map "x")
      (compat--should* 'foo (list a-map b-map) "x")
      (compat--should* 'bar (list b-map a-map) "x"))))

(provide 'compat-tests)
;;; compat-tests.el ends here
