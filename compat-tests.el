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
;; Note that not all functions have tests yet.  Grep the Compat code for
;; <UNTESTED> labels.  If you intend to use a function, which doesn't have
;; tests yet, please contribute tests here.  NO GUARANTEES ARE MADE FOR
;; FUNCTIONS WITHOUT TESTS.

;; The tests are written in a simple, explicit style.  Please inspect the
;; tests in order to find out the supported calling conventions.  In
;; particular, note the use of `compat-call' to call functions, where the
;; calling convention or behavior changed between Emacs versions.

;; The functions tested here are guaranteed to work on the Emacs versions
;; tested by continuous integration.  This includes 24.4, 24.5, 25.1, 25.2,
;; 25.3, 26.1, 26.2, 26.3, 27.1, 27.2, 28.1 and the current Emacs master
;; branch.

;;; Code:

(require 'ert)
(require 'compat)
(require 'subr-x)
(require 'time-date)
(require 'text-property-search nil t)

(defmacro should-equal (a b)
  `(should (equal ,a ,b)))

(ert-deftest compat-loaded-features ()
  (let ((version 0))
    (while (< version 30)
      (should-equal (> version emacs-major-version)
                    (featurep (intern (format "compat-%s" version))))
      (setq version (1+ version)))))

(ert-deftest compat-function ()
  (let ((sym (compat-function plist-put)) list)
    (should sym)
    (should (symbolp sym))
    (setq list (funcall sym list "first" 1 #'string=))
    (should (eq (compat-call plist-get list "first" #'string=) 1))))

(ert-deftest get-display-property ()
  (with-temp-buffer
    (insert (propertize "foo" 'face 'bold 'display '(height 2.0)))
    (should-equal (get-display-property 2 'height) 2.0))
  (with-temp-buffer
    (insert (propertize "foo" 'face 'bold 'display '((height 2.0)
                                                     (space-width 2.0))))
    (should-equal (get-display-property 2 'height) 2.0)
    (should-equal (get-display-property 2 'space-width) 2.0))
  (with-temp-buffer
    (insert (propertize "foo bar" 'face 'bold
                        'display '[(height 2.0)
                                   (space-width 20)]))
    (should-equal (get-display-property 2 'height) 2.0)
    (should-equal (get-display-property 2 'space-width) 20)))

(ert-deftest add-display-text-property ()
  (with-temp-buffer
    (insert "Foo bar zot gazonk")
    (add-display-text-property 4 8 'height 2.0)
    (add-display-text-property 2 12 'raise 0.5)
    (should-equal (get-text-property 2 'display) '(raise 0.5))
    (should-equal (get-text-property 5 'display)
                   '((raise 0.5) (height 2.0)))
    (should-equal (get-text-property 9 'display) '(raise 0.5)))
  (with-temp-buffer
    (insert "Foo bar zot gazonk")
    (put-text-property 4 8 'display [(height 2.0)])
    (add-display-text-property 2 12 'raise 0.5)
    (should-equal (get-text-property 2 'display) '(raise 0.5))
    (should-equal (get-text-property 5 'display)
                   [(raise 0.5) (height 2.0)])
    (should-equal (get-text-property 9 'display) '(raise 0.5)))
  (with-temp-buffer
    (should-equal (let ((str "some useless string"))
                     (add-display-text-property 4 8 'height 2.0 str)
                     (add-display-text-property 2 12 'raise 0.5 str)
                     str)
                   #("some useless string"
                     2 4 (display (raise 0.5))
                     4 8 (display ((raise 0.5) (height 2.0)))
                     8 12 (display (raise 0.5))))))

(ert-deftest line-number-at-pos ()
  (with-temp-buffer
    (insert "\n\n\n")
    (narrow-to-region (1+ (point-min)) (point-max))
    (should-equal 1 (compat-call line-number-at-pos (point-min)))
    (should-equal 2 (compat-call line-number-at-pos (1+ (point-min))))
    (should-equal 2 (compat-call line-number-at-pos (point-min) 'abs))
    (should-equal 3 (compat-call line-number-at-pos (1+ (point-min)) 'abs))))

(defvar compat-tests--map-1
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-f") #'find-file)
    (define-key map (kbd "SPC") #'minibuffer-complete-word)
    (define-key map (kbd "C-c") mode-specific-map)
    map))
(defvar-keymap compat-tests--map-2
  "C-x C-f" #'find-file
  "SPC" #'minibuffer-complete-word
  "C-c" mode-specific-map)
(defvar compat-tests--map-3
  (define-keymap
    "C-x C-f" #'find-file
    "SPC" #'minibuffer-complete-word
    "C-c" mode-specific-map))
(ert-deftest defvar-keymap ()
  (should-equal compat-tests--map-1 compat-tests--map-2)
  (should-equal compat-tests--map-1 compat-tests--map-3))

(ert-deftest key-valid-p ()
  (should-not (key-valid-p ""))
  (should (key-valid-p "f"))
  (should (key-valid-p "X"))
  (should-not (key-valid-p " X"))
  (should (key-valid-p "X f"))
  (should-not (key-valid-p "a  b"))
  (should-not (key-valid-p "foobar"))
  (should-not (key-valid-p "return"))

  (should (key-valid-p "<F2>"))
  (should (key-valid-p "<f1> <f2> TAB"))
  (should (key-valid-p "<f1> RET"))
  (should (key-valid-p "<f1> SPC"))
  (should (key-valid-p "<f1>"))
  (should-not (key-valid-p "[f1]"))
  (should (key-valid-p "<return>"))
  (should-not (key-valid-p "< right >"))

  ;; Modifiers:
  (should (key-valid-p "C-x"))
  (should (key-valid-p "C-x a"))
  (should (key-valid-p "C-;"))
  (should (key-valid-p "C-a"))
  (should (key-valid-p "C-c SPC"))
  (should (key-valid-p "C-c TAB"))
  (should (key-valid-p "C-c c"))
  (should (key-valid-p "C-x 4 C-f"))
  (should (key-valid-p "C-x C-f"))
  (should (key-valid-p "C-M-<down>"))
  (should-not (key-valid-p "<C-M-down>"))
  (should (key-valid-p "C-RET"))
  (should (key-valid-p "C-SPC"))
  (should (key-valid-p "C-TAB"))
  (should (key-valid-p "C-<down>"))
  (should (key-valid-p "C-c C-c C-c"))

  (should (key-valid-p "M-a"))
  (should (key-valid-p "M-<DEL>"))
  (should-not (key-valid-p "M-C-a"))
  (should (key-valid-p "C-M-a"))
  (should (key-valid-p "M-ESC"))
  (should (key-valid-p "M-RET"))
  (should (key-valid-p "M-SPC"))
  (should (key-valid-p "M-TAB"))
  (should (key-valid-p "M-x a"))
  (should (key-valid-p "M-<up>"))
  (should (key-valid-p "M-c M-c M-c"))

  (should (key-valid-p "s-SPC"))
  (should (key-valid-p "s-a"))
  (should (key-valid-p "s-x a"))
  (should (key-valid-p "s-c s-c s-c"))

  (should-not (key-valid-p "S-H-a"))
  (should (key-valid-p "S-a"))
  (should (key-valid-p "S-x a"))
  (should (key-valid-p "S-c S-c S-c"))

  (should (key-valid-p "H-<RET>"))
  (should (key-valid-p "H-DEL"))
  (should (key-valid-p "H-a"))
  (should (key-valid-p "H-x a"))
  (should (key-valid-p "H-c H-c H-c"))

  (should (key-valid-p "A-H-a"))
  (should (key-valid-p "A-SPC"))
  (should (key-valid-p "A-TAB"))
  (should (key-valid-p "A-a"))
  (should (key-valid-p "A-c A-c A-c"))

  (should (key-valid-p "C-M-a"))
  (should (key-valid-p "C-M-<up>"))

  ;; Special characters.
  (should (key-valid-p "DEL"))
  (should (key-valid-p "ESC C-a"))
  (should (key-valid-p "ESC"))
  (should (key-valid-p "LFD"))
  (should (key-valid-p "NUL"))
  (should (key-valid-p "RET"))
  (should (key-valid-p "SPC"))
  (should (key-valid-p "TAB"))
  (should-not (key-valid-p "\^i"))
  (should-not (key-valid-p "^M"))

  ;; With numbers.
  (should-not (key-valid-p "\177"))
  (should-not (key-valid-p "\000"))
  (should-not (key-valid-p "\\177"))
  (should-not (key-valid-p "\\000"))
  (should-not (key-valid-p "C-x \\150"))

  ;; Multibyte
  (should (key-valid-p "ñ"))
  (should (key-valid-p "ü"))
  (should (key-valid-p "ö"))
  (should (key-valid-p "ğ"))
  (should (key-valid-p "ա"))
  (should-not (key-valid-p "üüöö"))
  (should (key-valid-p "C-ü"))
  (should (key-valid-p "M-ü"))
  (should (key-valid-p "H-ü"))

  ;; Handle both new and old style key descriptions (bug#45536).
  (should (key-valid-p "s-<return>"))
  (should-not (key-valid-p "<s-return>"))
  (should (key-valid-p "C-M-<return>"))
  (should-not (key-valid-p "<C-M-return>"))

  (should (key-valid-p "<mouse-1>"))
  (should (key-valid-p "<Scroll_Lock>"))

  (should-not (key-valid-p "c-x"))
  (should-not (key-valid-p "C-xx"))
  (should-not (key-valid-p "M-xx"))
  (should-not (key-valid-p "M-x<TAB>")))

(defun compat-tests--function-put ())
(ert-deftest function-put ()
  (function-put #'compat-tests--function-put 'compat-test 42)
  (should-equal 42 (function-get #'compat-tests--function-put 'compat-test)))

(ert-deftest ignore-error ()
  (should-equal (ignore-error (end-of-file)
                  (read ""))
                nil)
  (should-equal (ignore-error end-of-file
                  (read ""))
                nil)
  (should-error (ignore-error foo
                  (read ""))))

(ert-deftest thread-first ()
  (should-equal (thread-first (+ 40 2)) 42)
  (should-equal (thread-first
                   5
                   (+ 20)
                   (/ 25)
                   -
                   (+ 40)) 39)
  (should-equal (thread-first
                   "this-is-a-string"
                   (split-string "-")
                   (nbutlast 2)
                   (append (list "good")))
                 (list "this" "is" "good")))

(ert-deftest thread-last ()
  (should-equal (thread-last (+ 40 2)) 42)
  (should-equal (thread-last
                   5
                   (+ 20)
                   (/ 25)
                   -
                   (+ 40)) 39)
  (should-equal (thread-last
                   (list 1 -2 3 -4 5)
                   (mapcar #'abs)
                   (cl-reduce #'+)
                   (format "abs sum is: %s"))
                 "abs sum is: 15"))

(ert-deftest ntake ()
  (should-not (ntake 5 nil))
  (should-equal '(1 2) (ntake 5 '(1 2)))
  (should-equal '(1 2 3) (ntake 3 '(1 2 3 4))))

(ert-deftest take ()
  (should-not (take 5 nil))
  (should-equal '(1 2) (take 5 '(1 2)))
  (should-equal '(1 2 3) (take 3 '(1 2 3 4))))

(ert-deftest format-message ()
  (should-equal (format-message "a=%s b=%s" 1 2) "a=1 b=2"))

(defvar compat-tests--boundp)
(defvar compat-tests--global-boundp)
(ert-deftest buffer-local-boundp ()
  (let ((buf (generate-new-buffer "boundp")))
    (with-current-buffer buf
      (setq-local compat-tests--boundp t))
    (setq compat-tests--global-boundp t)
    (should (buffer-local-boundp 'compat-tests--boundp buf))
    (should-not (buffer-local-boundp 'test-not-boundp buf))
    (should (buffer-local-boundp 'compat-tests--global-boundp buf))))

(defvar compat-tests--local-a nil)
(defvar compat-tests--local-b nil)
(defvar compat-tests--local-c nil)
(ert-deftest setq-local ()
  (compat-call setq-local compat-tests--local-a 1 compat-tests--local-b 2 compat-tests--local-c 3)
  (should-equal compat-tests--local-a 1)
  (should-equal compat-tests--local-b 2)
  (should-equal compat-tests--local-c 3))

(ert-deftest gensym ()
  (should (symbolp (gensym "compat")))
  (should (string-prefix-p "compat" (symbol-name (gensym 'compat))))
  (should (string-prefix-p "compat" (symbol-name (gensym "compat")))))

(ert-deftest plist-get ()
  (let (list)
    (setq list (compat-call plist-put list 'first 1))
    (setq list (compat-call plist-put list 'second 2))
    (setq list (compat-call plist-put list 'first 10))
    (should (eq (compat-call plist-get list 'first) 10))
    (should (eq (compat-call plist-get list 'second) 2))
    (should (compat-call plist-member list 'first))
    (should-not (compat-call plist-member list 'third)))
  (let (list)
    (setq list (compat-call plist-put list "first" 1 #'string=))
    (setq list (compat-call plist-put list "second" 2 #'string=))
    (setq list (compat-call plist-put list "first" 10 #'string=))
    (should (eq (compat-call plist-get list "first" #'string=) 10))
    (should (eq (compat-call plist-get list "second" #'string=) 2))
    (should (compat-call plist-member list "first" #'string=))
    (should-not (compat-call plist-member list "third" #'string=))))

(ert-deftest garbage-collect-maybe ()
  (garbage-collect-maybe 10))

(ert-deftest insert-into-buffer ()
  ;; Without optional compat--arguments
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
        (insert "def")
        (insert-into-buffer other))
      (should (string= (buffer-string) "abcdef"))))
  ;; With one optional argument
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
        (insert "def")
        (insert-into-buffer other 2))
      (should (string= (buffer-string) "abcef"))))
  ;; With two optional arguments
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
        (insert "def")
        (insert-into-buffer other 2 3))
      (should (string= (buffer-string) "abce")))))

(ert-deftest bool-vector ()
  (should-equal (bool-vector) (bool-vector-not (bool-vector)))
  (should-equal (bool-vector t) (bool-vector-not (bool-vector nil)))
  (should-equal (bool-vector nil) (bool-vector-not (bool-vector t)))
  (should-equal (bool-vector t t) (bool-vector-not (bool-vector nil nil)))
  (should-equal (bool-vector t nil) (bool-vector-not (bool-vector nil t)))
  (should-equal (bool-vector nil t) (bool-vector-not (bool-vector t nil)))
  (should-equal (bool-vector nil nil) (bool-vector-not (bool-vector t t))))

(ert-deftest assoc ()
  ;; Fallback behaviour:
  (should-not (compat-call assoc 1 nil))               ;empty list
  (should-equal '(1) (compat-call assoc 1 '((1))))            ;single element list
  (should-not (compat-call assoc 1 '(1)))
  (should-equal '(2) (compat-call assoc 2 '((1) (2) (3))))    ;multiple element list
  (should-not (compat-call assoc 2 '(1 2 3)))
  (should-equal '(2) (compat-call assoc 2 '(1 (2) 3)))
  (should-not (compat-call assoc 2 '((1) 2 (3))))
  (should-equal '(1) (compat-call assoc 1 '((3) (2) (1))))
  (should-equal '("a") (compat-call assoc "a" '(("a") ("b") ("c"))))  ;non-primitive elements
  (should-equal '("a" 0) (compat-call assoc "a" '(("c" . "a") "b" ("a" 0))))
  ;; With testfn:
  (should-equal '(1) (compat-call assoc 3 '((10) (4) (1) (9)) #'<))
  (should-equal '("a") (compat-call assoc "b" '(("c") ("a") ("b")) #'string-lessp))
  (should-equal '("b") (compat-call assoc "a" '(("a") ("a") ("b"))
         (lambda (s1 s2) (not (string= s1 s2)))))
  (should-equal
   '("\\.el\\'" . emacs-lisp-mode)
   (compat-call assoc
                "file.el"
                '(("\\.c\\'" . c-mode)
                  ("\\.p\\'" . pascal-mode)
                  ("\\.el\\'" . emacs-lisp-mode)
                  ("\\.awk\\'" . awk-mode))
                #'string-match-p)))

(ert-deftest assoc-delete-all ()
  (should-equal (list) (compat-call assoc-delete-all 0 (list)))
  ;; Test `eq'
  (should-equal '((1 . one)) (compat-call assoc-delete-all 0 (list (cons 1 'one))))
  (should-equal '((1 . one) a) (compat-call assoc-delete-all 0 (list (cons 1 'one) 'a)))
  (should-equal '((1 . one)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one))))
  (should-equal '((1 . one)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 0 'zero) (cons 1 'one))))
  (should-equal '((1 . one)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one) (cons 0 'zero))))
  (should-equal '((1 . one) a) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one) 'a  (cons 0 'zero))))
  (should-equal '(a (1 . one)) (compat-call assoc-delete-all 0 (list 'a (cons 0 'zero) (cons 1 'one) (cons 0 'zero))))
  ;; Test `equal'
  (should-equal '(("one" . one)) (compat-call assoc-delete-all "zero" (list (cons "one" 'one))))
  (should-equal '(("one" . one) a) (compat-call assoc-delete-all "zero" (list (cons "one" 'one) 'a)))
  (should-equal '(("one" . one)) (compat-call assoc-delete-all "zero" (list (cons "zero" 'zero) (cons "one" 'one))))
  (should-equal '(("one" . one)) (compat-call assoc-delete-all "zero" (list (cons "zero" 'zero) (cons "zero" 'zero) (cons "one" 'one))))
  (should-equal '(("one" . one)) (compat-call assoc-delete-all "zero" (list (cons "zero" 'zero) (cons "one" 'one) (cons "zero" 'zero))))
  (should-equal '(("one" . one) a) (compat-call assoc-delete-all "zero" (list (cons "zero" 'zero) (cons "one" 'one) 'a  (cons "zero" 'zero))))
  (should-equal '(a ("one" . one)) (compat-call assoc-delete-all "zero" (list 'a (cons "zero" 'zero) (cons "one" 'one) (cons "zero" 'zero))))
  ;; Test custom predicate
  (should-equal '() (compat-call assoc-delete-all 0 (list (cons 1 'one)) #'/=))
  (should-equal '(a) (compat-call assoc-delete-all 0 (list (cons 1 'one) 'a) #'/=))
  (should-equal '((0 . zero)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one)) #'/=))
  (should-equal '((0 . zero) (0 . zero)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 0 'zero) (cons 1 'one)) #'/=))
  (should-equal '((0 . zero) (0 . zero)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one) (cons 0 'zero)) #'/=))
  (should-equal '((0 . zero) a (0 . zero)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one) 'a  (cons 0 'zero)) #'/=))
  (should-equal '(a (0 . zero) (0 . zero)) (compat-call assoc-delete-all 0 (list 'a (cons 0 'zero) (cons 1 'one) (cons 0 'zero)) #'/=)))

(ert-deftest derived-mode-p ()
  (let ((one (make-symbol "1"))
        (two (make-symbol "2"))
        (three (make-symbol "3"))
        (one.5 (make-symbol "1.5"))
        (eins (make-symbol "𝟙")))
    (put two 'derived-mode-parent one)
    (put one.5 'derived-mode-parent one)
    (put three 'derived-mode-parent two)
    (should-equal one (provided-mode-derived-p one one))
    (should-equal one (provided-mode-derived-p two one))
    (should-equal one (provided-mode-derived-p three one))
    (should-not (provided-mode-derived-p one eins))
    (should-not (provided-mode-derived-p two eins))
    (should-not (provided-mode-derived-p two one.5))
    (should-equal one (provided-mode-derived-p two one.5 one))
    (should-equal two (provided-mode-derived-p two one.5 two))
    (should-equal one (provided-mode-derived-p three one.5 one))
    (should-equal two (provided-mode-derived-p three one.5 one two))
    (should-equal two (provided-mode-derived-p three one.5 two one))
    (should-equal three (provided-mode-derived-p three one.5 two one three))
    (should-equal three (provided-mode-derived-p three one.5 three two one))
    (let ((major-mode three))
      (should-equal one (derived-mode-p one))
      (should-equal one (derived-mode-p one.5 one))
      (should-equal two (derived-mode-p one.5 one two))
      (should-equal two (derived-mode-p one.5 two one))
      (should-equal three (derived-mode-p one.5 two one three))
      (should-equal three (derived-mode-p one.5 three two one)))))

(ert-deftest format-prompt ()
  (should-equal "Prompt: " (format-prompt "Prompt" nil))
  (should-equal "Prompt: " (format-prompt "Prompt" ""))
  (should-equal "Prompt (default  ): " (format-prompt "Prompt" " "))
  (should-equal "Prompt (default 3): " (format-prompt "Prompt" 3))
  (should-equal "Prompt (default abc): " (format-prompt "Prompt" "abc"))
  (should-equal "Prompt (default abc def): " (format-prompt "Prompt" "abc def"))
  (should-equal "Prompt 10: " (format-prompt "Prompt %d" nil 10))
  (should-equal "Prompt \"abc\" (default 3): " (format-prompt "Prompt %S" 3 "abc")))

(ert-deftest cXXXr ()
  (let ((xxx '(((a . b) . (c . d)) . ((e . f) . (g . h)))))
    (should-not (caaar ()))
    (should-not (caadr ()))
    (should-not (cadar ()))
    (should-not (caddr ()))
    (should-not (cdaar ()))
    (should-not (cdadr ()))
    (should-not (cddar ()))
    (should-not (cdddr ()))
    (should-equal 'a (caaar xxx))
    (should-equal 'e (caadr xxx))
    (should-equal 'c (cadar xxx))
    (should-equal 'g (caddr xxx))
    (should-equal 'b (cdaar xxx))
    (should-equal 'f (cdadr xxx))
    (should-equal 'd (cddar xxx))
    (should-equal 'h (cdddr xxx))))

(ert-deftest cXXXXr ()
  (let ((xxxx
         '((((a . b) . (c . d)) . ((e . f) . (g . h))) .
           (((i . j) . (k . l)) . ((m . j) . (o . p))))))
    (should-not (caaaar ()))
    (should-not (caaadr ()))
    (should-not (caadar ()))
    (should-not (caaddr ()))
    (should-not (cadaar ()))
    (should-not (cadadr ()))
    (should-not (caddar ()))
    (should-not (cadddr ()))
    (should-not (cdaaar ()))
    (should-not (cdaadr ()))
    (should-not (cdadar ()))
    (should-not (cdaddr ()))
    (should-not (cddaar ()))
    (should-not (cddadr ()))
    (should-not (cdddar ()))
    (should-equal 'a (caaaar xxxx))
    (should-equal 'i (caaadr xxxx))
    (should-equal 'e (caadar xxxx))
    (should-equal 'm (caaddr xxxx))
    (should-equal 'c (cadaar xxxx))
    (should-equal 'k (cadadr xxxx))
    (should-equal 'g (caddar xxxx))
    (should-equal 'o (cadddr xxxx))
    (should-equal 'b (cdaaar xxxx))
    (should-equal 'j (cdaadr xxxx))
    (should-equal 'f (cdadar xxxx))
    (should-equal 'j (cdaddr xxxx))
    (should-equal 'd (cddaar xxxx))
    (should-equal 'l (cddadr xxxx))
    (should-equal 'h (cdddar xxxx))))

(ert-deftest subr-primitive-p ()
  (should (subr-primitive-p (symbol-function 'identity)))       ;function from fns.c
  (unless (fboundp 'subr-native-elisp-p)
    (should-not (subr-primitive-p (symbol-function 'match-string)))) ;function from subr.el
  (should-not (subr-primitive-p (symbol-function 'defun)))        ;macro from subr.el
  (should-not (subr-primitive-p nil)))

(ert-deftest mapcan ()
  (should-not (mapcan #'identity nil))
  (should-equal (list 1)
                 (mapcan #'identity
                         (list (list 1))))
  (should-equal (list 1 2 3 4)
                 (mapcan #'identity
                         (list (list 1) (list 2 3) (list 4))))
  (should-equal (list (list 1) (list 2 3) (list 4))
                 (mapcan #'list
                         (list (list 1) (list 2 3) (list 4))))
  (should-equal (list 1 2 3 4)
                 (mapcan #'identity
                         (list (list 1) (list) (list 2 3) (list 4))))
  (should-equal (list (list 1) (list) (list 2 3) (list 4))
                 (mapcan #'list
                         (list (list 1) (list) (list 2 3) (list 4))))
  (should-equal (list)
                 (mapcan #'identity
                         (list (list) (list) (list) (list)))))

(ert-deftest xor ()
  (should (xor t nil))
  (should (xor nil t))
  (should-not (xor nil nil))
  (should-not (xor t t)))

(ert-deftest length= ()
  (should (length= '() 0))                  ;empty list
  (should (length= '(1) 1))                 ;single element
  (should (length= '(1 2 3) 3))             ;multiple elements
  (should-not (length= '(1 2 3) 2))           ;less than
  (should-not (length= '(1) 0))
  (should-not (length= '(1 2 3) 4))           ;more than
  (should-not (length= '(1) 2))
  (should-not (length= '() 1))
  (should (length= [] 0))                   ;empty vector
  (should (length= [1] 1))                  ;single element vector
  (should (length= [1 2 3] 3))              ;multiple element vector
  (should-not (length= [1 2 3] 2))            ;less than
  (should-not (length= [1 2 3] 4))            ;more than
  (should-error (length= 3 nil) :type 'wrong-type-argument))

(ert-deftest length< ()
  (should-not (length< '(1) 0))               ;single element
  (should-not (length< '(1 2 3) 2))           ;multiple elements
  (should-not (length< '(1 2 3) 3))           ;equal length
  (should-not (length< '(1) 1))
  (should (length< '(1 2 3) 4))             ;more than
  (should (length< '(1) 2))
  (should (length< '() 1))
  (should-not (length< [1] 0))                ;single element vector
  (should-not (length< [1 2 3] 2))            ;multiple element vector
  (should-not (length< [1 2 3] 3))            ;equal length
  (should (length< [1 2 3] 4))              ;more than
  (should-error (length< 3 nil) :type 'wrong-type-argument))

(ert-deftest length> ()
  (should (length> '(1) 0))                 ;single element
  (should (length> '(1 2 3) 2))             ;multiple elements
  (should-not (length> '(1 2 3) 3))           ;equal length
  (should-not (length> '(1) 1))
  (should-not (length> '(1 2 3) 4))           ;more than
  (should-not (length> '(1) 2))
  (should-not (length> '() 1))
  (should (length> [1] 0))                  ;single element vector
  (should (length> [1 2 3] 2))              ;multiple element vector
  (should-not (length> [1 2 3] 3))            ;equal length
  (should-not (length> [1 2 3] 4))            ;more than
  (should-error (length< 3 nil) :type 'wrong-type-argument))

(ert-deftest ensure-list ()
  (should-not (ensure-list nil))           ;; empty list
  (should-equal '(1) (ensure-list '(1)))         ;; single element list
  (should-equal '(1 2 3) (ensure-list '(1 2 3))) ;; multiple element list
  (should-equal '(1) (ensure-list 1)))           ;; atom

(ert-deftest proper-list-p ()
  (should-equal 0 (proper-list-p ()))            ;; empty list
  (should-equal 1 (proper-list-p '(1)))          ;; single element
  (should-equal 3 (proper-list-p '(1 2 3)))      ;; multiple elements
  (should-not (proper-list-p '(1 . 2)))    ;; cons
  (should-not (proper-list-p '(1 2 . 3)))  ;; dotted
  (should-not (let ((l (list 1 2 3)))       ;; circular
                       (setf (nthcdr 3 l) l)
                       (proper-list-p l)))
  (should-not (proper-list-p 1))           ;; non-lists
  (should-not (proper-list-p ""))
  (should-not (proper-list-p "abc"))
  (should-not (proper-list-p []))
  (should-not (proper-list-p [1 2 3])))

(ert-deftest always ()
  (should-equal t (always))                      ;; no arguments
  (should-equal t (always 1))                    ;; single argument
  (should-equal t (always 1 2 3 4)))             ;; multiple arguments

(ert-deftest directory-name-p ()
  (should (directory-name-p "/"))
  (should-not (directory-name-p "/file"))
  (should-not (directory-name-p "/dir/file"))
  (should (directory-name-p "/dir/"))
  (should-not (directory-name-p "/dir"))
  (should (directory-name-p "/dir/subdir/"))
  (should-not (directory-name-p "/dir/subdir"))
  (should (directory-name-p "dir/"))
  (should-not (directory-name-p "file"))
  (should-not (directory-name-p "dir/file"))
  (should (directory-name-p "dir/subdir/"))
  (should-not (directory-name-p "dir/subdir")))

(ert-deftest make-lock-file-name ()
  (should-equal (expand-file-name ".#") (make-lock-file-name ""))
  (should-equal (expand-file-name ".#a") (make-lock-file-name "a"))
  (should-equal (expand-file-name ".#foo") (make-lock-file-name "foo"))
  (should-equal (expand-file-name ".#.") (make-lock-file-name "."))
  (should-equal (expand-file-name ".#.#") (make-lock-file-name ".#"))
  (should-equal (expand-file-name ".#.a") (make-lock-file-name ".a"))
  (should-equal (expand-file-name ".#.#") (make-lock-file-name ".#"))
  (should-equal (expand-file-name "a/.#") (make-lock-file-name "a/"))
  (should-equal (expand-file-name "a/.#b") (make-lock-file-name "a/b"))
  (should-equal (expand-file-name "a/.#.#") (make-lock-file-name "a/.#"))
  (should-equal (expand-file-name "a/.#.") (make-lock-file-name "a/."))
  (should-equal (expand-file-name "a/.#.b") (make-lock-file-name "a/.b"))
  (should-equal (expand-file-name "a/.#foo") (make-lock-file-name "a/foo"))
  (should-equal (expand-file-name "bar/.#b") (make-lock-file-name "bar/b"))
  (should-equal (expand-file-name "bar/.#foo") (make-lock-file-name "bar/foo")))

(ert-deftest file-attribute-getters ()
  (let ((attrs '(type link-number user-id group-id access-time modification-time
                 status-change-time size modes unspecified inode-number device-number)))
    (should-equal (file-attribute-file-identifier attrs) '(inode-number device-number))
    (should-equal (file-attribute-type attrs) 'type)
    (should-equal (file-attribute-link-number attrs) 'link-number)
    (should-equal (file-attribute-user-id attrs) 'user-id)
    (should-equal (file-attribute-group-id attrs) 'group-id)
    (should-equal (file-attribute-access-time attrs) 'access-time)
    (should-equal (file-attribute-modification-time attrs) 'modification-time)
    (should-equal (file-attribute-status-change-time attrs) 'status-change-time)
    (should-equal (file-attribute-size attrs) 'size)
    (should-equal (file-attribute-modes attrs) 'modes)
    (should-equal (file-attribute-inode-number attrs) 'inode-number)
    (should-equal (file-attribute-device-number attrs) 'device-number)))

(ert-deftest file-attribute-collect ()
  (let ((attrs '(t l u g a m s S m U i d)))
    (should-equal (file-attribute-collect attrs 'group-id 'user-id) '(g u))
    (should-equal (file-attribute-collect attrs 'size 'inode-number 'type) '(S i t))))

(ert-deftest file-size-human-readable ()
  (should-equal "1000" (compat-call file-size-human-readable 1000))
  (should-equal "1k" (compat-call file-size-human-readable 1024))
  (should-equal "1M" (compat-call file-size-human-readable (* 1024 1024)))
  (should-equal "1G" (compat-call file-size-human-readable (expt 1024 3)))
  (should-equal "1T" (compat-call file-size-human-readable (expt 1024 4)))
  (should-equal "1k" (compat-call file-size-human-readable 1000 'si))
  (should-equal "1KiB" (compat-call file-size-human-readable 1024 'iec))
  (should-equal "1KiB" (compat-call file-size-human-readable 1024 'iec))
  (should-equal "1 KiB" (compat-call file-size-human-readable 1024 'iec " "))
  (should-equal "1KiA" (compat-call file-size-human-readable 1024 'iec nil "A"))
  (should-equal "1 KiA" (compat-call file-size-human-readable 1024 'iec " " "A"))
  (should-equal "1kA" (compat-call file-size-human-readable 1000 'si nil "A"))
  (should-equal "1 k" (compat-call file-size-human-readable 1000 'si " "))
  (should-equal "1 kA" (compat-call file-size-human-readable 1000 'si " " "A")))

(ert-deftest with-file-modes ()
  (let ((old (default-file-modes)))
    (with-file-modes (1+ old)
      (should-equal (default-file-modes) (1+ old)))
    (should-equal (default-file-modes) old)))

(ert-deftest file-modes-number-to-symbolic ()
  (should-equal "-rwx------" (file-modes-number-to-symbolic #o700))
  (should-equal "-rwxrwx---" (file-modes-number-to-symbolic #o770))
  (should-equal "-rwx---rwx" (file-modes-number-to-symbolic #o707))
  (should-equal "-rw-r-xr--" (file-modes-number-to-symbolic #o654))
  (should-equal "--wx-w---x" (file-modes-number-to-symbolic #o321))
  (should-equal "drwx------" (file-modes-number-to-symbolic #o700 ?d))
  (should-equal "?rwx------" (file-modes-number-to-symbolic #o700 ??))
  (should-equal "lrwx------" (file-modes-number-to-symbolic #o120700))
  (should-equal "prwx------" (file-modes-number-to-symbolic #o10700))
  (should-equal "-rwx------" (file-modes-number-to-symbolic #o30700)))

(ert-deftest file-name-absolute-p ()   ;assuming unix
  (should (file-name-absolute-p "/"))
  (should (file-name-absolute-p "/a"))
  (should-not (file-name-absolute-p "a"))
  (should-not (file-name-absolute-p "a/b"))
  (should-not (file-name-absolute-p "a/b/"))
  (should (file-name-absolute-p "~"))
  (when (version< "27.1" emacs-version)
    (should (file-name-absolute-p "~/foo"))
    (should-not (file-name-absolute-p "~foo"))
    (should-not (file-name-absolute-p "~foo/")))
  (should (file-name-absolute-p "~root"))
  (should (file-name-absolute-p "~root/"))
  (should (file-name-absolute-p "~root/file")))

(ert-deftest file-local-name ()
  (should-equal "" (file-local-name ""))
  (should-equal "foo" (file-local-name "foo"))
  (should-equal "/bar/foo" (file-local-name "/bar/foo"))
  ;; These tests fails prior to Emacs 26, because /ssh:foo was a valid
  ;; TRAMP path back then.
  ;; (should-equal "/ssh:foo" (file-local-name "/ssh:foo"))
  ;; (should-equal "/ssh:/bar/foo" (file-local-name "/ssh:/bar/foo"))
  (should-equal "foo" (file-local-name "/ssh::foo"))
  (should-equal "/bar/foo" (file-local-name "/ssh::/bar/foo"))
  (should-equal ":foo" (file-local-name "/ssh:::foo"))
  (should-equal ":/bar/foo" (file-local-name "/ssh:::/bar/foo")))

(ert-deftest file-name-quoted-p ()
  (should-not (compat-call file-name-quoted-p ""))
  (should (compat-call file-name-quoted-p "/:"))
  (should-not (compat-call file-name-quoted-p "//:"))
  (should (compat-call file-name-quoted-p "/::"))
  (should-not (compat-call file-name-quoted-p "/ssh::"))
  (should-not (compat-call file-name-quoted-p "/ssh::a"))
  (should (compat-call file-name-quoted-p "/ssh::/:a"))
  ;; These tests fails prior to Emacs 26, because /ssh:foo was a valid
  ;; TRAMP path back then.
  ;;
  ;; (should-not (compat-call file-name-quoted-p "/ssh:/:a")))
  )

(ert-deftest file-name-quote ()
  (should-equal "/:" (compat-call file-name-quote ""))
  (should-equal "/::"(compat-call file-name-quote  ":"))
  (should-equal "/:/" (compat-call file-name-quote "/"))
  (should-equal "/:" (compat-call file-name-quote "/:"))
  (should-equal "/:a" (compat-call file-name-quote "a"))
  (should-equal "/::a" (compat-call file-name-quote ":a"))
  (should-equal "/:/a" (compat-call file-name-quote "/a"))
  (should-equal "/:a" (compat-call file-name-quote "/:a"))
  (should-equal (concat "/ssh:" (system-name) ":/:a") (compat-call file-name-quote "/ssh::a")))

(ert-deftest file-name-concat ()
  (should-equal (file-name-concat "foo" "bar") "foo/bar")
  (should-equal (file-name-concat "foo" "bar") "foo/bar")
  (should-equal (file-name-concat "foo" "bar" "zot") "foo/bar/zot")
  (should-equal (file-name-concat "foo/" "bar") "foo/bar")
  (should-equal (file-name-concat "foo//" "bar") "foo//bar")
  (should-equal (file-name-concat "foo/" "bar/" "zot") "foo/bar/zot")
  (should-equal (file-name-concat "fóo" "bar") "fóo/bar")
  (should-equal (file-name-concat "foo" "bár") "foo/bár")
  (should-equal (file-name-concat "fóo" "bár") "fóo/bár")
  (should-equal (file-name-concat "foo") "foo")
  (should-equal (file-name-concat "foo/") "foo/")
  (should-equal (file-name-concat "foo" "") "foo")
  (should-equal (file-name-concat "foo" "" "" "" nil) "foo")
  (should-equal (file-name-concat "" "bar") "bar")
  (should-equal (file-name-concat "" "") ""))

(ert-deftest file-name-parent-directory ()
  (should-equal (file-name-parent-directory "/foo/bar") "/foo/")
  (should-equal (file-name-parent-directory "/foo/") "/")
  (should-equal (file-name-parent-directory "foo/bar") "foo/")
  (should-equal (file-name-parent-directory "foo") "./"))

(ert-deftest file-name-split ()
  (should-equal (file-name-split "foo/bar") '("foo" "bar"))
  (should-equal (file-name-split "/foo/bar") '("" "foo" "bar"))
  (should-equal (file-name-split "/foo/bar/zot") '("" "foo" "bar" "zot"))
  (should-equal (file-name-split "/foo/bar/") '("" "foo" "bar" ""))
  (should-equal (file-name-split "foo/bar/") '("foo" "bar" "")))

(ert-deftest file-name-with-extension ()
  (should-equal "file.ext" (file-name-with-extension "file" "ext"))
  (should-equal "file.ext" (file-name-with-extension "file" ".ext"))
  (should-equal "file.ext" (file-name-with-extension "file." ".ext"))
  (should-equal "file..ext" (file-name-with-extension "file.." ".ext"))
  (should-equal "file..ext" (file-name-with-extension "file." "..ext"))
  (should-equal "file...ext" (file-name-with-extension "file.." "..ext"))
  (should-equal "/abs/file.ext" (file-name-with-extension "/abs/file" "ext"))
  (should-equal "/abs/file.ext" (file-name-with-extension "/abs/file" ".ext"))
  (should-equal "/abs/file.ext" (file-name-with-extension "/abs/file." ".ext"))
  (should-equal "/abs/file..ext" (file-name-with-extension "/abs/file.." ".ext"))
  (should-equal "/abs/file..ext" (file-name-with-extension "/abs/file." "..ext"))
  (should-equal "/abs/file...ext" (file-name-with-extension "/abs/file.." "..ext"))
  (should-error (file-name-with-extension "file" "") :type 'error)
  (should-error (file-name-with-extension "" "ext") :type 'error)
  (should-error (file-name-with-extension "file" "") :type 'error)
  (should-error (file-name-with-extension "rel/" "ext") :type 'error)
  (should-error (file-name-with-extension "/abs/" "ext")) :type 'error)

(ert-deftest flatten-tree ()
  ;; Example from docstring:
  (should-equal '(1 2 3 4 5 6 7) (flatten-tree '(1 (2 . 3) nil (4 5 (6)) 7)))
  ;; Trivial example
  (should-not (flatten-tree ()))
  ;; Simple examples
  (should-equal '(1) (flatten-tree '(1)))
  (should-equal '(1 2) (flatten-tree '(1 2)))
  (should-equal '(1 2 3) (flatten-tree '(1 2 3)))
  ;; Regular sublists
  (should-equal '(1) (flatten-tree '((1))))
  (should-equal '(1 2) (flatten-tree '((1) (2))))
  (should-equal '(1 2 3) (flatten-tree '((1) (2) (3))))
  ;; Complex examples
  (should-equal '(1) (flatten-tree '(((((1)))))))
  (should-equal '(1 2 3 4) (flatten-tree '((1) nil 2 ((3 4)))))
  (should-equal '(1 2 3 4) (flatten-tree '(((1 nil)) 2 (((3 nil nil) 4))))))

(ert-deftest sort ()
  (should-equal (list 1 2 3) (compat-call sort (list 1 2 3) #'<))
  (should-equal (list 1 2 3) (compat-call sort (list 3 2 1) #'<))
  (should-equal '[1 2 3] (compat-call sort '[1 2 3] #'<))
  (should-equal '[1 2 3] (compat-call sort '[3 2 1] #'<)))

(ert-deftest replace-string-in-region ()
  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-string-in-region "foo" "new" (point-min) (point-max))
               2))
    (should (equal (buffer-string) "new bar zot newbar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-string-in-region "foo" "new" (point-min) 14)
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-error (replace-string-in-region "foo" "new" (point-min) 30)))

  (with-temp-buffer
    (insert "Foo bar zot foobar")
    (should (= (replace-string-in-region "Foo" "new" (point-min))
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  ;; There was a bug in the Emacs 28 implementation
  ;; Fixed in Emacs d8f392bccd46cdb238ec96964f220ffb9d81cc44
  (unless (= emacs-major-version 28)
    (with-temp-buffer
      (insert "foo bar baz")
      (should (= (replace-string-in-region "ba" "quux corge grault" (point-min))
                 2))
      (should (equal (buffer-string)
                     "foo quux corge graultr quux corge graultz")))

    (with-temp-buffer
      (insert "foo bar bar")
      (should (= (replace-string-in-region " bar" "" (point-min) 8)
                 1))
      (should (equal (buffer-string)
                     "foo bar")))))

(ert-deftest replace-regexp-in-region ()
  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-regexp-in-region "fo+" "new" (point-min) (point-max))
               2))
    (should (equal (buffer-string) "new bar zot newbar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should (= (replace-regexp-in-region "fo+" "new" (point-min) 14)
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-error (replace-regexp-in-region "fo+" "new" (point-min) 30)))

  (with-temp-buffer
    (insert "Foo bar zot foobar")
    (should (= (replace-regexp-in-region "Fo+" "new" (point-min))
               1))
    (should (equal (buffer-string) "new bar zot foobar")))

  ;; There was a bug in the Emacs 28 implementation
  ;; Fixed in Emacs d8f392bccd46cdb238ec96964f220ffb9d81cc44
  (unless (= emacs-major-version 28)
    (with-temp-buffer
      (insert "foo bar baz")
      (should (= (replace-regexp-in-region "ba." "quux corge grault" (point-min))
                 2))
      (should (equal (buffer-string)
                     "foo quux corge grault quux corge grault")))

    (with-temp-buffer
      (insert "foo bar bar")
      (should (= (replace-regexp-in-region " bar" "" (point-min) 8)
                 1))
      (should (equal (buffer-string)
                     "foo bar")))))

(ert-deftest string-split ()
  (should-equal '("a" "b" "c") (split-string "a b c"))
  (should-equal '("a" "b" "c") (string-split "a b c")))

(ert-deftest string-equal-ignore-case ()
  (should (string-equal-ignore-case "abc" "abc"))
  (should (string-equal-ignore-case "abc" "ABC"))
  (should (string-equal-ignore-case "abc" "abC"))
  (should-not (string-equal-ignore-case "abc" "abCD"))
  (should (string-equal-ignore-case "S" "s")))

(ert-deftest string-greaterp ()
  (should (string-greaterp "b" "a"))
  (should-not (string-greaterp "a" "b"))
  (should (string-greaterp "aaab" "aaaa"))
  (should-not (string-greaterp "aaaa" "aaab")))

(ert-deftest string-clean-whitespace ()
  (should-equal "a b c" (string-clean-whitespace "a b c"))
  (should-equal "a b c" (string-clean-whitespace "   a b c"))
  (should-equal "a b c" (string-clean-whitespace "a b c   "))
  (should-equal "a b c" (string-clean-whitespace "a    b c"))
  (should-equal "a b c" (string-clean-whitespace "a b    c"))
  (should-equal "a b c" (string-clean-whitespace "a    b    c"))
  (should-equal "a b c" (string-clean-whitespace "   a    b    c"))
  (should-equal "a b c" (string-clean-whitespace "a    b    c    "))
  (should-equal "a b c" (string-clean-whitespace "   a    b    c    "))
  (should-equal "aa bb cc" (string-clean-whitespace "aa bb cc"))
  (should-equal "aa bb cc" (string-clean-whitespace "   aa bb cc"))
  (should-equal "aa bb cc" (string-clean-whitespace "aa bb cc   "))
  (should-equal "aa bb cc" (string-clean-whitespace "aa    bb cc"))
  (should-equal "aa bb cc" (string-clean-whitespace "aa bb    cc"))
  (should-equal "aa bb cc" (string-clean-whitespace "aa    bb    cc"))
  (should-equal "aa bb cc" (string-clean-whitespace "   aa    bb    cc"))
  (should-equal "aa bb cc" (string-clean-whitespace "aa    bb    cc    "))
  (should-equal "aa bb cc" (string-clean-whitespace "   aa    bb    cc    ")))

(ert-deftest string-fill ()
  (should-equal "a a a a a" (string-fill "a a a a a" 9))
  (should-equal "a a a a a" (string-fill "a a a a a" 10))
  (should-equal "a a a a\na" (string-fill "a a a a a" 8))
  (should-equal "a a a a\na" (string-fill "a  a  a  a  a" 8))
  (should-equal "a a\na a\na" (string-fill "a a a a a" 4))
  (should-equal "a\na\na\na\na" (string-fill "a a a a a" 2))
  (should-equal "a\na\na\na\na" (string-fill "a a a a a" 1)))

(ert-deftest string-lines ()
  (should-equal '("a" "b" "c") (string-lines "a\nb\nc"))
  ;; TODO behavior changed on Emacs master (Emacs version 30)
  ;; (should-equal '("a" "b" "c" "") (string-lines "a\nb\nc\n"))
  (should-equal '("a" "b" "c") (string-lines "a\nb\nc\n" t))
  (should-equal '("abc" "bcd" "cde") (string-lines "abc\nbcd\ncde"))
  (should-equal '(" abc" " bcd " "cde ") (string-lines " abc\n bcd \ncde ")))

(ert-deftest string-pad ()
  (should-equal "a   " (string-pad "a" 4))
  (should-equal "aaaa" (string-pad "aaaa" 4))
  (should-equal "aaaaaa" (string-pad "aaaaaa" 4))
  (should-equal "a..." (string-pad "a" 4 ?.))
  (should-equal "   a" (string-pad "a" 4 nil t))
  (should-equal "...a" (string-pad "a" 4 ?. t)))

(ert-deftest string-chop-newline ()
  (should-equal "" (string-chop-newline ""))
  (should-equal "" (string-chop-newline "\n"))
  (should-equal "aaa" (string-chop-newline "aaa"))
  (should-equal "aaa" (string-chop-newline "aaa\n"))
  (should-equal "aaa\n" (string-chop-newline "aaa\n\n")))

(ert-deftest string-distance ()
  (should-equal 3 (string-distance "kitten" "sitting"))    ;from wikipedia
  (if (version<= "28" emacs-version) ;trivial examples
      (should-equal 0 (string-distance "" ""))
    ;; Up until Emacs 28, `string-distance' had a bug
    ;; when comparing two empty strings. This was fixed
    ;; in the following commit:
    ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=c44190c
    ;;
    ;; TODO Therefore, we must make sure, that the test
    ;; doesn't fail because of this bug:
    ;; (should (= (string-distance "" "") 0))
    )
  (should-equal 0 (string-distance "a" "a"))
  (should-equal 1 (string-distance "" "a"))
  (should-equal 1 (string-distance "b" "a"))
  (should-equal 2 (string-distance "aa" "bb"))
  (should-equal 2 (string-distance "aa" "bba"))
  (should-equal 2 (string-distance "aaa" "bba"))
  (should-equal 3 (string-distance "a" "あ" t))             ;byte example
  (should-equal 1 (string-distance "a" "あ")))

(ert-deftest string-width ()
  (should-equal 0 (compat-call string-width ""))
  (should-equal 3 (compat-call string-width "abc"))                 ;; no argument
  (should-equal 5 (compat-call string-width "abcあ"))
  (should-equal (1+ tab-width) (compat-call string-width "a	"))
  (should-equal 2 (compat-call string-width "abc" 1))               ;; with from
  (should-equal 4 (compat-call string-width "abcあ" 1))
  (should-equal tab-width (compat-call string-width "a	" 1))
  (should-equal 2 (compat-call string-width "abc" 0 2))             ;; with to
  (should-equal 3 (compat-call string-width "abcあ" 0 3))
  (should-equal 1 (compat-call string-width "a	" 0 1))
  (should-equal 1 (compat-call string-width "abc" 1 2))             ;; with from and to
  (should-equal 2 (compat-call string-width "abcあ" 3 4))
  (should-equal 0 (compat-call string-width "a	" 1 1)))

(ert-deftest string-trim-left ()
  (should-equal "a" (compat-call string-trim-left "---a" "-+")) ;; Additional regexp
  (should-equal "" (compat-call string-trim-left ""))                          ;empty string
  (should-equal "a" (compat-call string-trim-left "a"))                        ;"full" string
  (should-equal "aaa" (compat-call string-trim-left "aaa"))
  (should-equal "へっろ" (compat-call string-trim-left "へっろ"))
  (should-equal "hello world" (compat-call string-trim-left "hello world"))
  (should-equal "a " (compat-call string-trim-left "a "))                        ;right trailing
  (should-equal "aaa " (compat-call string-trim-left "aaa "))
  (should-equal "a    " (compat-call string-trim-left "a    "))
  (should-equal "a\t\t" (compat-call string-trim-left "a\t\t"))
  (should-equal "a\n  \t" (compat-call string-trim-left "a\n  \t"))
  (should-equal "a" (compat-call string-trim-left " a"))                        ;left trailing
  (should-equal "aaa" (compat-call string-trim-left " aaa"))
  (should-equal "a" (compat-call string-trim-left "a"))
  (should-equal "a" (compat-call string-trim-left "\t\ta"))
  (should-equal "a" (compat-call string-trim-left "\n  \ta"))
  (should-equal "a " (compat-call string-trim-left " a "))                       ;both trailing
  (should-equal "aaa  " (compat-call string-trim-left " aaa  "))
  (should-equal "a\t\n" (compat-call string-trim-left "\t\ta\t\n"))
  (should-equal "a  \n" (compat-call string-trim-left "\n  \ta  \n")))

(ert-deftest string-trim-right ()
  (should-equal "a" (compat-call string-trim-right "a---" "-+")) ;; Additional regexp
  (should-equal "" (compat-call string-trim-right ""))                          ;empty string
  (should-equal "a" (compat-call string-trim-right "a"))                        ;"full" string
  (should-equal "aaa" (compat-call string-trim-right "aaa"))
  (should-equal "へっろ" (compat-call string-trim-right "へっろ"))
  (should-equal "hello world" (compat-call string-trim-right "hello world"))
  (should-equal "a" (compat-call string-trim-right "a"))                      ;right trailing
  (should-equal "aaa" (compat-call string-trim-right "aaa"))
  (should-equal "a" (compat-call string-trim-right "a    "))
  (should-equal "a" (compat-call string-trim-right "a\t\t"))
  (should-equal "a" (compat-call string-trim-right "a\n  \t"))
  (should-equal " a" (compat-call string-trim-right " a"))                       ;left trailing
  (should-equal " aaa" (compat-call string-trim-right " aaa"))
  (should-equal "a" (compat-call string-trim-right "a"))
  (should-equal "\t\ta" (compat-call string-trim-right "\t\ta"))
  (should-equal "\n  \ta" (compat-call string-trim-right "\n  \ta"))
  (should-equal " a" (compat-call string-trim-right " a "))                        ;both trailing
  (should-equal " aaa" (compat-call string-trim-right " aaa"))
  (should-equal "\t\ta" (compat-call string-trim-right "\t\ta\t\n"))
  (should-equal "\n  \ta" (compat-call string-trim-right "\n  \ta  \n")))

(ert-deftest string-trim ()
  (should-equal "aaa" (compat-call string-trim "--aaa__" "-+" "_+")) ;; Additional regexp
  (should-equal "" (compat-call string-trim ""))                          ;empty string
  (should-equal "a" (compat-call string-trim "a"))                        ;"full" string
  (should-equal "aaa" (compat-call string-trim "aaa"))
  (should-equal "へっろ" (compat-call string-trim "へっろ"))
  (should-equal "hello world" (compat-call string-trim "hello world"))
  (should-equal "a" (compat-call string-trim "a "))                       ;right trailing
  (should-equal "aaa" (compat-call string-trim "aaa "))
  (should-equal "a" (compat-call string-trim "a    "))
  (should-equal "a" (compat-call string-trim "a\t\t"))
  (should-equal "a" (compat-call string-trim "a\n  \t"))
  (should-equal "a" (compat-call string-trim " a"))                       ;left trailing
  (should-equal "aaa" (compat-call string-trim " aaa"))
  (should-equal "a" (compat-call string-trim "a"))
  (should-equal "a" (compat-call string-trim "\t\ta"))
  (should-equal "a" (compat-call string-trim "\n  \ta"))
  (should-equal "a" (compat-call string-trim " a "))                      ;both trailing
  (should-equal "aaa" (compat-call string-trim " aaa  "))
  (should-equal "t\ta" (compat-call string-trim "t\ta\t\n"))
  (should-equal "a" (compat-call string-trim "\n  \ta  \n")))

(defmacro compat-tests--string-to-multibyte (str)
  ;; On Emacs 26 `string-to-multibyte' was declared obsolete.
  ;; This obsoletion was reverted on Emacs 27.
  (if (= emacs-major-version 26)
      `(with-no-warnings (string-to-multibyte ,str))
    `(string-to-multibyte ,str)))

(ert-deftest string-search ()
  ;; Find needle at the beginning of a haystack:
  (should-equal 0 (string-search "a" "abb"))
  ;; Find needle at the begining of a haystack, with more potential
  ;; needles that could be found:
  (should-equal 0 (string-search "a" "abba"))
  ;; Find needle with more than one charachter at the beginning of
  ;; a line:
  (should-equal 0 (string-search "aa" "aabbb"))
  ;; Find a needle midstring:
  (should-equal 1 (string-search "a" "bab"))
  ;; Find a needle at the end:
  (should-equal 2 (string-search "a" "bba"))
  ;; Find a longer needle midstring:
  (should-equal 1 (string-search "aa" "baab"))
  ;; Find a longer needle at the end:
  (should-equal 2 (string-search "aa" "bbaa"))
  ;; Find a case-sensitive needle:
  (should-equal 2 (string-search "a" "AAa"))
  ;; Find another case-sensitive needle:
  (should-equal 2 (string-search "aa" "AAaa"))
  ;; Test regular expression quoting (1):
  (should-equal 5 (string-search "." "abbbb.b"))
  ;; Test regular expression quoting (2):
  (should-equal 5 (string-search ".*" "abbbb.*b"))
  ;; Attempt to find non-existent needle:
  (should-not (string-search "a" "bbb"))
  ;; Attempt to find non-existent needle that has the form of a
  ;; regular expression:
  (should-not (string-search "." "bbb"))
  ;; Handle empty string as needle:
  (should-equal 0 (string-search "" "abc"))
  ;; Handle empty string as haystack:
  (should-not (string-search "a" ""))
  ;; Handle empty string as needle and haystack:
  (should-equal 0 (string-search "" ""))
  ;; Handle START argument:
  (should-equal 3 (string-search "a" "abba" 1))
  ;; Additional test copied from:
  (should-equal 6 (string-search "zot" "foobarzot"))
  (should-equal 0 (string-search "foo" "foobarzot"))
  (should-not (string-search "fooz" "foobarzot"))
  (should-not (string-search "zot" "foobarzo"))
  (should-equal 0 (string-search "ab" "ab"))
  (should-not (string-search "ab\0" "ab"))
  (should-equal 4 (string-search "ab" "abababab" 3))
  (should-not (string-search "ab" "ababac" 3))
  (should-not (string-search "aaa" "aa"))
  ;; The `make-string' calls with three arguments have been replaced
  ;; here with the result of their evaluation, to avoid issues with
  ;; older versions of Emacs that only support two arguments.
  (should-equal 5
                 (string-search (make-string 2 130)
                                ;; Per (concat "helló" (make-string 5 130 t) "bár")
                                "hellóbár"))
  (should-equal 5
                  (string-search (make-string 2 127)
                                 ;; Per (concat "helló" (make-string 5 127 t) "bár")
                                 "hellóbár"))
  (should-equal 1 (string-search "\377" "a\377ø"))
  (should-equal 1 (string-search "\377" "a\377a"))
  (should-not (string-search (make-string 1 255) "a\377ø"))
  (should-not (string-search (make-string 1 255) "a\377a"))
  (should-equal 3 (string-search "fóo" "zotfóo"))
  (should-not (string-search "\303" "aøb"))
  (should-not (string-search "\270" "aøb"))
  (should-not (string-search "ø" "\303\270"))
  (should-not (string-search "ø" (make-string 32 ?a)))
  (should-not (string-search "ø" (compat-tests--string-to-multibyte (make-string 32 ?a))))
  (should-equal 14 (string-search "o" (compat-tests--string-to-multibyte
                                        (apply #'string (number-sequence ?a ?z)))))
  (should-equal 2 (string-search "a\U00010f98z" "a\U00010f98a\U00010f98z"))
  (should-error (string-search "a" "abc" -1) :type '(args-out-of-range -1))
  (should-error (string-search "a" "abc" 4) :type '(args-out-of-range 4))
  (should-error (string-search "a" "abc" 100000000000) :type '(args-out-of-range 100000000000))
  (should-not (string-search "a" "aaa" 3))
  (should-not (string-search "aa" "aa" 1))
  (should-not (string-search "\0" ""))
  (should-equal 0 (string-search "" ""))
  (should-error (string-search "" "" 1) :type '(args-out-of-range 1))
  (should-equal 0 (string-search "" "abc"))
  (should-equal 2 (string-search "" "abc" 2))
  (should-equal 3 (string-search "" "abc" 3))
  (should-error (string-search "" "abc" 4) :type '(args-out-of-range 4))
  (should-error (string-search "" "abc" -1) :type '(args-out-of-range -1))
  (should-not (string-search "ø" "foo\303\270"))
  (should-not (string-search "\303\270" "ø"))
  (should-not (string-search "\370" "ø"))
  (should-not (string-search (compat-tests--string-to-multibyte "\370") "ø"))
  (should-not (string-search "ø" "\370"))
  (should-not (string-search "ø" (compat-tests--string-to-multibyte "\370")))
  (should-not (string-search "\303\270" "\370"))
  (should-not (string-search (compat-tests--string-to-multibyte "\303\270") "\370"))
  (should-not (string-search "\303\270" (compat-tests--string-to-multibyte "\370")))
  (should-not
                  (string-search (compat-tests--string-to-multibyte "\303\270")
                                 (compat-tests--string-to-multibyte "\370")))
  (should-not (string-search "\370" "\303\270"))
  (should-not (string-search (compat-tests--string-to-multibyte "\370") "\303\270"))
  (should-not (string-search "\370" (compat-tests--string-to-multibyte "\303\270")))
  (should-not
                 (string-search (compat-tests--string-to-multibyte "\370")
                                (compat-tests--string-to-multibyte "\303\270")))
  (should-equal 3 (string-search "\303\270" "foo\303\270"))
  (when (version<= "27" emacs-version)
    ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1 in
    ;; emacs.git fixes the behaviour of regular expressions matching
    ;; raw bytes.  The compatibility functions should updated to
    ;; backport this behaviour.
    (should-equal 2 (string-search (compat-tests--string-to-multibyte "\377") "ab\377c"))
    (should-equal 2
                    (string-search (compat-tests--string-to-multibyte "o\303\270")
                                   "foo\303\270"))))

(ert-deftest string-replace ()
  (should-equal "bba" (string-replace "aa" "bb" "aaa"))
  (should-equal "AAA" (string-replace "aa" "bb" "AAA"))
  ;; Additional test copied from subr-tests.el:
  (should-equal "zot" (string-replace "foo" "bar" "zot"))
  (should-equal "barzot" (string-replace "foo" "bar" "foozot"))
  (should-equal "barbarzot" (string-replace "foo" "bar" "barfoozot"))
  (should-equal "barfoobar" (string-replace "zot" "bar" "barfoozot"))
  (should-equal "barfoobarot" (string-replace "z" "bar" "barfoozot"))
  (should-equal "zat" (string-replace "zot" "bar" "zat"))
  (should-equal "zat" (string-replace "azot" "bar" "zat"))
  (should-equal "bar" (string-replace "azot" "bar" "azot"))
  (should-equal "foozotbar" (string-replace "azot" "bar" "foozotbar"))
  (should-equal "labarbarbarzot" (string-replace "fo" "bar" "lafofofozot"))
  (should-equal "axb" (string-replace "\377" "x" "a\377b"))
  (should-equal "axø" (string-replace "\377" "x" "a\377ø"))
  (when (version<= "27" emacs-version)
    ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1
    ;; in emacs.git fixes the behaviour of regular
    ;; expressions matching raw bytes.  The compatibility
    ;; functions should updated to backport this
    ;; behaviour.
    (should-equal "axb" (string-replace (compat-tests--string-to-multibyte "\377") "x" "a\377b"))
    (should-equal "axø" (string-replace (compat-tests--string-to-multibyte "\377") "x" "a\377ø")))
  (should-equal "ANAnas" (string-replace "ana" "ANA" "ananas"))
  (should-equal "" (string-replace "a" "" ""))
  (should-equal "" (string-replace "a" "" "aaaaa"))
  (should-equal "" (string-replace "ab" "" "ababab"))
  (should-equal "ccc" (string-replace "ab" "" "abcabcabc"))
  (should-equal "aaaaaa" (string-replace "a" "aa" "aaa"))
  (should-equal "defg" (string-replace "abc" "defg" "abc"))
  (when (version<= "24.4" emacs-version)
    ;; FIXME: Emacs 24.3 do not know of `wrong-length-argument' and
    ;; therefore fail this test, even if the right symbol is being
    ;; thrown.
    (should-error (string-replace "" "x" "abc") :type 'wrong-length-argument)))

(ert-deftest when-let* ()
  (should-equal "second"
   (when-let*
    ((x 3)
     (y 2)
     (z (+ x y))
     ((= z 5))
     (true t))
    "first" "second"))
  (should-not
   (when-let* (((= 5 6))) "first" "second")))

(ert-deftest if-let* ()
  (should-equal "then"
   (if-let*
    ((x 3)
     (y 2)
     (z (+ x y))
     ((= z 5))
     (true t))
    "then" "else"))
  (should-equal "else"
   (if-let* (((= 5 6))) "then" "else")))

(ert-deftest when-let ()
  (should-equal "last"
                (when-let (e (memq 0 '(1 2 3 0 5 6)))
                  "first" "last"))
  (should-equal "last" (when-let ((e (memq 0 '(1 2 3 0 5 6))))
                         "first" "last"))
  (should-not (when-let ((e (memq 0 '(1 2 3 5 6)))
                               (d (memq 0 '(1 2 3 0 5 6))))
                  "first" "last")))

(ert-deftest if-let ()
  (should (if-let (e (memq 0 '(1 2 3 0 5 6)))
              e))
  (should (if-let ((e (memq 0 '(1 2 3 0 5 6))))
              e))
  (should-not (if-let ((e (memq 0 '(1 2 3 5 6)))
                               (d (memq 0 '(1 2 3 0 5 6))))
                  t))
  (should-not (if-let ((d (memq 0 '(1 2 3 0 5 6)))
                               (e (memq 0 '(1 2 3 5 6))))
                  t))
  ;; TODO broken on Emacs 25
  ;;(should-not (if-let (((= 5 6))) t nil))
  )

(ert-deftest and-let* ()
  (should                               ;trivial body
   (and-let*
    ((x 3)
     (y 2)
     (z (+ x y))
     ((= z 5))
     (true t))
    true))
  (should                               ;no body
   (and-let*
    ((x 3)
     (y 2)
     (z (+ x y))
     ((= z 5))
     (true t))))
  (should-not
   (and-let* (((= 5 6))) t)))

(ert-deftest named-let ()
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

(ert-deftest alist-get ()
  ;; Fallback behaviour:
  (should-not (alist-get 1 nil))                      ;empty list
  (should-equal 'a (alist-get 1 '((1 . a))))                  ;single element list
  (should-not (alist-get 1 '(1)))
  (should-equal 'b (alist-get 2 '((1 . a) (2 . b) (3 . c))))  ;multiple element list
  (should-not (alist-get 2 '(1 2 3)))
  (should-equal 'b (alist-get 2 '(1 (2 . b) 3)))
  (should-not (alist-get 2 '((1 . a) 2 (3 . c))))
  (should-equal 'a (alist-get 1 '((3 . c) (2 . b) (1 . a))))
  (should-not (alist-get "a" '(("a" . 1) ("b" . 2) ("c" . 3))))  ;non-primitive elements

  ;; With testfn:
  (should-equal 1 (compat-call alist-get "a" '(("a" . 1) ("b" . 2) ("c" . 3)) nil nil #'equal))
  (should-equal 1 (compat-call alist-get 3 '((10 . 10) (4 . 4) (1 . 1) (9 . 9)) nil nil #'<))
  (should-equal '(a) (compat-call alist-get "b" '(("c" c) ("a" a) ("b" b)) nil nil #'string-lessp))
  (should-equal 'c (compat-call alist-get "a" '(("a" . a) ("a" . b) ("b" . c)) nil nil
                (lambda (s1 s2) (not (string= s1 s2)))))
  (should-equal 'emacs-lisp-mode
                (compat-call alist-get "file.el"
                '(("\\.c\\'" . c-mode)
                  ("\\.p\\'" . pascal-mode)
                  ("\\.el\\'" . emacs-lisp-mode)
                  ("\\.awk\\'" . awk-mode))
                nil nil #'string-match-p))
  (should-equal 'd (compat-call alist-get 0 '((1 . a) (2 . b) (3 . c)) 'd)) ;default value
  (should-equal 'd (compat-call alist-get 2 '((1 . a) (2 . b) (3 . c)) 'd nil #'ignore)))

(ert-deftest alist-get-gv ()
  (let ((alist-1 (list (cons 1 "one")
                       (cons 2 "two")
                       (cons 3 "three")))
        (alist-2 (list (cons "one" 1)
                       (cons "two" 2)
                       (cons "three" 3))))

    (setf (compat-call alist-get 1 alist-1) "eins")
    (should-equal (compat-call alist-get 1 alist-1) "eins")
    (setf (compat-call alist-get 2 alist-1 nil 'remove) nil)
    (should-equal alist-1 '((1 . "eins") (3 . "three")))
    (setf (compat-call alist-get "one" alist-2 nil nil #'string=) "eins")
    (should-equal (compat-call alist-get "one" alist-2 nil nil #'string=)
                   "eins")))

(ert-deftest json-serialize ()
  (let ((input-1 '((:key . ["abc" 2]) (yek . t)))
        (input-2 '(:key ["abc" 2] yek t))
        ;; TODO fix broken test
        ;;(input-3 (let ((ht (make-hash-table)))
        ;;           (puthash "key" ["abc" 2] ht)
        ;;           (puthash "yek" t ht)
        ;;           ht))
        )
    (should-equal (json-serialize input-1)
                   "{\":key\":[\"abc\",2],\"yek\":true}")
    (should-equal (json-serialize input-2)
                   "{\"key\":[\"abc\",2],\"yek\":true}")
    (should (member (json-serialize input-2)
                    '("{\"key\":[\"abc\",2],\"yek\":true}"
                      "{\"yek\":true,\"key\":[\"abc\",2]}")))
    ;; TODO fix broken test
    ;; (should-equal (json-serialize input-3)
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
  ;; TODO fix broken tests!
  ;; (should-equal 0 (json-parse-string "0"))
  ;; (should-equal 1 (json-parse-string "1"))
  ;; (should-equal 0.5 (json-parse-string "0.5"))
  ;; (should-equal 'foo (json-parse-string "null" :null-object 'foo))
  (should-equal [1 2 3] (json-parse-string "[1,2,3]"))
  (should-equal ["a" 2 3] (json-parse-string "[\"a\",2,3]"))
  (should-equal [["a" 2] 3] (json-parse-string "[[\"a\",2],3]"))
  (should-equal '(("a" 2) 3) (json-parse-string "[[\"a\",2],3]" :array-type 'list))
  (should-equal ["false" t] (json-parse-string "[false, true]" :false-object "false"))
  (let ((input "{\"key\":[\"abc\", 2], \"yek\": null}"))
    (let ((obj (json-parse-string input :object-type 'alist)))
      (should-equal (cdr (assq 'key obj)) ["abc" 2])
      (should-equal (cdr (assq 'yek obj)) :null))
    (let ((obj (json-parse-string input :object-type 'plist)))
      (should-equal (plist-get obj :key) ["abc" 2])
      (should-equal (plist-get obj :yek) :null))
    (let ((obj (json-parse-string input)))
      (should-equal (gethash "key" obj) ["abc" 2])
      (should-equal (gethash "yek" obj) :null))))

(ert-deftest json-insert ()
  (with-temp-buffer
    (json-insert '((:key . ["abc" 2]) (yek . t)))
    (should-equal (buffer-string) "{\":key\":[\"abc\",2],\"yek\":true}")))

(ert-deftest make-prop-match ()
  (should (prop-match-p (make-prop-match)))
  (should (prop-match-p (make-prop-match :end 1)))
  (should (prop-match-p (make-prop-match :beginning 1 :end 2 :value 3)))
  (should-equal 1 (prop-match-beginning (make-prop-match :beginning 1 :end 2 :value 3)))
  (should-equal 2 (prop-match-end (make-prop-match :beginning 1 :end 2 :value 3)))
  (should-equal 3 (prop-match-value (make-prop-match :beginning 1 :end 2 :value 3)))
  (should-not (prop-match-p nil))
  (should-not (prop-match-p []))
  (should-not (prop-match-p 'symbol))
  (should-not (prop-match-p "string"))
  (should-not (prop-match-p '(1 2 3))))

(ert-deftest text-property-search-forward ()
  (with-temp-buffer
    (insert "one "
            (propertize "two " 'prop 'val)
            "three "
            (propertize "four " 'prop 'wert)
            "five ")
    (goto-char (point-min))
    (let ((match (text-property-search-forward 'prop)))
      (should (prop-match-p match))
      (should-equal (prop-match-beginning match) 5)
      (should-equal (prop-match-end match) 9)
      (should-equal (prop-match-value match) 'val))
    (let ((match (text-property-search-forward 'prop)))
      (should (prop-match-p match))
      (should-equal (prop-match-beginning match) 15)
      (should-equal (prop-match-end match) 20)
      (should-equal (prop-match-value match) 'wert))
    (should-not (text-property-search-forward 'prop))
    (goto-char (point-min))
    (should-not (text-property-search-forward 'non-existant))))

(ert-deftest text-property-search-backward ()
  (with-temp-buffer
    (insert "one "
            (propertize "two " 'prop 'val)
            "three "
            (propertize "four " 'prop 'wert)
            "five ")
    (goto-char (point-max))
    (let ((match (text-property-search-backward 'prop)))
      (should (prop-match-p match))
      (should-equal (prop-match-beginning match) 15)
      (should-equal (prop-match-end match) 20)
      (should-equal (prop-match-value match) 'wert))
    (let ((match (text-property-search-backward 'prop)))
      (should (prop-match-p match))
      (should-equal (prop-match-beginning match) 5)
      (should-equal (prop-match-end match) 9)
      (should-equal (prop-match-value match) 'val))
    (should-not (text-property-search-backward 'prop))
    (goto-char (point-max))
    (should-not (text-property-search-backward 'non-existant))))

(ert-deftest color-values-from-color-spec ()
  ;; #RGB notation
  (should-equal '(0 0 0) (color-values-from-color-spec "#000"))
  (should-equal '(0 0 0) (color-values-from-color-spec "#000000"))
  (should-equal '(0 0 0) (color-values-from-color-spec "#000000000"))
  (should-equal '(0 0 0) (color-values-from-color-spec "#000000000000"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#00F"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#0000FF"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#000000FFF"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#00000000FFFF"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#00f"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#0000ff"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#000000fff"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#00000000ffff"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "#00000000ffFF"))
  (should-equal '(#xffff #x0000 #x5555) (color-values-from-color-spec "#f05"))
  (should-equal '(#x1f1f #xb0b0 #xc5c5) (color-values-from-color-spec "#1fb0C5"))
  (should-equal '(#x1f83 #xb0ad #xc5e2) (color-values-from-color-spec "#1f83b0ADC5e2"))
  (should-not (color-values-from-color-spec ""))
  (should-not (color-values-from-color-spec "#"))
  (should-not (color-values-from-color-spec "#0"))
  (should-not (color-values-from-color-spec "#00"))
  (should-not (color-values-from-color-spec "#0000FG"))
  (should-not (color-values-from-color-spec "#0000FFF"))
  (should-not (color-values-from-color-spec "#0000FFFF"))
  (should-equal '(0 4080 65535) (color-values-from-color-spec "#0000FFFFF"))
  (should-not (color-values-from-color-spec "#000FF"))
  (should-not (color-values-from-color-spec "#0000F"))
  (should-not (color-values-from-color-spec " #000000"))
  (should-not (color-values-from-color-spec "#000000 "))
  (should-not (color-values-from-color-spec " #000000 "))
  (should-not (color-values-from-color-spec "#1f83b0ADC5e2g"))
  (should-not (color-values-from-color-spec "#1f83b0ADC5e20"))
  (should-not (color-values-from-color-spec "#12345"))
  ;; rgb: notation
  (should-equal '(0 0 0) (color-values-from-color-spec "rgb:0/0/0"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgb:0/0/00"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgb:0/00/000"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgb:0/000/0000"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgb:000/0000/0"))
  (should-equal '(0 0 65535) (color-values-from-color-spec "rgb:000/0000/F"))
  (should-equal '(65535 0 65535) (color-values-from-color-spec "rgb:FFF/0000/F"))
  (should-equal '(65535 0 65535) (color-values-from-color-spec "rgb:FFFF/0000/FFFF"))
  (should-equal '(0 255 65535) (color-values-from-color-spec "rgb:0/00FF/FFFF"))
  (should-equal '(#xffff #x2323 #x28a2) (color-values-from-color-spec "rgb:f/23/28a"))
  (should-equal '(#x1234 #x5678 #x09ab) (color-values-from-color-spec "rgb:1234/5678/09ab"))
  (should-not (color-values-from-color-spec "rgb:/0000/FFFF"))
  (should-not (color-values-from-color-spec "rgb:0000/0000/FFFG"))
  (should-not (color-values-from-color-spec "rgb:0000/0000/FFFFF"))
  (should-not (color-values-from-color-spec "rgb:0000/0000"))
  (should-not (color-values-from-color-spec "rg:0000/0000/0000"))
  (should-not (color-values-from-color-spec "rgb: 0000/0000/0000"))
  (should-not (color-values-from-color-spec "rgbb:0000/0000/0000"))
  (should-not (color-values-from-color-spec "rgb:0000/0000/0000   "))
  (should-not (color-values-from-color-spec " rgb:0000/0000/0000  "))
  (should-not (color-values-from-color-spec "  rgb:0000/0000/0000"))
  (should-not (color-values-from-color-spec "rgb:0000/ 0000 /0000"))
  (should-not (color-values-from-color-spec "rgb: 0000 /0000 /0000"))
  (should-not (color-values-from-color-spec "rgb:0//0"))
  ;; rgbi: notation
  (should-equal '(0 0 0) (color-values-from-color-spec "rgbi:0/0/0"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgbi:0.0/0.0/0.0"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgbi:0.0/0/0"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgbi:0.0/0/0"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgbi:0/0/0."))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgbi:0/0/0.0000"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgbi:0/0/.0"))
  (should-equal '(0 0 0) (color-values-from-color-spec "rgbi:0/0/.0000"))
  (should-equal '(65535 0 0) (color-values-from-color-spec "rgbi:1/0/0.0000"))
  (should-equal '(65535 0 0) (color-values-from-color-spec "rgbi:1./0/0.0000"))
  (should-equal '(65535 0 0) (color-values-from-color-spec "rgbi:1.0/0/0.0000"))
  (should-equal '(65535 32768 0) (color-values-from-color-spec "rgbi:1.0/0.5/0.0000"))
  (should-equal '(6554 21843 65469) (color-values-from-color-spec "rgbi:0.1/0.3333/0.999"))
  (should-equal '(0 32768 6554) (color-values-from-color-spec "rgbi:0/0.5/0.1"))
  (should-equal '(66 655 65535) (color-values-from-color-spec "rgbi:1e-3/1.0e-2/1e0"))
  (should-equal '(6554 21843 65469) (color-values-from-color-spec "rgbi:1e-1/+0.3333/0.00999e2"))
  (should-not (color-values-from-color-spec "rgbi:1.0001/0/0"))
  (should-not (color-values-from-color-spec "rgbi:2/0/0"))
  (should-not (color-values-from-color-spec "rgbi:0.a/0/0"))
  (should-not (color-values-from-color-spec "rgbi:./0/0"))
  (should-not (color-values-from-color-spec "rgbi:./0/0"))
  (should-not (color-values-from-color-spec " rgbi:0/0/0"))
  (should-not (color-values-from-color-spec "rgbi:0/0/0 "))
  (should-not (color-values-from-color-spec "	rgbi:0/0/0 "))
  (should-not (color-values-from-color-spec "rgbi:0 /0/ 0"))
  (should-not (color-values-from-color-spec "rgbi:0/ 0 /0"))
  (should-not (color-values-from-color-spec "rgbii:0/0/0"))
  (should-not (color-values-from-color-spec "rgbi :0/0/0"))
  ;; strtod ignores leading whitespace, making these legal colour
  ;; specifications:
  ;;
  ;; (should-not (color-values-from-color-spec "rgbi: 0/0/0"))
  ;; (should-not (color-values-from-color-spec "rgbi: 0/ 0/ 0"))
  (should-not (color-values-from-color-spec "rgbi : 0/0/0"))
  (should-not (color-values-from-color-spec "rgbi:0/0.5/10")))

(ert-deftest lookup-key ()
  (let ((a-map (make-sparse-keymap))
        (b-map (make-sparse-keymap)))
    (define-key a-map "x" 'foo)
    (define-key b-map "x" 'bar)
    (should-equal 'foo (compat-call lookup-key a-map "x"))
    (should-equal 'bar (compat-call lookup-key b-map "x"))
    (should-equal 'foo (compat-call lookup-key (list a-map b-map) "x"))
    (should-equal 'bar (compat-call lookup-key (list b-map a-map) "x"))))

(ert-deftest macroexpand-1 ()
  (should-equal '(if a b c) (macroexpand-1 '(if a b c)))
  (should-equal '(if a (progn b)) (macroexpand-1 '(when a b)))
  (should-equal '(if a (progn (unless b c))) (macroexpand-1 '(when a (unless b c)))))

(ert-deftest time-equal-p ()
  (should (time-equal-p nil nil))

  ;; FIXME: Testing these values can be tricky, because the timestamp
  ;; might change between evaluating (current-time) and evaluating
  ;; `time-equal-p', especially in the interpreted compatibility
  ;; version.

  ;; (should (time-equal-p (current-time) nil))
  ;; (should (time-equal-p nil (current-time)))

  ;; While `sleep-for' returns nil, indicating the current time, this
  ;; behaviour seems to be undefined.  Relying on it is therefore not
  ;; advised.
  ;;(should-not (time-equal-p (current-time) (ignore (sleep-for 0.01))))
  ;;(should-not (time-equal-p (current-time) (progn
  ;;                             (sleep-for 0.01)
  ;;                            (current-time))))
  (should (time-equal-p '(1 2 3 4) '(1 2 3 4)))
  (should-not (time-equal-p '(1 2 3 4) '(1 2 3 5)))
  (should-not (time-equal-p '(1 2 3 5) '(1 2 3 4)))
  (should-not (time-equal-p '(1 2 3 4) '(1 2 4 4)))
  (should-not (time-equal-p '(1 2 4 4) '(1 2 3 4)))
  (should-not (time-equal-p '(1 2 3 4) '(1 3 3 4)))
  (should-not (time-equal-p '(1 3 3 4) '(1 2 3 4)))
  (should-not (time-equal-p '(1 2 3 4) '(2 2 3 4)))
  (should-not (time-equal-p '(2 2 3 4) '(1 2 3 4))))

(ert-deftest decoded-time-getters ()
  (let ((time '(second minute hour day month year weekday dst zone)))
    (should-equal (decoded-time-second time) 'second)
    (should-equal (decoded-time-minute time) 'minute)
    (should-equal (decoded-time-hour time) 'hour)
    (should-equal (decoded-time-day time) 'day)
    (should-equal (decoded-time-month time) 'month)
    (should-equal (decoded-time-year time) 'year)
    (should-equal (decoded-time-weekday time) 'weekday)
    (should-equal (decoded-time-dst time) 'dst)
    (should-equal (decoded-time-zone time) 'zone)))

(ert-deftest decoded-time-period ()
  (should-equal 0 (decoded-time-period '()))
  (should-equal 0 (decoded-time-period '(0)))
  (should-equal 1 (decoded-time-period '(1)))
  (should-equal 0.125 (decoded-time-period '((1 . 8))))

  (should-equal 60 (decoded-time-period '(0 1)))
  (should-equal 61 (decoded-time-period '(1 1)))
  (should-equal -59 (decoded-time-period '(1 -1)))

  (should-equal (* 60 60) (decoded-time-period '(0 0 1)))
  (should-equal (+ (* 60 60) 60) (decoded-time-period '(0 1 1)))
  (should-equal (+ (* 60 60) 120 1) (decoded-time-period '(1 2 1)))

  (should-equal (* 60 60 24) (decoded-time-period '(0 0 0 1)))
  (should-equal (+ (* 60 60 24) 1) (decoded-time-period '(1 0 0 1)))
  (should-equal (+ (* 60 60 24) (* 60 60) 60 1) (decoded-time-period '(1 1 1 1)))
  (should-equal (+ (* 60 60 24) (* 60 60) 120 1) (decoded-time-period '(1 2 1 1)))

  (should-equal (* 60 60 24 30) (decoded-time-period '(0 0 0 0 1)))
  (should-equal (+ (* 60 60 24 30) 1) (decoded-time-period '(1 0 0 0 1)))
  (should-equal (+ (* 60 60 24 30) 60 1) (decoded-time-period '(1 1 0 0 1)))
  (should-equal (+ (* 60 60 24 30) (* 60 60) 60 1)
         (decoded-time-period '(1 1 1 0 1)))
  (should-equal (+ (* 60 60 24 30) (* 60 60 24) (* 60 60) 120 1)
         (decoded-time-period '(1 2 1 1 1)))

  (should-equal (* 60 60 24 365) (decoded-time-period '(0 0 0 0 0 1)))
  (should-equal (+ (* 60 60 24 365) 1)
         (decoded-time-period '(1 0 0 0 0 1)))
  (should-equal (+ (* 60 60 24 365) 60 1)
         (decoded-time-period '(1 1 0 0 0 1)))
  (should-equal (+ (* 60 60 24 365) (* 60 60) 60 1)
         (decoded-time-period '(1 1 1 0 0 1)))
  (should-equal (+ (* 60 60 24 365) (* 60 60 24) (* 60 60) 60 1)
         (decoded-time-period '(1 1 1 1 0 1)))
  (should-equal (+ (* 60 60 24 365)
            (* 60 60 24 30)
            (* 60 60 24)
            (* 60 60)
            120 1)
         (decoded-time-period '(1 2 1 1 1 1)))

  (should-error (decoded-time-period 'a) :type 'wrong-type-argument)
  (should-error (decoded-time-period '(0 a)) :type 'wrong-type-argument)
  (should-error (decoded-time-period '(0 0 a)) :type 'wrong-type-argument)
  (should-error (decoded-time-period '(0 0 0 a)) :type 'wrong-type-argument)
  (should-error (decoded-time-period '(0 0 0 0 a)) :type 'wrong-type-argument)
  (should-error (decoded-time-period '(0 0 0 0 0 a)) :type 'wrong-type-argument))

(ert-deftest date-days-in-month ()
  (should-equal 31 (date-days-in-month 2020 1))
  (should-equal 30 (date-days-in-month 2020 4))
  (should-equal 29 (date-days-in-month 2020 2))
  (should-equal 28 (date-days-in-month 2021 2)))

(ert-deftest regexp-opt ()
  ;; Ensure `regexp-opt' doesn't change the existing
  ;; behaviour:
  (should-equal "[abc]" (compat-call regexp-opt '("a" "b" "c")))
  (should-equal "\\(?:abc\\|def\\|ghe\\)" (compat-call regexp-opt '("abc" "def" "ghe")))
  (should-equal "\\<\\([abc]\\)\\>" (compat-call regexp-opt '("a" "b" "c") 'words))
  ;; Test empty list:
  (should-equal "\\(?:\\`a\\`\\)" (compat-call regexp-opt '()))
  (should-equal "\\<\\(\\`a\\`\\)\\>" (compat-call regexp-opt '() 'words)))

(ert-deftest regexp-unmatchable ()
  (dolist (str '(""                     ;empty string
                 "a"                    ;simple string
                 "aaa"                  ;longer string
                 ))
    (should-not (string-match-p regexp-unmatchable str))))

(provide 'compat-tests)
;;; compat-tests.el ends here
