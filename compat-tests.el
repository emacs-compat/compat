;;; compat-tests.el --- Tests for Compat -*- lexical-binding: t; no-byte-compile: t; -*-

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
;; Note that all functions are covered by tests. When new functions are
;; added to Compat, they must come with test coverage!

;; Functions are marked with a link to the testsuite.  The link type
;; `compat-tests' must be registered first by evaluating the following
;; code.  If you intend to work on the test suite you can add this code to
;; your init.el.
;;
;; (require 'ol)
;; (org-link-set-parameters
;;  "compat-tests"
;;  :follow
;;  (lambda (link _)
;;    (org-link-open-from-string
;;     (format "[[file:compat-tests.el::ert-deftest compat-%s ()]]" link))))
;;
;;  You can then jump to the links with the command
;;  `org-open-at-point-global', ideally bound to a convenient key.

;; The tests are written in a simple, explicit style.  Please inspect the
;; tests in order to find out the supported calling conventions.  In
;; particular, note the use of `compat-call' to call functions, where the
;; calling convention or behavior changed between Emacs versions.

;; The functions tested here are guaranteed to work on the Emacs versions
;; tested by continuous integration.  This includes 24.4, 24.5, 25.1, 25.2,
;; 25.3, 26.1, 26.2, 26.3, 27.1, 27.2, 28.1, 28.2, 29.1 and the current
;; Emacs master branch.

;;; Code:

(require 'compat)
(require 'ert-x)
(require 'subr-x)
(require 'time-date)
(require 'image)
(require 'text-property-search nil t)

;; Setup tramp mock
(require 'tramp)
(add-to-list
 'tramp-methods
 '("mock"
   (tramp-login-program      "sh")
   (tramp-login-args         (("-i")))
   (tramp-direct-async       ("-c"))
   (tramp-remote-shell       "/bin/sh")
   (tramp-remote-shell-args  ("-c"))
   (tramp-connection-timeout 10)))
(add-to-list
 'tramp-default-host-alist
 `("\\`mock\\'" nil ,(system-name)))

(defmacro should-equal (a b)
  `(should (equal ,a ,b)))

(defmacro compat-tests--if (cond then &rest else)
  (declare (indent 2))
  (if (eval cond t) then (macroexp-progn else)))

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
    (should-equal (compat-call plist-get list "first" #'string=) 1)))

(defconst compat-tests--version (package-get-version))
(ert-deftest compat-package-get-version ()
  (should (stringp compat-tests--version))
  (should-equal 29 (car (version-to-list compat-tests--version))))

(ert-deftest compat-buffer-match-p ()
  (let ((b "*compat-test-buffer*")
        (child-mode (make-symbol "child"))
        (parent-mode (make-symbol "parent")))
    (put child-mode 'derived-mode-parent parent-mode)
    (with-current-buffer (get-buffer-create b)
      (setq major-mode child-mode))
    (should (buffer-match-p t b))
    (should-not (buffer-match-p nil b))
    (should (buffer-match-p "compat" b))
    (should (buffer-match-p #'always b))
    (should-not (buffer-match-p #'ignore b))
    (should (buffer-match-p `(derived-mode . ,parent-mode) b))
    (should-not (buffer-match-p '(derived-mode . text-mode) b))
    (should (buffer-match-p `(major-mode . ,child-mode) b))
    (should-not (buffer-match-p '(major-mode . prog-mode) b))
    (should (buffer-match-p '(not (major-mode . prog-mode)) b))
    (should (buffer-match-p `(and (major-mode . ,child-mode) "compat" t) b))
    (should (buffer-match-p `(or (major-mode . prog-mode) "foo" t) b))))

(ert-deftest compat-match-buffers ()
  (let ((b1 (get-buffer-create "*compat-buffer1*"))
        (b2 (get-buffer-create "*compat-buffer2*"))
        (b3 (get-buffer-create "*compat-buffer3*"))
        (m1 (make-symbol "mode1"))
        (m2 (make-symbol "mode2"))
        (m3 (make-symbol "mode3")))
  (with-current-buffer b1 (setq major-mode m1))
  (with-current-buffer b2 (setq major-mode m2))
  (with-current-buffer b3 (setq major-mode m3))
  (should-equal (list b2 b1)
                (match-buffers `(or (major-mode . ,m1) (major-mode . ,m2))
                               (list b1 b2 b3)))))

(ert-deftest compat-thing-at-mouse ()
  (save-window-excursion
    (with-temp-buffer
      (let ((event `(mouse-1 (,(selected-window) 1 (0 . 0) 0))))
        (set-window-buffer nil (current-buffer))
        (insert "http://emacs.org/")
        (goto-char (point-min))
        (should-equal "http://emacs.org/" (thing-at-mouse event 'url))
        (should-equal '(1 . 18) (bounds-of-thing-at-mouse event 'url))
        (should-not (region-active-p))
        (mark-thing-at-mouse event 'url)
        (should-equal (point) 18)
        (should-equal '((1 . 18)) (region-bounds))
        (let ((mouse-select-region-move-to-beginning t))
          (mark-thing-at-mouse event 'url))
        (should-equal (point) 1)
        (should-equal '((1 . 18)) (region-bounds))))))

(ert-deftest compat-dolist-with-progress-reporter ()
  (let (y)
    (should-equal
     (dolist-with-progress-reporter (x '(1 2 3) y) "Reporter"
       (push x y))
     '(3 2 1)))
  (let (y)
    (should-equal
     (dolist-with-progress-reporter
         (x '(1 2 3) y) (make-progress-reporter "Reporter")
       (push x y))
     '(3 2 1))))

(ert-deftest compat-minibuffer-history-value ()
  (let ((minibuffer-history-variable 'file-name-history)
        (file-name-history '("a" "b" "c")))
    (should-equal (minibuffer-history-value) '("a" "b" "c")))
  (let ((file-name-history '("x" "y" "z")))
    (should-equal
     (catch 'compat-tests--exit
       (minibuffer-with-setup-hook
           (lambda ()
             (message "%S" minibuffer-history-variable)
             (throw 'compat-tests--exit (minibuffer-history-value)))
         (let ((executing-kbd-macro t))
           (completing-read "Prompt: " #'completion-file-name-table
                            nil nil nil 'file-name-history))))
     '("x" "y" "z"))))

(ert-deftest compat-with-minibuffer-selected-window ()
  (let (ran)
    (should-not (minibuffer-selected-window))
    (should-not (with-minibuffer-selected-window
                  (setq ran t)))
    (should-not ran)
    (unwind-protect
        (progn
          (advice-add #'minibuffer-selected-window :override #'selected-window)
          (should-equal 'result (with-minibuffer-selected-window
                                  (setq ran t)
                                  'result))
          (should ran))
      (advice-remove #'minibuffer-selected-window #'selected-window))))

(ert-deftest compat-fixnump ()
  (should (fixnump 0))
  (should (fixnump most-negative-fixnum))
  (should (fixnump most-positive-fixnum)))

(ert-deftest compat-bignump ()
  (should-not (bignump 0))
  (should-not (bignump most-negative-fixnum))
  (should-not (bignump most-positive-fixnum))
  (should-equal (bignump (1+ most-positive-fixnum)) (> emacs-major-version 26))
  (should-equal (bignump (1- most-negative-fixnum)) (> emacs-major-version 26)))

(ert-deftest compat-buttonize ()
  (let ((b (buttonize "button" 'c 'd 'h)))
    (should-equal b "button")
    (should-equal 'c (get-text-property 0 'action b))
    (should-equal 'c (get-text-property 5 'action b))
    (should-equal 'd (get-text-property 0 'button-data b))
    (should-equal 'd (get-text-property 5 'button-data b))
    (should-equal 'h (get-text-property 0 'help-echo b))
    (should-equal 'h (get-text-property 5 'help-echo b))))

(ert-deftest compat-button-buttonize ()
  (let ((b (with-no-warnings (button-buttonize "button" 'c 'd))))
    (should-equal b "button")
    (should-equal 'c (get-text-property 0 'action b))
    (should-equal 'c (get-text-property 5 'action b))
    (should-equal 'd (get-text-property 0 'button-data b))
    (should-equal 'd (get-text-property 5 'button-data b))))

(ert-deftest compat-buttonize-region ()
  (with-temp-buffer
    (insert "<button>")
    (buttonize-region 2 7 'c 'd 'h)
    (should-not (get-text-property 1 'action))
    (should-not (get-text-property 7 'action))
    (should-equal 'c (get-text-property 2 'action))
    (should-equal 'c (get-text-property 6 'action))
    (should-equal 'd (get-text-property 2 'button-data))
    (should-equal 'd (get-text-property 6 'button-data))
    (should-equal 'h (get-text-property 2 'help-echo))
    (should-equal 'h (get-text-property 6 'help-echo))))

(ert-deftest compat-with-restriction ()
  (with-temp-buffer
    (insert "abc")
    (with-restriction 2 3 :label 'foo
      (should-equal "b" (buffer-string)))
    (should-equal "abc" (buffer-string))
    (with-restriction 2 3
      (should-equal "b" (buffer-string)))
    (should-equal "abc" (buffer-string))))

(ert-deftest compat-without-restriction ()
  (with-temp-buffer
    (insert "abc")
    (narrow-to-region 2 3)
    (without-restriction :label 'foo
      (should-equal "abc" (buffer-string)))
    (should-equal "b" (buffer-string))
    (without-restriction
      (should-equal "abc" (buffer-string)))
    (should-equal "b" (buffer-string))))

(ert-deftest compat-with-memoization ()
  (let ((x (cons nil nil)) y computed)
    (with-memoization (car x)
      (setq computed 'a))
    (should-equal (car x) 'a)
    (should-equal computed 'a)
    (with-memoization (car x)
      (setq computed 'b))
    (should-equal (car x) 'a)
    (should-equal computed 'a)
    (with-memoization (cdr x)
      (setq computed 'c))
    (should-equal (cdr x) 'c)
    (should-equal computed 'c)
    (with-memoization (cdr x)
      (setq computed 'd))
    (should-equal (cdr x) 'c)
    (should-equal computed 'c)
    (with-memoization y
      (setq computed 'e))
    (should-equal y 'e)
    (should-equal computed 'e)
    (with-memoization y
      (setq computed 'f))
    (should-equal y 'e)
    (should-equal computed 'e)))

(ert-deftest compat-make-separator-line ()
  (should-equal (length (make-separator-line 10)) 11)
  (should (string-suffix-p "\n" (make-separator-line 10)))
  (should (string-suffix-p "\n" (make-separator-line)))
  (should-equal (replace-regexp-in-string
                 "[^\n]" "" (make-separator-line)) "\n"))

(ert-deftest compat-pos-bol ()
  (with-temp-buffer
    (insert (propertize "one" 'field 1)
            (propertize "two" 'field 2)
            (propertize "tri" 'field 3)
            "\n")
    (insert (propertize "one" 'field 1)
            (propertize "two" 'field 2)
            (propertize "tri" 'field 3)
            "\n")
    (goto-char 5)
    (should-equal (line-beginning-position) 4)
    (should-equal (line-end-position) 7)
    (should-equal (pos-bol) 1)
    (should-equal (pos-eol) 10)
    (should-equal (line-beginning-position 1) 4)
    (should-equal (line-end-position 1) 7)
    (should-equal (pos-bol 1) 1)
    (should-equal (pos-eol 1) 10)
    (should-equal (line-beginning-position 2) 11)
    (should-equal (line-end-position 2) 20)
    (should-equal (pos-bol 2) 11)
    (should-equal (pos-eol 2) 20)
    (goto-char 15)
    (should-equal (line-beginning-position) 14)
    (should-equal (line-end-position) 17)
    (should-equal (pos-bol) 11)
    (should-equal (pos-eol) 20)
    (should-equal (line-beginning-position 1) 14)
    (should-equal (line-end-position 1) 17)
    (should-equal (pos-bol 1) 11)
    (should-equal (pos-eol 1) 20)
    (should-equal (line-beginning-position 0) 1)
    (should-equal (line-end-position 0) 10)
    (should-equal (pos-bol 0) 1)
    (should-equal (pos-eol 0) 10)))

(ert-deftest compat-image-property ()
  (let ((image (list 'image)))
    ;; Add properties.
    (setf (image-property image :scale) 1)
    (should-equal image '(image :scale 1))
    (setf (image-property image :width) 8)
    (should-equal image '(image :scale 1 :width 8))
    (setf (image-property image :height) 16)
    (should-equal image '(image :scale 1 :width 8 :height 16))
    ;; Delete properties.
    (setf (image-property image :type) nil)
    (should-equal image '(image :scale 1 :width 8 :height 16))
    (setf (image-property image :scale) nil)
    (should-equal image '(image :width 8 :height 16))
    (setf (image-property image :height) nil)
    (should-equal image '(image :width 8))
    (setf (image-property image :width) nil)
    (should-equal image '(image))))

(ert-deftest compat-read-answer ()
  (let ((orig-re (symbol-function #'read-event))
        (orig-rc (symbol-function #'read-char))
        (orig-rm (symbol-function #'read-from-minibuffer)))
    (unwind-protect
        (dolist (test '(("Choose "
                         ("first" ?a "first description")
                         ("second" ?b "second description")
                         ("third" ?c))
                        ("Do it? " ("yes" ?y) ("no" ?n))))
          (dolist (choice (cdr test))
            (fset #'read-char (lambda (&rest _) (cadr choice)))
            (fset #'read-event (lambda (&rest _) (cadr choice)))
            (fset #'read-from-minibuffer (lambda (&rest _) (car choice)))
            (should-equal (car choice) (read-answer (car test) (cdr test)))))
      (fset #'read-event orig-re)
      (fset #'read-char orig-rc)
      (fset #'read-from-minibuffer orig-rm))))

(ert-deftest compat-read-multiple-choice ()
  (let ((orig-re (symbol-function #'read-event))
        (orig-rc (symbol-function #'read-char))
        (orig-cr completing-read-function))
    (unwind-protect
        (dolist (test '(("Choose"
                         (?a "first" "first description")
                         (?b "second" "second description")
                         (?c "third"))
                        ("Do it?" (?y "yes") (?n "no"))))
          (dolist (choice (cdr test))
            (fset #'read-char (lambda (&rest _) (car choice)))
            (fset #'read-event (lambda (&rest _) (car choice)))
            (setq completing-read-function (lambda (&rest _) (cadr choice)))
            (should-equal choice (compat-call read-multiple-choice
                                              (car test) (cdr test) nil nil 'long))
            (should-equal choice (read-multiple-choice (car test) (cdr test)))))
      (fset #'read-event orig-re)
      (fset #'read-char orig-rc)
      (setq completing-read-function orig-cr))))

(ert-deftest compat-read-char-from-minibuffer ()
  (let ((orig (symbol-function #'read-from-minibuffer)))
    (unwind-protect
        (progn
          (fset #'read-from-minibuffer (lambda (&rest _) "a"))
          (should-equal ?a (read-char-from-minibuffer
                            "Prompt: " '(?a ?b ?c) 'read-char-history))
          (should-equal ?a (read-char-from-minibuffer "Prompt: " '(?a ?b ?c)))
          (should-equal ?a (read-char-from-minibuffer "Prompt: ")))
      (fset #'read-from-minibuffer orig))))

(ert-deftest compat-with-environment-variables ()
  (let ((A "COMPAT_TESTS__VAR") (B "/foo/bar"))
    (should-not (getenv A))
    (with-environment-variables ((A B))
      (should-equal (getenv A) B))
    (should-not (getenv A))))

(ert-deftest compat-window-configuration-equal-p ()
  (let ((wc (current-window-configuration)))
    (should (window-configuration-equal-p wc wc))
    (save-window-excursion
      (with-temp-buffer
        (pop-to-buffer (current-buffer))
        (should-not (window-configuration-equal-p (current-window-configuration) wc))))
    (should (window-configuration-equal-p (current-window-configuration) wc))))

(ert-deftest compat-with-window-non-dedicated ()
  (unwind-protect
      (progn
        (should-not (window-dedicated-p))
        (set-window-dedicated-p nil t)
        (should (window-dedicated-p))
        (with-window-non-dedicated nil
          (should-not (window-dedicated-p)))
        (should (window-dedicated-p)))
    (set-window-dedicated-p nil nil)))

(ert-deftest compat-count-windows ()
  (should (fixnump (compat-call count-windows)))
  (should (fixnump (compat-call count-windows t)))
  (should (fixnump (compat-call count-windows t t))))

(ert-deftest compat-recenter ()
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (compat-call recenter nil nil)
    (compat-call recenter nil t)
    (compat-call recenter 1 nil)
    (compat-call recenter 1 t)))

(ert-deftest compat-get-display-property ()
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

(ert-deftest compat-add-display-text-property ()
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
    (should-equal (let ((str (copy-sequence "some useless string")))
                     (add-display-text-property 4 8 'height 2.0 str)
                     (add-display-text-property 2 12 'raise 0.5 str)
                     str)
                   #("some useless string"
                     2 4 (display (raise 0.5))
                     4 8 (display ((raise 0.5) (height 2.0)))
                     8 12 (display (raise 0.5))))))

(ert-deftest compat-line-number-at-pos ()
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
    (define-key map (kbd "RET") #'exit-minibuffer)
    (define-key map [remap exit-minibuffer] #'minibuffer-force-complete-and-exit)
    (define-key map (kbd "C-c") mode-specific-map)
    (define-key map (kbd "s-c") [?\C-c ?\C-c])
    (define-key map [t] 'compat-default-command)
    map))
(defvar compat-tests--map-2
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-x C-f" #'find-file)
    (keymap-set map "SPC" #'minibuffer-complete-word)
    (keymap-set map "RET" #'exit-minibuffer)
    (keymap-set map "<remap> <exit-minibuffer>" #'minibuffer-force-complete-and-exit)
    (keymap-set map "C-c" mode-specific-map)
    (keymap-set map "s-c" "C-c C-c")
    (keymap-set map "<t>" 'compat-default-command)
    map))
(defvar-keymap compat-tests--map-3
  "C-x C-f" #'find-file
  "SPC" #'minibuffer-complete-word
  "RET" #'exit-minibuffer
  "<remap> <exit-minibuffer>" #'minibuffer-force-complete-and-exit
  "C-c" mode-specific-map
  "s-c" "C-c C-c"
  "<t>" 'compat-default-command)
(defvar compat-tests--map-4
  (define-keymap
    "C-x C-f" #'find-file
    "SPC" #'minibuffer-complete-word
    "RET" #'exit-minibuffer
    "<remap> <exit-minibuffer>" #'minibuffer-force-complete-and-exit
    "C-c" mode-specific-map
    "s-c" "C-c C-c"
    "<t>" 'compat-default-command))
(ert-deftest compat-defvar-keymap ()
  (should-equal compat-tests--map-1 compat-tests--map-2)
  (should-equal compat-tests--map-1 compat-tests--map-3)
  (should-equal compat-tests--map-1 compat-tests--map-4))

(ert-deftest compat-keymap-set-after ()
  (let ((map (make-sparse-keymap)))
    (keymap-set-after map "d" 'd "a")
    (keymap-set-after map "a" 'a)
    (keymap-set-after map "b" 'b)
    (keymap-set-after map "c" 'c)
    (keymap-set-after map "d" 'd "b") ;; TODO the after argument has no effect?!
    (should-equal map '(keymap (?a . a) (?b . b) (?c . c) (?d . d)))))

(ert-deftest compat-keymap-substitute ()
  (let ((map (define-keymap
               "C-x C-f" #'find-file
               "s-f" #'find-file
               "C-x b" #'switch-to-buffer)))
    (keymap-substitute map #'find-file 'ffap)
    (should-equal (keymap-lookup map "C-x b") #'switch-to-buffer)
    (should-equal (keymap-lookup map "C-x C-f") 'ffap)
    (should-equal (keymap-lookup map "s-f") 'ffap)))

(ert-deftest compat-key-parse ()
  (should-equal (key-parse "f") [?f])
  (should-equal (key-parse "X") [?X])
  (should-equal (key-parse "X f") [?X ?f])

  (should-equal (key-parse "<F2>") [F2])
  (should-equal (key-parse "<f1> <f2> TAB") [f1 f2 ?\t])
  (should-equal (key-parse "<f1> RET") [f1 ?\r])
  (should-equal (key-parse "<f1> SPC") [f1 ?\s])
  (should-equal (key-parse "<f1>") [f1])
  (should-equal (key-parse "<return>") [return])

  ;; ;; Modifiers:
  (should-equal (key-parse "C-x") [?\C-x])
  (should-equal (key-parse "C-x a") [?\C-x ?a])
  (should-equal (key-parse "C-;") [67108923])
  (should-equal (key-parse "C-a") [?\C-a])
  (should-equal (key-parse "C-c SPC") [?\C-c ?\s])
  (should-equal (key-parse "C-c TAB") [?\C-c ?\t])
  (should-equal (key-parse "C-c c") [?\C-c ?c])
  (should-equal (key-parse "C-x 4 C-f") [?\C-x ?4 ?\C-f])
  (should-equal (key-parse "C-x C-f") [?\C-x ?\C-f])
  (should-equal (key-parse "C-M-<down>") [C-M-down])
  (should-equal (key-parse "C-RET") [?\C-\r])
  (should-equal (key-parse "C-SPC") [?\C-\s])
  (should-equal (key-parse "C-TAB") [?\C-\t])
  (should-equal (key-parse "C-<down>") [C-down])
  (should-equal (key-parse "C-c C-c C-c") [?\C-c ?\C-c ?\C-c])

  (should-equal (key-parse "M-a") [?\M-a])
  (should-equal (key-parse "M-<DEL>") [?\M-\d])
  (should-equal (key-parse "C-M-a") [?\C-\M-a])
  (should-equal (key-parse "M-ESC") [?\M-\e])
  (should-equal (key-parse "M-RET") [?\M-\r])
  (should-equal (key-parse "M-SPC") [?\M-\s])
  (should-equal (key-parse "M-TAB") [?\M-\t])
  (should-equal (key-parse "M-x a") [?\M-x ?a])
  (should-equal (key-parse "M-<up>") [M-up])
  (should-equal (key-parse "M-c M-c M-c") [?\M-c ?\M-c ?\M-c])

  (should-equal (key-parse "s-SPC") [?\s-\s])
  (should-equal (key-parse "s-a") [?\s-a])
  (should-equal (key-parse "s-x a") [?\s-x ?a])
  (should-equal (key-parse "s-c s-c s-c") [?\s-c ?\s-c ?\s-c])

  (should-equal (key-parse "S-a") [?\S-a])
  (should-equal (key-parse "S-x a") [?\S-x ?a])
  (should-equal (key-parse "S-c S-c S-c") [?\S-c ?\S-c ?\S-c])

  (should-equal (key-parse "H-<RET>") [?\H-\r])
  (should-equal (key-parse "H-DEL") [?\H-\d])
  (should-equal (key-parse "H-a") [?\H-a])
  (should-equal (key-parse "H-x a") [?\H-x ?a])
  (should-equal (key-parse "H-c H-c H-c") [?\H-\c ?\H-\c ?\H-\c])

  (should-equal (key-parse "A-H-a") [?\A-\H-a])
  (should-equal (key-parse "A-SPC") [?\A-\s])
  (should-equal (key-parse "A-TAB") [?\A-\t])
  (should-equal (key-parse "A-a") [?\A-a])
  (should-equal (key-parse "A-c A-c A-c") [?\A-c ?\A-c ?\A-c])

  (should-equal (key-parse "C-M-a") [?\C-\M-a])
  (should-equal (key-parse "C-M-<up>") [C-M-up])

  ;; ;; Special characters.
  (should-equal (key-parse "DEL") [?\d])
  (should-equal (key-parse "ESC C-a") [?\e ?\C-a])
  (should-equal (key-parse "ESC") [?\e])
  (should-equal (key-parse "LFD") [?\n])
  (should-equal (key-parse "NUL") [?\0])
  (should-equal (key-parse "RET") [?\r])
  (should-equal (key-parse "SPC") [?\s])
  (should-equal (key-parse "TAB") [?\t])

  ;; ;; Multibyte
  (should-equal (key-parse "ñ") [?ñ])
  (should-equal (key-parse "ü") [?ü])
  (should-equal (key-parse "ö") [?ö])
  (should-equal (key-parse "ğ") [?ğ])
  (should-equal (key-parse "ա") [?ա])
  (should-equal (key-parse "C-ü") [?\C-ü])
  (should-equal (key-parse "M-ü") [?\M-ü])
  (should-equal (key-parse "H-ü") [?\H-ü])

  ;; ;; Handle both new and old style key descriptions (bug#45536).
  (should-equal (key-parse "s-<return>") [s-return])
  (should-equal (key-parse "C-M-<return>") [C-M-return])

  (should-equal (key-parse "<mouse-1>") [mouse-1])
  (should-equal (key-parse "<Scroll_Lock>") [Scroll_Lock]))

(ert-deftest compat-keymap--check ()
  (keymap--check "X")
  (should-error (keymap--check ""))
  (should-error (keymap--check " X")))

(ert-deftest compat-key-valid-p ()
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

(ert-deftest compat-keymap-lookup ()
  (should-not (keymap-lookup compat-tests--map-1 "C-x b"))
  (should-equal (keymap-lookup compat-tests--map-1 "C-x C-f") #'find-file)
  (should-equal (keymap-lookup compat-tests--map-1 "RET") #'exit-minibuffer)
  (should-equal (keymap-lookup compat-tests--map-1 "C-c") mode-specific-map)
  (should-equal (keymap-lookup compat-tests--map-1 "s-c") [?\C-c ?\C-c])
  (should-not (keymap-lookup compat-tests--map-1 "x"))
  (should-equal (keymap-lookup compat-tests--map-1 "x" t) 'compat-default-command))

(ert-deftest compat-keymap-local-lookup ()
  (let ((orig (current-local-map)))
    (unwind-protect
        (progn
          (use-local-map compat-tests--map-1)
          (should-not (keymap-local-lookup "C-x b"))
          (should-equal (keymap-local-lookup "C-x C-f") #'find-file)
          (should-equal (keymap-lookup compat-tests--map-1 "RET" nil t) #'exit-minibuffer)
          (should-equal (keymap-local-lookup "RET") #'minibuffer-force-complete-and-exit)
          (should-equal (keymap-local-lookup "C-c") mode-specific-map)
          (should-equal (keymap-local-lookup "s-c") [?\C-c ?\C-c])
          (should-not (keymap-local-lookup "x"))
          (should-equal (keymap-local-lookup "x" t) 'compat-default-command))
      (use-local-map orig))))

(ert-deftest compat-keymap-local-set ()
  (let ((orig (current-local-map)))
    (unwind-protect
        (progn
          (use-local-map (make-sparse-keymap))
          (should-not (keymap-local-lookup "s-c"))
          (should-not (keymap-local-lookup "x"))
          (keymap-local-set "s-c" 'test)
          (keymap-local-set "<t>" 'default)
          (should-equal (keymap-local-lookup "s-c") 'test)
          (should-equal (keymap-local-lookup "x" t) 'default)
          (should-not (keymap-local-lookup "x")))
      (use-local-map orig))
    (should-not (keymap-local-lookup "s-c"))))

(ert-deftest compat-keymap-global-set ()
  (let ((orig (current-global-map)))
    (unwind-protect
        (progn
          (use-global-map (make-sparse-keymap))
          (should-not (keymap-global-lookup "H-c"))
          (should-not (keymap-global-lookup "x"))
          (keymap-global-set "H-c" 'test)
          (keymap-global-set "<t>" 'default)
          (should-equal (keymap-global-lookup "H-c") 'test)
          (should-equal (keymap-global-lookup "x" t) 'default)
          (should-not (keymap-global-lookup "x")))
      (use-global-map orig))
    (should-not (keymap-global-lookup "H-c"))))

(ert-deftest compat-keymap-global-lookup ()
  (should-equal (keymap-global-lookup "C-x b") #'switch-to-buffer)
  (should-equal (keymap-global-lookup "C-x C-f") #'find-file)
  (should-equal (keymap-global-lookup "C-c") #'mode-specific-command-prefix))

(ert-deftest compat-keymap-unset ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-x" #'execute-extended-command)
    (define-key map "\C-x\C-f" #'find-file)
    (define-key map "\C-y" #'yank)
    (keymap-unset map "M-x")
    (keymap-unset map "C-x C-f")
    (keymap-unset map "C-y")
    (should-equal map '(keymap (25) (24 keymap (6)) (27 keymap (120)))))
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-x" #'execute-extended-command)
    (define-key map "\C-x\C-f" #'find-file)
    (define-key map "\C-y" #'yank)
    (keymap-unset map "M-x" t)
    (keymap-unset map "C-x C-f" t)
    (keymap-unset map "C-y" t)
    (should-equal map '(keymap (24 keymap) (27 keymap)))))

(ert-deftest compat-keymap-local-unset ()
  (let ((map (make-sparse-keymap))
        (orig (current-local-map)))
    (unwind-protect
        (progn
          (use-local-map map)
          (define-key map "\M-x" #'execute-extended-command)
          (define-key map "\C-x\C-f" #'find-file)
          (define-key map "\C-y" #'yank)
          (keymap-local-unset "M-x")
          (keymap-local-unset "C-x C-f" t)
          (keymap-local-unset "C-y" t)
          (should-equal (current-local-map) '(keymap (24 keymap) (27 keymap (120)))))
      (use-local-map orig))))

(ert-deftest compat-keymap-global-unset ()
  (let ((map (make-sparse-keymap))
        (orig (current-global-map)))
    (unwind-protect
        (progn
          (use-global-map map)
          (define-key map "\M-x" #'execute-extended-command)
          (define-key map "\C-x\C-f" #'find-file)
          (define-key map "\C-y" #'yank)
          (keymap-global-unset "M-x")
          (keymap-global-unset "C-x C-f" t)
          (keymap-global-unset "C-y" t)
          (should-equal (current-global-map) '(keymap (24 keymap) (27 keymap (120)))))
      (use-global-map orig))))

(ert-deftest compat-define-key ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-x" #'execute-extended-command)
    (define-key map "\C-x\C-f" #'find-file)
    (define-key map "\C-y" #'yank)
    (compat-call define-key map "\M-x" nil t)
    (compat-call define-key map "\C-x\C-f" nil t)
    (compat-call define-key map "\C-y" nil t)
    (should-equal map '(keymap (24 keymap) (27 keymap)))))

(ert-deftest compat-function-alias-p ()
  (defun compat-tests--alias-fun ())
  (should-not (function-alias-p 1))
  (should-not (function-alias-p 'compat-tests--alias-fun))
  (defalias 'compat-tests--alias-a 'compat-tests--alias-b)
  (defalias 'compat-tests--alias-b 'compat-tests--alias-c)
  (should-equal (function-alias-p 'compat-tests--alias-a)
                '(compat-tests--alias-b compat-tests--alias-c))
  ;; Emacs 30 disallows cyclic function aliases
  (compat-tests--if (>= emacs-major-version 30)
      (should-error
       (progn
         (defalias 'compat-tests--cyclic-alias-a 'compat-tests--cyclic-alias-b)
         (defalias 'compat-tests--cyclic-alias-b 'compat-tests--cyclic-alias-a)))
    (defalias 'compat-tests--cyclic-alias-a 'compat-tests--cyclic-alias-b)
    (defalias 'compat-tests--cyclic-alias-b 'compat-tests--cyclic-alias-a)
    (should-error (function-alias-p 'compat-tests--cyclic-alias-a))
    (should-equal (function-alias-p 'compat-tests--cyclic-alias-a 'noerror)
                  '(compat-tests--cyclic-alias-b))
    (should-equal (function-alias-p 'compat-tests--cyclic-alias-a t)
                  '(compat-tests--cyclic-alias-b))))

(ert-deftest compat-ignore-error ()
  (should-equal (ignore-error (end-of-file)
                  (read ""))
                nil)
  (should-equal (ignore-error end-of-file
                  (read ""))
                nil)
  (should-error (ignore-error foo
                  (read ""))))

(ert-deftest compat-hash-table-empty-p ()
  (should (hash-table-empty-p (make-hash-table)))
  (let ((ht (make-hash-table)))
    (puthash 'k 'v ht)
    (should-not (hash-table-empty-p ht))))

(ert-deftest compat-thread-first ()
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

(ert-deftest compat-thread-last ()
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

(ert-deftest compat-ntake ()
  (should-not (ntake 5 nil))
  (should-equal '(1 2) (ntake 5 (list 1 2)))
  (should-equal '(1 2 3) (ntake 3 (list 1 2 3 4))))

(ert-deftest compat-take ()
  (should-not (take 5 nil))
  (should-equal '(1 2) (take 5 '(1 2)))
  (should-equal '(1 2 3) (take 3 '(1 2 3 4))))

(ert-deftest compat-format-message ()
  (should-equal (format-message "a=%s b=%s" 1 2) "a=1 b=2"))

(defvar compat-tests--boundp)
(defvar compat-tests--global-boundp)
(ert-deftest compat-buffer-local-boundp ()
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
(ert-deftest compat-setq-local ()
  (compat-call setq-local
               compat-tests--local-a 1
               compat-tests--local-b 2
               compat-tests--local-c 3)
  (should-equal compat-tests--local-a 1)
  (should-equal compat-tests--local-b 2)
  (should-equal compat-tests--local-c 3))

(defvar compat-tests--global)
(defvar compat-tests--local)
(defvar compat-tests--unexist)
(ert-deftest compat-buffer-local-set-state ()
  (setq compat-tests--global 1)
  (with-temp-buffer
    (setq-local compat-tests--local 2)
    (let ((state (buffer-local-set-state compat-tests--global 10
                                         compat-tests--local 20
                                         compat-tests--unexist 30)))
      (should-equal compat-tests--global 10)
      (should-equal compat-tests--local 20)
      (should-equal compat-tests--unexist 30)
      (buffer-local-restore-state state)
      (should-equal compat-tests--global 1)
      (should-equal compat-tests--local 2)
      (should-not (boundp 'compat-tests--unexist)))))

(ert-deftest compat-gensym ()
  (let ((orig gensym-counter))
    (should (integerp gensym-counter))
    (should (symbolp (gensym "compat")))
    (should (string-prefix-p "compat" (symbol-name (gensym 'compat))))
    (should (string-prefix-p "compat" (symbol-name (gensym "compat"))))
    (should-equal gensym-counter (+ orig 3))))

(ert-deftest compat-delete-line ()
  (with-temp-buffer
    (insert "first\nsecond\nthird\n")
    (goto-char 7)
    (delete-line)
    (should-equal (buffer-string) "first\nthird\n")))

(ert-deftest compat-list-of-strings-p ()
  (should-not (list-of-strings-p 1))
  (should (list-of-strings-p nil))
  (should (list-of-strings-p '("a" "b")))
  (should-not (list-of-strings-p ["a" "b"]))
  (should-not (list-of-strings-p '("a" nil "b")))
  (should-not (list-of-strings-p '("a" "b" . "c"))))

(ert-deftest compat-plistp ()
  (should (plistp '(:a a :b b)))
  (should (plistp '(1 2 3 4)))
  (should-not (plistp '(1 2 3)))
  (should-not (plistp '(1 . 2)))
  (should-not (plistp '(1 2 . 3)))
  (should-not (let ((l (list 1 2 3)))
                (setf (nthcdr 3 l) l)
                (plistp l))))

(ert-deftest compat-plist-get ()
  (let (list)
    (setq list (compat-call plist-put list 'first 1))
    (setq list (compat-call plist-put list 'second 2))
    (setq list (compat-call plist-put list 'first 10))
    (should-equal (compat-call plist-get list 'first) 10)
    (should-equal (compat-call plist-get list 'second) 2)
    (should (compat-call plist-member list 'first))
    (should-not (compat-call plist-member list 'third)))
  (let (list)
    (setq list (compat-call plist-put list "first" 1 #'string=))
    (setq list (compat-call plist-put list "second" 2 #'string=))
    (setq list (compat-call plist-put list "first" 10 #'string=))
    (should-equal (compat-call plist-get list "first" #'string=) 10)
    (should-equal (compat-call plist-get list "second" #'string=) 2)
    (should (compat-call plist-member list "first" #'string=))
    (should-not (compat-call plist-member list "third" #'string=))))

(ert-deftest compat-garbage-collect-maybe ()
  (garbage-collect-maybe 10))

(ert-deftest compat-buffer-hash ()
  (should-equal (sha1 "foo") "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33")
  (should-equal (with-temp-buffer
                   (insert "foo")
                   (buffer-hash))
                 (sha1 "foo"))
  ;; This tests whether the presence of a gap in the middle of the
  ;; buffer is handled correctly.
  (should-equal (with-temp-buffer
                   (insert "foo")
                   (goto-char 2)
                   (insert " ")
                   (delete-char (- 1))
                   (buffer-hash))
                 (sha1 "foo")))

(ert-deftest compat-with-buffer-unmodified-if-unchanged ()
  (with-temp-buffer
    (with-buffer-unmodified-if-unchanged
      (insert "t"))
    (should (buffer-modified-p)))

  (with-temp-buffer
    (with-buffer-unmodified-if-unchanged
      (insert "t")
      (delete-char -1))
    (should-not (buffer-modified-p)))

  ;; Shouldn't error.
  (should
   (with-temp-buffer
     (with-buffer-unmodified-if-unchanged
       (insert "t")
       (delete-char -1)
       (kill-buffer))))

  (with-temp-buffer
    (let ((outer (current-buffer)))
      (with-temp-buffer
        (let ((inner (current-buffer)))
          (with-buffer-unmodified-if-unchanged
            (insert "t")
            (delete-char -1)
            (set-buffer outer))
          (with-current-buffer inner
            (should-not (buffer-modified-p))))))))

(ert-deftest compat-insert-into-buffer ()
  ;; Without optional compat--arguments
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
        (insert "def")
        (insert-into-buffer other))
      (should-equal (buffer-string) "abcdef")))
  ;; With one optional argument
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
        (insert "def")
        (insert-into-buffer other 2))
      (should-equal (buffer-string) "abcef")))
  ;; With two optional arguments
  (with-temp-buffer
    (let ((other (current-buffer)))
      (insert "abc")
      (with-temp-buffer
        (insert "def")
        (insert-into-buffer other 2 3))
      (should-equal (buffer-string) "abce"))))

(ert-deftest compat-bool-vector ()
  (should-equal (bool-vector) (bool-vector-not (bool-vector)))
  (should-equal (bool-vector t) (bool-vector-not (bool-vector nil)))
  (should-equal (bool-vector nil) (bool-vector-not (bool-vector t)))
  (should-equal (bool-vector t t) (bool-vector-not (bool-vector nil nil)))
  (should-equal (bool-vector t nil) (bool-vector-not (bool-vector nil t)))
  (should-equal (bool-vector nil t) (bool-vector-not (bool-vector t nil)))
  (should-equal (bool-vector nil nil) (bool-vector-not (bool-vector t t))))

(ert-deftest compat-assoc ()
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

(ert-deftest compat-assoc-delete-all ()
  (should-equal (list) (assoc-delete-all 0 (list)))
  ;; Test `eq'
  (should-equal '((1 . one)) (assoc-delete-all 0 (list (cons 1 'one))))
  (should-equal '((1 . one) a) (assoc-delete-all 0 (list (cons 1 'one) 'a)))
  (should-equal '((1 . one)) (assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one))))
  (should-equal '((1 . one)) (assoc-delete-all 0 (list (cons 0 'zero) (cons 0 'zero) (cons 1 'one))))
  (should-equal '((1 . one)) (assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one) (cons 0 'zero))))
  (should-equal '((1 . one) a) (assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one) 'a  (cons 0 'zero))))
  (should-equal '(a (1 . one)) (assoc-delete-all 0 (list 'a (cons 0 'zero) (cons 1 'one) (cons 0 'zero))))
  ;; Test `equal'
  (should-equal '(("one" . one)) (assoc-delete-all "zero" (list (cons "one" 'one))))
  (should-equal '(("one" . one) a) (assoc-delete-all "zero" (list (cons "one" 'one) 'a)))
  (should-equal '(("one" . one)) (assoc-delete-all "zero" (list (cons "zero" 'zero) (cons "one" 'one))))
  (should-equal '(("one" . one)) (assoc-delete-all "zero" (list (cons "zero" 'zero) (cons "zero" 'zero) (cons "one" 'one))))
  (should-equal '(("one" . one)) (assoc-delete-all "zero" (list (cons "zero" 'zero) (cons "one" 'one) (cons "zero" 'zero))))
  (should-equal '(("one" . one) a) (assoc-delete-all "zero" (list (cons "zero" 'zero) (cons "one" 'one) 'a  (cons "zero" 'zero))))
  (should-equal '(a ("one" . one)) (assoc-delete-all "zero" (list 'a (cons "zero" 'zero) (cons "one" 'one) (cons "zero" 'zero))))
  ;; Test custom predicate
  (should-equal '() (compat-call assoc-delete-all 0 (list (cons 1 'one)) #'/=))
  (should-equal '(a) (compat-call assoc-delete-all 0 (list (cons 1 'one) 'a) #'/=))
  (should-equal '((0 . zero)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one)) #'/=))
  (should-equal '((0 . zero) (0 . zero)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 0 'zero) (cons 1 'one)) #'/=))
  (should-equal '((0 . zero) (0 . zero)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one) (cons 0 'zero)) #'/=))
  (should-equal '((0 . zero) a (0 . zero)) (compat-call assoc-delete-all 0 (list (cons 0 'zero) (cons 1 'one) 'a  (cons 0 'zero)) #'/=))
  (should-equal '(a (0 . zero) (0 . zero)) (compat-call assoc-delete-all 0 (list 'a (cons 0 'zero) (cons 1 'one) (cons 0 'zero)) #'/=)))

(ert-deftest compat-provided-derived-mode-p ()
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

(ert-deftest compat-format-prompt ()
  (should-equal "Prompt: " (format-prompt "Prompt" nil))
  (should-equal "Prompt: " (format-prompt "Prompt" ""))
  (should-equal "Prompt (default  ): " (format-prompt "Prompt" " "))
  (should-equal "Prompt (default 3): " (format-prompt "Prompt" 3))
  (should-equal "Prompt (default abc): " (format-prompt "Prompt" "abc"))
  (should-equal "Prompt (default abc def): " (format-prompt "Prompt" "abc def"))
  (should-equal "Prompt 10: " (format-prompt "Prompt %d" nil 10))
  (should-equal "Prompt \"abc\" (default 3): " (format-prompt "Prompt %S" 3 "abc")))

(ert-deftest compat-cXXXr ()
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

(ert-deftest compat-cXXXXr ()
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

(ert-deftest compat-compiled-function-p ()
  (should-not (compiled-function-p '(lambda (x) x)))
  (should (compiled-function-p (symbol-function 'assq)))
  (should (compiled-function-p (symbol-function 'identity))))

(ert-deftest compat-native-comp-available-p ()
  (should (memq (native-comp-available-p) '(nil t))))

(ert-deftest compat-subr-native-elisp-p ()
  (should-not (subr-native-elisp-p (symbol-function 'identity))))

(ert-deftest compat-subr-primitive-p ()
  (should (subr-primitive-p (symbol-function 'identity)))       ;function from fns.c
  (when (< emacs-major-version 28)
    (should-not (subr-primitive-p (symbol-function 'match-string)))) ;function from subr.el
  (should-not (subr-primitive-p (symbol-function 'defun)))        ;macro from subr.el
  (should-not (subr-primitive-p nil)))

(ert-deftest compat-mapcan ()
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

(ert-deftest compat-xor ()
  (should (equal (xor 'a nil) 'a))
  (should (equal (xor nil 'b) 'b))
  (should-not (xor nil nil))
  (should-not (xor 'a 'b)))

(ert-deftest compat-length= ()
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

(ert-deftest compat-length< ()
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

(ert-deftest compat-length> ()
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

(ert-deftest compat-ensure-list ()
  (should-not (ensure-list nil))           ;; empty list
  (should-equal '(1) (ensure-list '(1)))         ;; single element list
  (should-equal '(1 2 3) (ensure-list '(1 2 3))) ;; multiple element list
  (should-equal '(1) (ensure-list 1)))           ;; atom

(ert-deftest compat-proper-list-p ()
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

(ert-deftest compat-always ()
  (should-equal t (always))                      ;; no arguments
  (should-equal t (always 1))                    ;; single argument
  (should-equal t (always 1 2 3 4)))             ;; multiple arguments

(ert-deftest compat-file-backup-file-names ()
  (ert-with-temp-directory dir
    (let ((file (file-name-concat dir "file")) backups)
      (make-empty-file file)
      (should (file-exists-p file))
      (should-not (file-backup-file-names file))
      (push (concat file "~") backups)
      (make-empty-file (car backups))
      (should-equal backups (file-backup-file-names file))
      (push (concat file ".~1~") backups)
      (make-empty-file (car backups))
      (should-equal backups (sort (file-backup-file-names file) #'string<)))))

(ert-deftest compat-make-temp-file ()
  (let ((file (compat-call make-temp-file "compat-tests" nil nil "test-content")))
    (unwind-protect
        (with-temp-buffer
          (insert-file-contents file)
          (should-equal "test-content" (buffer-string)))
      (delete-file file))))

(ert-deftest compat-make-nearby-temp-file ()
  (let ((file1 (make-nearby-temp-file "compat-tests"))
        (file2 (make-nearby-temp-file "compat-tests" nil "suffix"))
        (dir (make-nearby-temp-file "compat-tests" t)))
    (unwind-protect
        (progn
          (should (string-suffix-p "suffix" file2))
          (should (file-regular-p file1))
          (should (file-regular-p file2))
          (should (file-directory-p dir))
          (should-equal (file-name-directory file1) temporary-file-directory)
          (should-equal (file-name-directory file2) temporary-file-directory)
          (should-equal (file-name-directory dir) temporary-file-directory))
      (delete-file file1)
      (delete-file file2)
      (delete-directory dir))
    ;; Tramp test (mock protocol)
    (let* ((default-directory "/mock::/")
           (file (make-nearby-temp-file "compat-tests")))
      (unwind-protect
          (should (string-match-p "\\`/mock:.*:/tmp/compat-tests" file))
        (delete-file file)))))

(ert-deftest compat-executable-find ()
  (should (member (executable-find "sh") '("/usr/bin/sh" "/bin/sh")))
  (should (member (executable-find "ls") '("/usr/bin/ls" "/bin/ls")))
  ;; Tramp test (mock protocol)
  (let ((default-directory "/mock::/"))
    (should (member (compat-call executable-find "sh" t) '("/usr/bin/sh" "/bin/sh")))
    (should (member (compat-call executable-find "ls" t) '("/usr/bin/ls" "/bin/ls")))))

(ert-deftest compat-exec-path ()
  (should-equal (exec-path) exec-path)
  ;; Tramp test (mock protocol)
  (let ((default-directory "/mock::/"))
    (should (member "/bin" (exec-path)))))

(ert-deftest compat-lisp-directory ()
  (should-equal lisp-directory
                (file-truename
                 (file-name-directory
                  (locate-file "subr" load-path (get-load-suffixes))))))

(ert-deftest compat-with-existing-directory ()
  (let ((dir (make-temp-name "/tmp/not-exist-")))
    (let ((default-directory dir))
      (should-not (file-exists-p default-directory)))
    (with-existing-directory
      (should-not (equal dir default-directory))
      (should (file-exists-p default-directory)))))

(ert-deftest compat-temporary-file-directory ()
  (should-equal (temporary-file-directory) temporary-file-directory)
  (let ((default-directory "/mnt"))
    (should-equal (temporary-file-directory) default-directory))
  ;; Tramp test (mock protocol)
  (let ((default-directory "/mock::/"))
    (should (string-match-p "\\`/mock:.*:/tmp/?\\'" (temporary-file-directory)))))

(ert-deftest compat-directory-files ()
  (should-not (compat-call directory-files "." nil nil nil 0))
  (should (list-of-strings-p (compat-call directory-files "." nil nil nil 1)))
  (should-equal 1 (length (compat-call directory-files "." nil nil nil 1)))
  (should-equal 2 (length (compat-call directory-files "." nil nil nil 2))))

(ert-deftest compat-directory-files-and-attributes ()
  (should-not (compat-call directory-files-and-attributes "." nil nil nil nil 0))
  (should (consp (car (compat-call directory-files-and-attributes "." nil nil nil nil 1))))
  (should-equal 1 (length (compat-call directory-files-and-attributes "." nil nil nil nil 1)))
  (should-equal 2 (length (compat-call directory-files-and-attributes "." nil nil nil nil 2))))

(ert-deftest compat-directory-name-p ()
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

(ert-deftest compat-directory-empty-p ()
  (ert-with-temp-directory dir
    (should (directory-empty-p dir))
    (make-empty-file (file-name-concat dir "file"))
    (should-not (directory-empty-p dir))
    (delete-file (file-name-concat dir "file"))
    (should (directory-empty-p dir))))

(ert-deftest compat-directory-abbrev-apply ()
  (let ((directory-abbrev-alist
         (list
          (cons (directory-abbrev-make-regexp "/long/path/to/foo") "foo:")
          (cons (directory-abbrev-make-regexp "/long/path/to/bar") "bar:"))))
    (should-equal (directory-abbrev-apply "/long/path/to/foo/file") "foo:file")
    (should-equal (directory-abbrev-apply "/long/path/to/bar/file") "bar:file")))

(ert-deftest compat-directory-abbrev-make-regexp ()
  (should-equal (directory-abbrev-make-regexp "/home/user/") "\\`/home/user/\\(/\\|\\'\\)"))

(ert-deftest compat-make-empty-file ()
  (ert-with-temp-directory dir
    (let ((file (file-name-concat dir "file")))
      (should-not (file-exists-p file))
      (make-empty-file file)
      (should (file-exists-p file))
      (should-equal 0 (file-attribute-size (file-attributes file))))))

(ert-deftest compat-mounted-file-systems ()
  (should-not (string-match-p mounted-file-systems "/etc/"))
  (should (string-match-p mounted-file-systems "/mnt/")))

(ert-deftest compat-make-lock-file-name ()
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

(ert-deftest compat-file-has-changed-p ()
  (ert-with-temp-file file
    (should (file-has-changed-p file))
    (should-not (file-has-changed-p file))
    (should-not (file-has-changed-p file))
    (should (file-has-changed-p file 'tag1))
    (should-not (file-has-changed-p file 'tag1))
    (should-not (file-has-changed-p file 'tag1))
    (with-temp-buffer
      (insert "changed")
      (write-region (point-min) (point-max) file))
    (should (file-has-changed-p file))
    (should-not (file-has-changed-p file))
    (should-not (file-has-changed-p file))
    (should (file-has-changed-p file 'tag1))
    (should-not (file-has-changed-p file 'tag1))
    (should-not (file-has-changed-p file 'tag1))
    (should (file-has-changed-p file 'tag2))
    (should-not (file-has-changed-p file 'tag2))
    (should-not (file-has-changed-p file 'tag2))))

(ert-deftest compat-file-attribute-getters ()
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

(ert-deftest compat-file-attribute-collect ()
  (let ((attrs '(t l u g a m s S m U i d)))
    (should-equal (file-attribute-collect attrs 'group-id 'user-id) '(g u))
    (should-equal (file-attribute-collect attrs 'size 'inode-number 'type) '(S i t))))

(ert-deftest compat-file-size-human-readable-iec ()
  (should-equal "1 KiB" (file-size-human-readable-iec 1024))
  (should-equal "2.1 MiB" (file-size-human-readable-iec 2223456)))

(ert-deftest compat-file-size-human-readable ()
  (should-equal "1000" (compat-call file-size-human-readable 1000))
  (should-equal "1k" (compat-call file-size-human-readable 1024))
  (should-equal "1M" (compat-call file-size-human-readable (* 1024 1024)))
  (should-equal "1G" (compat-call file-size-human-readable (expt 1024 3)))
  (should-equal "1T" (compat-call file-size-human-readable (expt 1024 4)))
  (should-equal "1k" (compat-call file-size-human-readable 1000 'si))
  (should-equal "1KiB" (compat-call file-size-human-readable 1024 'iec))
  (should-equal "1__KiB" (compat-call file-size-human-readable 1024 'iec "__"))
  (should-equal "1 KiB" (compat-call file-size-human-readable 1024 'iec " "))
  (should-equal "1KiA" (compat-call file-size-human-readable 1024 'iec nil "A"))
  (should-equal "1 KiA" (compat-call file-size-human-readable 1024 'iec " " "A"))
  (should-equal "1kA" (compat-call file-size-human-readable 1000 'si nil "A"))
  (should-equal "1 k" (compat-call file-size-human-readable 1000 'si " "))
  (should-equal "1 kA" (compat-call file-size-human-readable 1000 'si " " "A")))

(ert-deftest compat-with-file-modes ()
  (let ((old (default-file-modes)))
    (with-file-modes (1+ old)
      (should-equal (default-file-modes) (1+ old)))
    (should-equal (default-file-modes) old)))

(ert-deftest compat-file-modes-number-to-symbolic ()
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

(ert-deftest compat-file-local-name ()
  (should-equal "" (file-local-name ""))
  (should-equal "foo" (file-local-name "foo"))
  (should-equal "/bar/foo" (file-local-name "/bar/foo"))
  ;; NOTE: These tests fails prior to Emacs 26, because /ssh:foo was a valid
  ;; TRAMP path back then.
  (when (>= emacs-major-version 26)
    (should-equal "/ssh:foo" (file-local-name "/ssh:foo"))
    (should-equal "/ssh:/bar/foo" (file-local-name "/ssh:/bar/foo")))
  (should-equal "foo" (file-local-name "/ssh::foo"))
  (should-equal "/bar/foo" (file-local-name "/ssh::/bar/foo"))
  (should-equal ":foo" (file-local-name "/ssh:::foo"))
  (should-equal ":/bar/foo" (file-local-name "/ssh:::/bar/foo")))

(ert-deftest compat-file-name-quoted-p ()
  (should-not (compat-call file-name-quoted-p "" t)) ;; top argument
  (should (compat-call file-name-quoted-p "/:" t)) ;; top argument
  (should-not (file-name-quoted-p ""))
  (should (file-name-quoted-p "/:"))
  (should-not (file-name-quoted-p "//:"))
  (should (file-name-quoted-p "/::"))
  (should-not (file-name-quoted-p "/ssh::"))
  (should-not (file-name-quoted-p "/ssh::a"))
  (should (file-name-quoted-p "/ssh::/:a"))
  ;; NOTE: These tests fails prior to Emacs 26, because /ssh:foo was a valid
  ;; TRAMP path back then.
  (when (>= emacs-major-version 26)
    (should-not (file-name-quoted-p "/ssh:/:a"))))

(ert-deftest compat-file-name-unquote ()
  (should-equal "/" (compat-call file-name-unquote "/:" t)) ;; top argument
  (should-equal ":"(compat-call file-name-unquote "/::" t)) ;; top argument
  (should-equal "/" (file-name-unquote "/:/"))
  (should-equal "/" (file-name-unquote "/:"))
  (should-equal ":" (file-name-unquote  "/::")))

(ert-deftest compat-file-name-quote ()
  (should-equal "/:" (compat-call file-name-quote "" t)) ;; top argument
  (should-equal "/::"(compat-call file-name-quote  ":" t)) ;; top argument
  (should-equal "/:" (file-name-quote ""))
  (should-equal "/::"(file-name-quote  ":"))
  (should-equal "/:/" (file-name-quote "/"))
  (should-equal "/:" (file-name-quote "/:"))
  (should-equal "/:a" (file-name-quote "a"))
  (should-equal "/::a" (file-name-quote ":a"))
  (should-equal "/:/a" (file-name-quote "/a"))
  (should-equal "/:a" (file-name-quote "/:a"))
  (should-equal (concat "/ssh:" (system-name) ":/:a") (file-name-quote "/ssh::a")))

(ert-deftest compat-file-name-concat ()
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

(ert-deftest compat-file-name-parent-directory ()
  (should-equal (file-name-parent-directory "/foo/bar") "/foo/")
  (should-equal (file-name-parent-directory "/foo/") "/")
  (should-equal (file-name-parent-directory "foo/bar") "foo/")
  (should-equal (file-name-parent-directory "foo") "./"))

(ert-deftest compat-file-name-split ()
  (should-equal (file-name-split "foo/bar") '("foo" "bar"))
  (should-equal (file-name-split "/foo/bar") '("" "foo" "bar"))
  (should-equal (file-name-split "/foo/bar/zot") '("" "foo" "bar" "zot"))
  (should-equal (file-name-split "/foo/bar/") '("" "foo" "bar" ""))
  (should-equal (file-name-split "foo/bar/") '("foo" "bar" "")))

(ert-deftest compat-file-name-with-extension ()
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

(ert-deftest compat-flatten-tree ()
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

(ert-deftest compat-sort ()
  (should-equal (list 1 2 3) (sort (list 1 2 3) #'<))
  (should-equal (list 1 2 3) (sort (list 1 3 2) #'<))
  (should-equal (list 1 2 3) (sort (list 3 2 1) #'<))
  (should-equal (list 1 2 3) (compat-call sort (list 1 2 3) #'<))
  (should-equal (list 1 2 3) (compat-call sort (list 1 3 2) #'<))
  (should-equal (list 1 2 3) (compat-call sort (list 3 2 1) #'<))
  (should-equal [1 2 3] (compat-call sort (vector 1 2 3) #'<))
  (should-equal [1 2 3] (compat-call sort (vector 1 3 2) #'<))
  (should-equal [1 2 3] (compat-call sort (vector 3 2 1) #'<))
  ;; Test side effect
  (let* ((vec (vector 4 5 8 3 1 2 3 2 3 4))
         (sorted (compat-call sort vec #'>)))
    (should-equal sorted [8 5 4 4 3 3 3 2 2 1])
    (should-equal vec [8 5 4 4 3 3 3 2 2 1])))

(ert-deftest compat-replace-string-in-region ()
  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-equal (replace-string-in-region "foo" "new" (point-min) (point-max)) 2)
    (should-equal (buffer-string) "new bar zot newbar"))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-equal (replace-string-in-region "foo" "new" (point-min) 14) 1)
    (should-equal (buffer-string) "new bar zot foobar"))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-error (replace-string-in-region "foo" "new" (point-min) 30)))

  (with-temp-buffer
    (insert "Foo bar zot foobar")
    (should-equal (replace-string-in-region "Foo" "new" (point-min)) 1)
    (should-equal (buffer-string) "new bar zot foobar"))

  ;; There was a bug in the Emacs 28 implementation
  ;; Fixed in Emacs d8f392bccd46cdb238ec96964f220ffb9d81cc44
  (unless (= emacs-major-version 28)
    (with-temp-buffer
      (insert "foo bar baz")
      (should-equal (replace-string-in-region "ba" "quux corge grault" (point-min)) 2)
      (should-equal (buffer-string)
                     "foo quux corge graultr quux corge graultz"))

    (with-temp-buffer
      (insert "foo bar bar")
      (should-equal (replace-string-in-region " bar" "" (point-min) 8) 1)
      (should-equal (buffer-string) "foo bar"))))

(ert-deftest compat-replace-regexp-in-region ()
  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-equal (replace-regexp-in-region "fo+" "new" (point-min) (point-max)) 2)
    (should-equal (buffer-string) "new bar zot newbar"))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-equal (replace-regexp-in-region "fo+" "new" (point-min) 14) 1)
    (should-equal (buffer-string) "new bar zot foobar"))

  (with-temp-buffer
    (insert "foo bar zot foobar")
    (should-error (replace-regexp-in-region "fo+" "new" (point-min) 30)))

  (with-temp-buffer
    (insert "Foo bar zot foobar")
    (should-equal (replace-regexp-in-region "Fo+" "new" (point-min)) 1)
    (should-equal (buffer-string) "new bar zot foobar"))

  ;; There was a bug in the Emacs 28 implementation
  ;; Fixed in Emacs d8f392bccd46cdb238ec96964f220ffb9d81cc44
  (unless (= emacs-major-version 28)
    (with-temp-buffer
      (insert "foo bar baz")
      (should-equal (replace-regexp-in-region "ba." "quux corge grault" (point-min)) 2)
      (should-equal (buffer-string) "foo quux corge grault quux corge grault"))

    (with-temp-buffer
      (insert "foo bar bar")
      (should-equal (replace-regexp-in-region " bar" "" (point-min) 8) 1)
      (should-equal (buffer-string) "foo bar"))))

(ert-deftest compat-char-uppercase-p ()
  (dolist (c (list ?R ?S ?Ω ?Ψ))
    (should (char-uppercase-p c)))
  (dolist (c (list ?a ?b ?α ?β))
    (should-not (char-uppercase-p c))))

(ert-deftest compat-string-split ()
  (should-equal '("a" "b" "c") (split-string "a b c"))
  (should-equal '("a" "b" "c") (string-split "a b c")))

(ert-deftest compat-string-equal-ignore-case ()
  (should (string-equal-ignore-case "abc" "abc"))
  (should (string-equal-ignore-case "abc" "ABC"))
  (should (string-equal-ignore-case "abc" "abC"))
  (should-not (string-equal-ignore-case "abc" "abCD"))
  (should (string-equal-ignore-case "S" "s")))

(ert-deftest compat-string-greaterp ()
  (should (string-greaterp "b" "a"))
  (should-not (string-greaterp "a" "b"))
  (should (string-greaterp "aaab" "aaaa"))
  (should-not (string-greaterp "aaaa" "aaab")))

(ert-deftest compat-string-clean-whitespace ()
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

(ert-deftest compat-string-fill ()
  (should-equal "a a a a a" (string-fill "a a a a a" 9))
  (should-equal "a a a a a" (string-fill "a a a a a" 10))
  (should-equal "a a a a\na" (string-fill "a a a a a" 8))
  (should-equal "a a a a\na" (string-fill "a  a  a  a  a" 8))
  (should-equal "a a\na a\na" (string-fill "a a a a a" 4))
  (should-equal "a\na\na\na\na" (string-fill "a a a a a" 2))
  (should-equal "a\na\na\na\na" (string-fill "a a a a a" 1)))

(ert-deftest compat-string-lines ()
  (should-equal '("a" "b" "c") (string-lines "a\nb\nc"))
  (should-equal '("a" "b" "c") (string-lines "a\nb\nc\n" t))
  (should-equal '("a" "b" "c") (string-lines "a\nb\n\nc\n" t))
  (should-equal '("abc" "bcd" "cde") (string-lines "abc\nbcd\ncde"))
  (should-equal '(" abc" " bcd " "cde ") (string-lines " abc\n bcd \ncde "))

  ;; NOTE: Behavior for trailing newline was different on Emacs 28
  (compat-tests--if (= emacs-major-version 28)
      (should-equal '("a" "b" "c" "") (string-lines "a\nb\nc\n"))
    (should-equal '("a" "b" "c") (string-lines "a\nb\nc\n"))
    (should-equal '("a\n" "\n" "b\n" "c\n") (string-lines "a\n\nb\nc\n" nil t))
    (should-equal '("a\n" "b\n" "c\n") (string-lines "a\n\nb\nc\n" t t))
    (should-equal '("a\n" "b\n" "c\n") (string-lines "a\nb\nc\n" nil t)))

  ;; Compatibility function provides the Emacs 29 behavior regarding trailing newlines
  (should-equal '("a" "b" "c") (compat-call string-lines "a\nb\nc\n"))
  (should-equal '("a\n" "\n" "b\n" "c\n") (compat-call string-lines "a\n\nb\nc\n" nil t))
  (should-equal '("a\n" "b\n" "c\n") (compat-call string-lines "a\n\nb\nc\n" t t))
  (should-equal '("a\n" "b\n" "c\n") (compat-call string-lines "a\nb\nc\n" nil t)))

(ert-deftest compat-string-pad ()
  (should-equal "a   " (string-pad "a" 4))
  (should-equal "aaaa" (string-pad "aaaa" 4))
  (should-equal "aaaaaa" (string-pad "aaaaaa" 4))
  (should-equal "a..." (string-pad "a" 4 ?.))
  (should-equal "   a" (string-pad "a" 4 nil t))
  (should-equal "...a" (string-pad "a" 4 ?. t)))

(ert-deftest compat-string-chop-newline ()
  (should-equal "" (string-chop-newline ""))
  (should-equal "" (string-chop-newline "\n"))
  (should-equal "aaa" (string-chop-newline "aaa"))
  (should-equal "aaa" (string-chop-newline "aaa\n"))
  (should-equal "aaa\n" (string-chop-newline "aaa\n\n")))

(ert-deftest compat-string-distance ()
  (should-equal 3 (string-distance "kitten" "sitting"))    ;from wikipedia
  ;; In Emacs 27, `string-distance' had a bug when comparing two empty
  ;; strings. This was fixed in the following commit:
  ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=c44190c
  (when (/= emacs-major-version 27)
    (should-equal 0 (string-distance "" "")))
  (should-equal 0 (string-distance "a" "a"))
  (should-equal 1 (string-distance "" "a"))
  (should-equal 1 (string-distance "b" "a"))
  (should-equal 2 (string-distance "aa" "bb"))
  (should-equal 2 (string-distance "aa" "bba"))
  (should-equal 2 (string-distance "aaa" "bba"))
  (should-equal 3 (string-distance "a" "あ" t))             ;byte example
  (should-equal 1 (string-distance "a" "あ")))

(ert-deftest compat-string-width ()
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

(ert-deftest compat-string-trim-left ()
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

(ert-deftest compat-string-trim-right ()
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

(ert-deftest compat-string-trim ()
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

(ert-deftest compat-string-search ()
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
  (should-equal 5 (string-search (make-string 2 130)
                                 ;; Per (concat "helló" (make-string 5 130 t) "bár")
                                 "hellóbár"))
  (should-equal 5 (string-search (make-string 2 127)
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
  (should-error (string-search "a" "abc" most-positive-fixnum) :type '(args-out-of-range most-positive-fixnum))
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
  (should-not (string-search (compat-tests--string-to-multibyte "\303\270")
                             (compat-tests--string-to-multibyte "\370")))
  (should-not (string-search "\370" "\303\270"))
  (should-not (string-search (compat-tests--string-to-multibyte "\370") "\303\270"))
  (should-not (string-search "\370" (compat-tests--string-to-multibyte "\303\270")))
  (should-not (string-search (compat-tests--string-to-multibyte "\370")
                             (compat-tests--string-to-multibyte "\303\270")))
  (should-equal 3 (string-search "\303\270" "foo\303\270"))
  (when (version<= "27" emacs-version)
    ;; FIXME The commit a1f76adfb03c23bb4242928e8efe6193c301f0c1 in
    ;; emacs.git fixes the behaviour of regular expressions matching
    ;; raw bytes.  The compatibility functions should updated to
    ;; backport this behaviour.
    (should-equal 2 (string-search (compat-tests--string-to-multibyte "\377") "ab\377c"))
    (should-equal 2 (string-search (compat-tests--string-to-multibyte "o\303\270")
                                   "foo\303\270")))
  ;; Ensure that `match-data' is preserved by `string-search'
  (string-match (rx (* "a") (group (* "b")) (* "a")) "abba")
  (should-equal '(0 4 1 3) (match-data))
  (should (string-search "foo" "foobar"))
  (should-equal '(0 4 1 3) (match-data)))

(ert-deftest compat-string-replace ()
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
  (should-error (string-replace "" "x" "abc") :type 'wrong-length-argument))

(ert-deftest compat-dlet ()
  (should-not (boundp 'compat-tests--dlet1))
  (should-not (boundp 'compat-tests--dlet2))
  (dlet ((compat-tests--dlet1 1)
         (compat-tests--dlet2 2))
    (should-equal (symbol-value 'compat-tests--dlet1) 1)
    (should-equal (symbol-value 'compat-tests--dlet2) 2))
  (should-not (boundp 'compat-tests--dlet1))
  (should-not (boundp 'compat-tests--dlet2)))

(ert-deftest compat-while-let ()
  (let ((list '(1 2 3 4)) rev)
    (while-let ((x (pop list)))
      (push x rev))
    (should-equal '(4 3 2 1) rev))
  (let ((first '(1 2 3 4)) (second '(a b c)) zipped)
    (while-let ((x (pop first)) (y (pop second)))
      (push (cons x y) zipped))
    (should-equal '((3 . c) (2 . b) (1 . a)) zipped)))

(ert-deftest compat-when-let* ()
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

(ert-deftest compat-if-let* ()
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

(ert-deftest compat-when-let ()
  ;; FIXME Broken on Emacs 25
  (compat-tests--if (= emacs-major-version 25)
      (should-equal "second"
                    (when-let
                        ((x 3)
                         (y 2)
                         (z (+ x y))
                         ;; ((= z 5)) ;; FIXME Broken on Emacs 25
                         (true t))
                      "first" "second"))
    (should-equal "second"
                  (when-let
                      ((x 3)
                       (y 2)
                       (z (+ x y))
                       ((= z 5))
                       (true t))
                    "first" "second"))
    (should-equal "then" (when-let (((= 5 5))) "then"))
    (should-not (when-let (((= 5 6))) t)))
  (should-equal "last"
                (when-let (e (memq 0 '(1 2 3 0 5 6)))
                  "first" "last"))
  (should-equal "last" (when-let ((e (memq 0 '(1 2 3 0 5 6))))
                         "first" "last"))
  (should-not (when-let ((e (memq 0 '(1 2 3 5 6)))
                               (d (memq 0 '(1 2 3 0 5 6))))
                  "first" "last")))

(ert-deftest compat-if-let ()
  ;; FIXME Broken on Emacs 25
  (compat-tests--if (= emacs-major-version 25)
      (should-equal "then"
                    (if-let
                        ((x 3)
                         (y 2)
                         (z (+ x y))
                         ;; ((= z 5)) ;; FIXME Broken on Emacs 25
                         (true t))
                        "then" "else"))
    (should-equal "then"
                  (if-let
                      ((x 3)
                       (y 2)
                       (z (+ x y))
                       ((= z 5))
                       (true t))
                      "then" "else"))
    (should-equal "else" (if-let (((= 5 6))) "then" "else"))
    (should-not (if-let (((= 5 6))) t nil)))
  (should (if-let (e (memq 0 '(1 2 3 0 5 6)))
              e))
  (should (if-let ((e (memq 0 '(1 2 3 0 5 6))))
              e))
  (should-not (if-let ((e (memq 0 '(1 2 3 5 6)))
                               (d (memq 0 '(1 2 3 0 5 6))))
                  t))
  (should-not (if-let ((d (memq 0 '(1 2 3 0 5 6)))
                               (e (memq 0 '(1 2 3 5 6))))
                  t)))

(ert-deftest compat-and-let* ()
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

(ert-deftest compat-named-let ()
  (should-equal (named-let l ((i 0)) (if (= i 8) i (l (1+ i))))
                8)
  (should-equal (named-let l ((i 0)) (if (= i 100000) i (l (1+ i))))
                100000)
  (should-equal (named-let l ((i 0))
                  (cond
                   ((= i 100000) i)
                   ((= (mod i 2) 0)
                    (l (+ i 2)))
                   ((l (+ i 3)))))
                100000)
  (should-equal (named-let l ((i 0) (x 1)) (if (= i 8) x (l (1+ i) (* x 2))))
                (expt 2 8))
  (should-equal (named-let lop ((x 1))
                  (if (> x 0)
                      (condition-case nil
                          (lop (1- x))
                        (arith-error 'ok))
                    (/ 1 x)))
                'ok)
  (should-equal (named-let lop ((n 10000))
                  (if (> n 0)
                      (condition-case nil
                          (/ n 0)
                        (arith-error (lop (1- n))))
                    'ok))
                'ok)
  (should-equal (named-let lop ((x nil))
                  (cond (x)
                        (t 'ok)))
                'ok)
  (should-equal (named-let lop ((x 100000))
                  (cond ((= x 0) 'ok)
                        ((lop (1- x)))))
                'ok)
  (should-equal (named-let lop ((x 100000))
                  (cond
                   ((= x -1) nil)
                   ((= x 0) 'ok)
                   ((lop -1))
                   ((lop (1- x)))))
                'ok)
  (should-equal (named-let lop ((x 10000))
                  (cond ((= x 0) 'ok)
                        ((and t (lop (1- x))))))
                'ok)
  (should-equal (let ((b t))
                  (named-let lop ((i 0))
                    (cond ((null i) nil) ((= i 10000) 'ok)
                          ((lop (and (setq b (not b)) (1+ i))))
                          ((lop (and (setq b (not b)) (1+ i)))))))
                'ok))

(ert-deftest compat-alist-get ()
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
  (should-equal 'd (compat-call alist-get "x" '(("a" . 1) ("b" . 2) ("c" . 3)) 'd nil #'equal))
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

(ert-deftest compat-alist-get-gv ()
  (let ((alist (list (cons 1 "one")
                     (cons 2 "two")
                     (cons 3 "three"))))
    (setf (alist-get 1 alist) "eins")
    (should-equal (alist-get 1 alist) "eins")
    (setf (alist-get 2 alist nil 'remove) nil)
    (should-equal alist '((1 . "eins") (3 . "three"))))
  (let ((alist (list (cons 1 "one")
                     (cons 2 "two")
                     (cons 3 "three"))))
    (setf (compat-call alist-get 1 alist) "eins")
    (should-equal (compat-call alist-get 1 alist) "eins")
    (setf (compat-call alist-get 2 alist nil 'remove) nil)
    (should-equal alist '((1 . "eins") (3 . "three"))))
  (let ((alist (list (cons "one" 1)
                     (cons "two" 2)
                     (cons "three" 3))))
    (setf (compat-call alist-get "one" alist nil nil #'string=) "eins")
    (should-equal (compat-call alist-get "one" alist nil nil #'string=) "eins")
    (should-equal alist '(("one" . "eins") ("two" . 2) ("three" . 3)))
    (setf (compat-call alist-get "two" alist nil 'remove #'string=) nil)
    (should-equal alist '(("one" . "eins") ("three" . 3)))))

(ert-deftest compat-plist-get-gv ()
  (let ((plist '(1 "one" 2 "two" 3 "three")))
    (setf (plist-get plist 1) "eins")
    (should-equal (plist-get plist 1) "eins")
    (setf (plist-get plist 2) nil)
    (should-equal plist '(1 "eins" 2 nil 3 "three")))
  (let ((plist '(1 "one" 2 "two" 3 "three")))
    (setf (compat-call plist-get plist 1) "eins")
    (should-equal (compat-call plist-get plist 1) "eins")
    (setf (compat-call plist-get plist 2) nil)
    (should-equal plist '(1 "eins" 2 nil 3 "three")))
  (let ((plist '("one" 1 "two" 2 "three" 3)))
    (setf (compat-call plist-get plist "one" #'string=) "eins")
    (should-equal (compat-call plist-get plist "one" #'string=) "eins")
    (should-equal plist '("one" "eins" "two" 2 "three" 3))))

(ert-deftest compat-prop-match ()
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

(ert-deftest compat-text-property-search-forward ()
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

(ert-deftest compat-text-property-search-backward ()
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

(ert-deftest compat-color-dark-p ()
  (should (color-dark-p '(0 0 0)))
  (should (color-dark-p '(0.5 0.5 0.5)))
  (should-not (color-dark-p '(0.5 0.7 0.5)))
  (should-not (color-dark-p '(1 1 1 ))))

(ert-deftest compat-color-values-from-color-spec ()
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

(ert-deftest compat-lookup-key ()
  (let ((a-map (make-sparse-keymap))
        (b-map (make-sparse-keymap)))
    (define-key a-map "x" 'foo)
    (define-key b-map "x" 'bar)
    (should-equal 'foo (compat-call lookup-key a-map "x"))
    (should-equal 'bar (compat-call lookup-key b-map "x"))
    (should-equal 'foo (compat-call lookup-key (list a-map b-map) "x"))
    (should-equal 'bar (compat-call lookup-key (list b-map a-map) "x"))))

;; We need an indirection since `macroexp-file-name' is a function and not a
;; macro. `macroexp-file-name' is not a function since it is used mostly in a
;; macro context.
(defmacro compat-tests--filename ()
  (macroexp-file-name))

(ert-deftest compat-macroexp-file-name ()
  (should-equal (file-name-nondirectory (compat-tests--filename)) "compat-tests.el"))

(ert-deftest compat-macroexp-warn-and-return ()
  (should-equal (macroexp-warn-and-return "test warning" '(some form)) '(some form)))

(ert-deftest compat-macroexp-parse-body ()
  (should-equal '(((declare test)) . (a b c))
                (macroexp-parse-body '((declare test) a b c)))
  (should-equal '(((interactive)) . (a b c))
                (macroexp-parse-body '((interactive) a b c)))
  (should-equal '(((interactive) (cl-declare)) . (a b c))
                (macroexp-parse-body '((interactive) (cl-declare) a b c))))

(ert-deftest compat-macroexp-quote ()
  (should-equal nil (macroexp-quote nil))
  (should-equal t (macroexp-quote t))
  (should-equal :key (macroexp-quote :key))
  (should-equal "str" (macroexp-quote "str"))
  (should-equal ''sym (macroexp-quote 'sym))
  (should-equal ''(1 2 3) (macroexp-quote '(1 2 3))))

(ert-deftest compat-macroexpand-1 ()
  (should-equal '(if a b c) (macroexpand-1 '(if a b c)))
  (should-equal '(if a (progn b)) (macroexpand-1 '(when a b)))
  (should-equal '(if a (progn (unless b c))) (macroexpand-1 '(when a (unless b c)))))

;; NOTE: `with-suppressed-warnings' does not work inside of `ert-deftest'?!
(defun compat-tests--with-suppressed-warnings ()
  (with-suppressed-warnings ((interactive-only goto-line)
                             (obsolete encode-time-value))
    (encode-time-value 1 2 3 4 0)
    (goto-line 10)))
(ert-deftest compat-with-suppressed-warnings () #'compat-tests--with-suppressed-warnings)

(ert-deftest compat-time-equal-p ()
  (should (time-equal-p nil nil))

  ;; FIXME: Testing these values can be tricky, because the timestamp
  ;; might change between evaluating (current-time) and evaluating
  ;; `time-equal-p', especially in the interpreted compatibility
  ;; version.

  ;; (should (time-equal-p (current-time) nil))
  ;; (should (time-equal-p nil (current-time)))

  (should (time-equal-p '(1 2 3 4) '(1 2 3 4)))
  (should-not (time-equal-p '(1 2 3 4) '(1 2 3 5)))
  (should-not (time-equal-p '(1 2 3 5) '(1 2 3 4)))
  (should-not (time-equal-p '(1 2 3 4) '(1 2 4 4)))
  (should-not (time-equal-p '(1 2 4 4) '(1 2 3 4)))
  (should-not (time-equal-p '(1 2 3 4) '(1 3 3 4)))
  (should-not (time-equal-p '(1 3 3 4) '(1 2 3 4)))
  (should-not (time-equal-p '(1 2 3 4) '(2 2 3 4)))
  (should-not (time-equal-p '(2 2 3 4) '(1 2 3 4)))

  ;; TODO fix broken tests
  ;; (should (time-equal-p 0 (time-since nil)))
  ;; (should (time-equal-p (days-to-time 0) '(0 0)))
  ;; (should (time-equal-p (days-to-time 1) '(1 20864)))
  ;; (should (time-equal-p (days-to-time 999) '(1317 2688)))
  ;; (should (time-equal-p (days-to-time 0.0) '(0 0 0 0)))
  ;; (should (time-equal-p (days-to-time 0.5) '(0 43200 0 0)))
  ;; (should (time-equal-p (days-to-time 1.0) '(1 20864 0 0)))
  ;; (should (time-equal-p (days-to-time 999.0) '(1317 2688 0 0)))
  )

(ert-deftest compat-decoded-time ()
  (let ((time (list 'second 'minute 'hour 'day 'month 'year 'weekday 'dst 'zone)))
    (should-equal (decoded-time-second time) 'second)
    (should-equal (decoded-time-minute time) 'minute)
    (should-equal (decoded-time-hour time) 'hour)
    (should-equal (decoded-time-day time) 'day)
    (should-equal (decoded-time-month time) 'month)
    (should-equal (decoded-time-year time) 'year)
    (should-equal (decoded-time-weekday time) 'weekday)
    (should-equal (decoded-time-dst time) 'dst)
    (should-equal (decoded-time-zone time) 'zone)
    (setf (decoded-time-second time) 'SECOND)
    (setf (decoded-time-minute time) 'MINUTE)
    (setf (decoded-time-hour time) 'HOUR)
    (setf (decoded-time-day time) 'DAY)
    (setf (decoded-time-month time) 'MONTH)
    (setf (decoded-time-year time) 'YEAR)
    (setf (decoded-time-weekday time) 'WEEKDAY)
    (setf (decoded-time-dst time) 'DST)
    (setf (decoded-time-zone time) 'ZONE)
    (should-equal (decoded-time-second time) 'SECOND)
    (should-equal (decoded-time-minute time) 'MINUTE)
    (should-equal (decoded-time-hour time) 'HOUR)
    (should-equal (decoded-time-day time) 'DAY)
    (should-equal (decoded-time-month time) 'MONTH)
    (should-equal (decoded-time-year time) 'YEAR)
    (should-equal (decoded-time-weekday time) 'WEEKDAY)
    (should-equal (decoded-time-dst time) 'DST)
    (should-equal (decoded-time-zone time) 'ZONE)))

(ert-deftest compat-decoded-time-period ()
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

(ert-deftest compat-make-decoded-time ()
  (should-equal '(s m h d M Y nil D Z)
                (make-decoded-time :second 's :minute 'm :hour 'h
                                   :day 'd :month 'M :year 'Y
                                   :dst 'D :zone 'Z)))

(ert-deftest compat-date-days-in-month ()
  (should-equal 31 (date-days-in-month 2020 1))
  (should-equal 30 (date-days-in-month 2020 4))
  (should-equal 29 (date-days-in-month 2020 2))
  (should-equal 28 (date-days-in-month 2021 2)))

(ert-deftest compat-date-ordinal-to-time ()
  (should-equal '(nil nil nil 9 4 2020 nil nil nil) (date-ordinal-to-time 2020 100))
  (should-equal '(nil nil nil 19 7 2021 nil nil nil) (date-ordinal-to-time 2021 200)))

(ert-deftest compat-regexp-opt ()
  ;; Ensure `regexp-opt' doesn't change the existing
  ;; behaviour:
  (should-equal "[abc]" (compat-call regexp-opt '("a" "b" "c")))
  (should-equal "\\(?:abc\\|def\\|ghe\\)" (compat-call regexp-opt '("abc" "def" "ghe")))
  (should-equal "\\<\\([abc]\\)\\>" (compat-call regexp-opt '("a" "b" "c") 'words))
  ;; Test empty list:
  (should-equal "\\(?:\\`a\\`\\)" (compat-call regexp-opt '()))
  (should-equal "\\<\\(\\`a\\`\\)\\>" (compat-call regexp-opt '() 'words)))

(ert-deftest compat-regexp-unmatchable ()
  (dolist (str '(""                     ;empty string
                 "a"                    ;simple string
                 "aaa"                  ;longer string
                 ))
    (should-not (string-match-p regexp-unmatchable str))))

(ert-deftest compat-use-region ()
  (with-temp-buffer
    (insert "abc\ndef\n")
    (set-mark 2)
    (goto-char 7)
    (transient-mark-mode)
    (should (use-region-p))
    (should-equal 2 (use-region-beginning))
    (should-equal 7 (use-region-end))))

(ert-deftest compat-region-bounds ()
  (should-error (region-bounds))
  ;; FIXME: On Emacs 24 `region-bounds' always returns a continuous region.
  (when (> emacs-major-version 24)
    (let ((region-extract-function #'ignore))
      (should-not (region-bounds)))
    (let ((region-extract-function (lambda (_) '((2 . 3) (6 . 7)))))
      (should-equal (region-bounds) '((2 . 3) (6 . 7)))))
  (with-temp-buffer
    (insert "abc\ndef\n")
    (set-mark 2)
    (goto-char 7)
    (should-equal (region-bounds) '((2 . 7)))))

(ert-deftest compat-region-noncontiguous-p ()
  (when (> emacs-major-version 24)
    (let ((region-extract-function (lambda (_) '((2 . 3) (6 . 7)))))
      (should (region-noncontiguous-p))))
  (with-temp-buffer
    (insert "abc\ndef\n")
    (set-mark 2)
    (goto-char 7)
    (transient-mark-mode)
    (should-not (region-noncontiguous-p))
    (should-not (use-region-noncontiguous-p))
    (should (use-region-p))
    ;; FIXME: On Emacs 24 `region-bounds' always returns a continuous region.
    (when (> emacs-major-version 24)
      (let ((region-extract-function (lambda (_) '((2 . 3) (6 . 7)))))
        (should (region-noncontiguous-p))
        (should (use-region-noncontiguous-p))))))

(ert-deftest compat-get-scratch-buffer-create ()
  (should-equal "*scratch*" (buffer-name (get-scratch-buffer-create)))
  (should-equal initial-major-mode
                (buffer-local-value 'major-mode (get-scratch-buffer-create))))

(ert-deftest compat-ring-resize ()
  (let ((ring (make-ring 3)))
    (ring-insert ring 1)
    (ring-insert ring 2)
    (ring-insert ring 3)
    (ring-resize ring 5)
    (should-equal (ring-size ring) 5)
    (should-equal (ring-elements ring) '(3 2 1)))
  (let ((ring (make-ring 3)))
    (ring-resize ring 5)
    (should-equal (ring-size ring) 5)
    (should-equal (ring-elements ring) '()))
  (let ((ring (make-ring 3)))
    (ring-insert ring 1)
    (ring-insert ring 2)
    (ring-insert ring 3)
    (ring-insert ring 4)
    (ring-insert ring 5)
    (ring-resize ring 5)
    (should-equal (ring-size ring) 5)
    (should-equal (ring-elements ring) '(5 4 3)))
  (let ((ring (make-ring 5)))
    (ring-insert ring 1)
    (ring-insert ring 2)
    (ring-insert ring 3)
    (ring-insert ring 4)
    (ring-insert ring 5)
    (ring-resize ring 3)
    (should-equal (ring-size ring) 3)
    (should-equal (ring-elements ring) '(5 4 3))))

(ert-deftest compat-save-mark-and-excursion ()
  (with-temp-buffer
    (insert "a\nb\nc")
    (goto-char 1)
    (set-mark 2)
    (should-equal (point) 1)
    (should-equal (mark) 2)
    (save-mark-and-excursion
      (goto-char 3)
      (set-mark 4)
      (should-equal (point) 3)
      (should-equal (mark) 4))
    (should-equal (point) 1)
    (should-equal (mark) 2)))

(ert-deftest compat-text-quoting-style ()
  (should (text-quoting-style))
  (let ((text-quoting-style t))
    (should-equal 'curve (text-quoting-style)))
  (let ((text-quoting-style 'foo))
    (should-equal 'curve (text-quoting-style)))
  (let ((text-quoting-style 'grave))
    (should-equal 'grave (text-quoting-style))))

(ert-deftest compat-substitute-quotes ()
  (let ((text-quoting-style 'curve))
    (should-equal (substitute-quotes "quotes ‘like this’") "quotes ‘like this’")
    (should-equal (substitute-quotes "`x'") "‘x’")
    (should-equal (substitute-quotes "`") "‘")
    (should-equal (substitute-quotes "'") "’")
    (should-equal (substitute-quotes "\\`") "\\‘"))
  (let ((text-quoting-style 'straight))
    (should-equal (substitute-quotes "quotes `like this'") "quotes 'like this'")
    (should-equal (substitute-quotes "`x'") "'x'")
    (should-equal (substitute-quotes "`") "'")
    (should-equal (substitute-quotes "'") "'")
    (should-equal (substitute-quotes "\\`") "\\'"))
  (let ((text-quoting-style 'grave))
    (should-equal (substitute-quotes "quotes `like this'") "quotes `like this'")
    (should-equal (substitute-quotes "`x'") "`x'")
    (should-equal (substitute-quotes "`") "`")
    (should-equal (substitute-quotes "'") "'")
    (should-equal (substitute-quotes "\\`") "\\`")))

(ert-deftest compat-readablep ()
  (should (readablep "foo"))
  (should (readablep '("foo" 1 2.3 (a . b) [x y z] :key)))
  (should-not (readablep (list (make-marker))))
  (should-not (readablep (make-marker))))

(ert-deftest compat-count-sentences ()
  (with-temp-buffer
    (insert "First sentence.  Second sentence.  Third sentence.  Fourth sentence.")
    (should-equal 4 (count-sentences (point-min) (point-max)))
    (should-equal 2 (count-sentences 16 50))))

(ert-deftest compat-major-mode-suspend ()
  (with-temp-buffer
    (should (local-variable-if-set-p 'major-mode--suspended))
    (should (get 'major-mode--suspended 'permanent-local))
    (text-mode)
    (should sentence-end-double-space)
    (setq-local sentence-end-double-space nil)
    (major-mode-suspend)
    (should-equal major-mode--suspended #'text-mode)
    (should sentence-end-double-space)
    (prog-mode)
    (should-equal major-mode #'prog-mode)
    (major-mode-restore)
    (should-not major-mode--suspended)
    (should sentence-end-double-space)
    (should-equal major-mode #'text-mode)))

(ert-deftest compat-with-delayed-message ()
  ;; No real test, since the backported function never displays a message.
  (should-equal 'result (with-delayed-message (1 "timeout") 'result))
  (should-equal 'result (funcall-with-delayed-message
                         1 "timeout" (lambda () 'result))))

(ert-deftest compat-set-transient-map ()
  (let (overriding-terminal-local-map)
    ;; TODO Implement a proper test.  Interactive features like
    ;; `set-transient-map' are hard to test and Emacs itself is lacking tests.
    ;; For now only test the calling convention here.
    (set-transient-map (define-keymap "x" #'ignore))
    (compat-call set-transient-map (define-keymap "x" #'ignore))
    (compat-call set-transient-map (define-keymap "x" #'ignore) nil nil "msg" 1)))

(ert-deftest compat-ert-with-temp-file ()
  (ert-with-temp-file file
    (should-not (directory-name-p file))
    (should (file-readable-p file))
    (should (file-writable-p file)))
  (ert-with-temp-file dir :directory t
    (should (directory-name-p dir))
    (should (file-directory-p dir)))
  (ert-with-temp-file file :buffer buffer
    (should (equal (current-buffer) buffer))
    (should-equal buffer-file-name file)
    (should-not (directory-name-p file))
    (should (file-readable-p file))
    (should (file-writable-p file))))

(ert-deftest compat-ert-with-temp-directory ()
  (ert-with-temp-directory dir
    (should (directory-name-p dir))
    (should (file-directory-p dir))))

(defmacro compat-tests--with-gensyms ()
  (cl-with-gensyms (x y)
    `(let ((,x 1) (,y 2)) (+ ,x ,y))))

(ert-deftest compat-cl-with-gensyms ()
  (should-equal 3 (compat-tests--with-gensyms)))

(defmacro compat-tests--once-only (x)
  (cl-once-only (x)
    `(cons ,x ,x)))

(ert-deftest compat-cl-once-only ()
  (let ((x 0))
    (should-equal (cons 1 1) (compat-tests--once-only (cl-incf x)))
    (should-equal 1 x)))

(ert-deftest compat-cl-constantly ()
  (should-equal (mapcar (cl-constantly 3) '(a b c d))
                '(3 3 3 3)))

(ert-deftest compat-process-lines-ignore-status ()
  (should-equal '("line1" "line2" "")
                (process-lines-ignore-status "echo" "line1\nline2\n")))

(ert-deftest compat-process-lines-handling-status ()
  (let (status)
    (should-equal '("line1" "line2")
                  (process-lines-handling-status
                   "echo" (lambda (s) (setq status s)) "line1\nline2"))
    (should-equal status 0)
    (should-not (process-lines-handling-status "false" (lambda (s) (setq status s))))
    (should-equal status 1)
    (should-error (process-lines-handling-status "false" nil))))

(ert-deftest compat-seq ()
  (should-equal 3 (seq-length '(a b c)))
  (should-equal 3 (seq-length [a b c])))

(ert-deftest compat-widget-natnum ()
  (with-temp-buffer
    (should-error (widget-create 'compat--not-existing))
    (should-equal (take 3 (widget-create 'natnum)) '(natnum :value "0"))))

(ert-deftest compat-widget-key ()
  (with-temp-buffer
    (should-equal (take 3 (widget-create 'key)) '(key :value ""))))

(provide 'compat-tests)
;;; compat-tests.el ends here
