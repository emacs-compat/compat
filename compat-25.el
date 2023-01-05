;;; compat-25.el --- Compatibility Layer for Emacs 25.1  -*- lexical-binding: t; -*-

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

;; Find here the functionality added in Emacs 25.1, needed by older
;; versions.

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
(compat-declare-version "25.1")

;;;; Defined in alloc.c

(compat-defun bool-vector (&rest objects) ;; <OK>
  "Return a new bool-vector with specified arguments as elements.
Allows any number of arguments, including zero.
usage: (bool-vector &rest OBJECTS)"
  (let ((vec (make-bool-vector (length objects) nil))
        (i 0))
    (while objects
      (when (car objects)
        (aset vec i t))
      (setq objects (cdr objects)
            i (1+ i)))
    vec))

;;;; Defined in fns.c

(compat-defun sort (seq predicate) ;; <OK>
  "Extend `sort' to sort SEQ as a vector."
  :explicit t
  (cond
   ((listp seq)
    (sort seq predicate))
   ((vectorp seq)
    (let ((cseq (sort (append seq nil) predicate)))
      (dotimes (i (length cseq))
        (setf (aref seq i) (nth i cseq)))
      (apply #'vector cseq)))
   ((signal 'wrong-type-argument 'list-or-vector-p))))

;;;; Defined in editfns.c

(compat-defalias format-message format) ;; <OK>

;;;; Defined in fileio.c

(compat-defun directory-name-p (name) ;; <OK>
  "Return non-nil if NAME ends with a directory separator character."
  (eq (eval-when-compile
        (if (memq system-type '(cygwin windows-nt ms-dos))
            ?\\ ?/))
      (aref name (1- (length name)))))

;;;; Defined in subr.el

(compat-defun string-greaterp (string1 string2) ;; <OK>
  "Return non-nil if STRING1 is greater than STRING2 in lexicographic order.
Case is significant.
Symbols are also allowed; their print names are used instead."
  (string-lessp string2 string1))

(compat-defmacro with-file-modes (modes &rest body) ;; <OK>
  "Execute BODY with default file permissions temporarily set to MODES.
MODES is as for `set-default-file-modes'."
  (declare (indent 1) (debug t))
  (let ((umask (make-symbol "umask")))
    `(let ((,umask (default-file-modes)))
       (unwind-protect
           (progn
             (set-default-file-modes ,modes)
             ,@body)
         (set-default-file-modes ,umask)))))

(compat-defun alist-get (key alist &optional default remove) ;; <OK>
  "Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT.
This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT."
  (ignore remove)
  (let ((x (assq key alist)))
    (if x (cdr x) default)))

(compat-defmacro if-let (spec then &rest else) ;; <OK>
  "Bind variables according to SPEC and evaluate THEN or ELSE.
Evaluate each binding in turn, as in `let*', stopping if a
binding value is nil.  If all are non-nil return the value of
THEN, otherwise the last form in ELSE.

Each element of SPEC is a list (SYMBOL VALUEFORM) that binds
SYMBOL to the value of VALUEFORM.  An element can additionally be
of the form (VALUEFORM), which is evaluated and checked for nil;
i.e. SYMBOL can be omitted if only the test result is of
interest.  It can also be of the form SYMBOL, then the binding of
SYMBOL is checked for nil.

As a special case, interprets a SPEC of the form \(SYMBOL SOMETHING)
like \((SYMBOL SOMETHING)).  This exists for backward compatibility
with an old syntax that accepted only one binding."
  (declare (indent 2)
           (debug ([&or (symbolp form)
                        (&rest [&or symbolp (symbolp form) (form)])]
                   body)))
  (when (and (<= (length spec) 2)
             (not (listp (car spec))))
    ;; Adjust the single binding case
    (setq spec (list spec)))
  (let ((empty (make-symbol "s"))
        (last t) list)
    (dolist (var spec)
      (push `(,(if (cdr var) (car var) empty)
              (and ,last ,(if (cdr var) (cadr var) (car var))))
            list)
      (when (or (cdr var) (consp (car var)))
        (setq last (caar list))))
    `(let* ,(nreverse list)
       (if ,(caar list) ,then ,@else))))

(compat-defmacro when-let (spec &rest body) ;; <OK>
  "Bind variables according to SPEC and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all are non-nil, return the value of the last form in BODY.

The variable list SPEC is the same as in `if-let'."
  (declare (indent 1)
           (debug ([&or (symbolp form)
                        (&rest [&or symbolp (symbolp form) (form)])]
                   body)))
  (when (and (<= (length spec) 2)
             (not (listp (car spec))))
    ;; Adjust the single binding case
    (setq spec (list spec)))
  (let ((empty (make-symbol "s"))
        (last t) list)
    (dolist (var spec)
      (push `(,(if (cdr var) (car var) empty)
              (and ,last ,(if (cdr var) (cadr var) (car var))))
            list)
      (when (or (cdr var) (consp (car var)))
        (setq last (caar list))))
    `(let* ,(nreverse list)
       (if ,(caar list) ,(macroexp-progn body)))))

;;;; Defined in subr-x.el

(compat-defmacro thread-first (&rest forms) ;; <OK>
  "Thread FORMS elements as the first argument of their successor.
Example:
    (thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ (- (/ (+ 5 20) 25)) 40)
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1)
           (debug (form &rest [&or symbolp (sexp &rest form)])))
  (let ((body (car forms)))
    (dolist (form (cdr forms))
      (when (symbolp form)
        (setq form (list form)))
      (setq body (append (list (car form))
                         (list body)
                         (cdr form))))
    body))

(compat-defmacro thread-last (&rest forms) ;; <OK>
  "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1) (debug thread-first))
  (let ((body (car forms)))
    (dolist (form (cdr forms))
      (when (symbolp form)
        (setq form (list form)))
      (setq body (append form (list body))))
    body))

;;;; Defined in macroexp.el

(compat-defun macroexpand-1 (form &optional environment) ;; <OK>
  "Perform (at most) one step of macro expansion."
  :feature macroexp
  (cond
   ((consp form)
    (let* ((head (car form))
           (env-expander (assq head environment)))
      (if env-expander
          (if (cdr env-expander)
              (apply (cdr env-expander) (cdr form))
            form)
        (if (not (and (symbolp head) (fboundp head)))
            form
          (let ((def (autoload-do-load (symbol-function head) head 'macro)))
            (cond
             ;; Follow alias, but only for macros, otherwise we may end up
             ;; skipping an important compiler-macro (e.g. cl--block-wrapper).
             ((and (symbolp def) (macrop def)) (cons def (cdr form)))
             ((not (consp def)) form)
             (t
              (if (eq 'macro (car def))
                  (apply (cdr def) (cdr form))
                form))))))))
   (t form)))

;;;; Defined in byte-run.el

(compat-defalias function-put put) ;; <OK>

(provide 'compat-25)
;;; compat-25.el ends here
