;;; compat-macs.el --- Compatibility Macros -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; These macros are used to define compatibility functions and macros.

;;; Code:

;; We always require subr-x at compile time for the fboundp check
;; since definitions have been moved around. The cl-lib macros are
;; needed by compatibility definitions.
(require 'subr-x)
(require 'cl-lib)

(defvar compat--version nil
  "Version of the currently defined compatibility definitions.")

(defmacro compat--strict (cond &rest error)
  "Assert strict COND, otherwise fail with ERROR."
  (when (bound-and-true-p compat-strict)
    `(compat--assert ,cond ,@error)))

(defmacro compat--assert (cond &rest error)
  "Assert COND, otherwise fail with ERROR."
  `(unless ,cond (error ,@error)))

(defun compat--docstring (type name docstring)
  "Format DOCSTRING for NAME of TYPE.
Prepend compatibility notice to the actual documentation string."
  (with-temp-buffer
    (insert
     (format
      "[Compatibility %s for `%S', defined in Emacs %s.  \
If this is not documented on yourself system, you can check \
`(compat) Emacs %s' for more details.]\n\n%s"
      type name
      compat--version compat--version
      docstring))
    (let ((fill-column 80))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun compat--check-attributes (attrs preds)
  "Check ATTRS given PREDS predicate plist and return rest."
  (while (keywordp (car attrs))
    (compat--assert (cdr attrs) "Attribute list length is odd")
    (let ((pred (plist-get preds (car attrs))))
      (compat--assert (and pred (or (eq pred t) (funcall pred (cadr attrs))))
                      "Invalid attribute %s" (car attrs)))
    (setq attrs (cddr attrs)))
  attrs)

(defun compat--guard (attrs preds fun)
  "Guard compatibility definition generation.
The version constraints specified by ATTRS are checked.  PREDS is
a plist of predicates for arguments which are passed to FUN."
  (declare (indent 2))
  (let* ((body (compat--check-attributes
                attrs `(,@preds :feature symbolp)))
         (feature (plist-get attrs :feature))
         (attrs `(:body ,body ,@attrs))
         args)
    ;; Require feature at compile time
    (when feature
      (compat--assert (not (eq feature 'subr-x)) "Invalid feature subr-x")
      (require feature))
    ;; The current Emacs must be older than the currently declared version.
    (when (version< emacs-version compat--version)
      (while preds
        (push (plist-get attrs (car preds)) args)
        (setq preds (cddr preds)))
      (setq body (apply fun (nreverse args)))
      (if (and feature body)
          `(with-eval-after-load ',feature ,@body)
        (macroexp-progn body)))))

(defun compat--guard-defun (type name arglist docstring rest)
  "Define function NAME of TYPE with ARGLIST and DOCSTRING.
REST are attributes and the function BODY."
  (compat--guard rest `(:extended ,(lambda (x) (or (booleanp x) (version-to-list x)))
                        :obsolete ,(lambda (x) (or (booleanp x) (stringp x)))
                        :body t)
    (lambda (extended obsolete body)
      (when (stringp extended)
        (setq extended (version<= extended emacs-version)))
      (compat--strict (eq extended (fboundp name))
                      "Wrong :extended flag for %s %s" type name)
      ;; Remove unsupported declares.  It might be possible to set these
      ;; properties otherwise.  That should be looked into and implemented
      ;; if it is the case.
      (when (and (listp (car-safe body)) (eq (caar body) 'declare))
        (when (<= emacs-major-version 25)
          (delq (assq 'side-effect-free (car body)) (car body))
          (delq (assq 'pure (car body)) (car body))))
      ;; Use `:extended' name if the function is already defined.
      (let* ((defname (if (and extended (fboundp name))
                          (intern (format "compat--%s" name))
                        name))
             (def `(,(if (memq '&key arglist)
                         (if (eq type 'macro) 'cl-defmacro 'cl-defun)
                       (if (eq type 'macro) 'defmacro 'defun))
                    ,defname ,arglist
                    ,(compat--docstring type name docstring)
                    ,@body)))
        `(,@(if (eq defname name)
                ;; An additional fboundp check is performed at runtime to make
                ;; sure that we never redefine an existing definition if Compat
                ;; is loaded on a newer Emacs version.  Declare the function,
                ;; such that the byte compiler does not complain about possibly
                ;; missing functions at runtime. The warnings are generated due
                ;; to the fboundp check.
                `((declare-function ,name nil)
                  (unless (fboundp ',name) ,def))
              (list def))
          ,@(when obsolete
              `((make-obsolete
                 ',name ,(if (stringp obsolete) obsolete "No substitute")
                 ,compat--version))))))))

(defmacro compat-guard (cond &rest rest)
  "Guard definition with a runtime COND and a version check.
The runtime condition must make sure that no definition is
overriden.  REST is an attribute plist followed by the definition
body.  The attributes specify the conditions under which the
definition is generated.

- :feature :: Wrap the definition with `with-eval-after-load' for
  the given feature."
  (declare (debug ([&rest keywordp sexp] def-body))
           (indent 1))
  (compat--guard rest '(:body t)
    (lambda (body)
      (if (eq cond t)
          body
        (compat--strict (eval cond t) "Guard %S failed" cond)
        `((when ,cond ,@body))))))

(defmacro compat-defalias (name def &rest attrs)
  "Define compatibility alias NAME as DEF.
ATTRS is a plist of attributes, which specify the conditions
under which the definition is generated.

- :obsolete :: Mark the alias as obsolete if t.

- :feature :: See `compat-guard'."
  (declare (debug (name symbolp [&rest keywordp sexp])))
  (compat--guard attrs '(:obsolete booleanp)
    (lambda (obsolete)
      (compat--strict (not (fboundp name)) "%s already defined" name)
      ;; The fboundp check is performed at runtime to make sure that we never
      ;; redefine an existing definition if Compat is loaded on a newer Emacs
      ;; version.
      `((unless (fboundp ',name)
          (defalias ',name ',def
            ,(compat--docstring 'function name
                                (get name 'function-documentation)))
          ,@(when obsolete
              `((make-obsolete ',name ',def ,compat--version))))))))

(defmacro compat-defun (name arglist docstring &rest rest)
  "Define compatibility function NAME with arguments ARGLIST.
The function must be documented in DOCSTRING.  REST is an
attribute plist followed by the function body.  The attributes
specify the conditions under which the definition is generated.

- :extended :: Mark the function as extended if t.  The function
  must be called explicitly via `compat-call'.  This attribute
  should be used for functions which extend already existing
  functions, e.g., functions which changed their calling
  convention or their behavior.  The value can also be a version
  string, which specifies the Emacs version when the original
  version of the function was introduced.

- :obsolete :: Mark the function as obsolete if t, can be a
  string describing the obsoletion.

- :feature :: See `compat-guard'."
  (declare (debug (&define name (&rest symbolp)
                           stringp
                           [&rest keywordp sexp]
                           def-body))
           (doc-string 3) (indent 2))
  (compat--guard-defun 'function name arglist docstring rest))

(defmacro compat-defmacro (name arglist docstring &rest rest)
  "Define compatibility macro NAME with arguments ARGLIST.
The macro must be documented in DOCSTRING.  REST is an attribute
plist followed by the macro body.  See `compat-defun' for
details."
  (declare (debug compat-defun) (doc-string 3) (indent 2))
  (compat--guard-defun 'macro name arglist docstring rest))

(defmacro compat-defvar (name initval docstring &rest attrs)
  "Define compatibility variable NAME with initial value INITVAL.
The variable must be documented in DOCSTRING.  ATTRS is a plist
of attributes, which specify the conditions under which the
definition is generated.

- :constant :: Mark the variable as constant if t.

- :local :: Make the variable buffer-local if t.  If the value is
  `permanent' make the variable additionally permanently local.

- :obsolete :: Mark the variable as obsolete if t, can be a
  string describing the obsoletion.

- :feature :: See `compat-guard'."
  (declare (debug (name form stringp [&rest keywordp sexp]))
           (doc-string 3) (indent 2))
  (compat--guard attrs `(:constant booleanp
                         :local ,(lambda (x) (memq x '(nil t permanent)))
                         :obsolete ,(lambda (x) (or (booleanp x) (stringp x))))
    (lambda (constant local obsolete)
      (compat--strict (not (boundp name)) "%s already defined" name)
      (compat--assert (not (and constant local)) "Both :constant and :local")
      ;; The boundp check is performed at runtime to make sure that we never
      ;; redefine an existing definition if Compat is loaded on a newer Emacs
      ;; version.
      `((unless (boundp ',name)
          (,(if constant 'defconst 'defvar)
           ,name ,initval
           ,(compat--docstring 'variable name docstring))
          ,@(when obsolete
              `((make-obsolete-variable
                 ',name ,(if (stringp obsolete) obsolete "No substitute")
                 ,compat--version))))
        ,@(and local `((make-variable-buffer-local ',name)))
        ,@(and (eq local 'permanent) `((put ',name 'permanent-local t)))))))

(defmacro compat-declare-version (version)
  "Set the Emacs version that is currently being handled to VERSION."
  (setq compat--version version)
  (let ((before (1- (car (version-to-list version)))))
    (when (and (< 24 before) (< emacs-major-version before))
      `(require ',(intern (format "compat-%d" before))))))

(provide 'compat-macs)
;;; compat-macs.el ends here
