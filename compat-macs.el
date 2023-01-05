;;; compat-macs.el --- Compatibility Macros           -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; We always require subr-x at compile since many functions have been moved
;; around.
(require 'subr-x)

(defvar compat--current-version nil
  "Default version to use when no explicit version was given.")

(defmacro compat-declare-version (version)
  "Set the Emacs version that is currently being handled to VERSION."
  (setq compat--current-version version)
  nil)

(defun compat--with-feature (feature &rest body)
  "Protect BODY with `with-eval-after-load' if FEATURE is non-nil."
  (declare (indent 1))
  (if feature
      `(with-eval-after-load ',feature ,@body)
    (macroexp-progn body)))

(defun compat--generate (name def-fn install-fn check-fn attr)
  "Function used to generate compatibility code.
The function must take six arguments: NAME, DEF-FN, INSTALL-FN,
CHECK-FN and ATTR.  The resulting body is constructed by invoking
the functions DEF-FN (passed the \"realname\" and the version
number, returning the compatibility definition), the
INSTALL-FN (passed the \"realname\" and returning the
installation code), CHECK-FN (passed the \"realname\" and
returning a check to see if the compatibility definition should
be installed).  ATTR is a plist used to modify the generated
code.  The following attributes are handled, all others are
ignored:

- :min-version :: Do not install the compatibility definition
  if Emacs version older than indicated.

- :max-version :: Do not install the compatibility definition
  if Emacs version newer or equal than indicated.

- :feature :: The library the code is supposed to be loaded
  with (via `eval-after-load').

- :cond :: Only install the compatibility code, iff the value
  evaluates to non-nil.

  For prefixed functions, this can be interpreted as a test to
  `defalias' an existing definition or not.

- :realname :: Manual specification of a \"realname\" to use for
  the compatibility definition (symbol).

- :explicit :: Add a `compat-' prefix to the name, and define the
  compatibility code unconditionally."
  (let* ((min-version (plist-get attr :min-version))
         (max-version (plist-get attr :max-version))
         (feature (plist-get attr :feature))
         (cond (plist-get attr :cond))
         (check))
    (unless compat--current-version
      (error "No compat version declared"))
    (when (and (plist-get attr :realname)
               (string= name (plist-get attr :realname)))
      (error "%S: Name is equal to realname" name))
    ;; subr-x is available at compile time.
    (when (eq feature 'subr-x)
      (error "Feature subr-x is forbidden"))
    (when feature
      (unless (require feature nil t)
        (setq feature nil)))
    (setq check
          (cond
           ((or (and min-version
                     (version< emacs-version min-version))
                (and max-version
                     (version<= max-version emacs-version)))
            nil)
           ((plist-get attr :explicit)
            t)
           ((and (version<= compat--current-version emacs-version) (not cond))
            nil)
           ((and (if cond (eval cond t) t)
                 (funcall check-fn)))))
    (cond
     ((and (plist-get attr :explicit)
           (let ((actual-name (intern (substring (symbol-name name)
                                                 (length "compat-")))))
             ;; NOTE: For prefixed/explicit functions check the Emacs version,
             ;; since the fboundp check cannot be used! We want to redefine
             ;; existing functions.
             (when (and (version<= compat--current-version emacs-version)
                        (fboundp actual-name)
                        check)
               (compat--with-feature feature
                 (funcall install-fn actual-name))))))
     ((let ((realname (plist-get attr :realname)))
        (when realname
          `(progn
             ,(funcall def-fn realname)
             ,(when check
                (compat--with-feature feature
                  (funcall install-fn realname)))))))
     (check
      (compat--with-feature feature
        (funcall def-fn name))))))

(defun compat--define-function (type name arglist docstring rest)
  "Generate compatibility code for a function NAME.
TYPE is one of `func', for functions and `macro' for macros, and
`advice' ARGLIST is passed on directly to the definition, and
DOCSTRING is prepended with a compatibility note.  REST contains
the remaining definition, that may begin with a property list of
attributes (see `compat--generate')."
  (let ((oldname name) (body rest))
    (while (keywordp (car body))
      (setq body (cddr body)))
    ;; It might be possible to set these properties otherwise.  That
    ;; should be looked into and implemented if it is the case.
    (when (and (listp (car-safe body)) (eq (caar body) 'declare))
      (when (version<= emacs-version "25")
        (delq (assq 'side-effect-free (car body)) (car body))
        (delq (assq 'pure (car body)) (car body))))
    ;; Ensure that :realname is not the same as compat--<name>,
    ;; since this is the compat-call/compat-function naming convention.
    (when (and (plist-get rest :realname)
               (string= (plist-get rest :realname) (format "compat--%s" name)))
      (error "%s: :realname must not be the same as compat--<name>" name))
    ;; Check if we want an explicitly prefixed function
    (when (plist-get rest :explicit)
      (setq name (intern (format "compat-%s" name))))
    (compat--generate
     name
     (lambda (realname)
       `(progn
          (,(cond
             ((eq type 'function) 'defun)
             ((eq type 'macro) 'defmacro)
             ((error "Unknown type")))
           ,(if (plist-get rest :explicit)
                (intern (format "compat--%s" oldname))
              realname)
           ,arglist
           ;; Prepend compatibility notice to the actual
           ;; documentation string.
           ,(with-temp-buffer
              (insert
               (format
                "[Compatibility %s for `%S', defined in Emacs %s.  \
If this is not documented on yourself system, you can check \
`(compat) Emacs %s' for more details.]\n\n"
                type oldname compat--current-version compat--current-version
                docstring))
              (let ((fill-column 80))
                (fill-region (point-min) (point-max)))
              (buffer-string))
           ,@body)
          ,@(and (plist-get rest :explicit)
                 (if (string= realname name)
                     `((defalias ',name ',(intern (format "compat--%s" oldname)))
                       (make-obsolete
                        ',name
                        "Use `compat-call' or `compat-function' instead"
                        "29.1.0.0"))
                   `((defalias ',realname #',(intern (format "compat--%s" oldname))))))))
     (lambda (realname)
       `(progn
          ;; Functions and macros are installed by aliasing the name of the
          ;; compatible function to the name of the compatibility function.
          (defalias ',name #',realname)
          ,@(when (and (plist-get rest :realname)
                       (not (string= (plist-get rest :realname) name))
                       (not (string= (plist-get rest :realname) realname)))
              `((defalias ',(plist-get rest :realname) #',realname)))
          ,@(when (and (plist-get rest :explicit) (string= realname oldname))
              `((make-obsolete
                 ',name
                 "Use `compat-call' or `compat-function' instead"
                 "29.1.0.0")))))
     (lambda ()
       `(not (fboundp ',name)))
     rest)))

(defmacro compat-defun (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility function.
The function must be documented in DOCSTRING.  REST may begin
with a plist, that is interpreted by the macro but not passed on
to the actual function.  See `compat--generate' for a
listing of attributes."
  (declare (debug (&define name (&rest symbolp)
                           stringp
                           [&rest keywordp sexp]
                           def-body))
           (doc-string 3) (indent 2))
  (compat--define-function 'function name arglist docstring rest))

(defmacro compat-defmacro (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility macro.
The macro must be documented in DOCSTRING.  REST may begin
with a plist, that is interpreted by this macro but not passed on
to the actual macro.  See `compat--generate' for a
listing of attributes."
  (declare (debug compat-defun) (doc-string 3) (indent 2)) ;; <UNTESTED>
  (compat--define-function 'macro name arglist docstring rest))

(defmacro compat-defalias (name def)
  "Declare compatibility alias NAME with DEF."
  (compat--generate
           name
           (lambda (realname)
             `(defalias ',realname ',def))
           (lambda (realname)
             `(defalias ',name ',realname))
           (lambda ()
             `(not (fboundp ',name)))
           nil))

(defmacro compat-defvar (name initval docstring &rest attr)
  "Declare compatibility variable NAME with initial value INITVAL.
The obligatory documentation string DOCSTRING must be given.

The remaining arguments ATTR form a plist, modifying the
behaviour of this macro.  See `compat--generate' for a
listing of attributes.  Furthermore, `compat-defvar' also handles
the attribute `:local' that either makes the variable permanent
local with a value of `permanent' or just buffer local with any
non-nil value."
  (declare (debug (name form stringp [&rest keywordp sexp]))
           (doc-string 3) (indent 2))
  (when (or (plist-get attr :explicit) (plist-get attr :realname))
    (error ":explicit cannot be specified for compatibility variables"))
  (compat--generate
           name
           (lambda (realname)
             (let ((localp (plist-get attr :local)))
               `(progn
                  (,(if (plist-get attr :constant) 'defconst 'defvar)
                   ,realname ,initval
                   ;; Prepend compatibility notice to the actual
                   ;; documentation string.
                   ,(with-temp-buffer
                      (insert
                       (format
                        "[Compatibility variable for `%S', defined in Emacs %s]\n\n%s"
                        name compat--current-version docstring))
                      (let ((fill-column 80))
                        (fill-region (point-min) (point-max)))
                      (buffer-string)))
                  ;; Make variable as local if necessary
                  ,(cond
                    ((eq localp 'permanent)
                     `(put ',realname 'permanent-local t))
                    (localp
                     `(make-variable-buffer-local ',realname))))))
           (lambda (realname)
             `(defvaralias ',name ',realname))
           (lambda ()
             `(not (boundp ',name)))
           attr))

(provide 'compat-macs)
;;; compat-macs.el ends here
