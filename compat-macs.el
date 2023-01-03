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

(defvar compat--current-version nil
  "Default version to use when no explicit version was given.")

(defmacro compat-declare-version (version)
  "Set the Emacs version that is currently being handled to VERSION."
  (setq compat--current-version version)
  nil)

(defun compat--with-feature (feature body)
  "Protect BODY with `eval-after-load' if FEATURE is non-nil."
  (declare (indent 1))
  (if feature
      ;; See https://nullprogram.com/blog/2018/02/22/:
      `(eval-after-load ,feature `(funcall ',(lambda () ,body)))
    body))

(defvar compat--generate-function #'compat--generate-default
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

- :min-version :: Prevent the compatibility definition from begin
  installed in versions older than indicated (string).

- :max-version :: Prevent the compatibility definition from begin
  installed in versions newer than indicated (string).

- :feature :: The library the code is supposed to be loaded
  with (via `eval-after-load').

- :cond :: Only install the compatibility code, iff the value
  evaluates to non-nil.

  For prefixed functions, this can be interpreted as a test to
  `defalias' an existing definition or not.

- :version :: Manual specification of the version the compatee
  code was defined in (string).

- :realname :: Manual specification of a \"realname\" to use for
  the compatibility definition (symbol).

- :explicit :: Add a `compat-' prefix to the name, and define the
  compatibility code unconditionally.")

(defun compat--generate-default (name def-fn install-fn check-fn attr)
  "Generate a leaner compatibility definition.
See `compat--generate-function' for details on the arguments NAME,
DEF-FN, INSTALL-FN, CHECK-FN and ATTR."
  (let* ((min-version (plist-get attr :min-version))
         (max-version (plist-get attr :max-version))
         (feature (plist-get attr :feature))
         (cond (plist-get attr :cond))
         (version (or (plist-get attr :version)
                      compat--current-version))
         (check (cond
                 ((or (and min-version
                           (version< emacs-version min-version))
                      (and max-version
                           (version< max-version emacs-version)))
                  nil)
                 ((plist-get attr :explicit)
                  '(progn))
                 ((and version (version<= version emacs-version) (not cond))
                  nil)
                 (`(when (and ,(if cond cond t)
                              ,(funcall check-fn)))))))
    (when (and (plist-get attr :realname)
               (string= name (plist-get attr :realname)))
      (error "%S: Name is equal to realname" name))
    (cond
     ((and (plist-get attr :explicit)
           (let ((actual-name (intern (substring (symbol-name name)
                                                 (length "compat-")))))
             (when (and (version<= version emacs-version)
                        (fboundp actual-name)
                        check)
               (compat--with-feature feature
                 `(,@check ,(funcall install-fn actual-name version)))))))
     ((let ((realname (plist-get attr :realname)))
        (when realname
          `(progn
             ,(funcall def-fn realname version)
             ,(when check
                (compat--with-feature feature
                  `(,@check ,(funcall install-fn realname version))))))))
     (check
      (compat--with-feature feature
        `(,@check ,(funcall def-fn name version)))))))

(defun compat--define-function (type name arglist docstring rest)
  "Generate compatibility code for a function NAME.
TYPE is one of `func', for functions and `macro' for macros, and
`advice' ARGLIST is passed on directly to the definition, and
DOCSTRING is prepended with a compatibility note.  REST contains
the remaining definition, that may begin with a property list of
attributes (see `compat--generate-function')."
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
    (funcall compat--generate-function
     name
     (lambda (realname version)
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
              (insert docstring)
              (newline 2)
              (insert
               "[Compatibility "
               (if version
                   (format
                    "%s for `%S', defined in Emacs %s.  \
If this is not documented on yourself system, you can check \
`(compat) Emacs %s' for more details."
                    type oldname version version)
                 (format
                  "code %s for `%S'"
                  type oldname))
               "]")
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
                        "29.1"))
                   `((defalias ',realname #',(intern (format "compat--%s" oldname))))))))
     (lambda (realname _version)
       ;; Functions and macros are installed by aliasing the name of the
       ;; compatible function to the name of the compatibility function.
       (if (and (plist-get rest :explicit) (string= realname oldname))
           `(progn
              (defalias ',name ',realname)
              (make-obsolete
               ',name
               "Use `compat-call' or `compat-function' instead"
               "29.1"))
         `(defalias ',name #',realname)))
     (lambda ()
       `(not (fboundp ',name)))
     rest)))

(defmacro compat-defun (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility function.
The function must be documented in DOCSTRING.  REST may begin
with a plist, that is interpreted by the macro but not passed on
to the actual function.  See `compat--generate-function' for a
listing of attributes.

The definition will only be installed, if the version this
function was defined in, as indicated by the `:version'
attribute, is greater than the current Emacs version."
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
to the actual macro.  See `compat--generate-function' for a
listing of attributes.

The definition will only be installed, if the version this
function was defined in, as indicated by the `:version'
attribute, is greater than the current Emacs version."
  (declare (debug compat-defun) (doc-string 3) (indent 2))
  (compat--define-function 'macro name arglist docstring rest))

(defmacro compat-defvar (name initval docstring &rest attr)
  "Declare compatibility variable NAME with initial value INITVAL.
The obligatory documentation string DOCSTRING must be given.

The remaining arguments ATTR form a plist, modifying the
behaviour of this macro.  See `compat--generate-function' for a
listing of attributes.  Furthermore, `compat-defvar' also handles
the attribute `:local' that either makes the variable permanent
local with a value of `permanent' or just buffer local with any
non-nil value."
  (declare (debug (name form stringp [&rest keywordp sexp]))
           (doc-string 3) (indent 2))
  (when (or (plist-get attr :explicit) (plist-get attr :realname))
    (error ":explicit cannot be specified for compatibility variables"))
  (funcall compat--generate-function
           name
           (lambda (realname version)
             (let ((localp (plist-get attr :local)))
               `(progn
                  (,(if (plist-get attr :constant) 'defconst 'defvar)
                   ,realname ,initval
                   ;; Prepend compatibility notice to the actual
                   ;; documentation string.
                   ,(if version
                        (format
                         "[Compatibility variable for `%S', defined in Emacs %s]\n\n%s"
                         name version docstring)
                      (format
                       "[Compatibility variable for `%S']\n\n%s"
                       name docstring)))
                  ;; Make variable as local if necessary
                  ,(cond
                    ((eq localp 'permanent)
                     `(put ',realname 'permanent-local t))
                    (localp
                     `(make-variable-buffer-local ',realname))))))
           (lambda (realname _version)
             `(defvaralias ',name ',realname))
           (lambda ()
             `(not (boundp ',name)))
           attr))

(provide 'compat-macs)
;;; compat-macs.el ends here
