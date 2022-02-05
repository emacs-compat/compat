;;; compat-macs.el --- Compatibility Macros           -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Keywords: lisp

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

;; These macros are used to define compatibility functions, macros and
;; advice.

;;; Code:

(defmacro compat--ignore (&rest _)
  "Ignore all arguments."
  nil)

(defun compat-generate-common (name def-fn install-fn check-fn attr type)
  "Common code for generating compatibility definitions for NAME.
The resulting body is constructed by invoking the functions
DEF-FN (passed the \"realname\" and the version number, returning
the compatibility definition), the INSTALL-FN (passed the
\"realname\" and returning the installation code),
CHECK-FN (passed the \"realname\" and returning a check to see if
the compatibility definition should be installed).  ATTR is a
plist used to modify the generated code.  The following
attributes are handled, all others are ignored:

- :min-version :: Prevent the compatibility definition from begin
  installed in versions older than indicated (string).

- :max-version :: Prevent the compatibility definition from begin
  installed in versions newer than indicated (string).

- :feature :: The library the code is supposed to be loaded
  with (via `eval-after-load').

- :cond :: Only install the compatibility code, iff the value
  evaluates to non-nil.

- :no-highlight :: Do not highlight this definition as
  compatibility function.

- :version :: Manual specification of the version the compatee
  code was defined in (string).

- :realname :: Manual specification of a \"realname\" to use for
  the compatibility definition (symbol).

- :notes :: Additional notes that a developer using this
  compatibility function should keep in mind.

TYPE is used to set the symbol property `compat-type' for NAME."
  (let* ((min-version (plist-get attr :min-version))
         (max-version (plist-get attr :max-version))
         (feature (plist-get attr :feature))
         (cond (plist-get attr :cond))
         (version (or (plist-get attr :version)
                      (let ((file (or (and (boundp 'byte-compile-current-file)
                                           byte-compile-current-file)
                                      load-file-name
                                      (buffer-file-name))))
                        ;; Guess the version from the file the macro is
                        ;; being defined in.
                        (and (string-match
                              "compat-\\([[:digit:]]+\\.[[:digit:]]+\\)\\.\\(?:elc?\\)\\'"
                              file)
                             (match-string 1 file)))))
         (realname (or (plist-get attr :realname)
                       (intern (format "compat--%S" name))))
         (body `(,@(cond
                    ((and (or (not version)
                              (version< emacs-version version))
                          (or (not min-version)
                              (version<= min-version emacs-version))
                          (or (not max-version)
                              (version<= emacs-version max-version)))
                     `(when (and ,(if cond cond t)
                                 ,(funcall check-fn))))
                    ('(compat--ignore)))
                 ,(unless (plist-get attr :no-highlight)
                    `(font-lock-add-keywords
                      'emacs-lisp-mode
                      ',`((,(concat "\\_<\\("
                                    (regexp-quote (symbol-name name))
                                    "\\)\\_>")
                           1 font-lock-preprocessor-face prepend))))
                 ,(funcall install-fn realname version))))
    `(progn
       (put ',realname 'compat-type ',type)
       (put ',realname 'compat-version ,version)
       (put ',realname 'compat-doc ,(plist-get attr :note))
       (put ',name 'compat-def ',realname)
       ,(funcall def-fn realname version)
       ,(if feature
            ;; See https://nullprogram.com/blog/2018/02/22/:
            `(eval-after-load ',feature `(funcall #',(lambda () ,body)))
          body))))

(defun compat-common-fdefine (type name arglist docstring rest)
  "Generate compatibility code for a function NAME.
TYPE is one of `func', for functions and `macro' for macros, and
`advice' ARGLIST is passed on directly to the definition, and
DOCSTRING is prepended with a compatibility note.  REST contains
the remaining definition, that may begin with a property list of
attributes (see `compat-generate-common')."
  (let ((body rest))
    (while (keywordp (car body))
      (setq body (cddr body)))
    ;; It might be possible to set these properties otherwise.  That
    ;; should be looked into and implemented if it is the case.
    (when (and (listp (car-safe body)) (eq (caar body) 'declare))
      (when (version<= "25" emacs-version)
        (delq (assq 'side-effect-free (car body)) (car body))
        (delq (assq 'pure (car body)) (car body))))
    (compat-generate-common
     name
     (lambda (realname version)
       `(,(cond
           ((memq type '(func advice)) 'defun)
           ((eq type 'macro) 'defmacro)
           ((error "Unknown type")))
         ,realname ,arglist
         ;; Prepend compatibility notice to the actual
         ;; documentation string.
         ,(let ((type (cond
                       ((eq type 'func) "function")
                       ((eq type 'macro) "macro")
                       ((eq type 'advice) "advice")
                       ((error "Unknown type")))))
            (if version
                (format
                 "[Compatibility %s for `%S', defined in Emacs %s]\n\n%s"
                 type name version docstring)
              (format
               "[Compatibility %s for `%S']\n\n%s"
               type name docstring)))
         ;; Advice may use the implicit variable `oldfun', but
         ;; to avoid triggering the byte compiler, we make
         ;; sure the argument is used at least once.
         ,@(if (eq type 'advice)
               (cons '(ignore oldfun) body)
             body)))
     (lambda (realname version)
       (cond
        ((memq type '(func macro))
         ;; Functions and macros are installed by
         ;; aliasing the name of the compatible
         ;; function to the name of the compatibility
         ;; function.
         `(defalias ',name #',realname))
        ((eq type 'advice)
         ;; nadvice.el was introduced in Emacs 24.4, so older versions
         ;; have to advise the function using advice.el's `defadvice'.
         (if (version<= "24.4" emacs-version)
             `(advice-add ',name :around #',realname)
           (let ((oldfun (make-symbol (format "compat--oldfun-%S" realname))))
             `(progn
                (defvar ,oldfun (indirect-function ',name))
                (put ',name 'compat-advice-fn #',realname)
                (defalias ',name
                  (lambda (&rest args)
                    ,(format
                      "[Manual compatibility advice for `%S', defined in Emacs %s]\n\n%s"
                      name version (if (fboundp name) (documentation name) docstring))
                    (apply #',realname (cons (autoload-do-load ,oldfun) args))))))))))
     (lambda ()
       (cond
        ((memq type '(func macro))
         `(not (fboundp ',name)))
        ((eq type 'advice) t)))
     rest type)))

(defmacro compat-defun (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility function.
The function must be documented in DOCSTRING.  REST may begin
with a plist, that is interpreted by the macro but not passed on
to the actual function.  See `compat-generate-common' for a
listing of attributes.

The definition will only be installed, if the version this
function was defined in, as indicated by the `:version'
attribute, is greater than the current Emacs version."
  (declare (debug (&define name (&rest symbolp)
                           stringp
                           [&rest keywordp sexp]
                           def-body))
           (doc-string 3) (indent 2))
  (compat-common-fdefine 'func name arglist docstring rest))

(defmacro compat-defmacro (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility macro.
The macro must be documented in DOCSTRING.  REST may begin
with a plist, that is interpreted by this macro but not passed on
to the actual macro.  See `compat-generate-common' for a
listing of attributes.

The definition will only be installed, if the version this
function was defined in, as indicated by the `:version'
attribute, is greater than the current Emacs version."
  (declare (debug compat-defun) (doc-string 3) (indent 2))
  (compat-common-fdefine 'macro name arglist docstring rest))

(defmacro compat-advise (name arglist docstring &rest rest)
  "Define NAME with arguments ARGLIST as a compatibility advice.
The advice function must be documented in DOCSTRING.  REST may
begin with a plist, that is interpreted by this macro but not
passed on to the actual advice function.  See
`compat-generate-common' for a listing of attributes.  The advice
wraps the old definition, that is accessible via using the symbol
`oldfun'.

The advice will only be installed, if the version this function
was defined in, as indicated by the `:version' attribute, is
greater than the current Emacs version."
  (declare (debug compat-defun) (doc-string 3) (indent 2))
  (compat-common-fdefine 'advice name (cons 'oldfun arglist) docstring rest))

(defmacro compat-defvar (name initval docstring &rest attr)
  "Declare compatibility variable NAME with initial value INITVAL.
The obligatory documentation string DOCSTRING must be given.

The remaining arguments ATTR form a plist, modifying the
behaviour of this macro.  See `compat-generate-common' for a
listing of attributes.  Furthermore, `compat-defvar' also handles
the attribute `:local' that either makes the variable permanent
local with a value of `permanent' or just buffer local with any
non-nil value."
  (declare (debug (name form stringp [&rest keywordp sexp]))
           (doc-string 3) (indent 2))
  (compat-generate-common
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
   attr 'variable))

(provide 'compat-macs)
;;; compat-macs.el ends here
