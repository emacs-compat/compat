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

;; We always require subr-x at compile since many functions have been moved
;; around.
(require 'subr-x)

(defvar compat--current-version nil
  "Version of the currently defined compatibility definitions.")

(defmacro compat-declare-version (version)
  "Set the Emacs version that is currently being handled to VERSION."
  (setq compat--current-version version)
  (let ((before (1- (car (version-to-list version)))))
    (when (and (< 24 before) (< emacs-major-version before))
      `(require ',(intern (format "compat-%d" before))))))

(defun compat--format-docstring (type name docstring)
  "Format DOCSTRING for NAME of TYPE.
Prepend compatibility notice to the actual documentation string."
  (with-temp-buffer
    (insert
     (format
      "[Compatibility %s for `%S', defined in Emacs %s.  \
If this is not documented on yourself system, you can check \
`(compat) Emacs %s' for more details.]\n\n%s"
      type name
      compat--current-version compat--current-version
      docstring))
    (let ((fill-column 80))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun compat--check-attributes (attrs allowed)
  "Check ATTRS for ALLOWED keys and return rest."
  (while (keywordp (car attrs))
    (unless (memq (car attrs) allowed)
      (error "Invalid attribute %s" (car attrs)))
    (unless (cdr attrs)
      (error "Odd number of element in attribute list"))
    (setq attrs (cddr attrs)))
  attrs)

(defun compat--condition-satisfied (attrs)
  "Check that version constraints specified by ATTRS are satisfied."
  (let ((min-version (plist-get attrs :min-version))
        (max-version (plist-get attrs :max-version))
        (cond (plist-get attrs :cond)))
    ;; Min/max version bounds must be satisfied.
    (and
     ;; Min/max version bounds must be satisfied.
     (or (not min-version) (version<= min-version emacs-version))
     (or (not max-version) (version< emacs-version max-version))
     ;; If a condition is specified, no version check is performed.
     (if cond
         (eval cond t)
       ;; The current Emacs must be older than the current declared Compat
       ;; version, see `compat-declare-version'.
       (version< emacs-version compat--current-version)))))

(defun compat--guarded-definition (attrs args fun)
  "Guard compatibility definition generation.
The version constraints specified by ATTRS are checked.
ARGS is a list of keywords which are looked up and passed to FUN."
  (declare (indent 2))
  (let* ((body (compat--check-attributes
                attrs `(,@args :min-version :max-version :cond :feature)))
         (feature (plist-get attrs :feature))
         (attrs `(:body ,body ,@attrs)))
    ;; Require feature at compile time
    (when feature
      (when (eq feature 'subr-x)
        (error "Feature subr-x must not be specified"))
      ;; If the feature does not exist, treat it as nil.  The function will then
      ;; be defined on the toplevel and not in a `with-eval-after-load' block.
      (setq feature (require feature nil t)))
    (when (compat--condition-satisfied attrs)
      (setq body (apply fun (mapcar (lambda (x) (plist-get attrs x)) args)))
      (when body
        (if feature
            `(with-eval-after-load ',feature ,@body)
          (macroexp-progn body))))))

(defun compat--function-definition (type name arglist docstring rest)
  "Define function NAME of TYPE with ARGLIST and DOCSTRING.
REST are attributes and the function BODY."
  (compat--guarded-definition rest '(:explicit :body)
    (lambda (explicit body)
      ;; Remove unsupported declares.  It might be possible to set these
      ;; properties otherwise.  That should be looked into and implemented
      ;; if it is the case.
      (when (and (listp (car-safe body)) (eq (caar body) 'declare))
        (when (version<= emacs-version "25")
          (delq (assq 'side-effect-free (car body)) (car body))
          (delq (assq 'pure (car body)) (car body))))
      ;; Use `:explicit' name if the function is already defined.
      (let* ((defname (if (and explicit (fboundp name))
                          (intern (format "compat--%s" name))
                        name))
             (def `(,(if (eq type 'macro) 'defmacro 'defun)
                    ,defname ,arglist
                    ,(compat--format-docstring type name docstring)
                    ,@body)))
        ;; An additional fboundp check is performed at runtime to make
        ;; sure that we never redefine an existing definition if Compat
        ;; is loaded on a newer Emacs version.
        (if (eq defname name)
            ;; Declare the function in a non-existing compat-declare
            ;; feature, such that the byte compiler does not complain
            ;; about possibly missing functions at runtime. The warnings
            ;; are generated due to the unless fboundp check.
            `((declare-function ,name nil)
              (unless (fboundp ',name) ,def))
          (list def))))))

(defmacro compat-defalias (name def &rest attrs)
  "Define compatibility alias NAME as DEF.
ATTRS is a plist of attributes, which specify the conditions
under which the definition is generated.

- :min-version :: Install the definition if the Emacs version is
  greater or equal than the given version.

- :max-version :: Install the definition if the Emacs version is
  smaller than the given version.

- :feature :: Wrap the definition with `with-eval-after-load'.

- :cond :: Install the definition if :cond evaluates to non-nil."
  (declare (debug (name symbolp [&rest keywordp sexp])))
  (compat--guarded-definition attrs ()
    (lambda ()
      ;; The fboundp check is performed at runtime to make sure that we never
      ;; redefine an existing definition if Compat is loaded on a newer Emacs
      ;; version.
      `((unless (fboundp ',name)
          (defalias ',name ',def))))))

(defmacro compat-defun (name arglist docstring &rest rest)
  "Define compatibility function NAME with arguments ARGLIST.
The function must be documented in DOCSTRING.  REST is an
attribute plist followed by the function body.  The attributes
specify the conditions under which the compatiblity function is
defined.

- :explicit :: Make the definition available such that it can be
  called explicitly via `compat-call'.

- :min-version :: Install the definition if the Emacs version is
  greater or equal than the given version.

- :max-version :: Install the definition if the Emacs version is
  smaller than the given version.

- :feature :: Wrap the definition with `with-eval-after-load'.

- :cond :: Install the definition if :cond evaluates to non-nil."
  (declare (debug (&define name (&rest symbolp)
                           stringp
                           [&rest keywordp sexp]
                           def-body))
           (doc-string 3) (indent 2))
  (compat--function-definition 'function name arglist docstring rest))

(defmacro compat-defmacro (name arglist docstring &rest rest)
  "Define compatibility macro NAME with arguments ARGLIST.
The macro must be documented in DOCSTRING.  REST is an attribute
plist followed by the macro body.  See `compat-defun' for
details."
  (declare (debug compat-defun) (doc-string 3) (indent 2))
  (compat--function-definition 'macro name arglist docstring rest))

(defmacro compat-defvar (name initval docstring &rest attrs)
  "Define compatibility variable NAME with initial value INITVAL.
The variable must be documented in DOCSTRING.  ATTRS is a plist
of attributes, which specify the conditions under which the
definition is generated.

- :constant :: Define a constant if non-nil.

- :local :: Make the variable permanently local if the value is
  `permanent'.  For other non-nil values make the variable
  buffer-local.

- :min-version :: Install the definition if the Emacs version is
  greater or equal than the given version.

- :max-version :: Install the definition if the Emacs version is
  smaller than the given version.

- :feature :: Wrap the definition with `with-eval-after-load'.

- :cond :: Install the definition if :cond evaluates to non-nil."
  (declare (debug (name form stringp [&rest keywordp sexp]))
           (doc-string 3) (indent 2))
  (compat--guarded-definition attrs '(:local :constant)
    (lambda (local constant)
      ;; The boundp check is performed at runtime to make sure that we never
      ;; redefine an existing definition if Compat is loaded on a newer Emacs
      ;; version.
      `((unless (boundp ',name)
          (,(if constant 'defconst 'defvar)
           ,name ,initval
           ,(compat--format-docstring 'variable name docstring))
          ,@(cond
             ((eq local 'permanent)
              `((put ',name 'permanent-local t)))
             (local
              `((make-variable-buffer-local ',name)))))))))

(provide 'compat-macs)
;;; compat-macs.el ends here
