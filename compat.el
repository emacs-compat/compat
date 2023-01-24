;;; compat.el --- Emacs Lisp Compatibility Library -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>, Compat Development <~pkal/compat-devel@lists.sr.ht>
;; Version: 29.1.3.0
;; URL: https://github.com/emacs-compat/compat
;; Package-Requires: ((emacs "24.4"))
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

;; To allow for the usage of Emacs functions and macros that are
;; defined in newer versions of Emacs, compat.el provides definitions
;; that are installed ONLY if necessary.  If Compat is installed on a
;; recent version of Emacs, all of the definitions are disabled at
;; compile time, such that no negative performance impact is incurred.
;; These reimplementations of functions and macros are at least
;; subsets of the actual implementations.  Be sure to read the
;; documentation string to make sure.
;;
;; Not every function provided in newer versions of Emacs is provided
;; here.  Some depend on new features from the core, others cannot be
;; implemented to a meaningful degree.  Please consult the Compat
;; manual for details regarding the usage of the Compat library and
;; the provided functionality.  The main audience for this library are
;; not regular users, but package maintainers.  Therefore no commands,
;; user-facing modes or user options are implemented here.

;;; Code:

(when (eval-when-compile (< emacs-major-version 29))
  (require 'compat-29))

;;;; Macros for extended compatibility function calls

(defmacro compat-function (fun)
  "Return compatibility function symbol for FUN.

If the Emacs version provides a sufficiently recent version of
FUN, the symbol FUN is returned itself.  Otherwise the macro
returns the symbol of a compatibility function which supports the
behavior and calling convention of the current stable Emacs
version.  For example Compat 29.1 will provide compatibility
functions which implement the behavior and calling convention of
Emacs 29.1.

See also `compat-call' to directly call compatibility functions."
  (let ((compat (intern (format "compat--%s" fun))))
    `#',(if (fboundp compat) compat fun)))

(defmacro compat-call (fun &rest args)
  "Call compatibility function or macro FUN with ARGS.

A good example function is `plist-get' which was extended with an
additional predicate argument in Emacs 29.1.  The compatibility
function, which supports this additional argument, can be
obtained via (compat-function plist-get) and called
via (compat-call plist-get plist prop predicate).  It is not
possible to directly call (plist-get plist prop predicate) on
Emacs older than 29.1, since the original `plist-get' function
does not yet support the predicate argument.  Note that the
Compat library never overrides existing functions.

See also `compat-function' to lookup compatibility functions."
  (let ((compat (intern (format "compat--%s" fun))))
    `(,(if (fboundp compat) compat fun) ,@args)))

;;;; Backported libjansson API

(unless (eval-when-compile (ignore-errors (eval '(json-parse-string "0") t)))
  (defvar json-null)
  (defvar json-false)
  (defvar json-array-type)
  (defvar json-object-type)
  (defvar json-key-type)
  (declare-function json-read nil)

  (declare-function compat--json--print nil)
  (unless (eval-when-compile (ignore-errors (eval '(json-parse-string "[]") t)))
    (defun compat--json--print (obj)
      (cond
       ((numberp obj) (prin1 obj))
       ((eq obj t) (insert "true"))
       ((eq obj json-null) (insert "null"))
       ((eq obj json-false) (insert "false"))
       ((not obj) (insert "{}"))
       ((stringp obj)
        (insert ?\")
        (goto-char (prog1 (point) (princ obj)))
        (while (re-search-forward "[\"\\[:cntrl:]]" nil 'move)
          (let ((char (preceding-char)))
            (delete-char -1)
            (insert ?\\ (or (car (rassq char
                                        '((?\" . ?\")
                                          (?\\ . ?\\)
                                          (?b . ?\b)
                                          (?f . ?\f)
                                          (?n . ?\n)
                                          (?r . ?\r)
                                          (?t . ?\t))))
                            (format "u%04x" char)))))
        (insert ?\"))
       ((hash-table-p obj)
        (insert ?\{)
        (let ((first t))
          (maphash
           (lambda (key val)
             (unless (stringp key)
               (signal 'wrong-type-argument `(stringp ,key)))
             (if first (setq first nil) (insert ?,))
             (compat--json--print key)
             (insert ?:)
             (compat--json--print val))
           obj))
        (insert ?\}))
       ((and (car-safe obj) (symbolp (car obj))) ;; plist
        (insert ?\{)
        (let ((head obj))
          (while obj
            (unless (and (car obj) (symbolp (car obj)))
              (signal 'wrong-type-argument `(symbolp ,obj)))
            (unless (cdr obj)
              (signal 'wrong-type-argument `(consp ,(cdr obj))))
            (unless (eq obj head) (insert ?,))
            (compat--json--print
             (if (keywordp (car obj))
                 (substring (symbol-name (car obj)) 1)
               (symbol-name (car obj))))
            (insert ?:)
            (compat--json--print (cadr obj))
            (setq obj (cddr obj))))
        (insert ?\}))
       ((consp (car-safe obj)) ;; alist
        (insert ?\{)
        (let ((head obj))
          (while obj
            (unless (and (caar obj) (symbolp (caar obj)))
              (signal 'wrong-type-argument `(symbolp ,(caar obj))))
            (unless (eq obj head) (insert ?,))
            (compat--json--print (symbol-name (caar obj)))
            (insert ?:)
            (compat--json--print (cdar obj))
            (pop obj)))
        (insert ?\}))
       ((vectorp obj)
        (insert ?\[)
        (dotimes (i (length obj))
          (when (> i 0) (insert ?,))
          (compat--json--print (aref obj i)))
        (insert ?\]))
       (t (signal 'wrong-type-argument `(vectorp ,obj))))))

  (defun compat--json-serialize (object &rest args) ;; <compat-tests:json-serialize>
    "Return the JSON representation of OBJECT as a string.

OBJECT must be t, a number, string, vector, hashtable, alist, plist,
or the Lisp equivalents to the JSON null and false values, and its
elements must recursively consist of the same kinds of values.  t will
be converted to the JSON true value.  Vectors will be converted to
JSON arrays, whereas hashtables, alists and plists are converted to
JSON objects.  Hashtable keys must be strings without embedded null
characters and must be unique within each object.  Alist and plist
keys must be symbols; if a key is duplicate, the first instance is
used.

The Lisp equivalents to the JSON null and false values are
configurable in the arguments ARGS, a list of keyword/argument pairs:

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'.

In you specify the same value for `:null-object' and `:false-object',
a potentially ambiguous situation, the JSON output will not contain
any JSON false values."
    (if (eval-when-compile (ignore-errors (eval '(json-parse-string "[]") t)))
        (if (or (listp object) (vectorp object))
            (apply 'json-serialize object args)
          (substring (apply 'json-serialize (vector object) args) 1 -1))
      (let ((json-false (if (plist-member args :false-object)
                            (plist-get args :false-object)
                          :false))
            (json-null (if (plist-member args :null-object)
                            (plist-get args :null-object)
                         :null)))
        (with-output-to-string
          (with-current-buffer standard-output
            (compat--json--print object))))))

  (defun compat--json-insert (object &rest args) ;; <compat-tests:json-insert>
    "Insert the JSON representation of OBJECT before point.
This is the same as (insert (json-serialize OBJECT)), but potentially
faster.  See the function `json-serialize' for allowed values of
OBJECT."
    (if (eval-when-compile (ignore-errors (eval '(json-parse-string "[]") t)))
        (if (or (listp object) (vectorp object))
            (apply 'json-insert object args)
          (insert (substring (apply 'json-serialize (vector object) args) 1 -1)))
      (let ((json-false (if (plist-member args :false-object)
                            (plist-get args :false-object)
                          :false))
            (json-null (if (plist-member args :null-object)
                            (plist-get args :null-object)
                         :null))
            (standard-output (current-buffer)))
        (compat--json--print object))))

  (defun compat--json-parse-buffer (&rest args) ;; <compat-tests:json-parse-buffer>
    "Read JSON object from current buffer starting at point.
Move point after the end of the object if parsing was successful.
On error, don't move point.

The returned object will be a vector, list, hashtable, alist, or
plist.  Its elements will be the JSON null value, the JSON false
value, t, numbers, strings, or further vectors, lists, hashtables,
alists, or plists.  If there are duplicate keys in an object, all
but the last one are ignored.

If the current buffer doesn't contain a valid JSON object, the
function signals an error of type `json-parse-error'.

The arguments ARGS are a list of keyword/argument pairs:

The keyword argument `:object-type' specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `hash-table'.

The keyword argument `:array-type' specifies which Lisp type is used
to represent arrays; it can be `array' (the default) or `list'.

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
    (if (eval-when-compile (ignore-errors (eval '(json-parse-string "[]") t)))
        (save-match-data
          (if (looking-at "\\s-*\\([^[{[:space:]]+\\)")
              (let ((str (match-string 1)))
                (goto-char (match-end 0))
                (apply 'compat--json-parse-string str args))
            (apply 'json-parse-buffer args)))
      (unless (fboundp 'json-read)
        (require 'json))
      (let ((json-key-type nil)
            (json-object-type (or (plist-get args :object-type) 'hash-table))
            (json-array-type (or (plist-get args :array-type) 'array))
            (json-false (if (plist-member args :false-object)
                            (plist-get args :false-object)
                          :false))
            (json-null (if (plist-member args :null-object)
                            (plist-get args :null-object)
                          :null)))
        (when (eq json-array-type 'array)
          (setq json-array-type 'vector))
      (json-read))))

  (defun compat--json-parse-string (string &rest args) ;; <compat-tests:json-parse-string>
    "Parse the JSON STRING into a Lisp object.
This is essentially the reverse operation of `json-serialize', which
see.  The returned object will be the JSON null value, the JSON false
value, t, a number, a string, a vector, a list, a hashtable, an alist,
or a plist.  Its elements will be further objects of these types.  If
there are duplicate keys in an object, all but the last one are
ignored.  If STRING doesn't contain a valid JSON object, this function
signals an error of type `json-parse-error'.

The arguments ARGS are a list of keyword/argument pairs:

The keyword argument `:object-type' specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `hash-table'.

The keyword argument `:array-type' specifies which Lisp type is used
to represent arrays; it can be `array' (the default) or `list'.

The keyword argument `:null-object' specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The keyword argument `:false-object' specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
    (if (eval-when-compile (ignore-errors (eval '(json-parse-string "[]") t)))
        (if (string-match-p "\\`\\s-*[[{]" string)
            (apply 'json-parse-string string args)
          ;; Add array wrapper and extract first element, in order to
          ;; support RFC 8259. The older RFC 4627 implemented by
          ;; `json-parse-string' did not support parsing toplevel atoms.
          (elt (apply 'json-parse-string (concat "[" string "]") args) 0))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        ;; Do not use `json-read-from-string' here, since it also creates a
        ;; temporary buffer.
        (prog1 (apply 'compat--json-parse-buffer args)
          (skip-chars-forward "[:space:]")
          (unless (eobp)
            (signal 'json-error "Trailing content after JSON stream")))))))

(provide 'compat)
;;; compat.el ends here
