;;; compat.el --- Emacs Lisp Compatibility Library -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>, Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>, Compat Development <~pkal/compat-devel@lists.sr.ht>
;; Version: 29.1.1.0
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
;; manual for details.  The main audience for this library are not
;; regular users, but package maintainers.  Therefore commands and
;; user options are usually not implemented here.

;;; Code:

(when (eval-when-compile (< emacs-major-version 29))
  (require 'compat-29))

;;;; Macros for explicit compatibility function calls

(defmacro compat-function (fun)
  "Return compatibility function symbol for FUN.

If the Emacs version provides a sufficiently recent version of
FUN, the symbol FUN is returned itself."
  (let ((compat (intern (format "compat--%s" fun))))
    `#',(if (fboundp compat) compat fun)))

(defmacro compat-call (fun &rest args)
  "Call compatibility function or macro FUN with ARGS.

See `compat-function' for the compatibility function resolution."
  (let ((compat (intern (format "compat--%s" fun))))
    `(,(if (fboundp compat) compat fun) ,@args)))

;;;; Emacs 27 (Conditionally defined functions)

;; TODO Maybe the functions should be moved to a separate file compat-cond.el,
;; which will be always loaded? However this file maybe empty, so maybe the best
;; place for these functions is indeed here. Conditionally-defined functions are
;; a special complicated edge case, which need more testing. Therefore the json
;; functions are currently marked as untested.

(eval-when-compile (load "compat-macs.el" nil t t))
(compat-declare-version "27.1")

;;;;; Defined in json.c

(declare-function json-serialize nil (object &rest args))
(declare-function json-encode "json" (object))
(declare-function json-read-from-string "json" (string))
(declare-function json-read "json" ())
(defvar json-encoding-pretty-print)
(defvar json-object-type)
(defvar json-array-type)
(defvar json-false)
(defvar json-null)

(compat-defun json-serialize (object &rest args) ;; <UNTESTED>
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
  :cond (not (condition-case nil
                 (equal (json-serialize '()) "{}")
               (:success t)
               (void-function nil)
               (json-unavailable nil)))
  (unless (fboundp 'json-encode)
    (require 'json))
  (letrec ((fix (lambda (obj)
                  (cond
                   ((hash-table-p obj)
                    (let ((ht (copy-hash-table obj)))
                      (maphash
                       (lambda (key val)
                         (unless (stringp key)
                           (signal
                            'wrong-type-argument
                            (list 'stringp key)))
                         (puthash key (funcall fix val) ht))
                       obj)
                      ht))
                   ((and (listp obj) (consp (car obj))) ;alist
                    (mapcar
                     (lambda (ent)
                       (cons (symbol-name (car ent))
                             (funcall fix (cdr ent))))
                     obj))
                   ((listp obj) ;plist
                    (let (alist)
                      (while obj
                        (push (cons (cond
                                     ((keywordp (car obj))
                                      (substring
                                       (symbol-name (car obj))
                                       1))
                                     ((symbolp (car obj))
                                      (symbol-name (car obj)))
                                     ((signal
                                       'wrong-type-argument
                                       (list 'symbolp (car obj)))))
                                    (funcall fix (cadr obj)))
                              alist)
                        (unless (consp (cdr obj))
                          (signal 'wrong-type-argument '(consp nil)))
                        (setq obj (cddr obj)))
                      (nreverse alist)))
                   ((vectorp obj)
                    (let ((vec (make-vector (length obj) nil)))
                      (dotimes (i (length obj))
                        (aset vec i (funcall fix (aref obj i))))
                      vec))
                   (obj))))
           (json-encoding-pretty-print nil)
           (json-false (or (plist-get args :false-object) :false))
           (json-null (or (plist-get args :null-object) :null)))
    (json-encode (funcall fix object))))

(compat-defun json-insert (object &rest args) ;; <UNTESTED>
  "Insert the JSON representation of OBJECT before point.
This is the same as (insert (json-serialize OBJECT)), but potentially
faster.  See the function `json-serialize' for allowed values of
OBJECT."
  :cond (not (condition-case nil
                 (equal (json-serialize '()) "{}")
               (:success t)
               (void-function nil)
               (json-unavailable nil)))
  (insert (apply #'json-serialize object args)))

(compat-defun json-parse-string (string &rest args) ;; <UNTESTED>
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
  :cond (not (condition-case nil
                 (equal (json-serialize '()) "{}")
               (:success t)
               (void-function nil)
               (json-unavailable nil)))
  (unless (fboundp 'json-read-from-string)
    (require 'json))
  (condition-case err
      (let ((json-object-type (or (plist-get args :object-type) 'hash-table))
            (json-array-type (or (plist-get args :array-type) 'vector))
            (json-false (or (plist-get args :false-object) :false))
            (json-null (or (plist-get args :null-object) :null)))
        (when (eq json-array-type 'array)
          (setq json-array-type 'vector))
        (json-read-from-string string))
    (json-error (signal 'json-parse-error err))))

(compat-defun json-parse-buffer (&rest args) ;; <UNTESTED>
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
  :cond (not (condition-case nil
                 (equal (json-serialize '()) "{}")
               (:success t)
               (void-function nil)
               (json-unavailable nil)))
  (unless (fboundp 'json-read)
    (require 'json))
  (condition-case err
      (let ((json-object-type (or (plist-get args :object-type) 'hash-table))
            (json-array-type (or (plist-get args :array-type) 'vector))
            (json-false (or (plist-get args :false-object) :false))
            (json-null (or (plist-get args :null-object) :null)))
        (when (eq json-array-type 'array)
          (setq json-array-type 'vector))
        (json-read))
    (json-error (signal 'json-parse-buffer err))))

(provide 'compat)
;;; compat.el ends here
