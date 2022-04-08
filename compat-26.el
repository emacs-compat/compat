;;; compat-26.el --- Compatibility Layer for Emacs 26.1  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Compat Development <~pkal/compat-devel@lists.sr.ht>
;; URL: https://git.sr.ht/~pkal/compat/
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

;; Find here the functionality added in Emacs 26.1, needed by older
;; versions.
;;
;; Do NOT load this library manually.  Instead require `compat'.

;;; Code:

(eval-when-compile (require 'compat-macs))
(declare-function compat-func-arity "compat" (func))

;;;; Defined in eval.c

(compat-defun func-arity (func)
  "Return minimum and maximum number of args allowed for FUNC.
FUNC must be a function of some kind.
The returned value is a cons cell (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number, or the symbol ‘many’, for a
function with ‘&rest’ args, or ‘unevalled’ for a special form."
  (cond
   ((or (null func) (and (symbolp func) (not (fboundp func))))
    (signal 'void-function func))
   ((and (symbolp func) (not (null func)))
    (compat-func-arity (symbol-function func)))
   ((eq (car-safe func) 'macro)
    (compat-func-arity (cdr func)))
   ((subrp func)
    (subr-arity func))
   ((memq (car-safe func) '(closure lambda))
    ;; See lambda_arity from eval.c
    (when (eq (car func) 'closure)
      (setq func (cdr func)))
    (let ((syms-left (if (consp func)
                         (car func)
                       (signal 'invalid-function func)))
          (min-args 0) (max-args 0) optional)
      (catch 'many
        (dolist (next syms-left)
          (cond
           ((not (symbolp next))
            (signal 'invalid-function func))
           ((eq next '&rest)
            (throw 'many (cons min-args 'many)))
           ((eq next '&optional)
            (setq optional t))
           (t (unless optional
                (setq min-args (1+ min-args)))
              (setq max-args (1+ max-args)))))
        (cons min-args max-args))))
   ((and (byte-code-function-p func) (numberp (aref func 0)))
    ;; See get_byte_code_arity from bytecode.c
    (let ((at (aref func 0)))
      (cons (logand at 127)
            (if (= (logand at 128) 0)
                (ash at -8)
              'many))))
   ((and (byte-code-function-p func) (numberp (aref func 0)))
    ;; See get_byte_code_arity from bytecode.c
    (let ((at (aref func 0)))
      (cons (logand at 127)
            (if (= (logand at 128) 0)
                (ash at -8)
              'many))))
   ((and (byte-code-function-p func) (listp (aref func 0)))
    ;; Based on `byte-compile-make-args-desc', this is required for
    ;; old versions of Emacs that don't use a integer for the argument
    ;; list description, per e2abe5a13dffb08d6371b6a611bc39c3a9ac2bc6.
    (let ((arglist (aref func 0)) (mandatory 0) nonrest)
      (while (and arglist (not (memq (car arglist) '(&optional &rest))))
        (setq mandatory (1+ mandatory))
        (setq arglist (cdr arglist)))
      (setq nonrest mandatory)
      (when (eq (car arglist) '&optional)
        (setq arglist (cdr arglist))
        (while (and arglist (not (eq (car arglist) '&rest)))
          (setq nonrest (1+ nonrest))
          (setq arglist (cdr arglist))))
      (cons mandatory (if arglist 'many nonrest))))
   ((autoloadp func)
    (autoload-do-load func)
    (compat-func-arity func))
   ((signal 'invalid-function func))))

;;;; Defined in fns.c

(compat-defun assoc (key alist &optional testfn)
  "Handle the optional argument TESTFN.
Equality is defined by the function TESTFN, defaulting to
‘equal’.  TESTFN is called with 2 arguments: a car of an alist
element and KEY.  With no optional argument, the function behaves
just like `assoc'."
  :cond (condition-case nil
            (or (assoc nil nil #'ignore) t)
          (wrong-number-of-arguments nil))
  :prefix t
  (if testfn
      (catch 'found
        (dolist (ent alist)
          (when (funcall testfn (car ent) key)
            (throw 'found ent))))
    (assoc key alist)))

(compat-defun mapcan (func sequence)
  "Apply FUNC to each element of SEQUENCE.
Concatenate the results by altering them (using `nconc').
SEQUENCE may be a list, a vector, a boolean vector, or a string."
  (apply #'nconc (mapcar func sequence)))

;;* UNTESTED
(compat-defun line-number-at-pos (&optional position absolute)
  "Handle optional argument ABSOLUTE:

If the buffer is narrowed, the return value by default counts the lines
from the beginning of the accessible portion of the buffer.  But if the
second optional argument ABSOLUTE is non-nil, the value counts the lines
from the absolute start of the buffer, disregarding the narrowing."
  :prefix t
  (if absolute
      (save-restriction
        (widen)
        (line-number-at-pos position))
    (line-number-at-pos position)))

;;;; Defined in subr.el

(declare-function compat--alist-get-full-elisp "compat-25"
                  (key alist &optional default remove testfn))
(compat-defun alist-get (key alist &optional default remove testfn)
  "Handle TESTFN manually."
  :realname compat--alist-get-handle-testfn
  :prefix t
  (if testfn
      (compat--alist-get-full-elisp key alist default remove testfn)
    (alist-get key alist default remove)))

(compat-defun string-trim-left (string &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  :realname compat--string-trim-left
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") string)
      (substring string (match-end 0))
    string))

(compat-defun string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  :realname compat--string-trim-right
  (let ((i (string-match-p
            (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'")
            string)))
    (if i (substring string 0 i) string)))

(compat-defun string-trim (string &optional trim-left trim-right)
  "Trim STRING of leading with and trailing matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  ;; `string-trim-left' and `string-trim-right' were moved from subr-x
  ;; to subr in Emacs 27, so to avoid loading subr-x we use the
  ;; compatibility function here:
  (compat--string-trim-left
   (compat--string-trim-right
    string
    trim-right)
   trim-left))

(compat-defun caaar (x)
  "Return the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (car (car (car x))))

(compat-defun caadr (x)
  "Return the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (car (cdr x))))

(compat-defun cadar (x)
  "Return the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (cdr (car x))))

(compat-defun caddr (x)
  "Return the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (cdr x))))

(compat-defun cdaar (x)
  "Return the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (car (car x))))

(compat-defun cdadr (x)
  "Return the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (cdr x))))

(compat-defun cddar (x)
  "Return the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (car x))))

(compat-defun cdddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (cdr x))))

(compat-defun caaaar (x)
  "Return the `car' of the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (car (car (car (car x)))))

(compat-defun caaadr (x)
  "Return the `car' of the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (car (car (cdr x)))))

(compat-defun caadar (x)
  "Return the `car' of the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (car (cdr (car x)))))

(compat-defun caaddr (x)
  "Return the `car' of the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (car (cdr (cdr x)))))

(compat-defun cadaar (x)
  "Return the `car' of the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (car (cdr (car (car x)))))

(compat-defun cadadr (x)
  "Return the `car' of the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (car (cdr x)))))

(compat-defun caddar (x)
  "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (cdr (cdr (car x)))))

(compat-defun cadddr (x)
  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (cdr (cdr x)))))

(compat-defun cdaaar (x)
  "Return the `cdr' of the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (car (car (car x)))))

(compat-defun cdaadr (x)
  "Return the `cdr' of the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (car (cdr x)))))

(compat-defun cdadar (x)
  "Return the `cdr' of the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (car (cdr (car x)))))

(compat-defun cdaddr (x)
  "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (cdr (cdr x)))))

(compat-defun cddaar (x)
  "Return the `cdr' of the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (car (car x)))))

(compat-defun cddadr (x)
  "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (car (cdr x)))))

(compat-defun cdddar (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (cdr (car x)))))

(compat-defun cddddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (cdr (cdr x)))))

(compat-defvar gensym-counter 0
  "Number used to construct the name of the next symbol created by `gensym'.")

(compat-defun gensym (&optional prefix)
  "Return a new uninterned symbol.
The name is made by appending `gensym-counter' to PREFIX.
PREFIX is a string, and defaults to \"g\"."
  (let ((num (prog1 gensym-counter
               (setq gensym-counter
                     (1+ gensym-counter)))))
    (make-symbol (format "%s%d" (or prefix "g") num))))

;;;; Defined in files.el

(declare-function temporary-file-directory nil)

;;* UNTESTED
(compat-defun make-nearby-temp-file (prefix &optional dir-flag suffix)
  "Create a temporary file as close as possible to `default-directory'.
If PREFIX is a relative file name, and `default-directory' is a
remote file name or located on a mounted file systems, the
temporary file is created in the directory returned by the
function `temporary-file-directory'.  Otherwise, the function
`make-temp-file' is used.  PREFIX, DIR-FLAG and SUFFIX have the
same meaning as in `make-temp-file'."
  (let ((handler (find-file-name-handler
                  default-directory 'make-nearby-temp-file)))
    (if (and handler (not (file-name-absolute-p default-directory)))
        (funcall handler 'make-nearby-temp-file prefix dir-flag suffix)
      (let ((temporary-file-directory (temporary-file-directory)))
        (make-temp-file prefix dir-flag suffix)))))

(compat-defvar mounted-file-systems
    (eval-when-compile
      (if (memq system-type '(windows-nt cygwin))
          "^//[^/]+/"
        (concat
         "^" (regexp-opt '("/afs/" "/media/" "/mnt" "/net/" "/tmp_mnt/")))))
  "File systems that ought to be mounted.")

;;* UNTESTED
(compat-defun temporary-file-directory ()
  "The directory for writing temporary files.
In case of a remote `default-directory', this is a directory for
temporary files on that remote host.  If such a directory does
not exist, or `default-directory' ought to be located on a
mounted file system (see `mounted-file-systems'), the function
returns `default-directory'.
For a non-remote and non-mounted `default-directory', the value of
the variable `temporary-file-directory' is returned."
  (let ((handler (find-file-name-handler
                  default-directory 'temporary-file-directory)))
    (if handler
        (funcall handler 'temporary-file-directory)
      (if (string-match mounted-file-systems default-directory)
          default-directory
        temporary-file-directory))))

;;;; Defined in subr-x.el

(declare-function compat--when-let* "compat-25" (varlist &rest body))
(compat-defmacro and-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
Like `when-let*', except if BODY is empty and all the bindings
are non-nil, then the result is non-nil."
  :feature 'subr-x
  (declare (indent 1) (debug if-let*))
  `(compat--when-let* ,varlist ,@(or body '(t))))

(provide 'compat-26)
;;; compat-26.el ends here
