;;; compat-24.el --- Compatibility Layer for Emacs 24.4  -*- lexical-binding: t; -*-

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

;; Find here the functionality added in Emacs 24.4, needed by older
;; versions.
;;
;; Do NOT load this library manually.  Instead require `compat'.

;;; Code:

(eval-when-compile (require 'compat-macs))

;;;; Defined in data.c

(compat-defun = (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :version "24.4"
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (= number-or-marker (car numbers-or-markers))
        (throw 'fail nil))
      (setq number-or-marker (pop numbers-or-markers)))
    t))

(compat-defun < (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :version "24.4"
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (< number-or-marker (car numbers-or-markers))
        (throw 'fail nil))
      (setq number-or-marker (pop numbers-or-markers)))
    t))

(compat-defun > (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :version "24.4"
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (> number-or-marker (car numbers-or-markers))
        (throw 'fail nil))
      (setq number-or-marker (pop numbers-or-markers)))
    t))

(compat-defun <= (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :version "24.4"
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (<= number-or-marker (car numbers-or-markers))
        (throw 'fail nil))
      (setq number-or-marker (pop numbers-or-markers)))
    t))

(compat-defun >= (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :version "24.4"
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (>= number-or-marker (pop numbers-or-markers))
        (throw 'fail nil)))
    t))

;;;; Defined in subr.el

(compat-defmacro with-eval-after-load (file &rest body)
  "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See `eval-after-load'
for more details about the different forms of FILE and their semantics."
  :version "24.4"
  (declare (indent 1) (debug (form def-body)))
  ;; See https://nullprogram.com/blog/2018/02/22/ on how
  ;; `eval-after-load' is used to preserve compatibility with 24.3.
  `(eval-after-load ,file `(funcall ',,`(lambda () ,@body))))

(compat-defun special-form-p (object)
  "Non-nil if and only if OBJECT is a special form."
  :version "24.4"
  (if (and (symbolp object) (fboundp object))
      (setq object (condition-case nil
                       (indirect-function object)
                     (void-function nil))))
  (and (subrp object) (eq (cdr (subr-arity object)) 'unevalled)))

(compat-defun macrop (object)
  "Non-nil if and only if OBJECT is a macro."
  :version "24.4"
  (let ((def (condition-case nil
                 (indirect-function object)
               (void-function nil))))
    (when (consp def)
      (or (eq 'macro (car def))
          (and (autoloadp def) (memq (nth 4 def) '(macro t)))))))

(compat-defun string-suffix-p (suffix string  &optional ignore-case)
  "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  :version "24.4"
  (let ((start-pos (- (length string) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                string start-pos nil ignore-case)))))

(compat-defun split-string (string &optional separators omit-nulls trim)
  "Extend `split-string' by a TRIM argument.
The remaining arguments STRING, SEPARATORS and OMIT-NULLS are
handled just as with `split-string'."
  :version "24.4"
  :prefix t
  (let* ((token (split-string string separators omit-nulls))
         (trimmed (if trim
                      (mapcar
                       (lambda (token)
                         (when (string-match (concat "\\`" trim) token)
                           (setq token (substring token (match-end 0))))
                         (when (string-match (concat trim "\\'") token)
                           (setq token (substring token 0 (match-beginning 0))))
                         token)
                       token)
                    token)))
    (if omit-nulls (delete "" trimmed) trimmed)))

(compat-defun delete-consecutive-dups (list &optional circular)
  "Destructively remove `equal' consecutive duplicates from LIST.
First and last elements are considered consecutive if CIRCULAR is
non-nil."
  :version "24.4"
  (let ((tail list) last)
    (while (cdr tail)
      (if (equal (car tail) (cadr tail))
          (setcdr tail (cddr tail))
        (setq last tail
              tail (cdr tail))))
    (if (and circular
             last
             (equal (car tail) (car list)))
        (setcdr last nil)))
  list)

(compat-defun define-error (name message &optional parent)
  "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
  :version "24.4"
  (unless parent (setq parent 'error))
  (let ((conditions
         (if (consp parent)
             (apply #'append
                    (mapcar (lambda (parent)
                              (cons parent
                                    (or (get parent 'error-conditions)
                                        (error "Unknown signal `%s'" parent))))
                            parent))
           (cons parent (get parent 'error-conditions)))))
    (put name 'error-conditions
         (delete-dups (copy-sequence (cons name conditions))))
    (when message (put name 'error-message message))))

;;;; Defined in subr-x.el

(compat-advise require (feature &rest args)
  "Allow for Emacs 24.x to require the inexistent FEATURE subr-x."
  :version "24.4"
  ;; As the compatibility advise around `require` is more a hack than
  ;; of of actual value, the highlighting is suppressed.
  :no-highlight t
  (if (eq feature 'subr-x)
      (let ((entry (assq feature after-load-alist)))
        (let ((load-file-name nil))
          (dolist (form (cdr entry))
            (funcall (eval form t)))))
    (apply oldfun feature args)))

(compat-defun hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  :version "24.4"
  (let (values)
    (maphash
     (lambda (k _v) (push k values))
     hash-table)
    values))

(compat-defun hash-table-values (hash-table)
  "Return a list of values in HASH-TABLE."
  :version "24.4"
  (let (values)
    (maphash
     (lambda (_k v) (push v values))
     hash-table)
    values))

(compat-defun string-empty-p (string)
  "Check whether STRING is empty."
  :version "24.4"
  (string= string ""))

(compat-defun string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR.
Optional argument SEPARATOR must be a string, a vector, or a list of
characters; nil stands for the empty string."
  :version "24.4"
  (mapconcat #'identity strings separator))

(compat-defun string-blank-p (string)
  "Check whether STRING is either empty or only whitespace.
The following characters count as whitespace here: space, tab, newline and
carriage return."
  :version "24.4"
  (string-match-p "\\`[ \t\n\r]*\\'" string))

(compat-defun string-remove-prefix (prefix string)
  "Remove PREFIX from STRING if present."
  :version "24.4"
  (if (string-prefix-p prefix string)
      (substring string (length prefix))
    string))

(compat-defun string-remove-suffix (suffix string)
  "Remove SUFFIX from STRING if present."
  :version "24.4"
  (if (string-suffix-p suffix string)
      (substring string 0 (- (length string) (length suffix)))
    string))

(provide 'compat-24)
;;; compat-24.el ends here
