;;; compat-24.4.el --- Compatibility Layer for Emacs 24.4  -*- lexical-binding: t; -*-

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
(declare-function compat-maxargs-/= "compat" (func n))

;;;; Defined in data.c

(compat-defun = (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (= number-or-marker (car numbers-or-markers))
        (throw 'fail nil))
      (setq number-or-marker (pop numbers-or-markers)))
    t))

(compat-defun < (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (< number-or-marker (car numbers-or-markers))
        (throw 'fail nil))
      (setq number-or-marker (pop numbers-or-markers)))
    t))

(compat-defun > (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (> number-or-marker (car numbers-or-markers))
        (throw 'fail nil))
      (setq number-or-marker (pop numbers-or-markers)))
    t))

(compat-defun <= (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
  :prefix t
  (catch 'fail
    (while numbers-or-markers
      (unless (<= number-or-marker (car numbers-or-markers))
        (throw 'fail nil))
      (setq number-or-marker (pop numbers-or-markers)))
    t))

(compat-defun >= (number-or-marker &rest numbers-or-markers)
  "Handle multiple arguments."
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
  (declare (indent 1) (debug (form def-body)))
  ;; See https://nullprogram.com/blog/2018/02/22/ on how
  ;; `eval-after-load' is used to preserve compatibility with 24.3.
  `(eval-after-load ,file `(funcall ',,`(lambda () ,@body))))

(compat-defun special-form-p (object)
  "Non-nil if and only if OBJECT is a special form."
  (if (and (symbolp object) (fboundp object))
      (setq object (condition-case nil
                       (indirect-function object)
                     (void-function nil))))
  (and (subrp object) (eq (cdr (subr-arity object)) 'unevalled)))

(compat-defun macrop (object)
  "Non-nil if and only if OBJECT is a macro."
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
  (let ((start-pos (- (length string) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                string start-pos nil ignore-case)))))

(compat-defun split-string (string &optional separators omit-nulls trim)
  "Extend `split-string' by a TRIM argument.
The remaining arguments STRING, SEPARATORS and OMIT-NULLS are
handled just as with `split-string'."
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

(provide 'compat-24.4)
;;; compat-24.4.el ends here
