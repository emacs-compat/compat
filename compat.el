;;; compat.el --- Compatibility Library              -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <philipk@posteo.net>
;; Version: 28.1.0.0-rc
;; URL: https://git.sr.ht/~pkal/compat/
;; Package-Requires: ((emacs "24.1"))
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
;; that are installed ONLY if necessary.  These reimplementations of
;; functions and macros are at least subsets of the actual
;; implementations.  Be sure to read the documentation string to make
;; sure.
;;
;; Not every function provided in newer versions of Emacs is provided
;; here.  Some depend on new features from the core, others cannot be
;; implemented to a meaningful degree.  The main audience for this
;; library are not regular users, but package maintainers.  Therefore
;; commands and user options are usually not implemented here.

;;; Code:

(eval-when-compile (require 'compat-macs))

;;;; Core functionality

(declare-function ad-is-active "advice" (function))
(declare-function advice--p "nadvice" (func))
(declare-function advice--car "nadvice" (func))

;; The implementation is extracted here so that compatibility advice
;; can check if the right number of arguments are being handled.
(defun compat-func-arity (func &optional handle-advice)
  "A reimplementation of `func-arity' for FUNC.
If HANDLE-ADVICE is non-nil, return the effective arity of the
advice."
  (cond
   ((or (null func) (and (symbolp func) (not (fboundp func))) )
    (signal 'void-function func))
   ((and handle-advice
         (featurep 'nadvice)
         (advice--p func))
    (let* ((adv (advice--car (symbol-function func)))
           (arity (compat-func-arity adv)))
      (cons (1- (car arity))
            (if (numberp (cdr arity))
                (1- (cdr arity))
              (cdr arity)))))
   ((and handle-advice (get func 'compat-advice-fn))
    ;; Handle manual advising:
    (let* ((adv (get func 'compat-advice-fn))
           (arity (compat-func-arity adv)))
      (cons (1- (car arity))
            (if (numberp (cdr arity))
                (1- (cdr arity))
              (cdr arity)))))
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

(eval-and-compile
  (defun compat-maxargs-/= (func n)
    "Non-nil when FUNC doesn't accept at most N arguments."
    (condition-case nil
        (not (eq (cdr (compat-func-arity func t)) n))
      (void-function t))))

;; Suppress errors triggered by requiring non-existent libraries in
;; older versions of Emacs (e.g. subr-x).
(compat-advise require (feature &optional filename noerror)
  "Avoid throwing an error if library has compatibility code."
  ;; As the compatibility advise around `require` is more a hack than
  ;; of of actual value, the highlighting is suppressed.
  :no-highlight t
  (condition-case err
      (funcall oldfun feature filename)
    (file-missing
     (let ((entry (assq feature after-load-alist)))
       (when (and entry
                  (get feature 'setup-deferred-p)
                  (null noerror))
         (signal (car err) (cdr err)))
       (let ((load-file-name nil))
         (dolist (form (cdr entry))
           (funcall (eval form t))))))
    (error
     (unless noerror
       (signal (car err) (cdr err))))))

;; Load the actual compatibility definitions:
(require 'compat-24.4)
(require 'compat-25.1)
(require 'compat-26.1)
(require 'compat-27.1)
(require 'compat-28.1)

;;;; Etcetera

;; To ensure that compat.el is loaded as soon as possible, a require
;; call is inserted directly into the autoload file:
;;;###autoload (require 'compat)

(provide 'compat)
;;; compat.el ends here
