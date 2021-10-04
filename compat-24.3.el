;;; compat-24.3.el --- Compatibility Layer for Emacs 24.3  -*- lexical-binding: t; -*-

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

;; Find here the functionality added in Emacs 24.3, needed by older
;; versions.
;;
;; Do NOT load this library manually.  Instead require `compat'.

;;; Code:

(eval-and-compile (require 'compat-macs))

;;;; Defined in subr.el

(compat-defmacro defvar-local (var val &optional docstring)
  "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
  (declare (debug defvar) (doc-string 3))
  `(progn
     (defvar ,var ,val ,docstring)
     (make-variable-buffer-local ',var)))

(provide 'compat-24.3)
;;; compat-24.3.el ends here
