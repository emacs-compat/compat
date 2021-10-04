;;; compat-24.2.el --- Compatibility Layer for Emacs 24.2  -*- lexical-binding: t; -*-

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

;; Find here the functionality added in Emacs 24.2, needed by older
;; versions.
;;
;; Do NOT load this library manually.  Instead require `compat'.

;;; Code:

(eval-and-compile (require 'compat-macs))

;;;; Defined in subr.el

(compat-defun autoloadp (object)
  "Non-nil if OBJECT is an autoload."
  (eq 'autoload (car-safe object)))

(compat-defun buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (- (point-max) (point-min)) (buffer-size)))

(compat-defun posnp (obj)
  "Return non-nil if OBJ appears to be a valid position.
A `posn' object is returned from functions such as `event-start'.
If OBJ is a valid `posn' object, but specifies a frame rather
than a window, return nil."
  (and (windowp (car-safe obj))
       (atom (car-safe (setq obj (cdr obj))))                ;AREA-OR-POS.
       (integerp (car-safe (car-safe (setq obj (cdr obj))))) ;XOFFSET.
       (integerp (car-safe (cdr obj)))))

;;;; Defined in files.el

(compat-defun file-name-base (filename)
  "Return the base name of the FILENAME: no directory, no extension."
  (file-name-sans-extension
   (file-name-nondirectory (or filename (buffer-file-name)))))

(provide 'compat-24.2)
;;; compat-24.2.el ends here
