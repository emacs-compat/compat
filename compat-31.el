;;; compat-31.el --- Functionality added in Emacs 31 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

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

;; Functionality added in Emacs 31, needed by older Emacs versions.

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
(compat-require compat-30 "30.1")

;; TODO Update to 31.1 as soon as the Emacs emacs-31 branch version bumped
(compat-version "31.0.50")

;;;; Defined in subr.el

(compat-defun oddp (integer) ;; <compat-tests:oddp>
  "Return t if INTEGER is odd."
  (not (eq (% integer 2) 0)))

(compat-defun evenp (integer) ;; <compat-tests:evenp>
  "Return t if INTEGER is even."
  (eq (% integer 2) 0))

(compat-defun plusp (number) ;; <compat-tests:plusp>
  "Return t if NUMBER is positive."
  (> number 0))

(compat-defun minusp (number) ;; <compat-tests:minusp>
  "Return t if NUMBER is negative."
  (< number 0))

(compat-defmacro incf (place &optional delta) ;; <compat-tests:incf>
  "Increment PLACE by DELTA (default to 1).

The DELTA is first added to PLACE, and then stored in PLACE.
Return the incremented value of PLACE.

See also `decf'."
  `(setf ,place (+ ,place (or ,delta 1))))

(compat-defmacro decf (place &optional delta) ;; <compat-tests:decf>
  "Decrement PLACE by DELTA (default to 1).

The DELTA is first subtracted from PLACE, and then stored in PLACE.
Return the decremented value of PLACE.

See also `incf'."
  `(setf ,place (- ,place (or ,delta 1))))

(provide 'compat-31)
;;; compat-31.el ends here
