;;; compat.el --- Compatibility Library              -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <philipk@posteo.net>
;; Version: 28.1.0.0-dev
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

;; Suppress errors triggered by requiring non-existent libraries in
;; older versions of Emacs (e.g. subr-x).
(compat-advise require (feature &optional filename noerror)
  "Avoid throwing an error if library has compatibility code."
  ;; As the compatibility advise around `require` is more a hack than
  ;; of of actual value, the highlighting is supressed.
  :no-highlight t
  :force t
  (condition-case err
      (funcall oldfun feature filename noerror)
    (file-missing
     ;; FIXME: avoid false negatives, check if compat defined a
     ;;        feature.
     (unless (assq feature after-load-alist)
       (signal (car err) (cdr err))))))

;; Load the actual compatibility definitions:
(when (version< emacs-version "24.4")
  (require 'compat-24.4))
(when (version< emacs-version "25")
  (require 'compat-25.1))
(when (version< emacs-version "26")
  (require 'compat-26.1))
(when (version< emacs-version "27")
  (require 'compat-27.1))
(when (version< emacs-version "28")
  (require 'compat-28.1))

;;;; Etcetera

;; ;;;; Ensure NonGNU ELPA is in `package-archives'
;; (with-eval-after-load 'package
;;   (unless (or (rassoc "https://elpa.nongnu.org/nongnu/" package-archives)
;;            (rassoc "http://elpa.nongnu.org/nongnu/" package-archives)
;;            (rassoc "https://elpa.nongnu.org/nongnu-devel/" package-archives)
;;            (rassoc "http://elpa.nongnu.org/nongnu-devel/" package-archives))
;;     (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))))

;; To ensure that compat.el is loaded as soon as possible, a require
;; call is inserted directly into the autoload file:
;;;###autoload (require 'compat)

(provide 'compat)
;;; compat.el ends here
