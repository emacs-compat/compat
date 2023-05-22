;;; compat-30.el --- Functionality added in Emacs 30 -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

;; Functionality added in Emacs 30, needed by older Emacs versions.

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
;; TODO Update to 29.1 as soon as the Emacs emacs-29 branch version bumped
(compat-require compat-29 "29.0.90")

;; TODO Update to 30.1 as soon as the Emacs emacs-30 branch version bumped
(compat-version "30.0.50")

;;;; Defined in subr.el

(compat-defun copy-tree (tree &optional vectors-and-records) ;; <compat-tests:copy-tree>
  "Handle copying records when optional arg is non-nil."
  :extended t
  (declare (side-effect-free error-free))
  (if (fboundp 'recordp)
      (if (consp tree)
          (let (result)
            (while (consp tree)
              (let ((newcar (car tree)))
                (if (or (consp (car tree))
                        (and vectors-and-records
                             (or (vectorp (car tree)) (recordp (car tree)))))
                    (setq newcar (compat--copy-tree (car tree) vectors-and-records)))
                (push newcar result))
              (setq tree (cdr tree)))
            (nconc (nreverse result)
                   (if (and vectors-and-records (or (vectorp tree) (recordp tree)))
                       (compat--copy-tree tree vectors-and-records)
                     tree)))
        (if (and vectors-and-records (or (vectorp tree) (recordp tree)))
            (let ((i (length (setq tree (copy-sequence tree)))))
              (while (>= (setq i (1- i)) 0)
                (aset tree i (compat--copy-tree (aref tree i) vectors-and-records)))
              tree)
          tree))
    (copy-tree tree vectors-and-records)))

(provide 'compat-30)
;;; compat-30.el ends here
