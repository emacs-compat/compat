;;; compat-29.el --- Compatibility Layer for Emacs 29.1  -*- lexical-binding: t; -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

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

;; Find here the functionality added in Emacs 29.1, needed by older
;; versions.
;;
;; Do NOT load this library manually.  Instead require `compat'.

;;; Code:

(eval-when-compile (require 'compat-macs))

;;;; Defined in xdisp.c

(compat-defun get-display-property (position prop &optional object properties)
  "Get the value of the `display' property PROP at POSITION.
If OBJECT, this should be a buffer or string where the property is
fetched from.  If omitted, OBJECT defaults to the current buffer.

If PROPERTIES, look for value of PROP in PROPERTIES instead of
the properties at POSITION."
  (if properties
      (unless (listp properties)
        (signal 'wrong-type-argument (list 'listp properties)))
    (setq properties (get-text-property position 'display object)))
  (cond
   ((vectorp properties)
    (catch 'found
      (dotimes (i (length properties))
        (let ((ent (aref properties i)))
          (when (eq (car ent) prop)
            (throw 'found (cadr ent )))))))
   ((consp (car properties))
    (condition-case nil
        (cadr (assq prop properties))
      ;; Silently handle improper lists:
      (wrong-type-argument nil)))
   ((and (consp (cdr properties))
         (eq (car properties) prop))
    (cadr properties))))

;;* UNTESTED
(compat-defun buffer-text-pixel-size
    (&optional buffer-or-name window x-limit y-limit)
  "Return size of whole text of BUFFER-OR-NAME in WINDOW.
BUFFER-OR-NAME must specify a live buffer or the name of a live buffer
and defaults to the current buffer.  WINDOW must be a live window and
defaults to the selected one.  The return value is a cons of the maximum
pixel-width of any text line and the pixel-height of all the text lines
of the buffer specified by BUFFER-OR-NAME.

The optional arguments X-LIMIT and Y-LIMIT have the same meaning as with
`window-text-pixel-size'.

Do not use this function if the buffer specified by BUFFER-OR-NAME is
already displayed in WINDOW.  `window-text-pixel-size' is cheaper in
that case because it does not have to temporarily show that buffer in
WINDOW."
  :realname compat--buffer-text-pixel-size
  (setq buffer-or-name (or buffer-or-name (current-buffer)))
  (setq window (or window (selected-window)))
  (save-window-excursion
    (set-window-buffer window buffer-or-name)
    (window-text-pixel-size window nil nil x-limit y-limit)))

;;;; Defined in fns.c

(compat-defun ntake (n list)
  "Modify LIST to keep only the first N elements.
If N is zero or negative, return nil.
If N is greater or equal to the length of LIST, return LIST unmodified.
Otherwise, return LIST after truncating it."
  :realname compat--ntake-elisp
  (and (> n 0) (let ((cons (nthcdr (1- n) list)))
                 (when cons (setcdr cons nil))
                 list)))

(compat-defun take (n list)
  "Return the first N elements of LIST.
If N is zero or negative, return nil.
If N is greater or equal to the length of LIST, return LIST (or a copy)."
  (compat--ntake-elisp n (copy-sequence list)))      ;FIXME: only copy as much as necessary

(compat-defun string-equal-ignore-case (string1 string2)
  "Like `string-equal', but case-insensitive.
Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
  (declare (pure t) (side-effect-free t))
  (eq t (compare-strings string1 0 nil string2 0 nil t)))

(compat-defun plist-get (plist prop &optional predicate)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).

This function returns the value corresponding to the given PROP, or
nil if PROP is not one of the properties on the list.  The comparison
with PROP is done using PREDICATE, which defaults to `eq'.

This function doesn't signal an error if PLIST is invalid."
  :prefix t
  (if (or (null predicate) (eq predicate 'eq))
      (plist-get plist prop)
    (catch 'found
      (while (consp plist)
        (when (funcall predicate prop (car plist))
          (throw 'found (cadr plist)))
        (setq plist (cddr plist))))))

(compat-defun plist-put (plist prop val &optional predicate)
  "Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...).

The comparison with PROP is done using PREDICATE, which defaults to `eq'.

If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects."
  :prefix t
  (if (or (null predicate) (eq predicate 'eq))
      (plist-put plist prop val)
    (catch 'found
      (let ((tail plist))
        (while (consp tail)
          (when (funcall predicate prop (car tail))
            (setcar (cdr tail) val)
            (throw 'found plist))
          (setq tail (cddr tail))))
      (nconc plist (list prop val)))))

(compat-defun plist-member (plist prop &optional predicate)
  "Return non-nil if PLIST has the property PROP.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...).

The comparison with PROP is done using PREDICATE, which defaults to
`eq'.

Unlike `plist-get', this allows you to distinguish between a missing
property and a property with the value nil.
The value is actually the tail of PLIST whose car is PROP."
  :prefix t
  (if (or (null predicate) (eq predicate 'eq))
      (plist-member plist prop)
    (catch 'found
      (while (consp plist)
        (when (funcall predicate prop (car plist))
          (throw 'found plist))
        (setq plist (cddr plist))))))

;;;; Defined in subr.el

(compat-defun function-alias-p (func &optional noerror)
  "Return nil if FUNC is not a function alias.
If FUNC is a function alias, return the function alias chain.

If the function alias chain contains loops, an error will be
signalled.  If NOERROR, the non-loop parts of the chain is returned."
  (declare (side-effect-free t))
  (let ((chain nil)
        (orig-func func))
    (nreverse
     (catch 'loop
       (while (and (symbolp func)
                   (setq func (symbol-function func))
                   (symbolp func))
         (when (or (memq func chain)
                   (eq func orig-func))
           (if noerror
               (throw 'loop chain)
             (signal 'cyclic-function-indirection (list orig-func))))
         (push func chain))
       chain))))

;;;; Defined in subr-x.el

(compat-defun string-limit (string length &optional end coding-system)
  "Return a substring of STRING that is (up to) LENGTH characters long.
If STRING is shorter than or equal to LENGTH characters, return the
entire string unchanged.

If STRING is longer than LENGTH characters, return a substring
consisting of the first LENGTH characters of STRING.  If END is
non-nil, return the last LENGTH characters instead.

If CODING-SYSTEM is non-nil, STRING will be encoded before
limiting, and LENGTH is interpreted as the number of bytes to
limit the string to.  The result will be a unibyte string that is
shorter than LENGTH, but will not contain \"partial\" characters,
even if CODING-SYSTEM encodes characters with several bytes per
character.

When shortening strings for display purposes,
`truncate-string-to-width' is almost always a better alternative
than this function."
  :feature 'subr-x
  (unless (natnump length)
    (signal 'wrong-type-argument (list 'natnump length)))
  (if coding-system
      (let ((result nil)
            (result-length 0)
            (index (if end (1- (length string)) 0)))
        (while (let ((encoded (encode-coding-char
                               (aref string index) coding-system)))
                 (and (<= (+ (length encoded) result-length) length)
                      (progn
                        (push encoded result)
                        (setq result-length
                              (+ result-length (length encoded)))
                        (setq index (if end (1- index)
                                      (1+ index))))
                      (if end (> index -1)
                        (< index (length string)))))
          ;; No body.
          )
        (apply #'concat (if end result (nreverse result))))
    (cond
     ((<= (length string) length) string)
     (end (substring string (- (length string) length)))
     (t (substring string 0 length)))))

(provide 'compat-29)
;;; compat-29.el ends here
