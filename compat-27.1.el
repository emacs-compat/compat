;;; compat-27.1.el --- Compatibility Layer for Emacs 27.1  -*- lexical-binding: t; -*-

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

;; Find here the functionality added in Emacs 27.1, needed by older
;; versions.
;;
;; Do NOT load this library manually.  Instead require `compat'.

;;; Code:

(eval-when-compile (require 'compat-macs))
(declare-function compat-maxargs-/= "compat" (func n))

;;;; Defined in fns.c

(compat-defun proper-list-p (object)
  "Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr
is nil)."
  :min-version "26.1"
  :max-version "26.3"
  :realname compat--proper-list-p-length-signal
  (condition-case nil
      (length object)
    (wrong-type-argument nil)
    (circular-list nil)))

(compat-defun proper-list-p (object)
  "Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr
is nil)."
  :max-version "25.3"
  :realname compat--proper-list-p-tortoise-hare
  (when (listp object)
    (catch 'cycle
      (let ((hare object) (tortoise object)
            (max 2) (q 2) )
        (while (consp hare)
          (setq hare (cdr hare))
          (when (and (or (/= 0 (setq q (1- q)))
                         (ignore
                          (setq max (ash max 1)
                                q max
                                tortoise hare)))
                     (eq hare tortoise))
            (throw 'cycle nil)))
        (and (null hare) (length object))))))

(compat-defun string-distance (string1 string2 &optional bytecompare)
  "Return Levenshtein distance between STRING1 and STRING2.
The distance is the number of deletions, insertions, and substitutions
required to transform STRING1 into STRING2.
If BYTECOMPARE is nil or omitted, compute distance in terms of characters.
If BYTECOMPARE is non-nil, compute distance in terms of bytes.
Letter-case is significant, but text properties are ignored."
  ;; https://en.wikipedia.org/wiki/Levenshtein_distance
  (let ((s1 (if bytecompare
                (encode-coding-string string1 'raw-text)
              (concat string1 "")))
        (s2 (if bytecompare
                (encode-coding-string string2 'raw-text)
              string2)))
    (let* ((len1 (length s1))
           (len2 (length s2))
           (column (make-vector (1+ len1) 0)))
      (dotimes (y len1)
        (setf (aref column (1+ y)) y))
      (dotimes (x len2)
        (setf (aref column 0) (1+ x))
        (let ((lastdiag x) olddiag)
          (dotimes (y len1)
            (setf olddiag (aref column (1+ y))
                  (aref column (1+ y))
                  (min (+ (if (= (aref s1 y) (aref s2 x)) 0 1)
                          lastdiag)
                       (1+ (aref column (1+ y)))
                       (1+ (aref column y)))
                  lastdiag olddiag))))
      (aref column len1))))

;;;; Defined in window.c

(compat-advise recenter (&optional arg redisplay)
  "Handle optional argument REDISPLAY."
  (funcall oldfun arg)
  (when (and redisplay recenter-redisplay)
    (redisplay)))

;;;; Defined in subr.el

(compat-advise setq-local (&rest pairs)
  "Handle multiple assignments."
  :cond (compat-maxargs-/= #'setq-local 'many)
  (declare (debug setq))
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable/value members"))
  (let (body)
    (while pairs
      (let* ((sym (pop pairs))
             (val (pop pairs)))
        (unless (symbolp sym)
          (error "Attempting to set a non-symbol: %s" (car pairs)))
        (push `(set (make-local-variable ,sym) ,val)
              body)))
    (cons 'progn (nreverse body))))

(compat-defmacro ignore-error (condition &rest body)
  "Execute BODY; if the error CONDITION occurs, return nil.
Otherwise, return result of last form in BODY.

CONDITION can also be a list of error conditions."
  (declare (debug t) (indent 1))
  `(condition-case nil (progn ,@body) (,condition nil)))

(compat-defmacro dolist-with-progress-reporter (spec reporter-or-message &rest body)
  "Loop over a list and report progress in the echo area.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

REPORTER-OR-MESSAGE is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

At each iteration, print the reporter message followed by progress
percentage in the echo area.  After the loop is finished,
print the reporter message followed by the word \"done\".

\(fn (VAR LIST [RESULT]) REPORTER-OR-MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  (let ((prep (make-symbol "--dolist-progress-reporter--"))
        (count (make-symbol "--dolist-count--"))
        (list (make-symbol "--dolist-list--")))
    `(let ((,prep ,reporter-or-message)
           (,count 0)
           (,list ,(cadr spec)))
       (when (stringp ,prep)
         (setq ,prep (make-progress-reporter ,prep 0 (1- (length ,list)))))
       (dolist (,(car spec) ,list)
         ,@body
         (progress-reporter-update ,prep (setq ,count (1+ ,count))))
       (progress-reporter-done ,prep)
       (or ,@(cdr (cdr spec)) nil))))

(compat-defun flatten-tree (tree)
  "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.

\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7)"
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

(compat-defun xor (cond1 cond2)
  "Return the boolean exclusive-or of COND1 and COND2.
If only one of the arguments is non-nil, return it; otherwise
return nil."
  (declare (pure t) (side-effect-free error-free))
  (cond ((not cond1) cond2)
        ((not cond2) cond1)))

(compat-defvar regexp-unmatchable "\\`a\\`"
  "Standard regexp guaranteed not to match any string at all."
  :constant t)

;;;; Defined in files.el

(compat-advise file-size-human-readable (file-size &optional flavor space unit)
  "Handle the optional third and forth argument:

Optional third argument SPACE is a string put between the number and unit.
It defaults to the empty string.  We recommend a single space or
non-breaking space, unless other constraints prohibit a space in that
position.

Optional fourth argument UNIT is the unit to use.  It defaults to \"B\"
when FLAVOR is `iec' and the empty string otherwise.  We recommend \"B\"
in all cases, since that is the standard symbol for byte."
  (let ((power (if (or (null flavor) (eq flavor 'iec))
                   1024.0
                 1000.0))
        (prefixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr prefixes))
      (setq file-size (/ file-size power)
            prefixes (cdr prefixes)))
    (let* ((prefix (car prefixes))
           (prefixed-unit (if (eq flavor 'iec)
                              (concat
                               (if (string= prefix "k") "K" prefix)
                               (if (string= prefix "") "" "i")
                               (or unit "B"))
                            (concat prefix unit))))
      (format (if (and (>= (mod file-size 1.0) 0.05)
                       (< (mod file-size 1.0) 0.95))
                  "%.1f%s%s"
                "%.0f%s%s")
              file-size
              (if (string= prefixed-unit "") "" (or space ""))
              prefixed-unit))))

;; TODO provide advice for directory-files-recursively

;;;; Defined in format-spec.el

;; TODO provide advice for format-spec

;;;; Defined in regexp-opt.el

(compat-advise regexp-opt (strings &optional paren)
  "Handle an empty list of strings."
  (if (null strings)
      (let ((re "\\`a\\`"))
        (cond ((null paren)
               (concat "\\(?:" re "\\)"))
              ((stringp paren)
               (concat paren re "\\)"))
              ((eq paren 'words)
               (concat "\\<\\(" re "\\)\\>"))
              ((eq paren 'symbols)
               (concat "\\_\\(<" re "\\)\\_>"))
              ((concat "\\(" re "\\)"))))
    (funcall oldfun strings paren)))

;;;; Defined in package.el

(declare-function lm-header "lisp-mnt")

(compat-defun package-get-version ()
  "Return the version number of the package in which this is used.
Assumes it is used from an Elisp file placed inside the top-level directory
of an installed ELPA package.
The return value is a string (or nil in case we can’t find it)."
  ;; In a sense, this is a lie, but it does just what we want: precompute
  ;; the version at compile time and hardcodes it into the .elc file!
  (declare (pure t))
  ;; Hack alert!
  (let ((file
         (or (and (boundp 'byte-compile-current-file) byte-compile-current-file)
             load-file-name
             buffer-file-name)))
    (cond
     ((null file) nil)
     ;; Packages are normally installed into directories named "<pkg>-<vers>",
     ;; so get the version number from there.
     ((string-match
       "/[^/]+-\\([0-9]\\(?:[0-9.]\\|pre\\|beta\\|alpha\\|snapshot\\)+\\)/[^/]+\\'"
       file)
      (match-string 1 file))
     ;; For packages run straight from the an elpa.git clone, there's no
     ;; "-<vers>" in the directory name, so we have to fetch the version
     ;; the hard way.
     ((let* ((pkgdir (file-name-directory file))
             (pkgname (file-name-nondirectory (directory-file-name pkgdir)))
             (mainfile (expand-file-name (concat pkgname ".el") pkgdir)))
        (when (file-readable-p mainfile)
          (require 'lisp-mnt)
          (with-temp-buffer
            (insert-file-contents mainfile)
            (or (lm-header "package-version")
                (lm-header "version")))))))))

(provide 'compat-27.1)
;;; compat-27.1.el ends here
