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
  (gv-letplace (getter setter) place
    (funcall setter `(+ ,getter ,(or delta 1)))))

(compat-defmacro decf (place &optional delta) ;; <compat-tests:decf>
  "Decrement PLACE by DELTA (default to 1).

The DELTA is first subtracted from PLACE, and then stored in PLACE.
Return the decremented value of PLACE.

See also `incf'."
  (gv-letplace (getter setter) place
    (funcall setter `(- ,getter ,(or delta 1)))))

;;;; Defined in color.el

(compat-defun color-blend (a b &optional alpha) ;; <compat-tests:color-blend>
  "Blend the two colors A and B in linear space with ALPHA.
A and B should be lists (RED GREEN BLUE), where each element is
between 0.0 and 1.0, inclusive.  ALPHA controls the influence A
has on the result and should be between 0.0 and 1.0, inclusive.

For instance:

   (color-blend \\='(1 0.5 1) \\='(0 0 0) 0.75)
      => (0.75 0.375 0.75)"
  (setq alpha (or alpha 0.5))
  (let (blend)
    (dotimes (i 3)
      (push (+ (* (nth i a) alpha) (* (nth i b) (- 1 alpha))) blend))
    (nreverse blend)))

;;;; Defined in time-date.el

(compat-defvar seconds-to-string ;; <compat-tests:seconds-to-string>
  (list (list 1 "ms" 0.001)
        (list 100 "s" 1)
        (list (* 60 100) "m" 60.0)
        (list (* 3600 30) "h" 3600.0)
        (list (* 3600 24 400) "d" (* 3600.0 24.0))
        (list nil "y" (* 365.25 24 3600)))
  "Formatting used by the function `seconds-to-string'.")

(compat-defvar seconds-to-string-readable ;; <compat-tests:seconds-to-string>
  `(("Y" "year"   "years"   ,(round (* 60 60 24 365.2425)))
    ("M" "month"  "months"  ,(round (* 60 60 24 30.436875)))
    ("w" "week"   "weeks"   ,(* 60 60 24 7))
    ("d" "day"    "days"    ,(* 60 60 24))
    ("h" "hour"   "hours"   ,(* 60 60))
    ("m" "minute" "minutes" 60)
    ("s" "second" "seconds" 1))
  "Formatting used by the function `seconds-to-string' with READABLE set.
The format is an alist, with string keys ABBREV-UNIT, and elements like:

  (ABBREV-UNIT UNIT UNIT-PLURAL SECS)

where UNIT is a unit of time, ABBREV-UNIT is the abbreviated form of
UNIT, UNIT-PLURAL is the plural form of UNIT, and SECS is the number of
seconds per UNIT.")

(compat-defun seconds-to-string (delay &optional readable abbrev precision) ;; <compat-tests:seconds-to-string>
  "Handle optional arguments READABLE, ABBREV and PRECISION."
  :extended t
  (cond
   ((< delay 0)
    (concat "-" (seconds-to-string (- delay) readable precision)))
   (readable
    (let* ((stsa seconds-to-string-readable)
           (expanded (eq readable 'expanded))
           digits
           (round-to (cond
                      ((wholenump precision)
                       (setq digits precision)
                       (expt 10 (- precision)))
                      ((and (floatp precision) (< precision 1.))
                       (setq digits (- (floor (log precision 10))))
                       precision)
                      (t (setq digits 0) 1)))
           (dformat (if (> digits 0) (format "%%0.%df" digits)))
           (padding (if abbrev "" " "))
           here cnt cnt-pre here-pre cnt-val isfloatp)
      (if (= (round delay round-to) 0)
          (format "0%s" (if abbrev "s" " seconds"))
        (while (and (setq here (pop stsa)) stsa
                    (< (/ delay (nth 3 here)) 1)))
        (or (and
             expanded stsa 	; smaller unit remains
             (progn
               (setq
                here-pre here here (car stsa)
                cnt-pre (floor (/ (float delay) (nth 3 here-pre)))
                cnt (round
                     (/ (- (float delay) (* cnt-pre (nth 3 here-pre)))
                        (nth 3 here))
                     round-to))
               (if (> cnt 0) t (setq cnt cnt-pre here here-pre here-pre nil))))
            (setq cnt (round (/ (float delay) (nth 3 here)) round-to)))
        (setq cnt-val (* cnt round-to)
              isfloatp (and (> digits 0)
                            (> (- cnt-val (floor cnt-val)) 0.)))
        (cl-labels
            ((unit (val here &optional plural)
               (cond (abbrev (car here))
                     ((and (not plural) (<= (floor val) 1)) (nth 1 here))
                     (t (nth 2 here)))))
          (concat
           (when here-pre
             (concat (number-to-string cnt-pre) padding
                     (unit cnt-pre here-pre) " "))
           (if isfloatp (format dformat cnt-val)
             (number-to-string (floor cnt-val)))
           padding
           (unit cnt-val here isfloatp)))))) ; float formats are always plural
   ((= 0 delay) "0s")
   (t (let ((sts seconds-to-string) here)
        (while (and (car (setq here (pop sts)))
                    (<= (car here) delay)))
        (concat (format "%.2f" (/ delay (car (cddr here)))) (cadr here))))))

;;;; Defined in minibuffer.el

(compat-defun completion-list-candidate-at-point (&optional pt) ;; <compat-tests:completion-list-candidate-at-point>
  "Candidate string and bounds at PT in completions buffer.
The return value has the format (STR BEG END).
The optional argument PT defaults to (point)."
  (let ((pt (or pt (point))) beg end)
    (cond
     ((and (/= pt (point-max)) (get-text-property pt 'mouse-face))
      (setq end pt beg (1+ pt)))
     ((and (/= pt (point-min)) (get-text-property (1- pt) 'mouse-face))
      (setq end (1- pt) beg pt)))
    (when (and beg end)
      (setq beg (previous-single-property-change beg 'mouse-face))
      (setq end (or (next-single-property-change end 'mouse-face) (point-max)))
      (list (or (get-text-property beg 'completion--string)
                (buffer-substring beg end))
            beg end))))

(compat-defun completion-table-with-metadata (table metadata) ;; <compat-tests:completion-table-with-metadata>
  "Return new completion TABLE with METADATA.
METADATA should be an alist of completion metadata.  See
`completion-metadata' for a list of supported metadata."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata . ,metadata)
      (complete-with-action action table string pred))))

;;;; Defined in subr-x.el

(compat-defvar work-buffer--list nil ;; <compat-tests:with-work-buffer>
  "List of work buffers.")

(compat-defvar work-buffer-limit 10 ;; <compat-tests:with-work-buffer>
  "Maximum number of reusable work buffers.
When this limit is exceeded, newly allocated work buffers are
automatically killed, which means that in a such case
`with-work-buffer' becomes equivalent to `with-temp-buffer'.")

(compat-defun work-buffer--get () ;; <compat-tests:with-work-buffer>
  "Get a work buffer."
  (let ((buffer (pop work-buffer--list)))
    (if (buffer-live-p buffer)
        buffer
      ;; `generate-new-buffer' and `get-buffer-create' accept an
      ;; INHIBIT-BUFFER-HOOKS argument on Emacs 28 and newer.
      ;; Unfortunately it is hard or not possible to port this back. See
      ;; issue <compat-gh:42>.
      (static-if (>= emacs-major-version 28)
          (generate-new-buffer " *work*" t)
        (generate-new-buffer " *work*")))))

(compat-defun work-buffer--release (buffer) ;; <compat-tests:with-work-buffer>
  "Release work BUFFER."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t) deactivate-mark)
          (erase-buffer))
        (delete-all-overlays)
        (let (change-major-mode-hook)
          ;; TODO Port back the KILL-PERMANENT argument from Emacs 29
          ;; Right now permanent variables are not killed.
          (static-if (>= emacs-major-version 29)
              (kill-all-local-variables t)
            (kill-all-local-variables)))
        (push buffer work-buffer--list)))
  (when (> (length work-buffer--list) work-buffer-limit)
    (mapc #'kill-buffer (nthcdr work-buffer-limit work-buffer--list))
    (setq work-buffer--list (ntake work-buffer-limit work-buffer--list))))

(compat-defmacro with-work-buffer (&rest body) ;; <compat-tests:with-work-buffer>
  "Create a work buffer, and evaluate BODY there like `progn'.
Like `with-temp-buffer', but reuse an already created temporary
buffer when possible, instead of creating a new one on each call."
  (declare (indent 0) (debug t))
  (let ((work-buffer (make-symbol "work-buffer")))
    `(let ((,work-buffer (work-buffer--get)))
       (with-current-buffer ,work-buffer
         (unwind-protect
             (progn ,@body)
           (work-buffer--release ,work-buffer))))))

(provide 'compat-31)
;;; compat-31.el ends here
