;;; compat-27.el --- Compatibility Layer for Emacs 27.1  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
(compat-declare-version "27.1")

;;;; Defined in fns.c

(compat-defun proper-list-p (object) ;; <compat-tests:proper-list-p>
  "Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr
is nil)."
  (if (eval-when-compile (< emacs-major-version 26))
      ;; On older Emacs than 26.1 use Tortoise and Hare algorithm
      (when (listp object)
        (catch 'cycle
          (let ((hare object) (tortoise object)
                (max 2) (q 2))
            (while (consp hare)
              (setq hare (cdr hare))
              (when (and (or (/= 0 (setq q (1- q)))
                             (ignore
                              (setq max (ash max 1)
                                    q max
                                    tortoise hare)))
                         (eq hare tortoise))
                (throw 'cycle nil)))
            (and (null hare) (length object)))))
    ;; Errors on 26.1 and newer
    (and (listp object) (ignore-errors (length object)))))

(compat-defun string-distance (string1 string2 &optional bytecompare) ;; <compat-tests:string-distance>
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

(compat-defun recenter (&optional arg redisplay) ;; <compat-tests:recenter>
  "Handle optional argument REDISPLAY."
  :explicit t
  (recenter arg)
  (when (and redisplay recenter-redisplay)
    (redisplay)))

;;;; Defined in keymap.c

(compat-defun lookup-key (keymap key &optional accept-default) ;; <compat-tests:lookup-key>
  "Allow for KEYMAP to be a list of keymaps."
  :explicit t
  (cond
   ((keymapp keymap)
    (lookup-key keymap key accept-default))
   ((listp keymap)
    (catch 'found
      (dolist (map keymap)
        (let ((fn (lookup-key map key accept-default)))
          (when fn (throw 'found fn))))))
   ((signal 'wrong-type-argument (list 'keymapp keymap)))))

;;;; Defined in timefns.c

(compat-defun time-equal-p (t1 t2) ;; <compat-tests:time-equal-p>
  "Return non-nil if time value T1 is equal to time value T2.
A nil value for either argument stands for the current time.

NOTE: This function is not as accurate as the actual `time-equal-p'."
  (cond
   ((eq t1 t2))
   ((and (consp t1) (consp t2))
    (equal t1 t2))
   (t
    ;; Due to inaccuracies and the relatively slow evaluating of
    ;; Emacs Lisp compared to C, we allow for slight inaccuracies
    ;; (less than a millisecond) when comparing time values.
    (< (abs (- (float-time t1) (float-time t2)))
       (if (and t1 t2) 1e-6 1e-5)))))

;;;; Defined in fileio.c

(compat-defun file-name-absolute-p (filename) ;; <compat-tests:file-name-absolute-p>
  "Return t if FILENAME is an absolute file name.
On Unix, absolute file names start with `/'.  In Emacs, an absolute
file name can also start with an initial `~' or `~USER' component,
where USER is a valid login name."
  ;; See definitions in filename.h
  (let ((drive
         (eval-when-compile
           (cond
            ((memq system-type '(windows-nt ms-dos))
             "\\`[A-Za-z]:[\\/]")
            ((eq system-type 'cygwin)
             "\\`\\([\\/]\\|[A-Za-z]:\\)")
            ("\\`/"))))
        (home
         (eval-when-compile
           (if (memq system-type '(cygwin windows-nt ms-dos))
               "\\`~[\\/]" "\\`~/")))
        (user-home
         (eval-when-compile
           (format "\\`\\(~.*?\\)\\(%s.*\\)?$"
                   (if (memq system-type '(cygwin windows-nt ms-dos))
                       "[\\/]" "/")))))
    (or (and (string-match-p drive filename) t)
        (and (string-match-p home filename) t)
        (save-excursion
          (when (string-match user-home filename)
            (let ((init (match-string 1 filename)))
              (not (string=
                    (file-name-base (expand-file-name init))
                    init))))))))

;;;; Defined in subr.el

(compat-defalias fixnump integerp) ;; <compat-tests:fixnump>
(compat-defalias bignump ignore) ;; <compat-tests:bignump>

(compat-defmacro setq-local (&rest pairs) ;; <compat-tests:setq-local>
  "Handle multiple assignments."
  :explicit t
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable/value members"))
  (let (body)
    (while pairs
      (let* ((sym (pop pairs))
             (val (pop pairs)))
        (unless (symbolp sym)
          (error "Attempting to set a non-symbol: %s" (car pairs)))
        (push `(set (make-local-variable ',sym) ,val)
              body)))
    (cons 'progn (nreverse body))))

(compat-defun provided-mode-derived-p (mode &rest modes) ;; <compat-tests:derived-mode-p>
  "Non-nil if MODE is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards.
If you just want to check `major-mode', use `derived-mode-p'."
  ;; If MODE is an alias, then look up the real mode function first.
  (let ((alias (symbol-function mode)))
    (when (and alias (symbolp alias))
      (setq mode alias)))
  (while
      (and
       (not (memq mode modes))
       (let* ((parent (get mode 'derived-mode-parent))
              (parentfn (symbol-function parent)))
         (setq mode (if (and parentfn (symbolp parentfn)) parentfn parent)))))
  mode)

(compat-defun derived-mode-p (&rest modes) ;; <compat-tests:derived-mode-p>
  "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
  (apply #'provided-mode-derived-p major-mode modes))

(compat-defmacro ignore-error (condition &rest body) ;; <compat-tests:ignore-error>
  "Execute BODY; if the error CONDITION occurs, return nil.
Otherwise, return result of last form in BODY.

CONDITION can also be a list of error conditions."
  (declare (debug t) (indent 1))
  `(condition-case nil (progn ,@body) (,condition nil)))

(compat-defmacro dolist-with-progress-reporter (spec reporter-or-message &rest body) ;; <compat-tests:dolist-with-progress-reporter>
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
         (setq ,prep (make-progress-reporter ,prep 0 (length ,list))))
       (dolist (,(car spec) ,list)
         ,@body
         (progress-reporter-update ,prep (setq ,count (1+ ,count))))
       (progress-reporter-done ,prep)
       (or ,@(cdr (cdr spec)) nil))))

(compat-defun flatten-tree (tree) ;; <compat-tests:flatten-tree>
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

(compat-defun xor (cond1 cond2) ;; <compat-tests:xor>
  "Return the boolean exclusive-or of COND1 and COND2.
If only one of the arguments is non-nil, return it; otherwise
return nil."
  (declare (pure t) (side-effect-free error-free))
  (cond ((not cond1) cond2)
        ((not cond2) cond1)))

(compat-defvar regexp-unmatchable "\\`a\\`" ;; <compat-tests:regexp-unmatchable>
  "Standard regexp guaranteed not to match any string at all."
  :constant t)

(compat-defun assoc-delete-all (key alist &optional test) ;; <compat-tests:assoc-delete-all>
  "Handle optional argument TEST."
  :explicit t
  (unless test (setq test #'equal))
  (while (and (consp (car alist))
              (funcall test (caar alist) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
               (funcall test (caar tail-cdr) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail tail-cdr))))
  alist)

;;;; Defined in simple.el

(compat-defun decoded-time-second (time) ;; <compat-tests:decoded-time-accessors>
  "The seconds in TIME, which is a value returned by `decode-time'.
This is an integer between 0 and 60 (inclusive).  (60 is a leap
second, which only some operating systems support.)"
  (nth 0 time))

(compat-defun decoded-time-minute (time) ;; <compat-tests:decoded-time-accessors>
  "The minutes in TIME, which is a value returned by `decode-time'.
This is an integer between 0 and 59 (inclusive)."
  (nth 1 time))

(compat-defun decoded-time-hour (time) ;; <compat-tests:decoded-time-accessors>
  "The hours in TIME, which is a value returned by `decode-time'.
This is an integer between 0 and 23 (inclusive)."
  (nth 2 time))

(compat-defun decoded-time-day (time) ;; <compat-tests:decoded-time-accessors>
  "The day-of-the-month in TIME, which is a value returned by `decode-time'.
This is an integer between 1 and 31 (inclusive)."
  (nth 3 time))

(compat-defun decoded-time-month (time) ;; <compat-tests:decoded-time-accessors>
  "The month in TIME, which is a value returned by `decode-time'.
This is an integer between 1 and 12 (inclusive).  January is 1."
  (nth 4 time))

(compat-defun decoded-time-year (time) ;; <compat-tests:decoded-time-accessors>
  "The year in TIME, which is a value returned by `decode-time'.
This is a four digit integer."
  (nth 5 time))

(compat-defun decoded-time-weekday (time) ;; <compat-tests:decoded-time-accessors>
  "The day-of-the-week in TIME, which is a value returned by `decode-time'.
This is a number between 0 and 6, and 0 is Sunday."
  (nth 6 time))

(compat-defun decoded-time-dst (time) ;; <compat-tests:decoded-time-accessors>
  "The daylight saving time in TIME, which is a value returned by `decode-time'.
This is t if daylight saving time is in effect, and nil if not."
  (nth 7 time))

(compat-defun decoded-time-zone (time) ;; <compat-tests:decoded-time-accessors>
  "The time zone in TIME, which is a value returned by `decode-time'.
This is an integer indicating the UTC offset in seconds, i.e.,
the number of seconds east of Greenwich."
  (nth 8 time))

(when (eval-when-compile (< emacs-major-version 27))
  (gv-define-setter decoded-time-second (v x)  `(setcar (nthcdr 0 ,x) ,v)) ;; <compat-tests:decoded-time-accessors>
  (gv-define-setter decoded-time-minute (v x)  `(setcar (nthcdr 1 ,x) ,v))
  (gv-define-setter decoded-time-hour (v x)    `(setcar (nthcdr 2 ,x) ,v))
  (gv-define-setter decoded-time-day (v x)     `(setcar (nthcdr 3 ,x) ,v))
  (gv-define-setter decoded-time-month (v x)   `(setcar (nthcdr 4 ,x) ,v))
  (gv-define-setter decoded-time-year (v x)    `(setcar (nthcdr 5 ,x) ,v))
  (gv-define-setter decoded-time-weekday (v x) `(setcar (nthcdr 6 ,x) ,v))
  (gv-define-setter decoded-time-dst (v x)     `(setcar (nthcdr 7 ,x) ,v))
  (gv-define-setter decoded-time-zone (v x)    `(setcar (nthcdr 8 ,x) ,v)))

;;;; Defined in minibuffer.el

(compat-defmacro with-minibuffer-selected-window (&rest body) ;; <compat-tests:with-minibuffer-selected-window>
  "Execute the forms in BODY from the minibuffer in its original window.
When used in a minibuffer window, select the window selected just before
the minibuffer was activated, and execute the forms."
  (declare (indent 0) (debug t))
  `(let ((window (minibuffer-selected-window)))
     (when window
       (with-selected-window window
         ,@body))))

;;;; Defined in image.el

(compat-defun image--set-property (image property value) ;; <compat-tests:image-property>
  "Set PROPERTY in IMAGE to VALUE.
Internal use only."
  :explicit t
  :feature image
  (if (null value)
      (while (cdr image)
        (if (eq (cadr image) property)
            (setcdr image (cdddr image))
          (setq image (cddr image))))
    (setcdr image (plist-put (cdr image) property value)))
  value)

(if (eval-when-compile (< emacs-major-version 26))
    (with-eval-after-load 'image
      (gv-define-simple-setter image-property image--set-property)) ;; <compat-tests:image-property>
  ;; HACK: image--set-property was broken with an off-by-one error on Emacs 26.
  ;; The bug was fixed in a4ad7bed187493c1c230f223b52c71f5c34f7c89. Therefore we
  ;; override the gv expander until Emacs 27.1.
  (when (eval-when-compile (< emacs-major-version 27))
    (with-eval-after-load 'image
      (gv-define-simple-setter image-property compat--image--set-property)))) ;; <compat-tests:image-property>

;;;; Defined in files.el

(compat-defun file-size-human-readable (file-size &optional flavor space unit) ;; <compat-tests:file-size-human-readable>
  "Handle the optional arguments SPACE and UNIT.

Optional third argument SPACE is a string put between the number and unit.
It defaults to the empty string.  We recommend a single space or
non-breaking space, unless other constraints prohibit a space in that
position.

Optional fourth argument UNIT is the unit to use.  It defaults to \"B\"
when FLAVOR is `iec' and the empty string otherwise.  We recommend \"B\"
in all cases, since that is the standard symbol for byte."
  :explicit t
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

(compat-defun exec-path () ;; <compat-tests:exec-path>
  "Return list of directories to search programs to run in remote subprocesses.
The remote host is identified by `default-directory'.  For remote
hosts that do not support subprocesses, this returns nil.
If `default-directory' is a local directory, this function returns
the value of the variable `exec-path'."
  (let ((handler (find-file-name-handler default-directory 'exec-path)))
    ;; NOTE: The handler may fail since it was added in 27.1.
    (or (and handler (ignore-errors (funcall handler 'exec-path)))
        (if (file-remote-p default-directory)
            ;; FIXME: Just return some standard path on remote
            '("/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin")
          exec-path))))

(compat-defun executable-find (command &optional remote) ;; <compat-tests:executable-find>
  "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'.  If
REMOTE is non-nil, search on the remote host indicated by
`default-directory' instead."
  :explicit t
  (if (and remote (file-remote-p default-directory))
      (let ((res (locate-file
                  command
                  (mapcar
                   (apply-partially
                    #'concat (file-remote-p default-directory))
                   (exec-path))
                  exec-suffixes 'file-executable-p)))
        (when (stringp res) (file-local-name res)))
    (executable-find command)))

(compat-defun make-empty-file (filename &optional parents) ;; <compat-tests:make-empty-file>
  "Create an empty file FILENAME.
Optional arg PARENTS, if non-nil then creates parent dirs as needed."
  (when (and (file-exists-p filename) (null parents))
    (signal 'file-already-exists (list "File exists" filename)))
  (let ((paren-dir (file-name-directory filename)))
    (when (and paren-dir (not (file-exists-p paren-dir)))
      (make-directory paren-dir parents)))
  (write-region "" nil filename nil 0))

;;;; Defined in regexp-opt.el

(compat-defun regexp-opt (strings &optional paren) ;; <compat-tests:regexp-opt>
  "Handle an empty list of STRINGS."
  :explicit t
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
    (regexp-opt strings paren)))

;;;; Defined in package.el

(declare-function lm-header "lisp-mnt")
(declare-function macroexp-file-name nil)

(compat-defun package-get-version () ;; <compat-tests:package-get-version>
  "Return the version number of the package in which this is used.
Assumes it is used from an Elisp file placed inside the top-level directory
of an installed ELPA package.
The return value is a string (or nil in case we can’t find it)."
  ;; No :feature since the function is autoloaded.
  ;; In a sense, this is a lie, but it does just what we want: precompute
  ;; the version at compile time and hardcodes it into the .elc file!
  (declare (pure t))
  ;; Hack alert!
  (let ((file (or (macroexp-file-name) buffer-file-name)))
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

;;;; Defined in dired.el

(compat-defun dired-get-marked-files ;; <compat-tests:dired-get-marked-files>
    (&optional localp arg filter distinguish-one-marked error)
  "Handle optional argument ERROR."
  :feature dired
  :explicit t
  (let ((result (dired-get-marked-files localp arg filter distinguish-one-marked)))
    (if (and (null result) error)
        (user-error (if (stringp error) error "No files specified"))
      result)))

;;;; Defined in time-date.el

(compat-defun date-days-in-month (year month) ;; <compat-tests:date-days-in-month>
  "The number of days in MONTH in YEAR."
  :feature time-date
  (unless (and (numberp month)
               (<= 1 month)
               (<= month 12))
    (error "Month %s is invalid" month))
  (if (= month 2)
      (if (date-leap-year-p year)
          29
        28)
    (if (memq month '(1 3 5 7 8 10 12))
        31
      30)))

;;;; Defined in text-property-search.el

(compat-defun make-prop-match (&rest attr) ;; <compat-tests:make-prop-match>
  "Constructor for objects of type ‘prop-match’."
  :feature text-property-search
  ;; Vector for older than 26.1, Record on newer Emacs.
  (funcall (eval-when-compile (if (< emacs-major-version 26) 'vector 'record))
           'prop-match
           (plist-get attr :beginning)
           (plist-get attr :end)
           (plist-get attr :value)))

(compat-defun prop-match-p (match) ;; <compat-tests:make-prop-match>
  "Return non-nil if MATCH is a `prop-match' object."
  :feature text-property-search
  ;; Vector for older than 26.1, Record on newer Emacs.
  (if (eval-when-compile (< emacs-major-version 26))
      (and (vectorp match)
           (> (length match) 0)
           (eq (aref match 0) 'prop-match))
    (eq (type-of match) 'prop-match)))

(compat-defun prop-match-beginning (match) ;; <compat-tests:make-prop-match>
  "Retrieve the position where MATCH begins."
  :feature text-property-search
  (aref match 1))

(compat-defun prop-match-end (match) ;; <compat-tests:make-prop-match>
  "Retrieve the position where MATCH ends."
  :feature text-property-search
  (aref match 2))

(compat-defun prop-match-value (match) ;; <compat-tests:make-prop-match>
  "Retrieve the value that MATCH holds."
  :feature text-property-search
  (aref match 3))

(compat-defun text-property-search-forward ;; <compat-tests:text-property-search-forward>
    (property &optional value predicate not-current)
  "Search for the next region of text where PREDICATE is true.
PREDICATE is used to decide whether a value of PROPERTY should be
considered as matching VALUE.

If PREDICATE is a function, it will be called with two arguments:
VALUE and the value of PROPERTY.  The function should return
non-nil if these two values are to be considered a match.

Two special values of PREDICATE can also be used:
If PREDICATE is t, that means a value must `equal' VALUE to be
considered a match.
If PREDICATE is nil (which is the default value), a value will
match if is not `equal' to VALUE.  Furthermore, a nil PREDICATE
means that the match region is ended if the value changes.  For
instance, this means that if you loop with

  (while (setq prop (text-property-search-forward \\='face))
    ...)

you will get all distinct regions with non-nil `face' values in
the buffer, and the `prop' object will have the details about the
match.  See the manual for more details and examples about how
VALUE and PREDICATE interact.

If NOT-CURRENT is non-nil, the function will search for the first
region that doesn't include point and has a value of PROPERTY
that matches VALUE.

If no matches can be found, return nil and don't move point.
If found, move point to the end of the region and return a
`prop-match' object describing the match.  To access the details
of the match, use `prop-match-beginning' and `prop-match-end' for
the buffer positions that limit the region, and
`prop-match-value' for the value of PROPERTY in the region."
  :feature text-property-search
  (let* ((match-p
          (lambda (prop-value)
            (funcall
             (cond
              ((eq predicate t)
               #'equal)
              ((eq predicate nil)
               (lambda (val p-val)
                 (not (equal val p-val))))
              (predicate))
             value prop-value)))
         (find-end
          (lambda (start)
            (let (end)
              (if (and value
                       (null predicate))
                  ;; This is the normal case: We're looking for areas where the
                  ;; values aren't, so we aren't interested in sub-areas where the
                  ;; property has different values, all non-matching value.
                  (let ((ended nil))
                    (while (not ended)
                      (setq end (next-single-property-change (point) property))
                      (if (not end)
                          (progn
                            (goto-char (point-max))
                            (setq end (point)
                                  ended t))
                        (goto-char end)
                        (unless (funcall match-p (get-text-property (point) property))
                          (setq ended t)))))
                ;; End this at the first place the property changes value.
                (setq end (next-single-property-change (point) property nil (point-max)))
                (goto-char end))
              (make-prop-match
               :beginning start
               :end end
               :value (get-text-property start property))))))
    (cond
     ;; No matches at the end of the buffer.
     ((eobp)
      nil)
     ;; We're standing in the property we're looking for, so find the
     ;; end.
     ((and (funcall match-p (get-text-property (point) property))
           (not not-current))
      (funcall find-end (point)))
     (t
      (let ((origin (point))
            (ended nil)
            pos)
        ;; Find the next candidate.
        (while (not ended)
          (setq pos (next-single-property-change (point) property))
          (if (not pos)
              (progn
                (goto-char origin)
                (setq ended t))
            (goto-char pos)
            (if (funcall match-p (get-text-property (point) property))
                (setq ended (funcall find-end (point)))
              ;; Skip past this section of non-matches.
              (setq pos (next-single-property-change (point) property))
              (unless pos
                (goto-char origin)
                (setq ended t)))))
        (and (not (eq ended t))
             ended))))))

(compat-defun text-property-search-backward ;; <compat-tests:text-property-search-backward>
    (property &optional value predicate not-current)
  "Search for the previous region of text whose PROPERTY matches VALUE.

Like `text-property-search-forward', which see, but searches backward,
and if a matching region is found, place point at the start of the region."
  :feature text-property-search
  (let* ((match-p
          (lambda (prop-value)
            (funcall
             (cond
              ((eq predicate t)
               #'equal)
              ((eq predicate nil)
               (lambda (val p-val)
                 (not (equal val p-val))))
              (predicate))
             value prop-value)))
         (find-end
          (lambda (start)
            (let (end)
              (if (and value
                       (null predicate))
                  ;; This is the normal case: We're looking for areas where the
                  ;; values aren't, so we aren't interested in sub-areas where the
                  ;; property has different values, all non-matching value.
                  (let ((ended nil))
                    (while (not ended)
                      (setq end (previous-single-property-change (point) property))
                      (if (not end)
                          (progn
                            (goto-char (point-min))
                            (setq end (point)
                                  ended t))
                        (goto-char (1- end))
                        (unless (funcall match-p (get-text-property (point) property))
                          (goto-char end)
                          (setq ended t)))))
                ;; End this at the first place the property changes value.
                (setq end (previous-single-property-change
                           (point) property nil (point-min)))
                (goto-char end))
              (make-prop-match
               :beginning end
               :end (1+ start)
               :value (get-text-property end property))))))
    (cond
     ;; We're at the start of the buffer; no previous matches.
     ((bobp)
      nil)
     ;; We're standing in the property we're looking for, so find the
     ;; end.
     ((funcall match-p (get-text-property (1- (point)) property))
      (let ((origin (point))
            (match (funcall find-end (1- (point)) property value predicate)))
        ;; When we want to ignore the current element, then repeat the
        ;; search if we haven't moved out of it yet.
        (if (and not-current
                 (equal (get-text-property (point) property)
                        (get-text-property origin property)))
            (text-property-search-backward property value predicate)
          match)))
     (t
      (let ((origin (point))
            (ended nil)
            pos)
        ;; Find the previous candidate.
        (while (not ended)
          (setq pos (previous-single-property-change (point) property))
          (if (not pos)
              (progn
                (goto-char origin)
                (setq ended t))
            (goto-char (1- pos))
            (if (funcall match-p (get-text-property (point) property))
                (setq ended
                      (funcall find-end (point)))
              ;; Skip past this section of non-matches.
              (setq pos (previous-single-property-change (point) property))
              (unless pos
                (goto-char origin)
                (setq ended t)))))
        (and (not (eq ended t))
             ended))))))

(provide 'compat-27)
;;; compat-27.el ends here
