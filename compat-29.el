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
;; Only load this library if you need to use one of the following
;; functions:
;;
;; - `plist-get'
;; - `plist-put'
;; - `plist-member'
;; - `define-key'

;;; Code:

(eval-when-compile (require 'compat-macs))

(compat-declare-version "29.1")

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

;;;; Defined in keymap.c

(compat-defun define-key (keymap key def &optional remove)
  "In KEYMAP, define key sequence KEY as DEF.
This is a legacy function; see `keymap-set' for the recommended
function to use instead.

KEYMAP is a keymap.

KEY is a string or a vector of symbols and characters, representing a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be represented by vectors.
Two types of vector have special meanings:
 [remap COMMAND] remaps any key binding for COMMAND.
 [t] creates a default definition, which applies to any event with no
    other definition in KEYMAP.

DEF is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right) and
    STRING is the menu item name (which is used only if the containing
    keymap has been created with a menu name, see `make-keymap'),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node `(elisp)Extended Menu Items'.)

If REMOVE is non-nil, the definition will be removed.  This is almost
the same as setting the definition to nil, but makes a difference if
the KEYMAP has a parent, and KEY is shadowing the same binding in the
parent.  With REMOVE, subsequent lookups will return the binding in
the parent, and with a nil DEF, the lookups will return nil.

If KEYMAP is a sparse keymap with a binding for KEY, the existing
binding is altered.  If there is no binding for KEY, the new pair
binding KEY to DEF is added at the front of KEYMAP."
  :realname compat--define-key-with-remove
  :prefix t
  (if remove
      (let ((prev (lookup-key keymap key))
            (parent (memq 'key (cdr keymap)))
            fresh entry)
        (when prev
          ;; IMPROVEME: Kind of a hack to avoid relying on the specific
          ;; behaviour of how `define-key' changes KEY before inserting
          ;; it into the map.
          (define-key keymap key (setq fresh (make-symbol "fresh")))
          (setq entry (rassq fresh (cdr keymap)))
          (if (> (length (memq entry (cdr keymap)))
                 (length parent))
              ;; Ensure that we only remove an element in the current
              ;; keymap and not a parent, by ensuring that `entry' is
              ;; located before `parent'.
              (ignore (setcdr keymap (delq entry (cdr keymap))))
            (define-key keymap key prev))))
    (define-key keymap key def)))

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

(declare-function compat--provided-mode-derived-p
                  "compat-27" (mode &rest modes))
(declare-function compat--func-arity
                  "compat-26" (func))

;;* UNTESTED
(compat-defun buffer-match-p (condition buffer-or-name &optional arg)
  "Return non-nil if BUFFER-OR-NAME matches CONDITION.
CONDITION is either:
- the symbol t, to always match,
- the symbol nil, which never matches,
- a regular expression, to match a buffer name,
- a predicate function that takes a buffer object and ARG as
  arguments, and returns non-nil if the buffer matches,
- a cons-cell, where the car describes how to interpret the cdr.
  The car can be one of the following:
  * `derived-mode': the buffer matches if the buffer's major mode
    is derived from the major mode in the cons-cell's cdr.
  * `major-mode': the buffer matches if the buffer's major mode
    is eq to the cons-cell's cdr.  Prefer using `derived-mode'
    instead when both can work.
  * `not': the cdr is interpreted as a negation of a condition.
  * `and': the cdr is a list of recursive conditions, that all have
    to be met.
  * `or': the cdr is a list of recursive condition, of which at
    least one has to be met."
  :realname compat--buffer-match-p
  (letrec
      ((buffer (get-buffer buffer-or-name))
       (match
        (lambda (conditions)
          (catch 'match
            (dolist (condition conditions)
              (when (cond
                     ((eq condition t))
                     ((stringp condition)
                      (string-match-p condition (buffer-name buffer)))
                     ((functionp condition)
                      (if (eq 1 (cdr (compat--func-arity condition)))
                          (funcall condition buffer)
                        (funcall condition buffer arg)))
                     ((eq (car-safe condition) 'major-mode)
                      (eq
                       (buffer-local-value 'major-mode buffer)
                       (cdr condition)))
                     ((eq (car-safe condition) 'derived-mode)
                      (compat--provided-mode-derived-p
                       (buffer-local-value 'major-mode buffer)
                       (cdr condition)))
                     ((eq (car-safe condition) 'not)
                      (not (funcall match (cdr condition))))
                     ((eq (car-safe condition) 'or)
                      (funcall match (cdr condition)))
                     ((eq (car-safe condition) 'and)
                      (catch 'fail
                        (dolist (c (cdr conditions))
                          (unless (funcall match c)
                            (throw 'fail nil)))
                        t)))
                (throw 'match t)))))))
    (funcall match (list condition))))

;;* UNTESTED
(compat-defun match-buffers (condition &optional buffers arg)
  "Return a list of buffers that match CONDITION.
See `buffer-match' for details on CONDITION.  By default all
buffers are checked, this can be restricted by passing an
optional argument BUFFERS, set to a list of buffers to check.
ARG is passed to `buffer-match', for predicate conditions in
CONDITION."
  (let (bufs)
    (dolist (buf (or buffers (buffer-list)))
      (when (compat--buffer-match-p condition (get-buffer buf) arg)
        (push buf bufs)))
    bufs))

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

;;* UNTESTED
(compat-defun string-pixel-width (string)
  "Return the width of STRING in pixels."
  (if (zerop (length string))
      0
    ;; Keeping a work buffer around is more efficient than creating a
    ;; new temporary buffer.
    (with-current-buffer (get-buffer-create " *string-pixel-width*")
      (delete-region (point-min) (point-max))
      (insert string)
      (car (compat--buffer-text-pixel-size nil nil t)))))

;;* UNTESTED
(compat-defmacro with-buffer-unmodified-if-unchanged (&rest body)
  "Like `progn', but change buffer-modified status only if buffer text changes.
If the buffer was unmodified before execution of BODY, and
buffer text after execution of BODY is identical to what it was
before, ensure that buffer is still marked unmodified afterwards.
For example, the following won't change the buffer's modification
status:

  (with-buffer-unmodified-if-unchanged
    (insert \"a\")
    (delete-char -1))

Note that only changes in the raw byte sequence of the buffer text,
as stored in the internal representation, are monitored for the
purpose of detecting the lack of changes in buffer text.  Any other
changes that are normally perceived as \"buffer modifications\", such
as changes in text properties, `buffer-file-coding-system', buffer
multibyteness, etc. -- will not be noticed, and the buffer will still
be marked unmodified, effectively ignoring those changes."
  (declare (debug t) (indent 0))
  (let ((hash (make-symbol "hash"))
        (buffer (make-symbol "buffer")))
    `(let ((,hash (and (not (buffer-modified-p))
                       (buffer-hash)))
           (,buffer (current-buffer)))
       (prog1
           (progn
             ,@body)
         ;; If we didn't change anything in the buffer (and the buffer
         ;; was previously unmodified), then flip the modification status
         ;; back to "unchanged".
         (when (and ,hash (buffer-live-p ,buffer))
           (with-current-buffer ,buffer
             (when (and (buffer-modified-p)
                        (equal ,hash (buffer-hash)))
               (restore-buffer-modified-p nil))))))))

;;;; Defined in files.el

(compat-defun file-parent-directory (filename)
  "Return the directory name of the parent directory of FILENAME.
If FILENAME is at the root of the filesystem, return nil.
If FILENAME is relative, it is interpreted to be relative
to `default-directory', and the result will also be relative."
  (let* ((expanded-filename (expand-file-name filename))
         (parent (file-name-directory (directory-file-name expanded-filename))))
    (cond
     ;; filename is at top-level, therefore no parent
     ((or (null parent)
          (file-equal-p parent expanded-filename))
      nil)
     ;; filename is relative, return relative parent
     ((not (file-name-absolute-p filename))
      (file-relative-name parent))
     (t
      parent))))

(defvar compat--file-has-changed-p--hash-table (make-hash-table :test #'equal)
  "Internal variable used by `file-has-changed-p'.")

;;* UNTESTED
(compat-defun file-has-changed-p (file &optional tag)
  "Return non-nil if FILE has changed.
The size and modification time of FILE are compared to the size
and modification time of the same FILE during a previous
invocation of `file-has-changed-p'.  Thus, the first invocation
of `file-has-changed-p' always returns non-nil when FILE exists.
The optional argument TAG, which must be a symbol, can be used to
limit the comparison to invocations with identical tags; it can be
the symbol of the calling function, for example."
  (let* ((file (directory-file-name (expand-file-name file)))
         (remote-file-name-inhibit-cache t)
         (fileattr (file-attributes file 'integer))
	 (attr (and fileattr
                    (cons (nth 7 fileattr)
		          (nth 5 fileattr))))
	 (sym (concat (symbol-name tag) "@" file))
	 (cachedattr (gethash sym compat--file-has-changed-p--hash-table)))
     (when (not (equal attr cachedattr))
       (puthash sym attr compat--file-has-changed-p--hash-table))))

(provide 'compat-29)
;;; compat-29.el ends here
