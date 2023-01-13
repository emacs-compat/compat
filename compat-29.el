;;; compat-29.el --- Compatibility Layer for Emacs 29.1  -*- lexical-binding: t; -*-

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

;; Find here the functionality added in Emacs 29.1, needed by older
;; versions.

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
;; TODO Update to 29.1 as soon as the Emacs emacs-29 branch version bumped
(compat-declare-version "29.0")

;;;; Defined in xdisp.c

(compat-defun get-display-property (position prop &optional object properties) ;; <OK>
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

;;;; Defined in fns.c

(compat-defun ntake (n list) ;; <OK>
  "Modify LIST to keep only the first N elements.
If N is zero or negative, return nil.
If N is greater or equal to the length of LIST, return LIST unmodified.
Otherwise, return LIST after truncating it."
  (and (> n 0) (let ((cons (nthcdr (1- n) list)))
                 (when cons (setcdr cons nil))
                 list)))

(compat-defun take (n list) ;; <OK>
  "Return the first N elements of LIST.
If N is zero or negative, return nil.
If N is greater or equal to the length of LIST, return LIST (or a copy)."
  (declare (pure t) (side-effect-free t))
  (let (copy)
    (while (and (< 0 n) list)
      (push (pop list) copy)
      (setq n (1- n)))
    (nreverse copy)))

(compat-defun string-equal-ignore-case (string1 string2) ;; <OK>
  "Like `string-equal', but case-insensitive.
Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
  (declare (pure t) (side-effect-free t))
  (eq t (compare-strings string1 0 nil string2 0 nil t)))

(compat-defun plist-get (plist prop &optional predicate) ;; <OK>
  "Handle optional argument PREDICATE."
  :explicit t
  (if (or (null predicate) (eq predicate 'eq))
      (plist-get plist prop)
    (catch 'found
      (while (consp plist)
        (when (funcall predicate prop (car plist))
          (throw 'found (cadr plist)))
        (setq plist (cddr plist))))))

(compat-defun plist-put (plist prop val &optional predicate) ;; <OK>
  "Handle optional argument PREDICATE."
  :explicit t
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

(compat-defun plist-member (plist prop &optional predicate) ;; <OK>
  "Handle optional argument PREDICATE."
  :explicit t
  (if (or (null predicate) (eq predicate 'eq))
      (plist-member plist prop)
    (catch 'found
      (while (consp plist)
        (when (funcall predicate prop (car plist))
          (throw 'found plist))
        (setq plist (cddr plist))))))

;;;; Defined in editfns.c

(compat-defun pos-bol (&optional n) ;; <OK>
  "Return the position of the first character on the current line.
With optional argument N, scan forward N - 1 lines first.
If the scan reaches the end of the buffer, return that position.

This function ignores text display directionality; it returns the
position of the first character in logical order, i.e. the smallest
character position on the logical line.  See `vertical-motion' for
movement by screen lines.

This function does not move point.  Also see `line-beginning-position'."
  (declare (side-effect-free t))
  (let ((inhibit-field-text-motion t))
    (line-beginning-position n)))

(compat-defun pos-eol (&optional n) ;; <OK>
  "Return the position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

This function ignores text display directionality; it returns the
position of the last character in logical order, i.e. the largest
character position on the line.

This function does not move point.  Also see `line-end-position'."
  (declare (side-effect-free t))
  (let ((inhibit-field-text-motion t))
    (line-end-position n)))

;;;; Defined in keymap.c

(compat-defun define-key (keymap key def &optional remove) ;; <UNTESTED>
  "Handle optional argument REMOVE."
  :explicit t
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

(compat-defmacro with-memoization (place &rest code) ;; <OK>
  "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
  (declare (indent 1))
  (gv-letplace (getter setter) place
    `(or ,getter
         ,(macroexp-let2 nil val (macroexp-progn code)
            `(progn
               ,(funcall setter val)
               ,val)))))

(compat-defalias string-split split-string) ;; <OK>

(compat-defun function-alias-p (func &optional noerror) ;; <OK>
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

(compat-defun buffer-match-p (condition buffer-or-name &optional arg) ;; <UNTESTED>
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
  * `not': the cadr is interpreted as a negation of a condition.
  * `and': the cdr is a list of recursive conditions, that all have
    to be met.
  * `or': the cdr is a list of recursive condition, of which at
    least one has to be met."
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
                      (condition-case nil
                          (funcall condition buffer)
                        (wrong-number-of-arguments
                         (funcall condition buffer arg))))
                     ((eq (car-safe condition) 'major-mode)
                      (eq
                       (buffer-local-value 'major-mode buffer)
                       (cdr condition)))
                     ((eq (car-safe condition) 'derived-mode)
                      (provided-mode-derived-p
                       (buffer-local-value 'major-mode buffer)
                       (cdr condition)))
                     ((eq (car-safe condition) 'not)
                      (not (funcall match (cdr condition))))
                     ((eq (car-safe condition) 'or)
                      (funcall match (cdr condition)))
                     ((eq (car-safe condition) 'and)
                      (catch 'fail
                        (dolist (c (cdr condition))
                          (unless (funcall match (list c))
                            (throw 'fail nil)))
                        t)))
                (throw 'match t)))))))
    (funcall match (list condition))))

(compat-defun match-buffers (condition &optional buffers arg) ;; <UNTESTED>
  "Return a list of buffers that match CONDITION.
See `buffer-match' for details on CONDITION.  By default all
buffers are checked, this can be restricted by passing an
optional argument BUFFERS, set to a list of buffers to check.
ARG is passed to `buffer-match', for predicate conditions in
CONDITION."
  (let (bufs)
    (dolist (buf (or buffers (buffer-list)))
      (when (buffer-match-p condition (get-buffer buf) arg)
        (push buf bufs)))
    bufs))

;;;; Defined in subr-x.el

(compat-defun add-display-text-property (start end prop value ;; <OK>
                                               &optional object)
  "Add display property PROP with VALUE to the text from START to END.
If any text in the region has a non-nil `display' property, those
properties are retained.

If OBJECT is non-nil, it should be a string or a buffer.  If nil,
this defaults to the current buffer."
  (let ((sub-start start)
        (sub-end 0)
        disp)
    (while (< sub-end end)
      (setq sub-end (next-single-property-change sub-start 'display object
                                                 (if (stringp object)
                                                     (min (length object) end)
                                                   (min end (point-max)))))
      (if (not (setq disp (get-text-property sub-start 'display object)))
          ;; No old properties in this range.
          (put-text-property sub-start sub-end 'display (list prop value)
                             object)
        ;; We have old properties.
        (let ((vector nil))
          ;; Make disp into a list.
          (setq disp
                (cond
                 ((vectorp disp)
                  (setq vector t)
                  (append disp nil))
                 ((not (consp (car disp)))
                  (list disp))
                 (t
                  disp)))
          ;; Remove any old instances.
          (when-let ((old (assoc prop disp)))
            (setq disp (delete old disp)))
          (setq disp (cons (list prop value) disp))
          (when vector
            (setq disp (vconcat disp)))
          ;; Finally update the range.
          (put-text-property sub-start sub-end 'display disp object)))
      (setq sub-start sub-end))))

(compat-defmacro while-let (spec &rest body) ;; <OK>
  "Bind variables according to SPEC and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all bindings are non-nil, eval BODY and repeat.

The variable list SPEC is the same as in `if-let'."
  (declare (indent 1) (debug if-let))
  (when (and (<= (length spec) 2) (not (listp (car spec))))
    ;; Adjust the single binding case
    (setq spec (list spec)))
  (let ((done (gensym "done")))
    `(catch ',done
       (while t
         (if-let* ,spec
             (progn
               ,@body)
           (throw ',done nil))))))

;;;; Defined in files.el

(compat-defun file-name-split (filename) ;; <OK>
  "Return a list of all the components of FILENAME.
On most systems, this will be true:

  (equal (string-join (file-name-split filename) \"/\") filename)"
  (let ((components nil))
    ;; If this is a directory file name, then we have a null file name
    ;; at the end.
    (when (directory-name-p filename)
      (push "" components)
      (setq filename (directory-file-name filename)))
    ;; Loop, chopping off components.
    (while (length> filename 0)
      (push (file-name-nondirectory filename) components)
      (let ((dir (file-name-directory filename)))
        (setq filename (and dir (directory-file-name dir)))
        ;; If there's nothing left to peel off, we're at the root and
        ;; we can stop.
        (when (and dir (equal dir filename))
          (push (if (equal dir "") ""
                  ;; On Windows, the first component might be "c:" or
                  ;; the like.
                  (substring dir 0 -1))
                components)
          (setq filename nil))))
    components))

(compat-defun file-attribute-file-identifier (attributes) ;; <OK>
  "The inode and device numbers in ATTRIBUTES returned by `file-attributes'.
The value is a list of the form (INODENUM DEVICE), where DEVICE could be
either a single number or a cons cell of two numbers.
This tuple of numbers uniquely identifies the file."
  (nthcdr 10 attributes))

(compat-defun file-name-parent-directory (filename) ;; <OK>
  "Return the directory name of the parent directory of FILENAME.
If FILENAME is at the root of the filesystem, return nil.
If FILENAME is relative, it is interpreted to be relative
to `default-directory', and the result will also be relative."
  (let* ((expanded-filename (expand-file-name filename))
         (parent (file-name-directory (directory-file-name expanded-filename))))
    (cond
     ;; filename is at top-level, therefore no parent
     ((or (null parent)
          ;; `equal' is enough, we don't need to resolve symlinks here
          ;; with `file-equal-p', also for performance
          (equal parent expanded-filename))
      nil)
     ;; filename is relative, return relative parent
     ((not (file-name-absolute-p filename))
      (file-relative-name parent))
     (t
      parent))))

(compat-defvar file-has-changed-p--hash-table ;; <UNTESTED>
               (make-hash-table :test #'equal)
  "Internal variable used by `file-has-changed-p'.")

(compat-defun file-has-changed-p (file &optional tag) ;; <UNTESTED>
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
                    (cons (file-attribute-size fileattr)
                          (file-attribute-modification-time fileattr))))
         (sym (concat (symbol-name tag) "@" file))
         (cachedattr (gethash sym file-has-changed-p--hash-table)))
     (when (not (equal attr cachedattr))
       (puthash sym attr file-has-changed-p--hash-table))))

;;;; Defined in keymap.el

(compat-defun key-valid-p (keys) ;; <OK>
  "Say whether KEYS is a valid key.
A key is a string consisting of one or more key strokes.
The key strokes are separated by single space characters.

Each key stroke is either a single character, or the name of an
event, surrounded by angle brackets.  In addition, any key stroke
may be preceded by one or more modifier keys.  Finally, a limited
number of characters have a special shorthand syntax.

Here's some example key sequences.

  \"f\"           (the key `f')
  \"S o m\"       (a three key sequence of the keys `S', `o' and `m')
  \"C-c o\"       (a two key sequence of the keys `c' with the control modifier
                 and then the key `o')
  \"H-<left>\"    (the key named \"left\" with the hyper modifier)
  \"M-RET\"       (the \"return\" key with a meta modifier)
  \"C-M-<space>\" (the \"space\" key with both the control and meta modifiers)

These are the characters that have shorthand syntax:
NUL, RET, TAB, LFD, ESC, SPC, DEL.

Modifiers have to be specified in this order:

   A-C-H-M-S-s

which is

   Alt-Control-Hyper-Meta-Shift-super"
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil))
    (and
     (stringp keys)
     (string-match-p "\\`[^ ]+\\( [^ ]+\\)*\\'" keys)
     (save-match-data
       (catch 'exit
         (let ((prefixes
                "\\(A-\\)?\\(C-\\)?\\(H-\\)?\\(M-\\)?\\(S-\\)?\\(s-\\)?"))
           (dolist (key (split-string keys " "))
             ;; Every key might have these modifiers, and they should be
             ;; in this order.
             (when (string-match (concat "\\`" prefixes) key)
               (setq key (substring key (match-end 0))))
             (unless (or (and (= (length key) 1)
                              ;; Don't accept control characters as keys.
                              (not (< (aref key 0) ?\s))
                              ;; Don't accept Meta'd characters as keys.
                              (or (multibyte-string-p key)
                                  (not (<= 127 (aref key 0) 255))))
                         (and (string-match-p "\\`<[-_A-Za-z0-9]+>\\'" key)
                              ;; Don't allow <M-C-down>.
                              (= (progn
                                   (string-match
                                    (concat "\\`<" prefixes) key)
                                   (match-end 0))
                                 1))
                         (string-match-p
                          "\\`\\(NUL\\|RET\\|TAB\\|LFD\\|ESC\\|SPC\\|DEL\\)\\'"
                          key))
               ;; Invalid.
               (throw 'exit nil)))
           t))))))

(compat-defun keymap--check (key) ;; <OK>
  "Signal an error if KEY doesn't have a valid syntax."
  (unless (key-valid-p key)
    (error "%S is not a valid key definition; see `key-valid-p'" key)))

(compat-defun key-parse (keys) ;; <OK>
  "Convert KEYS to the internal Emacs key representation.
See `kbd' for a descripion of KEYS."
  (declare (pure t) (side-effect-free t))
  ;; A pure function is expected to preserve the match data.
  (save-match-data
    (let ((case-fold-search nil)
          (len (length keys)) ; We won't alter keys in the loop below.
          (pos 0)
          (res []))
      (while (and (< pos len)
                  (string-match "[^ \t\n\f]+" keys pos))
        (let* ((word-beg (match-beginning 0))
               (word-end (match-end 0))
               (word (substring keys word-beg len))
               (times 1)
               key)
          ;; Try to catch events of the form "<as df>".
          (if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
              (setq word (match-string 0 word)
                    pos (+ word-beg (match-end 0)))
            (setq word (substring keys word-beg word-end)
                  pos word-end))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times (string-to-number (substring word 0 (match-end 1))))
            (setq word (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key (vconcat (if (eq (key-binding [?\M-x])
                                            'execute-extended-command)
                                        [?\M-x]
                                      (or (car (where-is-internal
                                                'execute-extended-command))
                                          [?\M-x]))
                                    (substring word 2 -2) "\r")))
                ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
                      (progn
                        (setq word (concat (match-string 1 word)
                                           (match-string 3 word)))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key (list (intern word))))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos (string-match "$" keys pos)))
                (t
                 (let ((orig-word word) (prefix 0) (bits 0))
                   (while (string-match "^[ACHMsS]-." word)
                     (setq bits (+ bits
                                   (cdr
                                    (assq (aref word 0)
                                          '((?A . ?\A-\0) (?C . ?\C-\0)
                                            (?H . ?\H-\0) (?M . ?\M-\0)
                                            (?s . ?\s-\0) (?S . ?\S-\0))))))
                     (setq prefix (+ prefix 2))
                     (setq word (substring word 2)))
                   (when (string-match "^\\^.$" word)
                     (setq bits (+ bits ?\C-\0))
                     (setq prefix (1+ prefix))
                     (setq word (substring word 1)))
                   (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                              ("LFD" . "\n") ("TAB" . "\t")
                                              ("ESC" . "\e") ("SPC" . " ")
                                              ("DEL" . "\177")))))
                     (when found (setq word (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (let ((n 0))
                       (dolist (ch (cdr (string-to-list word)))
                         (setq n (+ (* n 8) ch -48)))
                       (setq word (vector n))))
                   (cond ((= bits 0)
                          (setq key word))
                         ((and (= bits ?\M-\0) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key (mapcar (lambda (x) (+ x bits))
                                            (append word nil))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\0) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key (list (+ bits (- ?\C-\0)
                                             (logand (aref word 0) 31)))))
                         (t
                          (setq key (list (+ bits (aref word 0)))))))))
          (when key
            (dolist (_ (number-sequence 1 times))
              (setq res (vconcat res key))))))
      res)))

(compat-defun keymap-set (keymap key definition) ;; <OK>
  "Set KEY to DEFINITION in KEYMAP.
KEY is a string that satisfies `key-valid-p'.

DEFINITION is anything that can be a key's definition:
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
 (See info node `(elisp)Extended Menu Items'.)"
  (keymap--check key)
  (when (stringp definition)
    (keymap--check definition)
    (setq definition (key-parse definition)))
  (define-key keymap (key-parse key) definition))

(compat-defun keymap-unset (keymap key &optional remove) ;; <UNTESTED>
  "Remove key sequence KEY from KEYMAP.
KEY is a string that satisfies `key-valid-p'.

If REMOVE, remove the binding instead of unsetting it.  This only
makes a difference when there's a parent keymap.  When unsetting
a key in a child map, it will still shadow the same key in the
parent keymap.  Removing the binding will allow the key in the
parent keymap to be used."
  (keymap--check key)
  (compat--define-key keymap (key-parse key) nil remove))

(compat-defun keymap-global-set (key command) ;; <OK>
  "Give KEY a global binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.

KEY is a string that satisfies `key-valid-p'.

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function.

NOTE: The compatibility version is not a command."
  (keymap-set (current-global-map) key command))

(compat-defun keymap-local-set (key command) ;; <OK>
  "Give KEY a local binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.

KEY is a string that satisfies `key-valid-p'.

The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode.

NOTE: The compatibility version is not a command."
  (let ((map (current-local-map)))
    (unless map
      (use-local-map (setq map (make-sparse-keymap))))
    (keymap-set map key command)))

(compat-defun keymap-global-unset (key &optional remove) ;; <UNTESTED>
  "Remove global binding of KEY (if any).
KEY is a string that satisfies `key-valid-p'.

If REMOVE (interactively, the prefix arg), remove the binding
instead of unsetting it.  See `keymap-unset' for details.

NOTE: The compatibility version is not a command."
  (keymap-unset (current-global-map) key remove))

(compat-defun keymap-local-unset (key &optional remove) ;; <UNTESTED>
  "Remove local binding of KEY (if any).
KEY is a string that satisfies `key-valid-p'.

If REMOVE (interactively, the prefix arg), remove the binding
instead of unsetting it.  See `keymap-unset' for details.

NOTE: The compatibility version is not a command."
  (when (current-local-map)
    (keymap-unset (current-local-map) key remove)))

(compat-defun keymap-substitute (keymap olddef newdef &optional oldmap prefix) ;; <UNTESTED>
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF wherever it appears.
Alternatively, if optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those keys that are defined as OLDDEF in OLDMAP.

If you don't specify OLDMAP, you can usually get the same results
in a cleaner way with command remapping, like this:
  (define-key KEYMAP [remap OLDDEF] NEWDEF)
\n(fn OLDDEF NEWDEF KEYMAP &optional OLDMAP)"
  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning

  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (unless prefix
    (setq prefix ""))
  (let* ((scan (or oldmap keymap))
         (prefix1 (vconcat prefix [nil]))
         (key-substitution-in-progress
          (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (map-keymap
     (lambda (char defn)
       (aset prefix1 (length prefix) char)
       (substitute-key-definition-key defn olddef newdef prefix1 keymap))
     scan)))

(compat-defun keymap-set-after (keymap key definition &optional after) ;; <UNTESTED>
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is like `keymap-set' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.  Note that AFTER must be an event type (like KEY), NOT a command
\(like DEFINITION).

If AFTER is t or omitted, the new binding goes at the end of the keymap.
AFTER should be a single event type--a symbol or a character, not a sequence.

Bindings are always added before any inherited map.

The order of bindings in a keymap matters only when it is used as
a menu, so this function is not useful for non-menu keymaps."
  (keymap--check key)
  (when after
    (keymap--check after))
  (define-key-after keymap (key-parse key) definition
    (and after (key-parse after))))

(compat-defun keymap-lookup ;; <OK>
    (keymap key &optional accept-default no-remap position)
  "Return the binding for command KEY.
KEY is a string that satisfies `key-valid-p'.

If KEYMAP is nil, look up in the current keymaps.  If non-nil, it
should either be a keymap or a list of keymaps, and only these
keymap(s) will be consulted.

The binding is probably a symbol with a function definition.

Normally, `keymap-lookup' ignores bindings for t, which act as
default bindings, used when nothing else in the keymap applies;
this makes it usable as a general function for probing keymaps.
However, if the optional second argument ACCEPT-DEFAULT is
non-nil, `keymap-lookup' does recognize the default bindings,
just as `read-key-sequence' does.

Like the normal command loop, `keymap-lookup' will remap the
command resulting from looking up KEY by looking up the command
in the current keymaps.  However, if the optional third argument
NO-REMAP is non-nil, `keymap-lookup' returns the unmapped
command.

If KEY is a key sequence initiated with the mouse, the used keymaps
will depend on the clicked mouse position with regard to the buffer
and possible local keymaps on strings.

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by `event-start' and `event-end', and the lookup
occurs in the keymaps associated with it instead of KEY.  It can also
be a number or marker, in which case the keymap properties at the
specified buffer position instead of point are used."
  (keymap--check key)
  (when (and keymap position)
    (error "Can't pass in both keymap and position"))
  (if keymap
      (let ((value (lookup-key keymap (key-parse key) accept-default)))
        (if (and (not no-remap)
                   (symbolp value))
            (or (command-remapping value) value)
          value))
    (key-binding (kbd key) accept-default no-remap position)))

(compat-defun keymap-local-lookup (keys &optional accept-default) ;; <OK>
  "Return the binding for command KEYS in current local keymap only.
KEY is a string that satisfies `key-valid-p'.

The binding is probably a symbol with a function definition.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `keymap-lookup' for more details
about this."
  (when-let ((map (current-local-map)))
    (keymap-lookup map keys accept-default)))

(compat-defun keymap-global-lookup (keys &optional accept-default _message) ;; <OK>
  "Return the binding for command KEYS in current global keymap only.
KEY is a string that satisfies `key-valid-p'.

The binding is probably a symbol with a function definition.
This function's return values are the same as those of `keymap-lookup'
\(which see).

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `keymap-lookup' for more details
about this.

NOTE: The compatibility version is not a command."
  (keymap-lookup (current-global-map) keys accept-default))

(compat-defun define-keymap (&rest definitions) ;; <OK>
  "Create a new keymap and define KEY/DEFINITION pairs as key bindings.
The new keymap is returned.

Options can be given as keywords before the KEY/DEFINITION
pairs.  Available keywords are:

:full      If non-nil, create a chartable alist (see `make-keymap').
             If nil (i.e., the default), create a sparse keymap (see
             `make-sparse-keymap').

:suppress  If non-nil, the keymap will be suppressed (see `suppress-keymap').
             If `nodigits', treat digits like other chars.

:parent    If non-nil, this should be a keymap to use as the parent
             (see `set-keymap-parent').

:keymap    If non-nil, instead of creating a new keymap, the given keymap
             will be destructively modified instead.

:name      If non-nil, this should be a string to use as the menu for
             the keymap in case you use it as a menu with `x-popup-menu'.

:prefix    If non-nil, this should be a symbol to be used as a prefix
             command (see `define-prefix-command').  If this is the case,
             this symbol is returned instead of the map itself.

KEY/DEFINITION pairs are as KEY and DEF in `keymap-set'.  KEY can
also be the special symbol `:menu', in which case DEFINITION
should be a MENU form as accepted by `easy-menu-define'.

\(fn &key FULL PARENT SUPPRESS NAME PREFIX KEYMAP &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let (full suppress parent name prefix keymap)
    ;; Handle keywords.
    (while (and definitions
                (keywordp (car definitions))
                (not (eq (car definitions) :menu)))
      (let ((keyword (pop definitions)))
        (unless definitions
          (error "Missing keyword value for %s" keyword))
        (let ((value (pop definitions)))
          (pcase keyword
            (:full (setq full value))
            (:keymap (setq keymap value))
            (:parent (setq parent value))
            (:suppress (setq suppress value))
            (:name (setq name value))
            (:prefix (setq prefix value))
            (_ (error "Invalid keyword: %s" keyword))))))

    (when (and prefix
               (or full parent suppress keymap))
      (error "A prefix keymap can't be defined with :full/:parent/:suppress/:keymap keywords"))

    (when (and keymap full)
      (error "Invalid combination: :keymap with :full"))

    (let ((keymap (cond
                   (keymap keymap)
                   (prefix (define-prefix-command prefix nil name))
                   (full (make-keymap name))
                   (t (make-sparse-keymap name))))
          seen-keys)
      (when suppress
        (suppress-keymap keymap (eq suppress 'nodigits)))
      (when parent
        (set-keymap-parent keymap parent))

      ;; Do the bindings.
      (while definitions
        (let ((key (pop definitions)))
          (unless definitions
            (error "Uneven number of key/definition pairs"))
          (let ((def (pop definitions)))
            (if (eq key :menu)
                (easy-menu-define nil keymap "" def)
              (if (member key seen-keys)
                  (error "Duplicate definition for key: %S %s" key keymap)
                (push key seen-keys))
              (keymap-set keymap key def)))))
      keymap)))

(compat-defmacro defvar-keymap (variable-name &rest defs) ;; <OK>
  "Define VARIABLE-NAME as a variable with a keymap definition.
See `define-keymap' for an explanation of the keywords and KEY/DEFINITION.

In addition to the keywords accepted by `define-keymap', this
macro also accepts a `:doc' keyword, which (if present) is used
as the variable documentation string.

The `:repeat' keyword can also be specified; it controls the
`repeat-mode' behavior of the bindings in the keymap.  When it is
non-nil, all commands in the map will have the `repeat-map'
symbol property.

More control is available over which commands are repeatable; the
value can also be a property list with properties `:enter' and
`:exit', for example:

     :repeat (:enter (commands ...) :exit (commands ...))

`:enter' specifies the list of additional commands that only
enter `repeat-mode'.  When the list is empty, then by default all
commands in the map enter `repeat-mode'.  This is useful when
there is a command that has the `repeat-map' symbol property, but
doesn't exist in this specific map.  `:exit' is a list of
commands that exit `repeat-mode'.  When the list is empty, no
commands in the map exit `repeat-mode'.  This is useful when a
command exists in this specific map, but it doesn't have the
`repeat-map' symbol property on its symbol.

\(fn VARIABLE-NAME &key DOC FULL PARENT SUPPRESS NAME PREFIX KEYMAP REPEAT &rest [KEY DEFINITION]...)"
  (declare (indent 1))
  (let ((opts nil)
        doc repeat props)
    (while (and defs
                (keywordp (car defs))
                (not (eq (car defs) :menu)))
      (let ((keyword (pop defs)))
        (unless defs
          (error "Uneven number of keywords"))
        (cond
         ((eq keyword :doc) (setq doc (pop defs)))
         ((eq keyword :repeat) (setq repeat (pop defs)))
         (t (push keyword opts)
            (push (pop defs) opts)))))
    (unless (zerop (% (length defs) 2))
      (error "Uneven number of key/definition pairs: %s" defs))

    (let ((defs defs)
          key seen-keys)
      (while defs
        (setq key (pop defs))
        (pop defs)
        (when (not (eq key :menu))
          (if (member key seen-keys)
              (error "Duplicate definition for key '%s' in keymap '%s'"
                     key variable-name)
            (push key seen-keys)))))

    (when repeat
      (let ((defs defs)
            def)
        (dolist (def (plist-get repeat :enter))
          (push `(put ',def 'repeat-map ',variable-name) props))
        (while defs
          (pop defs)
          (setq def (pop defs))
          (when (and (memq (car def) '(function quote))
                     (not (memq (cadr def) (plist-get repeat :exit))))
            (push `(put ,def 'repeat-map ',variable-name) props)))))

    (let ((defvar-form
           `(defvar ,variable-name
              (define-keymap ,@(nreverse opts) ,@defs)
              ,@(and doc (list doc)))))
      (if props
          `(progn
             ,defvar-form
             ,@(nreverse props))
        defvar-form))))

;;;; Defined in button.el

(compat-defun button--properties (callback data help-echo) ;; <OK>
  "Helper function."
  (list 'font-lock-face 'button
        'mouse-face 'highlight
        'help-echo help-echo
        'button t
        'follow-link t
        'category t
        'button-data data
        'keymap button-map
        'action callback))

(compat-defun buttonize (string callback &optional data help-echo) ;; <OK>
  "Make STRING into a button and return it.
When clicked, CALLBACK will be called with the DATA as the
function argument.  If DATA isn't present (or is nil), the button
itself will be used instead as the function argument.

If HELP-ECHO, use that as the `help-echo' property.

Also see `buttonize-region'."
  (let ((string
         (apply #'propertize string
                (button--properties callback data help-echo))))
    ;; Add the face to the end so that it can be overridden.
    (add-face-text-property 0 (length string) 'button t string)
    string))

(compat-defun buttonize-region (start end callback &optional data help-echo) ;; <OK>
  "Make the region between START and END into a button.
When clicked, CALLBACK will be called with the DATA as the
function argument.  If DATA isn't present (or is nil), the button
itself will be used instead as the function argument.

If HELP-ECHO, use that as the `help-echo' property.

Also see `buttonize'."
  (add-text-properties start end (button--properties callback data help-echo))
  (add-face-text-property start end 'button t))

;; Obsolete Alias since 29
(compat-defalias button-buttonize buttonize :obsolete t)

(provide 'compat-29)
;;; compat-29.el ends here
