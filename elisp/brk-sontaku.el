;;; brk-sontaku.el --- Context-aware region expander -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-sontaku.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)

(defgroup brk-sontaku nil
  "Context-aware region expander"
  :group 'convenience
  :group 'tools
  :prefix "brk-sontaku-")

;;;; Internal Variables

(defvar brk-sontaku--mode-state-machine-registry nil
  "Registry mapping MODE symbols to their state machine functions.")

(defvar brk-sontaku--region-history nil
  "History of expanded regions for contraction support.")

;; FIXME: Do we need this? The only thing it can be helpful for is the last action...
(defvar brk-sontaku--prev-state
  '(:char nil :syntax-class nil :last-action nil)
  "State machine's previous state.
It includes:

- the character at point and its syntax class
- the last action taken
- ")

(make-variable-buffer-local 'brk-sontaku--region-history)
(make-variable-buffer-local 'brk-sontaku--prev-state)

;;;; Internal Constants

(defconst brk-sontaku--extra-paren-pairs
  '(("＜" . "＞"))
  "Extra bracket pairs considered by `brk-sontaku--in-paren-p'.")

;;;; User Options

(defcustom brk-sontaku-expand-to-whole-buffer-at-top-level t
  "If non-nil, expanding region at top-level will select the entire buffer.
Otherwise, region expansion stops silently when no broader syntactic unit exists."
  :type 'boolean
  :group 'brk-sontaku)

(defcustom brk-sontaku-default-state-machine-function
  #'brk-sontaku--emacs-lisp-mode-state-machine
  "A default state machine function called when expanding,
contracting, or moving."
  :type '(choice
          (const :tag "State machine for `emacs-lisp-mode'"
                 brk-sontaku--elisp-mode-state-machine)
          (const :tag "State machine for `org-mode'"
                 brk-sontaku--org-mode-state-machine)
          (function :tag "Custom state machine")))

;;;; Error handling

(defun brk-sontaku--error-if-wrong-bound-pos (direction bound)
  "Signal an error when BOUND is invalid for given DIRECTION.
DIRECTION must be either \\='forward or \\='backward."
  (let ((correct-bound-pos-p (pcase direction
                               ('forward (lambda () (and bound (< bound (point)))))
                               ('backward (lambda () (and bound (> bound (point)))))
                               (_ (error "Invalid DIRECTION: %S" direction)))))
    (when (funcall correct-bound-pos-p)
      (error "Invalid bound position: %s is %s the point for %s movement"
             bound
             (if (eq direction 'forward) "before" "after")
             direction))))

;;;; Syntax Reference

(defun brk-sontaku--syntax-class-to-char (syntax-class)
  "Return the designator char of SYNTAX-CLASS."
  (aref "- .w_()'\"$\\/<>@!|" syntax-class))

(defun brk-sontaku--syntax-char-after (&optional pt)
  "Return the syntax code after PT, described by a char.
If PT is nil, return the syntax code after the current point.
When PT does not exist, or there is no char after PT,
return nil.

For the meaning of the returned char, see `modify-syntax-entry'."
  (let ((pt (or pt (point))))
    (unless (or (< pt (point-min))
                (>= pt (point-max)))
      (brk-sontaku--syntax-class-to-char (syntax-class (syntax-after pt))))))

(defun brk-sontaku--syntactic-depth-at (pt)
  "Return syntactic depth at PT."
  (let ((pt (or pt (point))))
    (car (syntax-ppss pt))))

;;;; Misc

(defun brk-sontaku--resolve-direction (direction forward-val backward-val)
  "Return a value according to DIRECTION.
DIRECTION must be either \\='forward or \\=''backward.

If DIRECTION is:

- \\='forward, return FORWARD-VAL.
- \\='backward, return BACKWARD-VAL.
- neither of them, signal an error."
  (pcase direction
    ('forward forward-val)
    ('backward backward-val)
    (_ (error "Invalid DIRECTION: %S (must be 'forward or 'backward)" direction))))

(defun brk-sontaku--active-region-direction ()
  "Return the direction of active region.
If the point is:

- at the beginning, return `backward'
- at the end, return `forward'

When no active region exists, return nil."
  (when (use-region-p)
    (if (> (mark) (point))
        'backward
      'forward)))

(defun brk-sontaku--mark-region (beg end replace-mark)
  "Mark and activate a region between BEG and END.
If REPLACE-MARK is non-nil, replace the existing mark with the new one.
Otherwise, push the existing one into mark ring."
  (let* ((direction (or (brk-sontaku--active-region-direction)
                        'forward))
         (mark-pos (brk-sontaku--resolve-direction direction beg end))
         (goto-pos (brk-sontaku--resolve-direction direction end beg)))
    (setq deactivate-mark nil)
    (if replace-mark
        (set-mark mark-pos)
      (push-mark mark-pos 'nomsg))
    (goto-char goto-pos)
    (activate-mark)))

(defun brk-sontaku--maybe-reset-region-history ()
  "Reset region history if the last command execution was
neither an expansion nor a contraction."
  (unless (memq last-command '(brk-sontaku-expand-region
                               brk-sontaku-contract-region))
    (setq brk-sontaku--region-history nil)))

(defun brk-sontaku--update-region-history (beg end)
  "Update region history according to the last command execution.
Record the positions of BEG and END unless the whole buffer is selected."
  (unless (and (= beg (point-min))
               (= end (point-max)))
    (push (cons (min beg end)
                (max beg end))
          brk-sontaku--region-history)))

(defun brk-sontaku--interval-contains-p (outer inner &optional mode)
  "Return non-nil when OUTER contains INNER.
Both OUTER and INNER must be cons cells of integer (BEG . END).
MODE determines the containment type:

- 'strict: strict containment (<)
- 'proper: inclusive but equal is not allowed
- nil: inclusive containment (<=)
- none of them listed above: signal an error"
  (cl-destructuring-bind (obeg . oend) outer
    (cl-destructuring-bind (ibeg . iend) inner
      (pcase mode
        ('strict
         (and (< obeg ibeg) (< iend oend)))
        ('proper
         (and (<= obeg ibeg)
              (<= iend oend)
              (not (and (= obeg ibeg)
                        (= iend oend)))))
        ((pred null)
         (and (<= obeg ibeg) (<= iend oend)))
        (_
         (error "Invalid MODE: %S (must be 'strict, 'proper, or nil)" winner))))))

(defun brk-sontaku--compare-intervals-between (i1 i2 winner)
  "Return either I1 or I2 depending on WINNER.
If WINNER is:

- 'bigger, return the one that contains the other.
- 'smaller, return the one that is contained by the other.
- none of them listed above, signal an error.

Both I1 and I2 must be cons cells of integer.
When I1 or I2 is nil, or neither of them contains the other,
return nil."
  (when (and i1 i2)
    (pcase winner
      ('bigger (cond ((brk-sontaku--interval-contains-p i1 i2) i1)
                     ((brk-sontaku--interval-contains-p i2 i1) i2)))
      ('smaller (cond ((brk-sontaku--interval-contains-p i1 i2) i2)
                      ((brk-sontaku--interval-contains-p i2 i1) i1)))
      (_ (error "Invalid WINNER: %S (must be 'bigger or 'smaller)" winner)))))

;; TODO: Revisit this and think over when to use it.
(defun brk-sontaku--search-next-bounds (bounds bounds-fn)
  "Return next bigger bounds according to BOUNDS-FN.
BOUNDS is the original bounds to start from."
  (brk-sontaku--compare-intervals-between
   (save-excursion (goto-char (car bounds))
                   (funcall bounds-fn))
   (save-excursion (goto-char (cdr bounds))
                   (funcall bounds-fn))
   'bigger))

(defun brk-sontaku--valid-derived-major-mode-symbol-p (symbol)
  "Return non-nil if SYMBOL is a valid derived major mode."
  (and (symbolp symbol)
       (fboundp symbol)
       (not (memq symbol minor-mode-list))
       (string-suffix-p "-mode" (symbol-name symbol) t)
       (> (length (derived-mode-all-parents symbol)) 1)))

;; TODO: reconsider if this function can be helpful anywhere.
(defun brk-sontaku--safe-scan-sexps (n &optional pt)
  "Safely scan and return the position N sexps away from PT.
If PT is not provided, use the current point.
Return nil if the scan reaches the beginning
or end of the accessible part in the middle."
  (let ((pt (or pt (point))))
    (ignore-errors (scan-sexps pt n))))

(defun brk-sontaku--move-within (action direction &optional limit)
  "Call ACTION (a function that moves the point according to DIRECTION).
DIRECTION must be either \\='forward or \\='backward.
Return t if the resulting point stays within LIMIT.
Otherwise, restore point and return nil.

LIMIT is the farthest point ACTION is allowed to reach.
If ACTION moves:

- forward, LIMIT must be >= point.
- backward, LIMIT must be <= point.

When LIMIT is nil, simply call ACTION."
  (if (null limit)
      (funcall action direction)
    (let* ((from (point))
           (to (progn (funcall action direction) (point)))
           (ok t)
           (direction (cond
                       ((> to from) 1)
                       ((< to from) -1)
                       (t 0))))
      (setq ok
            (pcase direction
              (1 (<= to limit))
              (-1 (>= to limit))
              (0 nil)))
      (unless ok
        (goto-char from))
      ok)))

(defun brk-sontaku--register-state-machine (mode fn)
  "Register FN in `brk-sontaku--mode-state-machine-registry'
as the state machine for MODE.  It is stored as an alist of (MODE . FN)."
  (let ((cell (assoc mode brk-sontaku--mode-state-machine-registry)))
    (if cell
        (setcdr cell fn)
      (push (cons mode fn)
            brk-sontaku--mode-state-machine-registry))))

(defun brk-sontaku--get-state-machine (mode)
  "Return the state machine function for MODE.
If not found, return nil."
  (alist-get mode brk-sontaku--mode-state-machine-registry))

;;;; Predicates & Unit Actions

;;;;; Whitespace

(defun brk-sontaku--at-whitespace-p (&optional pt)
  "Return non-nil if char at PT is whitespace.
i.e., a whitespace or a newline."
  (let ((pt (or pt (point))))
    (eq (brk-sontaku--syntax-char-after pt) ?-)))

(defun brk-sontaku--skip-whitespace (direction &optional bound)
  "Move across contiguous whitespace in DIRECTION.
DIRECTION must be either \\='forward or \\='backward.
If BOUND is before or after point and DIRECTION is
'forward and 'backward, respectively, signal an error."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let ((n (brk-sontaku--resolve-direction
            direction 1 -1))
        (over-bound-p (brk-sontaku--resolve-direction
                       direction
                       (lambda (pos bound) (> pos bound))
                       (lambda (pos bound) (< pos bound))))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point)))))
        (moved nil))
    (cl-block nil
      (while t
        (let ((next-pos (+ (point) n)))
          (when (and bound
                     (funcall over-bound-p next-pos bound))
            (cl-return))
          (when (or (<= next-pos (point-min))
                    (>= next-pos (point-max)))
            (cl-return))
          (let* ((char (char-after (funcall point-fn)))
                 (syntax (and char (char-syntax char))))
            (if (eq syntax ?\s)
                (progn
                  (setq moved t)
                  (goto-char next-pos))
              (cl-return))))))
    (when moved (point))))

;;;;; Character

(defun brk-sontaku--match-char-p (char &optional pt)
  "Return non-nil if CHAR equals the character at PT.
CHAR is either a one-length string or a char."
  (let* ((pt (or pt (point)))
         (ch (char-after pt)))
    (when (and c (<= (1+ pt) (point-max)))
      (cond
       ((characterp char)
        (eq ch char))
       ((stringp char)
        (eq ch (string-to-char char)))
       (t (user-error "CHAR must be a char or a string, got %S"
                      char))))))

(defun brk-sontaku--match-chars-p (chars &optional pt)
  "Return non-nil if the character at PT matches any element in CHARS.
CHARS is a list whose elements may be characters, one-length strings,
or a mixture of them."
  (let* ((pt (or pt (point)))
         (ch (char-after pt)))
    (when (and c (<= (1+ pt) (point-max)))
      (let ((targets
             (mapcar
              (lambda (elt)
                (cond
                 ((characterp elt) elt)
                 ((stringp elt)
                  (if (= (length elt) 1)
                      (string-to-char elt)
                    (user-error "String element %S must be length 1")))
                 (t
                  (user-error "CHARS elements must be chars or strings, got %S"
                              elt))))
              chars)))
        (memq c targets)))))

(defun brk-sontaku--skip-chars (string direction &optional bound)
  "Behave like `skip-chars-forward' or `skip-chars-backward' given DIRECTION,
except that:

- It signals an error if BOUND is before or after point and DIRECTION is
'forward and 'backward, respectively.
- It returns nil if it fails and the point after move if successful.

For more specific definitions of STRING, see `skip-chars-forward'.
DIRECTION must be either \\='forward or \\='backward."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let* ((skip-fn (brk-sontaku--resolve-direction
                   direction #'skip-chars-forward #'skip-chars-backward))
         (moved (funcall skip-fn string bound)))
    (when (/= moved 0) (point))))

(defun brk-sontaku--skip-same-chars (direction &optional bound)
  "Move through same contiguous chars.
DIRECTION must be either \\='forward or \\='backward.
If BOUND is non-nil, stop before BOUND.
Return nil if it fails and the final point after move if successful."
  (let ((bob-or-eob-p (brk-sontaku--resolve-direction
                       direction #'eobp #'bobp))
        (char-before-or-after (brk-sontaku--resolve-direction
                               direction #'char-after #'char-before)))
    (unless (funcall bob-or-eob-p)
      (brk-sontaku--skip-chars (regexp-quote
                                (char-to-string (funcall char-before-or-after)))
                               direction bound))))

;;;;; Comment

(defun brk-sontaku--in-comment-p (&optional pt)
  "Return non-nil if PT is inside a comment."
  (let ((pt (or pt (point))))
    (eq (syntax-ppss-context (syntax-ppss pt)) 'comment)))

(defun brk-sontaku--skip-comment-block (direction)
  "Move across a contiguous comment block according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

This function is an enhanced version of `forward-comment' in that:

- Instead of nil, it returns the point when it skips before
a single line comment, there is no trailing newline, and
reaches the end of buffer."
  (let ((arg (brk-sontaku--resolve-direction direction 1 -1))
        (skip-whitespace-direction (brk-sontaku--resolve-direction
                                    direction 'backward 'forward))
        (from (point))
        to)
    (if (eq direction 'forward)
        (progn
          (save-excursion
            (when (progn (forward-comment arg)
                         (not (eq (point) from)))
              (setq to (point))))
          (when to (goto-char to)))
      (forward-comment arg)
      (brk-sontaku--skip-whitespace skip-whitespace-direction)
      (point))))

;;;;; Syntax

(defun brk-sontaku--match-syntax-p (syntax &optional pt)
  "Return non-nil if the char at PT belongs to SYNTAX class.
SYNTAX must be:

- a single syntax class char, e.g., ?w, ?_, ?., etc.
- a list of such chars, e.g., \\='(?w ?_ ?.)

For the meaning of syntax class chars, see `modify-syntax-entry'."
  (let* ((pt (or pt (point)))
         (actual (brk-sontaku--syntax-char-after pt)))
    (when actual
      (cond
       ((characterp syntax)
        (eq actual syntax))
       ((and (listp syntax)
             (cl-every #'characterp syntax))
        (memq actual syntax))
       (t (user-error "SYNTAX must be char or list of chars, got %S"
                      syntax))))))

(defun brk-sontaku--skip-syntax (syntax direction &optional bound)
  "Behave like `skip-syntax-forward' or `skip-syntax-backward' given DIRECTION
except that:

- it signals an error if BOUND is before or after point and DIRECTION is
'forward and 'backward, respectively.
- it returns nil if it fails, and the point after move
if successful, respectively.

SYNTAX is a string of syntax code characters and will be passed onto either of them.
DIRECTION must be either \\='forward or \\='backward."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let* ((skip-fn (brk-sontaku--resolve-direction
                   direction #'skip-syntax-forward #'skip-syntax-backward))
         (moved (funcall skip-fn syntax bound)))
    (when (/= moved 0) (point))))

(defun brk-sontaku--skip-same-syntax (direction &optional bound)
  "Move through all chars from the same syntax class.
DIRECTION must be either \\='forward or \\='backward.
If BOUND is non-nil, stop before BOUND.

This function is an enhanced version of `forward-same-syntax' in that
it takes `syntax-table' text properties into account. More precisely:

- `forward-same-syntax' internally uses `char-syntax'
- `char-syntax' does not take `syntax-table' text properties into account.
- The docstring of `char-syntax' recommends using `syntax-after' instead when
the syntax of chars is what you are after."
  (let ((point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point))))))
    (when-let* ((syntax (brk-sontaku--syntax-char-after (funcall point-fn))))
      (brk-sontaku--skip-syntax (char-to-string syntax) direction bound))))

(defun brk-sontaku--skip-same-chars-and-syntax (direction &optional bound)
  "Move across same contiguous chars from the same syntax class.
DIRECTION must be either \\='forward or \\='backward.
If BOUND is non-nil, stop before BOUND."
  (let* ((char-bound (save-excursion
                       (brk-sontaku--skip-same-chars direction bound)))
         (syntax-bound (save-excursion
                         (brk-sontaku--skip-same-syntax direction bound)))
         (min-or-max (brk-sontaku--resolve-direction
                      direction #'min #'max)))
    (when (and char-bound syntax-bound)
      (goto-char (funcall min-or-max char-bound syntax-bound)))))

;;;;; S-Expression

(defun brk-sontaku--at-outermost-p (&optional pt)
  "Return non-nil if PT is at the outermost of the current buffer.
That is, it is not enclosed in any parenthetical constructs,
string, or comment."
  (let ((pt (or pt (point))))
    (and (not (brk-sontaku--in-string-p pt))
         (not (brk-sontaku--in-comment-p pt))
         (not (brk-sontaku--in-parens-p pt)))))

(defun brk-sontaku--in-parens-p (&optional pt)
  "Return non-nil if PT is inside any parenthetical constructs:
(), [], {}, or those in `brk-sontaku--extra-paren-pairs'.

Note that this function temporarily modifies the in-buffer syntax table
by `modify-syntax-entry' to add extra bracket-like pairs
from `brk-sontaku--extra-paren-pairs'."
  (let ((pt (or pt (point))))
    (cl-some
     (lambda (pair)
       (let ((open (car pair))
             (close (cdr pair)))
         (with-syntax-table (copy-syntax-table (syntax-table))
           (modify-syntax-entry (string-to-char open) (concat "(" close))
           (modify-syntax-entry (string-to-char close) (concat ")" open))
           ;; Check if PT is surrounded by any pairs registered as parentheses
           ;; in the in-buffer syntax table.
           (> (nth 0 (syntax-ppss pt)) 0))))
     brk-sontaku--extra-paren-pairs)))

(defun brk-sontaku--in-delims-p (&optional pt)
  "Return non-nil if PT is inside any syntactic delimiter constructs:
parentheses, brackets, braces, strings, comments, or paired tag-like constructs."
  (let ((pt (or pt (point))))
    (or (brk-sontaku--in-string-p pt)
        (brk-sontaku--in-comment-p pt)
        (brk-sontaku--in-parens-p pt))))

(defun brk-sontaku--skip-sexp (direction &optional n)
  "Move across a sexp according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.
With N, do it that many times. Negative arg -N means move
backward across N sexps. That means:

- It moves forward with -N when given \\='backward.
- It moves backward with -N when given \\='forward.

This is a safer version of `forward-sexp'.
The original `forward-sexp' behaves capriciously
depending on major modes:

- In `emacs-lisp-mode', it throws a `scan-error'.
- In `nxml-mode', it throws a plain error.
- In `web-mode', it does nothing and returns nil.

This fixes that variability, always returning nil if it fails."
  (let ((skip-fn (brk-sontaku--resolve-direction
                  direction #'forward-sexp #'backward-sexp)))
    (condition-case _
        (let ((from (point))
              (to (progn (funcall skip-fn n) (point))))
          (unless (eq from to) to))
      (error nil))))

(defun brk-sontaku--primitive-skip-sexp (direction)
  "Move across a sexp according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

This is similar to `forward-sexp'/`backward-sexp', but it takes care of
some behavioral glitches they have. See the implementation for more details."
  (let ((skip-syntax-direction (brk-sontaku--resolve-direction
                                direction 'backward 'forward))
        (skip-char-fn (brk-sontaku--resolve-direction
                       direction #'forward-char #'backward-char))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point))))))
    (if (brk-sontaku--skip-sexp direction)
        (progn (brk-sontaku--skip-syntax " " skip-syntax-direction) (point))
      (when (eq (brk-sontaku--syntax-char-after (funcall point-fn)) ?')
        (funcall skip-char-fn) (point)))))

;;;;; Symbol

(defun brk-sontaku--at-symbol-p (&optional pt)
  "Return non-nil if char at PT has symbol or word syntax.
That classification is based on the docstring of `forward-symbol'.

This function also takes care of a preceding escape syntax
followed by a char of symbol or word syntax. (e.g., \"\\foo\")"
  (let ((pt (or pt (point))))
    (or (memq (brk-sontaku--syntax-char-after pt) '(?_ ?w))
        (and (eq (brk-sontaku--syntax-char-after pt) ?\\)
             (memq (brk-sontaku--syntax-char-after (1+ pt)) '(?_ ?w))))))

(defun brk-sontaku--at-symbol-prefix-p (&optional pt)
  "Return non-nil if char at PT is a symbol prefix.
That is, it returns non-nil when there is a char with a preceding
single quotation mark and PT is right before it.
(e.g., \"*'foo\" where \"*\" is PT)"
  (let ((pt (or pt (point))))
    (save-excursion
      (goto-char pt)
      (and (brk-sontaku--skip-syntax "'" 'forward)
           (brk-sontaku--at-symbol-p)))))

(defun brk-sontaku--skip-symbol (direction &optional bound)
  "Move across a symbol according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

This function skips contiguous chars of the same syntax altogether, rather
regarding them as a symbol, which could be helpful to handle operators
in some programming languages.  If BOUND is non-nil, stop before BOUND.

Unlike `forward-symbol', it returns nil when the current point is:

- at the end of a symbol and DIRECTION is \\='forward.
(i.e., at a whitespace)
- at the beginning of a symbol and DIRECTION is \\='backward."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let* ((from (point))
         (peek-of-primitive-sexp (save-excursion
                                   (brk-sontaku--primitive-skip-sexp direction)))
         (min-or-max (brk-sontaku--resolve-direction
                      direction #'min #'max))
         (point-fn (brk-sontaku--resolve-direction
                    direction #'point (lambda () (1- (point)))))
         (bound (if (and peek-of-primitive-sexp bound)
                    (funcall min-or-max peek-of-primitive-sexp bound)
                  (or peek-of-primitive-sexp bound))))
    (when (eq direction 'forward)
      (and (brk-sontaku--at-symbol-prefix-p)
           (brk-sontaku--skip-same-syntax direction bound)))
    (while (and (brk-sontaku--at-symbol-p (funcall point-fn))
                (brk-sontaku--skip-same-syntax direction bound)))
    (let ((to (point)))
      (unless (eq from to)
        (if (eq direction 'forward)
            to
          (brk-sontaku--skip-syntax "'" direction bound)
          (point))))))

;;;;; String

(defun brk-sontaku--in-string-p (&optional pt)
  "Return non-nil if PT is inside a string."
  (let ((pt (or pt (point))))
    (eq (syntax-ppss-context (syntax-ppss pt)) 'string)))

;; TODO: make it so that this also turns to t when point is in double--camel-case.
;; TODO: make it so that this does nothing when STYLE is either camel or pascal,
;; and CASE-STATE is either upper or lower.
;; TODO: add user-error logic.
(defun brk-sontaku--match-string-case-p (style &optional case-state pt)
  "Return non-nil if the element at PT matches STYLE-case in CASE-STATE.
STYLE must be either of:

- \\='camel: \"camelCase\"
- \\='dot: \"dot.case\"
- \\='kebab: \"kebab-case\"
- \\='pascal: \"PascalCase\"
- \\='snake: \"snake_case\"

CASE-STATE is either \\='upper, \\='lower, or nil.  When STYLE is either
\\='camel or \\='pascal, that does nothing."
  (let* ((pt (or pt (point)))
         (bounds (save-excursion
                   (goto-char pt)
                   (skip-chars-backward "A-Za-z0-9_.-")
                   (let ((beg (point)))
                     (skip-chars-forward "A-Za-z0-9_.-")
                     (let ((end (point)))
                       (when (> end beg)
                         (cons beg end))))))
         ;; TODO: `brk-sontaku--at-symbol-p' behaves unexpectedly.
         ;; Use or make a function that fits the bill here.
         ;; (bounds (when (brk-sontaku--at-symbol-p pt)
         ;;           (save-excursion
         ;;             (goto-char pt)
         ;;             (skip-chars-backward "A-Za-z0-9_.-")
         ;;             (let ((beg (point)))
         ;;               (skip-chars-forward "A-Za-z0-9_.-")
         ;;               (let ((end (point)))
         ;;                 (when (> end beg)
         ;;                   (cons beg end)))))))
         (str (when bounds
                (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (sep (pcase style
                ('dot ".")
                ('kebab "-")
                ('snake "_")
                (_ nil)))
         (case-state-frag (pcase case-state
                            ('lower '(+ (or lower digit)))
                            ('upper '(+ (or upper digit)))
                            (_ '(+ (or alpha upper)))))
         (re (rx-to-string
              (pcase style
                ((or 'dot 'kebab 'snake)
                 `(seq bos
                       ,case-state-frag
                       (+ (seq ,sep ,case-state-frag))
                       eos))
                ('camel
                 `(seq bos
                       (+ lower)
                       (+ (seq upper (* (or lower digit))))
                       eos))
                ('pascal
                 `(seq bos
                       upper (* (or lower digit))
                       (* (seq upper (* (or lower digit))))
                       eos))
                (_
                 `(seq bos
                       alpha (+ alpha)
                       eos))))))
    (and str (string-match-p re str) t)))

(defun brk-sontaku--skip-string (direction)
  "Move across a string in the specified DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

By \"string\", it specifically means:

- String quotes
- String delimiters

Note that this function does nothing when
the current point is inside \"string\".  That built-in logic
keeps unexpected behaviors occurred when called within
themselves at bay."
  (let ((from (point))
        quote to)
    (unless (brk-sontaku--in-string-p)
      (save-excursion
        (when (progn (brk-sontaku--skip-syntax "\\" direction)
                     (or (brk-sontaku--skip-syntax "\"" direction)
                         (brk-sontaku--skip-syntax "|" direction)))
          (setq quote (point))
          (let ((forward-sexp-function nil))
            (goto-char from)
            (or (brk-sontaku--primitive-skip-sexp direction)
                (goto-char quote)))
          (setq to (point))))
      (when (and to (not (eq from to)))
        (goto-char to)))))

;;;;; Word

(defun brk-sontaku--at-word-p (&optional pt)
  "Return non-nil if char at PT has word syntax."
  (let ((pt (or pt (point))))
    (eq (brk-sontaku--syntax-char-after pt) ?w)))

(defun brk-sontaku--match-word-p (word &optional pt)
  "Return non-nil if the word at PT equals WORD.
This behaves like:

- Find the contiguous word or symbol zone around PT
using `skip-syntax-forward' and `skip-syntax-backward' with \"w_\".
- Return non-nil if the text of that zone equals WORD and PT is
inside that zone, but no at its trailing end. (i.e. beg <= PT < end)"
  (unless (stringp word)
    (user-error "WORD must be a string, got %S" word))
  (let* ((pt (or pt (point)))
         (beg (save-excursion
                (goto-char pt)
                (brk-sontaku--skip-syntax "w_" 'backward)
                (point)))
         (end (save-excursion
                (goto-char pt)
                (brk-sontaku--skip-syntax "w_" 'forward)
                (point))))
    ;; Do not regard the trailing end of the word as part of it.
    (when (and (<= beg pt) (< pt end))
      (let ((text (buffer-substring-no-properties beg end)))
        (string= text word)))))

(defun brk-sontaku--skip-word (direction &optional bound)
  "Move across a word according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

Unlike `forward-word' and `backward-word', this returns nil
when the current point is:

- at the end of a word and DIRECTION is \\='forward.
- at the beginning of a word and DIRECTION is \\='backward.

Also, it regards a string in either of the following
cases as a word:

- \"dot.case\"
- \"kebab-case\"
- \"snake_case\"

If BOUND is non-nil, stop before BOUND."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let* ((from (point))
         (over-bound-p (brk-sontaku--resolve-direction
                        direction
                        (lambda (pos bound) (> pos bound))
                        (lambda (pos bound) (< pos bound))))
         (before-bound (brk-sontaku--resolve-direction
                        direction
                        (lambda (bound) (1- bound))
                        (lambda (bound) (1+ bound))))
         (skip-fn (brk-sontaku--resolve-direction
                   direction #'forward-word #'backward-word))
         (point-fn (brk-sontaku--resolve-direction
                    direction #'point (lambda () (1- (point)))))
         (str-cases-p
          (lambda ()
            (cl-some (lambda (case)
                       (brk-sontaku--match-string-case-p case nil (funcall point-fn)))
                     '(dot kebab snake)))))
    (cl-loop while (or (brk-sontaku--at-word-p (funcall point-fn))
                       (funcall str-cases-p))
             do (funcall skip-fn)
             when (and bound (funcall over-bound-p (point) bound))
             do (progn
                  (goto-char (funcall before-bound bound))
                  (cl-return nil))
             finally (let ((to (point)))
                       (unless (eq from to) to)))))

;;;;; Regexp

(defun brk-sontaku--with-regexp-p (regexp &optional direction bound pt)
  "Return non-nil if REGEXP matches relative to PT.
DIRECTION must be either of:

- nil: test REGEXP at point.
- \\='forward: test REGEXP ahead within BOUND.
- \\='backward: test REGEXP behind within BOUND."
  (let ((pt (or pt (point))))
    (pcase direction
      ('nil
       (save-match-data
         (with-current-buffer (current-buffer)
           (save-excursion
             (goto-char pt)
             (looking-at-p regexp)))))
      ('forward
       (save-match-data
         (with-current-buffer (current-buffer)
           (save-excursion
             (goto-char pt)
             (and (re-search-forward regexp bound t)
                  t)))))
      ('backward
       (save-match-data
         (with-current-buffer (current-buffer)
           (save-excursion
             (goto-char pt)
             (and (re-search-backward regexp bound t)
                  t)))))
      (_ (user-error "Invalid DIRECTION %S" direction)))))

;;;;; Dispatch tables

(defconst brk-sontaku--unit-dispatch-table
  '((chars . brk-sontaku--skip-chars)
    (string . brk-sontaku--skip-string)
    (comment . brk-sontaku--skip-comment-block)
    (same-char . brk-sontaku--skip-same-chars)
    (same-syntax . brk-sontaku--skip-same-syntax)
    (same-chars-and-syntax . brk-sontaku--skip-same-chars-and-syntax)
    (sexp . brk-sontaku--skip-sexp)
    (symbol . brk-sontaku--skip-symbol)
    (syntax . brk-sontaku--skip-syntax)
    (whitespace . brk-sontaku--skip-whitespace)
    (word . brk-sontaku--skip-word))
  "An alist mapping unit names to the corresponding primitive action functions.")

(defconst brk-sontaku--pred-dispatch-table
  '((at-bob . bobp)
    (at-bol . bolp)
    (at-eob . eobp)
    (at-eol . eolp)
    (at-outermost . brk-sontaku--at-outermost-p)
    (at-symbol . brk-sontaku--at-symbol-p)
    (at-symbol-prefix . brk-sontaku--at-symbol-prefix-p)
    (at-whitespace . brk-sontaku--at-whitespace-p)
    (at-word . brk-sontaku--at-word-p)
    (in-string . brk-sontaku--in-string-p)
    (in-comment . brk-sontaku--in-comment-p)
    (in-parens . brk-sontaku--in-parens-p)
    (in-delims . brk-sontaku--in-delims-p)
    (match-string-case . brk-sontaku--match-string-case-p)
    (match-char . brk-sontaku--match-char-p)
    (match-chars . brk-sontaku--match-chars-p)
    (match-syntax . brk-sontaku--match-syntax-p)
    (match-word . brk-sontaku--match-word-p)
    (with-regexp . brk-sontaku--with-regexp-p))
  "An alist mapping predicates to the corresponding predicate functions.")

;;;; Commands

;; TODO: Refine docstring.
;;;###autoload
(defun brk-sontaku-expand-region ()
  "Expand current region according to the specified strategy for current major mode."
  (interactive)
  (let ((orig-bounds (if (use-region-p)
                         (cons (region-beginning) (region-end))
                       (cons (point) (point))))
        (sm-fn (brk-sontaku--get-state-machine major-mode)))
    (pcase-let* ((target-bounds (funcall
                                 (or sm-fn
                                     brk-sontaku-default-state-machine-function)))
                 (`(,beg . ,end) target-bounds))
      (if (brk-sontaku--interval-contains-p target-bounds orig-bounds 'proper)
          (progn
            (brk-sontaku--maybe-reset-region-history)
            (brk-sontaku--mark-region beg end replace-mark)
            (brk-sontaku--update-region-history beg end))
        (user-error "Cannot expand region further")))))

;;;###autoload
(defun brk-sontaku-contract-region (arg)
  "Contract current region according to the specified strategy for current major mode.
When given a numeric prefix argument ARG, contract that many times.
If ARG is negative, perform expansion instead."
  (interactive "p")
  (brk-sontaku--maybe-reset-region-history)
  (cond ((< arg 0)
         (brk-sontaku-expand-region))
        ((>= arg 0)
         (when brk-sontaku--region-history
           (when (= arg 0)
             (setq arg (length brk-sontaku--region-history)))
           (while (and (cdr brk-sontaku--region-history)
                       (> arg 0))
             (setq arg (1- arg))
             (pop brk-sontaku--region-history))
           (pcase-let ((`(,beg . ,end) (car brk-sontaku--region-history)))
             (set-mark beg)
             (goto-char end))))))

;;;; Macro User Interface

(defun brk-sontaku--form-head (form)
  "Return the car of FORM if it is a cons,
otherwise return nil."
  (when (consp form) (car form)))

(defun brk-sontaku--compile-pred-form (pred-form)
  "Compile PRED-FORM into an executable form.
Return a form that evaluates to non-nil when matched.
PRED-FORM should look like:

- logical combinators: (and ...), (or ...), (not ...)
- a symbol: \\='default
- (in-string), (at-char \"@\"), (with-pred (lambda () (...)))
- a user-defined predicate function symbol: treat it as a function

Otherwise, it signals an error.
For valid predicate keywords, see `brk-sontaku--pred-dispatch-table'.
"
  (let* ((head (brk-sontaku--form-head pred-form))
         (pred (alist-get head brk-sontaku--pred-dispatch-table)))
    (cond
     ;; Handle the default case here.
     ((eq pred-form 'default) 't)
     ;; Accept built-in `and', and `or'. Recurse this for the inner.
     ((eq head 'and)
      `(and ,@(mapcar (lambda (f) (brk-sontaku--compile-pred-form f))
                      (cdr pred-form))))
     ((eq head 'or)
      `(or ,@(mapcar (lambda (f) (brk-sontaku--compile-pred-form f))
                     (cdr pred-form))))
     ;; Accept built-in `not'.
     ((eq head 'not)
      `(not ,(brk-sontaku--compile-pred-form (cadr pred-form))))
     ;; Dispatch reserved predicate cases.
     ((not (null pred))
      (let ((fn (cdr pred)))
        `(,fn ,@(cdr pred-form))))
     ;; If it is a symbol/predicate name, assume it is a predicate to call.
     ((symbolp pred-form)
      `(funcall #',pred-form))
     ;; Otherwise, signal an error.
     (t (user-error "Invalid predicate form: %S" pred-form)))))

(defun brk-sontaku--compile-action-form (action-form)
  "Compile ACTION-FORM into a sequential unit operation.
ACTION-FORM should look like (unit NAME [COUNT]) or a list of them,
where NAME is a key of an alist reserved in
`brk-sontaku--unit-dispatch-table' and COUNT is a positive integer.
For example:

- (unit word)
- (unit char 3)
- ((unit char 3) (unit word))

This validates NAME against `brk-sontaku--unit-dispatch-table'
and returns the corresponding predicate function when matched
upon macro expansion.
At runtime, it is called and returns the last bounds after
applying all unit actions."
  (let ((actions (if (and (consp action-form)
                          (not (eq (car action-form) 'unit)))
                     action-form
                   (list action-form))))
    `(save-excursion
       (let (last-bounds)
         ,@(mapcar
            (lambda (a)
              ;; cadr a: (unit UNIT-NAME COUNT) --> UNIT-NAME
              ;; caddr a: (unit UNIT-NAME COUNT) --> COUNT
              (let* ((unit-name (cadr a))
                     (count (caddr a))
                     (entry (alist-get unit-name brk-sontaku--unit-dispatch-table)))
                (if entry
                    (let ((fn (cdr entry)))
                      (pcase a
                        (`(unit ,unit-name ,count)
                         (unless (and (integerp count) (> count 0))
                           (user-error "COUNT must be a positive integer in %S" a))
                         `(dotimes (_ ,count)
                            (setq last-bounds (,fn))))
                        (`(unit ,unit-name)
                         `(setq last-bounds (,fn)))
                        (_ (user-error "Invalid unit action syntax: %S" a))))
                  (user-error "Unknown unit action: %S" unit-name))))
            actions)
         last-bounds))))

(defun brk-sontaku-compile-done (done-val)
  "Compile a done clause into a predicate form, which checks
whether to stop proceeding at every state change,
If it returns non-nil, abort the state machine.
DONE-VAL must be either of:

 - a key of an alist reserved in `brk-sontaku--pred-dispatch-table':
call the corresponding predicate function.
 - the other symbol: call it as a function.
 - a lambda or a function: simply call it.

Otherwise, it signals an error."
  (cond
   ((assq done-val brk-sontaku--pred-dispatch-table)
    (let ((fn (alist-get done-val brk-sontaku--pred-dispatch-table)))
      `(,fn)))
   ((symbolp done-val)
    `(funcall #',done-val))
   ((and (consp done-val) (memq (car done-val) '(lambda function)))
    done-val)
   (t (user-error "Invalid :done form: %S" done-val))))

(cl-defmacro brk-sontaku-define-mode-state-machine (mode docstring &rest strategies &key done)
  "Define a state machine-based expansion/contraction/action strategy for MODE.
MODE is a symbol that must be a valid derived major mode. (e.g., `org-mode')
DOCSTRING is the function's docstring.

Multiple :strategy clauses may be given.  The generated function will be
named `sontaku--MODE-state-machine' and registered in
`brk-sontaku--mode-state-machine-registry' under the MODE symbol."
  (declare (debug (symbolp stringp body [":done" [&or symbolp form]]))
           (indent defun))
  (unless (brk-sontaku--valid-derived-major-mode-symbol-p mode)
    (user-error "brk-sontaku-define-mode-state-machine: `%s' is not
a valid derived major mode function" mode))
  (when (brk-sontaku--get-state-machine mode)
    (message "brk-sontaku--mode-state-machine-registry: Redefining
an expansion strategy for `%s'" mode))
  (let ((strategy-forms (cl-loop for (key val) on strategies by #'cddr
                                 when (eq key :strategy)
                                 collect val))
        (fn-name (intern (format "brk-sontaku--%s-state-machine" mode)))
        (fn-docstring (format "A state machine auto-generated for `%s'.\n\n%s"
                              mode docstring)))
    `(progn
       (brk-sontaku--register-state-machine ',mode ',fn-name)

       (defun ,fn-name ()
         ,fn-docstring
         (let ((done-expr (brk-sontaku-compile-done done)))
           (if (,done-expr)
               (user-error "Current region reaches the bounds limit.")
             (cond
              ,@(cl-loop
                 for sf in strategy-forms
                 collect
                 (let* ((pred-form (car sf))
                        (action-form (cdr sf))
                        (pred-expr (brk-sontaku--compile-pred-form pred-form))
                        (action-expr (brk-sontaku--compile-action-form action-form)))
                   `(,pred-expr ,action-expr))))))))))

;;;; Built-in Expansion Strategies

;; TODO: Add more edge cases if you find them.
;; NOTE: We don't need this. Delegate this logic building part to users.
;; That said, this can be helpful when writing strategies in terms of
;; the orders actions should be called and strategies.
;; (defun brk-sontaku--skip-syntax-block (direction &optional limit)
;;   "Move across a syntax block according to DIRECTION.
;; When LIMIT is non-nil, do not move past LIMIT."
;;   (let* ((point-fn (brk-sontaku--resolve-direction
;;                     direction #'point (lambda () (1- (point)))))
;;          (syntax-char (brk-sontaku--syntax-char-after (funcall point-fn)))
;;          (syntax-classes (brk-sontaku--resolve-direction
;;                           direction '(?\( ?$) '(?\) ?$))))
;;     (or (brk-sontaku--move-within #'brk-sontaku--skip-symbol direction limit)
;;         (brk-sontaku--move-within #'brk-sontaku--skip-string direction limit)
;;         (brk-sontaku--move-within #'brk-sontaku--skip-comment-block direction limit)
;;         (when (memq syntax-char syntax-classes)
;;           (let ((forward-sexp-function nil))
;;             (brk-sontaku--move-within #'brk-sontaku--primitive-skip-sexp
;;                                       direction limit)))
;;         (when (eq syntax-char ?.)
;;           (progn (forward-char) (point)))
;;         (brk-sontaku--move-within #'brk-sontaku--skip-same-chars-and-syntax
;;                                   direction limit))))

;; TODO: add OOTB strategies for some popular major modes.
;; emacs-lisp-mode, org-mode.

;; TODO: add a default strategy that works as a fallback. (Maybe the one like puni?)
;; That can be used in `brk-sontaku-expand-region' in case no state machine-fn found
;; for the current mode.

(provide 'brk-sontaku)
;;; brk-sontaku.el ends here
