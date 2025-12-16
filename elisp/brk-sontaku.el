;;; brk-sontaku.el --- Context-aware region expander -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience, tools
;; URL: https://github.com/brklntmhwk/elisp/brk-sontaku.el
;; Package-Requires: ((emacs "26.1"))
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

(defvar brk-sontaku--mode-navigator-registry nil
  "Registry mapping mode symbols to their navigator functions.
Exceptionally, this accepts \\='default for the default navigator.")

(defvar brk-sontaku--expander-registry nil
  "Registry mapping expander names to the functions.")

(defvar brk-sontaku--region-history nil
  "History of expanded regions for contraction support.")

(make-variable-buffer-local 'brk-sontaku--region-history)

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

(defcustom brk-sontaku-default-navigator-function
  #'brk-sontaku--default-navigator
  "Default navigator function."
  :type '(choice (const :tag "Default navigator"
                        brk-sontaku--default-navigator)
                 (function :tag "Custom navigator"))
  :group 'brk-sontaku)

(defcustom brk-sontaku-default-expander-function
  #'brk-sontaku--balanced-expander
  "Default expander function."
  :type '(choice (const :tag "Default expander"
                        brk-sontaku--balanced-expander)
                 (function :tag "Custom expander"))
  :group 'brk-sontaku)

(defcustom brk-sontaku-mode-expander-alist
  '((emacs-lisp-mode . linear)
    (org-mode . alternate))
  "Alist mapping derived major modes to expander functions' styles.
It must be one of the style names registered in `brk-sontaku--expander-registry'."
  :type '(alist :key-type symbol :value-type symbol))

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

;;;; Misc

(defun brk-sontaku--resolve-direction (direction forward-val backward-val)
  "Return a value according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

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

- at the beginning, return \\='backward
- at the end, return \\='forward

When no active region exists, return nil."
  (when (use-region-p)
    (if (> (mark) (point))
        'backward
      'forward)))

(defun brk-sontaku--mark-region (beg end &optional replace-mark alternate)
  "Mark and activate a region between BEG and END.

If REPLACE-MARK is non-nil, replace the existing mark with the new one.
Otherwise, push the existing one into mark ring.

When ALTERNATE is non-nil, move the point to the other end of region after
activating the new region marker."
  (let* ((direction (or (brk-sontaku--active-region-direction)
                        'forward))
         (mark-pos (brk-sontaku--resolve-direction direction beg end))
         (goto-pos (brk-sontaku--resolve-direction direction end beg)))
    (setq deactivate-mark nil)
    (if replace-mark
        (set-mark mark-pos)
      (push-mark mark-pos 'nomsg))
    (goto-char goto-pos)
    (activate-mark)
    (when alternate
      (exchange-point-and-mark))))

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
Both OUTER and INNER must be cons cells of integer \"(BEG . END)\".
MODE determines the containment type:

- 'strict: strict containment (<)
- 'proper: inclusive but equal is not allowed
- nil: inclusive containment (<=)
- none of them listed above: signal an error

If OUTER or INNER is nil, return nil."
  (when (and outer inner)
    (cl-destructuring-bind (obeg . oend) outer
      (cl-destructuring-bind (ibeg . iend) inner
        (pcase mode
          ('strict
           (< obeg ibeg iend oend))
          ('proper
           (and (<= obeg ibeg iend oend)
                (or (/= obeg ibeg)
                    (/= iend oend))))
          ((pred null)
           (<= obeg ibeg iend oend))
          (_
           (error "Invalid MODE: %S (must be 'strict, 'proper, or nil)" mode)))))))

(defun brk-sontaku--interval-winner-between (i1 i2 winner)
  "Return either I1 or I2 depending on WINNER.
If WINNER is:

- 'bigger, return the one that contains the other.
- 'smaller, return the one that is contained by the other.
- none of them listed above, signal an error.

Both I1 and I2 must be cons cells of integer.  If I1 or I2 is nil, or
neither of them contains the other, return nil."
  (when (and i1 i2)
    (pcase winner
      ('bigger (cond ((brk-sontaku--interval-contains-p i1 i2) i1)
                     ((brk-sontaku--interval-contains-p i2 i1) i2)))
      ('smaller (cond ((brk-sontaku--interval-contains-p i1 i2) i2)
                      ((brk-sontaku--interval-contains-p i2 i1) i1)))
      (_ (error "Invalid WINNER: %S (must be 'bigger or 'smaller)" winner)))))

;; TODO: Revisit this and think over when to use it.
(defun brk-sontaku--find-next-bounds (bounds expander)
  "Return next bigger bounds according to EXPANDER.
BOUNDS is the original bounds to start from."
  (brk-sontaku--interval-winner-between
   (save-excursion (goto-char (car bounds))
                   (funcall expander))
   (save-excursion (goto-char (cdr bounds))
                   (funcall expander))
   'bigger))

(defun brk-sontaku--beyond-bound-p (direction bound &optional pos)
  "Return non-nil if POS is beyond BOUND according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.
Technically, it returns non-nil when:

- DIRECTION is \\='forward and POS > BOUND
- DIRECTION is \\='backward and POS < BOUND."
  (let* ((pos (or pos (point)))
         (beyond-bound-p (brk-sontaku--resolve-direction
                          direction
                          (lambda (bound pos) (> pos bound))
                          (lambda (bound pos) (< pos bound)))))
    (funcall beyond-bound-p)))

(defun brk-sontaku--valid-derived-major-mode-symbol-p (symbol)
  "Return non-nil if SYMBOL is a valid derived major mode."
  (and (symbolp symbol)
       (fboundp symbol)
       (not (memq symbol minor-mode-list))
       (string-suffix-p "-mode" (symbol-name symbol) t)
       (> (length (derived-mode-all-parents symbol)) 1)))

(defun brk-sontaku--skip-chars (string direction &optional bound)
  "Behave like `skip-chars-forward' or `skip-chars-backward' given DIRECTION,
except that:

- it signals an error if BOUND is before or after point and DIRECTION is
'forward and 'backward, respectively.
- it returns nil if it fails and the point after move if successful.

For more specific definitions of STRING, see `skip-chars-forward'.
DIRECTION must be either \\='forward or \\='backward."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let* ((skip-fn (brk-sontaku--resolve-direction
                   direction #'skip-chars-forward #'skip-chars-backward))
         (moved (funcall skip-fn string bound)))
    (when (/= moved 0) (point))))

(defun brk-sontaku--skip-sexp (direction &optional n)
  "Move across a sexp according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.
With N, do it that many times. Negative arg -N means move
backward across N sexps. That means:

- It moves forward with -N when given \\='backward.
- It moves backward with -N when given \\='forward.

This is a safer version of `forward-sexp'.
The original `forward-sexp' behaves capriciously
depending on the major modes:

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

(defun brk-sontaku--primitive-skip-sexp (direction)
  "Move across a sexp according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

This is similar to `forward-sexp'/`backward-sexp', but it takes care of
some behavioral glitches (quirks?) they have.  See the implementation
for more details."
  (let ((skip-syntax-direction (brk-sontaku--resolve-direction
                                direction 'backward 'forward))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point)))))
        (skip-char-fn (brk-sontaku--resolve-direction
                       direction #'forward-char #'backward-char)))
    (if (brk-sontaku--skip-sexp direction)
        ;; Play it safe and skip back whitespace ahead of the point.
        (progn (brk-sontaku--skip-syntax " " skip-syntax-direction)
               (point))
      ;; Skip an expression prefix ahead if present.
      ;; In the example below, "*" is the point.
      ;; e.g., (foo *') --> 'forward --> (foo '*)
      (when (eq (brk-sontaku--syntax-char-after (funcall point-fn)) ?')
        (funcall skip-char-fn) (point)))))

(defun brk-sontaku--beginning-of-list-around-point ()
  "Move to the beginning of the list around point.
Return the point when successful, otherwise return nil."
  (let (moved)
    (while (brk-sontaku--primitive-skip-sexp 'backward)
      (setq moved t))
    (when moved (point))))

(defun brk-sontaku--end-of-list-around-point ()
  "Move to the end of the list around point.
Return the point when successful, otherwise return nil."
  (let (moved)
    (while (brk-sontaku--primitive-skip-sexp 'forward)
      (setq moved t))
    (when moved (point))))

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
           (ok t))
      (setq ok
            (pcase direction
              ('forward (<= to limit))
              ('backward (>= to limit))
              (_ nil)))
      (unless ok
        (goto-char from))
      ok)))

(defun brk-sontaku--register-navigator (mode fn)
  "Register FN in `brk-sontaku--mode-navigator-registry'
as the navigator for MODE.  It is stored as an alist of (MODE . FN)."
  (let ((cell (assoc mode brk-sontaku--mode-navigator-registry)))
    (if cell
        (setcdr cell fn)
      (push (cons mode fn)
            brk-sontaku--mode-navigator-registry))))

(defun brk-sontaku--get-navigator (mode)
  "Return the navigator function for MODE.
If not found, return nil.

This refers to `brk-sontaku--mode-state-machine-registry'."
  (alist-get mode brk-sontaku--mode-navigator-registry))

(defun brk-sontaku--call-navigator (direction &optional pos)
  "Call the navigator function at POS for `major-mode' according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward."
  (let ((pos (or pos (point)))
        (sm-fn (brk-sontaku--get-navigator major-mode)))
    (save-excursion
      (goto-char pos)
      (funcall
       (or sm-fn
           brk-sontaku-default-navigator-function)
       direction))))

(defun brk-sontaku--register-expander (style fn)
  "Register FN in `brk-sontaku--expander-registry'
as the STYLE expander.  It is stored as an alist of (STYLE . FN)."
  (let ((cell (assoc style brk-sontaku--expander-registry)))
    (if cell
        (setcdr cell fn)
      (push (cons style fn)
            brk-sontaku--expander-registry))))

(defun brk-sontaku--get-expander-style (mode)
  "Return a symbol that depicts the expander style for MODE."
  (alist-get mode brk-sontaku-mode-expander-alist))

(defun brk-sontaku--get-expander (mode)
  "Return the expander function for MODE.
If not found, return nil.

This refers to `brk-sontaku--expander-registry'."
  (if-let* ((style (brk-sontaku--get-expander-style mode))
            (fn (alist-get style brk-sontaku--expander-registry)))
      fn
    brk-sontaku-default-expander-function))

;;;; Syntax Reference & Utils

(defun brk-sontaku--syntax-class-to-char (syntax-class)
  "Return the designator char of SYNTAX-CLASS."
  (aref " .w_()'\"$\\/<>@!|" syntax-class))

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

(defun brk-sontaku--syntactic-depth-at (&optional pt)
  "Return syntactic depth at PT."
  (let ((pt (or pt (point))))
    (car (syntax-ppss pt))))

(defun brk-sontaku--syntax-string-start (&optional pt)
  "Return the beginning position of string or comment at PT.
If PT is not inside either of them, return nil."
  (let ((pt (or pt (point))))
    (nth 8 (syntax-ppss pt))))

(defun brk-sontaku--safe-scan-lists (n depth &optional pt)
  "Safely scan and return the position N lists away from PT.
Return nil if:

- the scan reaches the beginning or end of the accessible part
in the middle.
- the depth at that point is zero

For more detailed information, see `scan-lists'."
  (let ((pt (or pt (point))))
    (ignore-errors (scan-lists pt n depth))))

(defun brk-sontaku--safe-scan-sexps (n &optional pt)
  "Safely scan and return the position N sexps away from PT.
Return nil if the scan reaches the beginning or end of the
accessible part in the middle.

For more detailed information, see `scan-sexps'."
  (let ((pt (or pt (point))))
    (ignore-errors (scan-sexps pt n))))

;;;; Navigator Predicates & Unit Actions

;;;;; Empty line

(defun brk-sontaku--at-empty-line-p (&optional pt)
  "Return non-nil if text at PT is an empty line."
  (let ((pt (or pt (point))))
    (save-excursion
      (goto-char pt)
      (beginning-of-line)
      (looking-at-p (rx (* (any " \t")) eol)))))

(defun brk-sontaku--empty-line-action (direction &optional bound)
  "Move across contiguous empty lines according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.
If BOUND is before or after point and DIRECTION is
'forward and 'backward, respectively, signal an error."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let ((n (brk-sontaku--resolve-direction
            direction 1 -1))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point)))))
        moved)
    (cl-block nil
      (while t
        (let ((next-pos (+ (point) n)))
          (when (and bound
                     (brk-sontaku--beyond-bound-p direction bound next-pos))
            (cl-return))
          (when (or (<= next-pos (point-min))
                    (>= next-pos (point-max)))
            (cl-return))
          (if (brk-sontaku--at-empty-line-p (funcall point-fn))
              (progn
                (setq moved t)
                (goto-char next-pos))
            (cl-return)))))
    (when moved (point))))

;;;;; Whitespace

(defun brk-sontaku--at-whitespace-p (&optional pt)
  "Return non-nil if the character at PT is whitespace.
i.e., a whitespace or a newline."
  (let ((pt (or pt (point))))
    ;; "? " stands for whitespace syntax class.
    (eq (brk-sontaku--syntax-char-after pt) ? )))

(defun brk-sontaku--whitespace-action (direction &optional bound)
  "Move across contiguous whitespace according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.
If BOUND is before or after point and DIRECTION is
'forward and 'backward, respectively, signal an error."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let ((n (brk-sontaku--resolve-direction
            direction 1 -1))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point)))))
        moved)
    (cl-block nil
      (while t
        (let ((next-pos (+ (point) n)))
          (when (and bound
                     (brk-sontaku--beyond-bound-p direction bound next-pos))
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
    (when (and ch (<= (1+ pt) (point-max)))
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
    (when (and ch (<= (1+ pt) (point-max)))
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
        (memq ch targets)))))

(defun brk-sontaku--char-action (direction)
  "Move across a single character according to DIRECTION.

This simply integrates `forward-char' and `backward-char'
into a single function by taking DIRECTION as an argument.
DIRECTION must be either \\='forward or \\='backward."
  (let ((skip-fn (brk-sontaku--resolve-direction
                  direction #'forward-char #'backward-char)))
    (funcall skip-fn)
    (point)))

(defun brk-sontaku--same-chars-action (direction &optional bound)
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

(defun brk-sontaku--at-first-comment-starter-p (&optional pt)
  "Return non-nil if PT is at the first comment starter.
Technically, it returns non-nil when PT is at:

- a syntactic comment starter and the beginning of a comment line.
(e.g., \"|;; comment.\" in Emacs Lisp)
- the beginning of a comment line but not at a syntactic comment
starter.
(e.g., \"|/* comment. */\" in JavaScript)

When it comes to a comment block that traverses multiple lines,
it implys the first line's one."
  (let* ((pt (or pt (point)))
         (next-pos (1+ pt))
         (prev-pos (1- pt)))
    (save-excursion
      (cl-labels
          ((in-comment-p (pos)
             (brk-sontaku--in-comment-p pos))
           (starter-p (pos)
             (eq (brk-sontaku--syntax-char-after pos) ?<))
           (first-starter-p (pos)
             (and (starter-p pos)
                  (save-excursion
                    (goto-char pos)
                    (brk-sontaku--skip-syntax "<" 'backward)
                    (= pos (point)))))
           (comment-delim-p (pos)
             ;; e.g., "/* ... */" in JavaScript --> Punctuation chars.
             (and (memq (brk-sontaku--syntax-char-after pos) '(?< ?.)) t))
           (first-comment-delim-p (pos)
             (and (comment-delim-p pos)
                  (save-excursion
                    (goto-char pos)
                    (brk-sontaku--skip-syntax "." 'backward)
                    (= pos (point)))))
           (prev-line-beg-of-comment-p ()
             (save-excursion
               (goto-char pt)
               (previous-logical-line)
               (first-starter-p (point)))))
        (or
         ;; At the beginning of buffer and either a syntactic
         ;; comment starter or a comment delimiter.
         (and (bobp)
              (or (starter-p pt) (comment-delim-p pt)))
         (and (not (in-comment-p pt))
              (or
               ;; At the first comment starter and the beginning
               ;; of a comment line.
               (first-starter-p pt)
               ;; At the first comment delimiter and the beginning
               ;; of a comment line.
               (first-comment-delim-p pt))
              (not (prev-line-beg-of-comment-p))))))))

(defun brk-sontaku--at-last-comment-ender-p (&optional pt)
  "Return non-nil if PT is at the last comment ender.
Technically, it returns non-nil when PT is at:

- a syntactic comment ender but not in a comment block.
(e.g., \"/* comment. */|\" in JavaScript)
- a syntactic comment ender and in a comment block.
(e.g., \";; comment.|\" in Emacs Lisp)

When it comes to a comment block that traverses multiple lines,
it implys the last line's one."
  (let* ((pt (or pt (point)))
         (next-pos (1+ pt))
         (prev-pos (1- pt)))
    (save-excursion
      (cl-labels
          ((ender-p (pos)
             (eq (brk-sontaku--syntax-char-after pos) ?>))
           (in-comment-p (pos)
             (brk-sontaku--in-comment-p pos))
           (at-empty-line-p (pos)
             (brk-sontaku--at-empty-line-p pos))
           (next-line-end-of-comment-p ()
             (save-excursion
               (goto-char next-pos)
               (and (bolp)
                    (progn
                      (end-of-line)
                      ;; NOTE: This still doesn't take care of multiple
                      ;; trailing inline comments across contiguous lines.
                      ;; In that case, this pred results in nil.
                      (not (in-comment-p (point))))))))
        (or
         ;; At a comment ender and in a comment block.
         (and (in-comment-p pt)
              (ender-p pt)
              (or (and (at-empty-line-p next-pos)
                       (ender-p next-pos))
                  (next-line-end-of-comment-p)))
         ;; At a comment ender but not in a comment block.
         (and (not (in-comment-p pt))
              (in-comment-p prev-pos)
              (not (save-excursion (goto-char pt) (bolp)))
              (or (and (at-empty-line-p next-pos)
                       (ender-p next-pos))
                  (next-line-end-of-comment-p))))))))

(defun brk-sontaku--at-comment-sentence-beg-p (&optional pt)
  "Return non-nil if PT is at the beginning of a comment
sentence.
In a comment block, for example:

- \";; |Comment\"
- \"/* |Comment */ \"

When it comes to a comment block that traverses multiple lines,
it implys the first line's one."
  (let ((pt (or pt (point))))
    (cl-labels
        ((in-comment-p (pos)
           (brk-sontaku--in-comment-p pos))
         (goto-comment-start (pos)
           (goto-char (brk-sontaku--syntax-string-start pos)))
         (forward-inside-comment-prefixes (pos)
           (goto-char pos)
           ;; Consider the JavaScript style of multiple line comment.
           (brk-sontaku--skip-chars " \t\n*" 'forward))
         (at-comment-delim-p (pos)
           (and (memq (brk-sontaku--syntax-char-after pos) '(?< ?> ?.)) t))
         (prev-empty-line-p ()
           (save-excursion
             (beginning-of-line)
             (and (not (bobp))
                  (progn
                    (previous-logical-line)
                    (brk-sontaku--at-empty-line-p)))))
         (beg-of-comment-sentence-p (pos)
           (save-excursion
             (goto-comment-start pos)
             (if (/= (line-number-at-pos pos t)
                     (line-number-at-pos (point) t))
                 ;; Take care of multiple line comment blocks.
                 (progn
                   (re-search-forward comment-start-skip
                                      (line-end-position) t nil)
                   (forward-line)
                   (forward-inside-comment-prefixes (point))
                   (= pos (point)))
               ;; Otherwise, apply conditions for single line ones.
               (and (not (at-comment-delim-p pos))
                    (at-comment-delim-p (point))
                    (boundp 'comment-start-skip)
                    comment-start-skip
                    (re-search-forward comment-start-skip
                                       (line-end-position) t nil)
                    (and (= pos (match-end 0))
                         (if (not (prev-empty-line-p))
                             (progn (backward-word)
                                    (not (in-comment-p (point))))
                           t)))))))
      (and (in-comment-p pt)
           (beg-of-comment-sentence-p pt)))))

(defun brk-sontaku--at-comment-sentence-end-p (&optional pt)
  "Return non-nil if PT is at the end of a comment sentence.
In a comment block, for example:

- \";; Comment|\"
- \"/* Comment| */ \"

When it comes to a comment block that traverses multiple lines,
it implys the last line's one."
  (let ((pt (or pt (point))))
    (cl-labels
        ((goto-comment-start (pos)
           (goto-char (brk-sontaku--syntax-string-start pos)))
         (goto-comment-end (pos)
           (goto-comment-start pos)
           (forward-comment 1))
         (in-comment-p (pos)
           (brk-sontaku--in-comment-p pos))
         (backward-empty-line-and-whitespace (pos)
           (goto-char pos)
           (brk-sontaku--skip-chars " \t\n" 'backward))
         (end-of-comment-sentence-p (pos)
           (save-excursion
             (goto-comment-end pos)
             (backward-empty-line-and-whitespace (point))
             (if (and (boundp 'comment-end-skip)
                      comment-end-skip
                      (re-search-backward comment-end-skip
                                          (line-beginning-position) t nil))
                 (progn
                   (goto-char (point))
                   (backward-empty-line-and-whitespace (point))
                   (= pos (point)))
               (and (= pos (point))
                    (if (not (brk-sontaku--at-empty-line-p (1+ (point))))
                        (progn (forward-word)
                               (not (in-comment-p (point))))
                      t))))))
      (and (in-comment-p pt)
           (end-of-comment-sentence-p pt)))))

(defun brk-sontaku--comment-action (direction)
  "Move across a comment block according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

This function behaves like `forward-comment' except that:

- it returns nil and does nothing when the char ahead is
whitespace.
- when DIRECTION is \\='forward, it stops at the end of
the comment without moving to the next line.
- when DIRECTION is \\='backward and the point is at the
end of the comment, it skips to the beginning instead of
returning nil.
- instead of nil, it returns the point when it skips before
a single line comment, there is no trailing newline, and
reaches the end of buffer."
  (let ((arg (brk-sontaku--resolve-direction direction 1 -1))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point))))))
    (if (brk-sontaku--at-whitespace-p (funcall point-fn))
        nil
      (let* ((from (point))
             (to
              (pcase arg
                ;; \\='forward
                (1 (save-excursion
                     (forward-comment arg)
                     (while (or (bolp) (not (brk-sontaku--in-comment-p)))
                       ;; Move back to the end of the comment
                       ;; skipped just now.
                       (backward-char))
                     (unless (= (point) from)
                       (point))))
                ;; \\='backward
                (-1 (save-excursion
                      (when (eolp)
                        ;; Move forward one char in advance for
                        ;; `forward-comment' to work as expected.
                        (forward-char))
                      (when (forward-comment arg)
                        (brk-sontaku--whitespace-action 'forward))
                      (unless (brk-sontaku--in-comment-p)
                        ;; These bring it back to the beginning of
                        ;; the comment block.
                        (forward-comment 1)
                        (forward-comment -1))
                      (unless (= (point) from)
                        (point)))))))
        (when to
          (goto-char to)
          to)))))

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

(defun brk-sontaku--syntax-action (direction &optional bound)
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

(defun brk-sontaku--same-chars-and-syntax-action (direction &optional bound)
  "Move across same contiguous chars from the same syntax class.
DIRECTION must be either \\='forward or \\='backward.
If BOUND is non-nil, stop before BOUND."
  (let* ((char-bound (save-excursion
                       (brk-sontaku--same-chars-action direction bound)))
         (syntax-bound (save-excursion
                         (brk-sontaku--syntax-action direction bound)))
         (min-or-max (brk-sontaku--resolve-direction
                      direction #'min #'max)))
    (when (and char-bound syntax-bound)
      (goto-char (funcall min-or-max char-bound syntax-bound)))))

;;;;; S-Expression

(defun brk-sontaku--at-top-level-p (&optional pt)
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

(defun brk-sontaku--sexp-action (direction &optional bound)
  "Move across a sexp according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.
If BOUND is non-nil, stop before BOUND.

Unlike `forward-sexp' and `backward-sexp', this returns nil
and does nothing when the current point is:

inside a string and:
- at the beginning of a string sentence(s) and DIRECTION is \\='backward.
- at the end of a string sentence(s) and DIRECTION is \\='forward.

inside a comment block and:
- the next call will move the point beyond the current comment block.

For the meaning of \"string\", see `brk-sontaku--string-action'."
  (brk-sontaku--error-if-wrong-bound-pos direction bound)
  (let ((from (point))
        (at-string-peak-p
         (brk-sontaku--resolve-direction
          direction
          #'brk-sontaku--at-string-sentence-end-p
          #'brk-sontaku--at-string-sentence-beg-p)))
    (cond
     ((brk-sontaku--in-comment-p)
      (let* ((orig (point))
             (next (save-excursion
                     (goto-char orig)
                     (brk-sontaku--primitive-skip-sexp direction)))
             (beg (brk-sontaku--syntax-string-start orig))
             (end (save-excursion
                    (goto-char beg)
                    (forward-comment 1)
                    (point))))
        (when (and (< beg next) (> end next))
          (brk-sontaku--primitive-skip-sexp direction))))
     ((brk-sontaku--in-string-p)
      (unless (funcall at-string-peak-p)
        (brk-sontaku--primitive-skip-sexp direction)))
     (t (brk-sontaku--primitive-skip-sexp direction)))))

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

(defun brk-sontaku--symbol-action (direction &optional bound)
  "Move across a symbol according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

This function skips contiguous chars of the same syntax altogether,
regarding them as a symbol.  That could be helpful to handle operators
in some programming languages.
If BOUND is non-nil, stop before BOUND.

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
           (brk-sontaku--syntax-action direction bound)))
    (while (and (brk-sontaku--at-symbol-p (funcall point-fn))
                (brk-sontaku--syntax-action direction bound)))
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

(defun brk-sontaku--at-string-sentence-beg-p (&optional pt)
  "Return non-nil if PT is at the beginning of a sentence
in a string: right after an opening quotation mark."
  (let ((pt (or pt (point))))
    (and (brk-sontaku--in-string-p pt)
         (= pt (1+ (brk-sontaku--syntax-string-start))))))

(defun brk-sontaku--at-string-sentence-end-p (&optional pt)
  "Return non-nil if PT is at the end of a sentence in a string:
right before an closing quotation mark."
  (let* ((pt (or pt (point)))
         (string-beg (brk-sontaku--syntax-string-start pt))
         (string-end (brk-sontaku--safe-scan-sexps 1 string-beg)))
    (and (brk-sontaku--in-string-p pt)
         (= pt (1- string-end)))))

(defun brk-sontaku--match-string-case-p (style &optional case-state pt)
  "Return non-nil if the element at PT matches STYLE-case in CASE-STATE.
STYLE must be either of:

- \\='camel: \"camelCase\"
- \\='dot: \"dot.case\"
- \\='kebab: \"kebab-case\"
- \\='pascal: \"PascalCase\"
- \\='snake: \"snake_case\"

Note that this function works on the assumption that
camelCase and PascalCase should have at least one set of ups and downs.

CASE-STATE is either \\='upper, \\='lower, or nil.  When STYLE is either
\\='camel or \\='pascal, CASE-STATE is ignored and treated as nil."
  (unless (memq style '(camel dot kebab pascal snake))
    (user-error "STYLE must be either \\='camel, \\='dot, \\='kebab, \\='pascal or \\='snake, got %S" style))
  (when (and case-state (not (memq case-state '(upper lower))))
    (user-error "CASE-STATE must be either \\='upper, \\='lower or nil, got %S" case-state))
  (let* ((pt (or pt (point)))
         (case-state (if (memq style '(camel pascal))
                         nil
                       case-state))
         (bounds (save-excursion
                   (goto-char pt)
                   (skip-chars-backward "A-Za-z0-9_.-")
                   (let ((beg (point)))
                     (skip-chars-forward "A-Za-z0-9_.-")
                     (let ((end (point)))
                       (when (> end beg)
                         (cons beg end))))))
         (str (when bounds
                (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (sep (pcase style
                ('dot ".")
                ('kebab '(+ "-")) ; Allow one or more hyphens.
                ('snake "_")
                (_ nil)))
         (case-state-frag (pcase case-state
                            ('lower '(+ (or lower digit)))
                            ('upper '(+ (or upper digit)))
                            (_ '(+ (or alpha lower)))))
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
                       upper
                       (+ (or lower digit))
                       (* (seq upper (+ (or lower digit))))
                       eos))
                (_
                 `(seq bos
                       alpha (+ alpha)
                       eos)))))
         ;; Make it case-sensitive temporarily.
         (case-fold-search nil))
    (and str (string-match-p re str) t)))

(defun brk-sontaku--string-action (direction)
  "Move across a string in the specified DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

By \"string\", it specifically means:

- String quotes
- String delimiters

Note that this function does nothing when the current point
is inside \"string\".  That built-in logic
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

(defun brk-sontaku--word-action (direction &optional bound)
  "Move across a word according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

Unlike `forward-word' and `backward-word', this returns nil
and does nothing when the current point is:

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
             ;; "? " stands for whitespace syntax class.
             if (eq (brk-sontaku--syntax-char-after (funcall point-fn)) ? )
             do (cl-return nil)
             else do (funcall skip-fn)
             when (and bound
                       (brk-sontaku--beyond-bound-p direction bound (point)))
             do (progn
                  (goto-char (funcall before-bound bound))
                  (cl-return nil))
             finally (cl-return nil))
    (let ((to (point)))
      (unless (eq from to) to))))

;;;;; Regexp

(defun brk-sontaku--with-regexp-p (regexp &optional direction bound pt)
  "Return non-nil if REGEXP matches relative to PT.
REGEXP may be:

- a string regexp
- an RX regexp (Lisp form)

DIRECTION must be either:

- nil: test REGEXP at point.
- \\='forward: test REGEXP ahead within BOUND.
- \\='backward: test REGEXP behind within BOUND."
  (let ((pt (or pt (point)))
        (pattern (if (stringp regexp)
                     regexp
                   (rx-to-string regexp))))
    (save-match-data
      (save-excursion
        (goto-char pt)
        (pcase direction
          ('nil
           (looking-at-p pattern))
          ('forward
           (and (re-search-forward pattern bound t) t))
          ('backward
           (and (re-search-backward pattern bound t) t))
          (_ (user-error "Invalid DIRECTION %S" direction)))))))

;;;;; Dispatch Tables

(defconst brk-sontaku--navigator-pred-dispatch-table
  '((at-comment-sentense-beg . brk-sontaku--at-comment-sentence-beg-p)
    (at-comment-sentense-end . brk-sontaku--at-comment-sentence-end-p)
    (at-empty-line . brk-sontaku--at-empty-line-p)
    (at-first-comment-starter . brk-sontaku--at-first-comment-starter-p)
    (at-last-comment-ender . brk-sontaku--at-last-comment-ender-p)
    (at-string-sentence-beg . brk-sontaku--at-string-sentence-beg-p)
    (at-string-sentence-end . brk-sontaku--at-string-sentence-end-p)
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
  "Alist mapping navigator predicates to the corresponding predicate functions.")

(defconst brk-sontaku--unit-dispatch-table
  '((char . brk-sontaku--char-action)
    (empty-line . brk-sontaku--empty-line-action)
    (string . brk-sontaku--string-action)
    (comment . brk-sontaku--comment-action)
    (same-chars . brk-sontaku--same-chars-action)
    (same-chars-and-syntax . brk-sontaku--same-chars-and-syntax-action)
    (sexp . brk-sontaku--sexp-action)
    (symbol . brk-sontaku--symbol-action)
    (syntax . brk-sontaku--syntax-action)
    (whitespace . brk-sontaku--whitespace-action)
    (word . brk-sontaku--word-action))
  "Alist mapping unit names to the corresponding primitive action functions.")

;;;; Expander Predicates & Bounds Functions

;;;;; Navigator

(defun brk-sontaku--navigator-within-line-p (&optional direction)
  "Return non-nil if the point stays within the current line after
calling the navigator according to DIRECTION.

DIRECTION must be either \\='forward or \\='backward when specified.
If nil, call the navigator in both ways."
  (let ((orig-line-num (line-number-at-pos))
        (beg (if (use-region-p)
                 (brk-sontaku--call-navigator 'backward (region-beginning))
               (brk-sontaku--call-navigator 'backward)))
        (end (if (use-region-p)
                 (brk-sontaku--call-navigator 'forward (region-end))
               (brk-sontaku--call-navigator 'forward))))
    (pcase direction
      ('forward
       (= orig-line-num (line-number-at-pos end)))
      ('backward
       (= orig-line-num (line-number-at-pos beg)))
      (_
       (= orig-line-num (line-number-at-pos beg) (line-number-at-pos end))))))

(defun brk-sontaku--bounds-of-navigator-at-point ()
  "Return resulting bounds of the navigator action at point.
It is a cons cell of integer \"(beg . end)\"."
  (let ((from (point))
        forward-beg forward-end backward-beg backward-end smaller-bounds)
    (save-excursion
      (setq forward-end (brk-sontaku--call-navigator 'forward)
            forward-beg (brk-sontaku--call-navigator 'backward)))
    (save-excursion
      (setq backward-beg (brk-sontaku--call-navigator 'backward)
            backward-end (brk-sontaku--call-navigator 'forward)))
    (cond
     ((eq forward-beg from)
      (cons forward-beg forward-end))
     ((eq backward-end from)
      (cons backward-beg backward-end))
     ((and (setq smaller-bounds
                 (brk-sontaku--interval-winner-between
                  (cons forward-beg forward-end)
                  (cons backward-beg backward-end)
                  'smaller))
           (<= (car smaller-bounds) from (cdr smaller-bounds)))
      smaller-bounds))))

(defun brk-sontaku--bounds-of-forward-navigator ()
  "Return resulting bounds of the forward navigator action.
This simply calls `brk-sontaku--call-navigator' forward and
returns the points as a cons cell of integer \"(beg . end)\"."
  (let ((beg (if (use-region-p)
                 (region-beginning)
               (brk-sontaku--call-navigator 'backward)))
        (end (if (use-region-p)
                 (brk-sontaku--call-navigator 'forward (region-end))
               (brk-sontaku--call-navigator 'forward))))
    (cons beg end)))

(defun brk-sontaku--bounds-of-backward-navigator ()
  "Return resulting bounds of the backward navigator action.
This simply calls `brk-sontaku--call-navigator' backward and
returns the points as a cons cell of integer \"(beg . end)\"."
  (let ((beg (if (use-region-p)
                 (brk-sontaku--call-navigator 'backward (region-beginning))
               (brk-sontaku--call-navigator 'backward)))
        (end (if (use-region-p)
                 (region-end)
               (brk-sontaku--call-navigator 'forward))))
    (cons beg end)))

(defun brk-sontaku--bounds-of-normal-navigator ()
  "Return resulting bounds of the navigator action.
By \"normal\", it means this calls `brk-sontaku--call-navigator'
in the same direction as the region end at which the point is located:

- When no active region, call it with \\='forward
- When the point is at the beginning of region, call it with \\='backward
- When the point is at the end of region, call it with \\='forward

The returned value is a cons cell of integer \"(beg . end)\"."
  (let ((direction (or (brk-sontaku--active-region-direction)
                       'forward))
        (beg (if (use-region-p)
                 (region-beginning)
               (if (memq (brk-sontaku--syntax-char-after (1- (point))) '(?_ ?w))
                   (brk-sontaku--call-navigator 'backward)
                 (point))))
        (end (if (use-region-p)
                 (region-end)
               (point))))
    (pcase direction
      ('forward
       (cons beg
             (save-excursion
               (goto-char end)
               (brk-sontaku--skip-syntax " " 'forward)
               (brk-sontaku--call-navigator 'forward))))
      ('backward
       (cons (save-excursion
               (goto-char beg)
               (brk-sontaku--skip-syntax " " 'backward)
               (brk-sontaku--call-navigator 'backward))
             end)))))

(defun brk-sontaku--bounds-of-alternate-navigator ()
  "Return resulting bounds of the alternate navigator action.
By \"alternate\", it means this calls `brk-sontaku--call-navigator'
forward and backward alternately:

- When no active region, call it with \\='forward
- When the point is at the beginning of region, call it with \\='forward
- When the point is at the end of region, call it with \\='backward

The returned value is a cons cell of integer \"(beg . end)\"."
  (let ((direction (or (brk-sontaku--active-region-direction)
                       'backward))
        (beg (if (use-region-p)
                 (region-beginning)
               (if (memq (brk-sontaku--syntax-char-after (1- (point))) '(?_ ?w))
                   (brk-sontaku--call-navigator 'backward)
                 (point))))
        (end (if (use-region-p)
                 (region-end)
               (point))))
    (pcase direction
      ('forward
       (cons (save-excursion
               (goto-char beg)
               (brk-sontaku--skip-syntax " " 'backward)
               (brk-sontaku--call-navigator 'backward))
             end))
      ('backward
       (cons beg
             (save-excursion
               (goto-char end)
               (brk-sontaku--skip-syntax " " 'forward)
               (brk-sontaku--call-navigator 'forward)))))))

;;;;; Line

(defun brk-sontaku--match-line-and-region-beg-p ()
  "Return non-nil if the point of the current region beginning
matches that of the current line beginning."
  (let* ((reg-beg (region-beginning))
         (line-beg (save-excursion
                     (goto-char reg-beg)
                     (line-beginning-position))))
    (= reg-beg line-beg)))

(defun brk-sontaku--match-line-and-region-end-p ()
  "Return non-nil if the point of the current region end
matches that of the current line end."
  (let* ((reg-end (region-end))
         (line-end (save-excursion
                     (goto-char reg-end)
                     (line-end-position))))
    (= reg-end line-end)))

(defun brk-sontaku--bounds-of-line-at-point ()
  "Return bounds of the line at point.
It is a cons cell of integer \"(beg . end)\"."
  (when-let* ((beg (line-beginning-position))
              (end (line-end-position)))
    (cons beg end)))

(defun brk-sontaku--bounds-of-line-around-region ()
  "Return bounds of the line around region.
It is a cons cell of integer \"(beg . end)\".

This takes care of multi-line region cases.
If no active region, return nil."
  (when (use-region-p)
    (let* ((orig-beg (region-beginning))
           (orig-end (region-end))
           (beg (save-excursion
                  (goto-char orig-beg)
                  (line-beginning-position)))
           (end (save-excursion
                  (goto-char orig-end)
                  (line-end-position))))
      (cons beg end))))

(defun brk-sontaku--bounds-of-line-ahead ()
  "Return bounds of the previous line beginning or next line end
according to which region end the point is located at.
It is a cons cell of integer \"(beg . end)\".
If no active region, return nil.

When the point is at:

- the region beginning, this extends the bounds' beginning to
the position of the previous line beginning.
- the region end, this extends the bounds' end to the position
of the next line end.

This keeps the other bound intact either way and takes care of
multi-line region cases."
  (when-let* ((direction (brk-sontaku--active-region-direction)))
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (pcase direction
        ('forward (cons reg-beg
                        (save-excursion
                          (forward-line 1)
                          (line-end-position))))
        ('backward (cons (save-excursion
                           (forward-line -1)
                           (line-beginning-position))
                         reg-end))))))

;;;;; S-Expression

(defun brk-sontaku--bounds-of-sexp-at-point ()
  "Return bounds of the sexp at point.
It is a cons cell of integer \"(beg . end)\".
If not present, return nil."
  (let ((from (point))
        forward-beg forward-end backward-beg backward-end smaller-bounds)
    (save-excursion
      (setq forward-end (brk-sontaku--primitive-skip-sexp 'forward)
            forward-beg (brk-sontaku--primitive-skip-sexp 'backward)))
    (save-excursion
      (setq backward-beg (brk-sontaku--primitive-skip-sexp 'backward)
            backward-end (brk-sontaku--primitive-skip-sexp 'forward)))
    (cond
     ((eq forward-beg from)
      (cons forward-beg forward-end))
     ((eq backward-end from)
      (cons backward-beg backward-end))
     ((and (setq smaller-bounds
                 (brk-sontaku--interval-winner-between
                  (cons forward-beg forward-end)
                  (cons backward-beg backward-end)
                  'smaller))
           (<= (car smaller-bounds) from (cdr smaller-bounds)))
      smaller-bounds))))

(defun brk-sontaku--bounds-of-sexp-list-around-point ()
  "Return bounds of the sexp list around point.
It is a cons cell of integer \"(beg . end)\".

Note that this returns the bounds that are identical to
the beginning and end of buffer when the point is
at the top level.  For the meaning of \"top level\",
see `brk-sontaku--at-top-level-p'."
  (when-let* ((beg (save-excursion
                     (or (brk-sontaku--beginning-of-list-around-point)
                         (point))))
              (end (save-excursion
                     (or (brk-sontaku--end-of-list-around-point)
                         (point)))))
    (cons beg end)))

(defun brk-sontaku--bounds-of-sexp-around-point ()
  "Return bounds of the outer sexp around point.
It is a cons cell of integer \"(beg . end)\".
If no enclosing sexp is found, return nil."
  (when-let* ((beg (nth 1 (syntax-ppss)))
              (end (brk-sontaku--safe-scan-lists 1 1)))
    (cons beg end)))

;;;;; Word

(defun brk-sontaku--bounds-of-word-at-point ()
  "Return bounds of the word at point.
It is a cons cell of integer \"(beg . end)\"."
  (let ((from (point))
        forward-beg forward-end backward-beg backward-end smaller-bounds)
    (save-excursion
      (setq forward-end (progn (forward-word) (point))
            forward-beg (progn (backward-word) (point))))
    (save-excursion
      (setq backward-beg (progn (backward-word) (point))
            backward-end (progn (forward-word) (point))))
    (cond
     ((eq forward-beg from)
      (cons forward-beg forward-end))
     ((eq backward-end from)
      (cons backward-beg backward-end))
     ((and (setq smaller-bounds
                 (brk-sontaku--interval-winner-between
                  (cons forward-beg forward-end)
                  (cons backward-beg backward-end)
                  'smaller))
           (<= (car smaller-bounds) from (cdr smaller-bounds)))
      smaller-bounds))))

;;;;; Dispatch Tables

(defconst brk-sontaku--expander-pred-dispatch-table
  '((match-line-and-region-beg . brk-sontaku--match-line-and-region-beg-p)
    (match-line-and-region-end . brk-sontaku--match-line-and-region-end-p)
    (navigator-within-line . brk-sontaku--navigator-within-line-p))
  "Alist mapping expander predicates to the corresponding predicate functions.")

(defconst brk-sontaku--bounds-fn-dispatch-table
  '((alternate-navigator . brk-sontaku--bounds-of-alternate-navigator)
    (backward-navigator . brk-sontaku--bounds-of-backward-navigator)
    (forward-navigator . brk-sontaku--bounds-of-forward-navigator)
    (line-ahead . brk-sontaku--bounds-of-line-ahead)
    (line-around-region . brk-sontaku--bounds-of-line-around-region)
    (line-at-point . brk-sontaku--bounds-of-line-at-point)
    (navigator-at-point . brk-sontaku--bounds-of-navigator-at-point)
    (normal-navigator . brk-sontaku--bounds-of-normal-navigator)
    (sexp-around-point . brk-sontaku--bounds-of-sexp-around-point)
    (sexp-at-point . brk-sontaku--bounds-of-sexp-at-point)
    (sexp-list-around-point . brk-sontaku--bounds-of-sexp-list-around-point)
    (word-at-point . brk-sontaku--bounds-of-word-at-point))
  "Alist mapping bounds function names to the corresponding bounds functions.")

;;;; Commands

;;;###autoload
(defun brk-sontaku-forward ()
  "Move forward according to the navigator designed for
the current major mode.

This command performs forward navigation based on the navigator
registered for the current `major-mode'.  Each invocation moves the
point forward by a certain unit according to the strategies defined for
that mode.
For more details, see `brk-sontaku-define-mode-navigator'."
  (interactive "^")
  (let ((from (point)) to)
    (setq to (brk-sontaku--call-navigator 'forward from))
    (when (/= from to)
      (goto-char to))))

;;;###autoload
(defun brk-sontaku-backward ()
  "Move backward according to the navigator designed for
the current major mode.

This command performs backward navigation based on the navigator
registered for the current `major-mode'.  Each invocation moves the
point backward by a certain unit according to the strategies defined for
that mode.
For more details, see `brk-sontaku-define-mode-navigator'."
  (interactive "^")
  (let ((from (point)) to)
    (setq to (brk-sontaku--call-navigator 'backward from))
    (when (/= from to)
      (goto-char to))))

;;;###autoload
(defun brk-sontaku-expand-region ()
  "Expand the active region according to the expander enabled on
or the navigator designed for the current major mode.

This command performs region expansion based on the expander
enabled for the current `major-mode'.  Each invocation expands
the region outward one step, following the strategies defined for
that mode.

They can be defined via `brk-sontaku--define-expander'.

If expansion is not possible, echo the message."
  (interactive)
  (let* ((from (point))
         (orig-bounds (if (use-region-p)
                          (cons (region-beginning) (region-end))
                        (cons (point) (point))))
         (expander-style (brk-sontaku--get-expander-style major-mode))
         (expander-fn (brk-sontaku--get-expander major-mode))
         (replace-mark (eq last-command this-command))
         (alternate (eq expander-style 'alternate)))
    (pcase-let* ((target-bounds (funcall expander-fn))
                 (`(,beg . ,end) target-bounds))
      (if (brk-sontaku--interval-contains-p target-bounds orig-bounds 'proper)
          (progn
            (brk-sontaku--maybe-reset-region-history)
            (brk-sontaku--mark-region beg end replace-mark alternate)
            (brk-sontaku--update-region-history beg end))
        (message "Cannot expand region further")))))

;;;###autoload
(defun brk-sontaku-contract-region (arg)
  "Contract the active region according to `brk-sontaku--region-history'.
This command simply undoes what `brk-sontaku-expand-region' does in
reverse order.

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

(defun brk-sontaku--compile-navigator-pred-form (pred-form direction-sym)
  "Compile PRED-FORM into an executable form.
Return a form that evaluates to non-nil when matched.
PRED-FORM should look like:

- logical combinators: (and ...), (or ...), (not ...)
- \\='default symbol
- (in-string), (at-char \"@\"), (with-pred (lambda () (...)))
- a user-defined predicate function symbol: treat it as a function

Otherwise, it signals an error.
For the valid predicate keywords, see `brk-sontaku--navigator-pred-dispatch-table'.

DIRECTION-SYM is a symbol naming the variable that will hold the
runtime direction.  The generated code will refer to this variable.
For the valid values, see `brk-sontaku--resolve-direction'."
  (let* ((head (brk-sontaku--form-head pred-form))
         (pred (alist-get head brk-sontaku--navigator-pred-dispatch-table))
         (point-fn `(brk-sontaku--resolve-direction
                     ,direction-sym #'point (lambda () (1- (point))))))
    (cond
     ;; Handle the default case here.
     ((eq pred-form 'default) 't)
     ;; Accept built-in `and', and `or'. Recurse this for the inner.
     ((eq head 'and)
      `(and ,@(mapcar (lambda (f) (brk-sontaku--compile-navigator-pred-form
                                   f direction-sym))
                      (cdr pred-form))))
     ((eq head 'or)
      `(or ,@(mapcar (lambda (f) (brk-sontaku--compile-navigator-pred-form
                                  f direction-sym))
                     (cdr pred-form))))
     ;; Accept built-in `not'.
     ((eq head 'not)
      `(not ,(brk-sontaku--compile-navigator-pred-form
              (cadr pred-form) direction-sym)))
     ;; Dispatch reserved predicate cases.
     (pred
      `(,pred ,@(cdr pred-form) (funcall ,point-fn)))
     ;; If it is a symbol/predicate name, assume it is a predicate to call.
     ((symbolp pred-form)
      `(funcall #',pred-form))
     ;; Otherwise, signal an error.
     (t (user-error "Invalid navigator predicate form: %S" pred-form)))))

(defun brk-sontaku--compile-action-form (action-form direction-sym)
  "Compile ACTION-FORM into a sequential unit operation.
ACTION-FORM should look like \"(unit NAME [COUNT])\" or a list of them,
where NAME is a key of an alist reserved in
`brk-sontaku--unit-dispatch-table' and COUNT is a positive integer.
For example:

- (unit word)
- (unit char 3)
- ((unit char 3) (unit word))

This validates NAME against `brk-sontaku--unit-dispatch-table'
and returns the corresponding action function when matched
upon macro expansion.
When called at runtime, it returns the last point after applying
all unit actions.

DIRECTION-SYM is a symbol naming the variable that will hold the
runtime direction.  The generated code will refer to this variable.
For the valid values, see `brk-sontaku--resolve-direction'."
  (let ((actions (if (and (consp action-form)
                          (not (eq (car action-form) 'unit)))
                     action-form
                   (list action-form))))
    `(save-excursion
       (let (to)
         ,@(mapcar
            (lambda (a)
              ;; cadr a: (unit UNIT-NAME COUNT) --> UNIT-NAME
              ;; caddr a: (unit UNIT-NAME COUNT) --> COUNT
              (let* ((unit-name (cadr a))
                     (count (caddr a))
                     (unit-action (alist-get unit-name brk-sontaku--unit-dispatch-table)))
                (unless unit-action
                  (user-error "Unknown unit action: %S" unit-name))
                (pcase a
                  (`(unit ,unit-name . ,rest)
                   (let* ((last-elt (car (last rest)))
                          (count (and (integerp last-elt) (> last-elt 0) last-elt)))
                     (if count
                         `(dotimes (_ ,count)
                            (setq to (,unit-action ,direction-sym ,@(butlast rest))))
                       `(setq to (,unit-action ,direction-sym ,@rest)))))
                  (_ (user-error "Invalid unit action syntax: %S" a)))))
            actions)
         to))))

(cl-defmacro brk-sontaku-define-mode-navigator (mode docstring &key strategies)
  "Define a navigator for MODE.

MODE is a symbol that must be a valid derived major mode. (e.g., `org-mode')

DOCSTRING is the function's docstring.

STRATEGIES is a list of strategy forms.  They consist of a combination of
predicates and actions.  For the detailed explanations on them, see
`brk-sontaku--compile-navigator-pred-form' and `brk-sontaku--compile-action-form', respectively.

The generated function will be named `brk-sontaku--MODE-navigator'
and registered in `brk-sontaku--mode-navigator-registry' under the MODE symbol.

The user-defined navigator functions play a role as an agency in:

- verifying the given predicates
- pinning down the corresponding unit action(s)
- running them according to DIRECTION
- returning the resulting point

Sontaku commands use them to look up the next point to move to or
the next region to expand to."
  (declare (debug (symbolp stringp (:strategies sexp)))
           (indent defun))
  (unless (or (eq mode 'default)
              (brk-sontaku--valid-derived-major-mode-symbol-p mode))
    (user-error "MODE must be either one of them as a symbol or \\='default, got %s" mode))
  (unless strategies
    (user-error "STRATEGIES not provided. \":strategies\" keyword must be given"))
  (unless (listp strategies)
    (user-error "STRATEGIES must be a list, got %S" strategies))
  (dolist (sf strategies)
    (unless (and (consp sf) (car sf) (cdr sf))
      (user-error "Each strategy must be a cons cell of (pred-form . action-form), got %S" sf)))
  (when (brk-sontaku--get-navigator mode)
    (message "Redefining the navigator function for `%s'" mode))
  (let ((fn-name (intern (format "brk-sontaku--%s-navigator" mode)))
        (fn-docstring
         (concat
          (if (eq mode 'default)
              "Default navigator."
            (format "Navigator for `%s'." mode))
          "\nDIRECTION must be either \\='forward or \\='backward.\n\n"
          docstring))
        (cond-clauses
         (mapcar
          (lambda (sf)
            (let ((pred-expr (brk-sontaku--compile-navigator-pred-form
                              (car sf) 'direction))
                  (action-expr (brk-sontaku--compile-action-form
                                (cdr sf) 'direction)))
              `(,pred-expr ,action-expr)))
          strategies)))
    `(progn
       (defun ,fn-name (direction)
         ,fn-docstring
         (cond ,@cond-clauses))

       (brk-sontaku--register-navigator ',mode ',fn-name))))

;; TODO: Improve this.
(brk-sontaku-define-mode-navigator default
  "This function is automatically generated via `brk-sontaku-define-mode-navigator'
and is the default value of `brk-sontaku-default-navigator-function'.

When no navigator is provided for a certain mode, this works as a fallback."
  :strategies
  (((at-word) . (unit word))
   ((match-syntax '(?\( ?\) ?$)) . (unit syntax))
   ((at-whitespace) . (unit whitespace))
   (default . (unit char))))

(defun brk-sontaku--compile-expander-pred-form (pred-form)
  "Compile PRED-FORM into an executable form.
Return a form that evaluates to non-nil when matched.
PRED-FORM should look like:

- logical combinators: (and ...), (or ...), (not ...)
- \\='default symbol
- (match-line-and-region-beg), (), ()
- a user-defined predicate function symbol: treat it as a function

Otherwise, it signals an error.
For the valid predicate keywords, see `brk-sontaku--expander-pred-dispatch-table'."
  (let* ((head (brk-sontaku--form-head pred-form))
         (pred (alist-get head brk-sontaku--expander-pred-dispatch-table)))
    (cond
     ;; Handle the default case here.
     ((eq pred-form 'default) 't)
     ;; Accept built-in `and', and `or'. Recurse this for the inner.
     ((eq head 'and)
      `(and ,@(mapcar (lambda (f) (brk-sontaku--compile-expander-pred-form f))
                      (cdr pred-form))))
     ((eq head 'or)
      `(or ,@(mapcar (lambda (f) (brk-sontaku--compile-expander-pred-form f))
                     (cdr pred-form))))
     ;; Accept built-in `not'.
     ((eq head 'not)
      `(not ,(brk-sontaku--compile-expander-pred-form (cadr pred-form))))
     ;; Dispatch reserved predicate cases.
     (pred
      `(,pred ,@(cdr pred-form)))
     ;; If it is a symbol/predicate name, assume it is a predicate to call.
     ((symbolp pred-form)
      `(funcall #',pred-form))
     ;; Otherwise, signal an error.
     (t (error "Invalid expander predicate form: %S" pred-form)))))

(defun brk-sontaku--compile-bounds-form (bounds-form)
  "Compile BOUNDS-FORM into a valid bounds function operation.
BOUNDS-FORM should look like \"(bounds NAME)\" where NAME is
a key of an alist reserved in `brk-sontaku--bounds-fn-dispatch-table'.
(e.g., \"(bounds navigator-both)\")

This validates NAME against `brk-sontaku--bounds-fn-dispatch-table'
and returns the corresponding bounds function when matched
upon macro expansion.
When called at runtime, it returns a cons cell of integer \"(beg . end)\"."
  (let* ((bounds-fn-name (cadr bounds-form))
         (bounds-fn (alist-get bounds-fn-name brk-sontaku--bounds-fn-dispatch-table)))
    (unless bounds-fn
      (error "Unknown bounds function: %S" bounds-fn))
    (pcase bounds-form
      (`(bounds ,bounds-fn-name)
       `(,bounds-fn))
      (_ (error "Invalid bounds function syntax: %S" bounds-form)))))

(cl-defmacro brk-sontaku--define-expander (style docstring &key strategies)
  "Define a region expander, a brain that decides how `brk-sontaku-expand-region'
does its job.

STYLE is a symbol that depicts the algorithm of expansion.

DOCSTRING is the function's docstring.

STRATEGIES is a list of strategy forms.  They consist of a combination of
predicates and bounds functions.
For the detailed explanations, see `brk-sontaku--compile-expander-pred-form'
and `brk-sontaku--compile-bounds-form', respectively.

The generated function will be named `brk-sontaku--STYLE-expander'
and registered in `brk-sontaku--expander-registry' under the STYLE symbol."
  (declare (debug (symbolp stringp (:strategies sexp)))
           (indent defun))
  (unless (symbolp style)
    (error "STYLE must be a symbol, got %S" style))
  (unless strategies
    (error "STRATEGIES not provided."))
  (unless (listp strategies)
    (error "STRATEGIES must be a list, got %S" strategies))
  (dolist (sf strategies)
    (unless (and (consp sf) (car sf) (cdr sf))
      (error "Each strategy must be a cons cell of (pred-form . bounds-form), got %S" sf)))
  (let* ((fn-name (intern (format "brk-sontaku--%s-expander" style)))
         (cond-clauses
          (mapcar
           (lambda (sf)
             (let ((pred-expr (brk-sontaku--compile-expander-pred-form (car sf)))
                   (bounds-expr (brk-sontaku--compile-bounds-form (cdr sf))))
               `(,pred-expr ,bounds-expr)))
           strategies)))
    `(progn
       (defun ,fn-name ()
         ,docstring
         (when (or (bobp) (eobp)
                   (and (null brk-sontaku-expand-to-whole-buffer-at-top-level)
                        (brk-sontaku--at-top-level-p)))
           (user-error "Reached top level boundary. Expansion aborted"))
         (cond ,@cond-clauses))

       (brk-sontaku--register-expander ',style ',fn-name))))

(brk-sontaku--define-expander alternate
  "Expander for alternate region expansion.

By \"alternate\", it means expanding forward and backward alternately.
The algorithm is as follows in order:

- Expand forward at the mercy of the user-defined navigator if no region active.
- Expand forward or backward at the mercy of the user-defined navigator
according to which region end the point is located at.
(beg --> forward, end --> backward)"
  :strategies ((default . (bounds alternate-navigator))))

(brk-sontaku--define-expander balanced
  "Expander for balanced region expansion.

By \"balanced\", it means the structured editing-based style
that shines in Lisp family languages.  The algorithm is as follows
in order:

- Expand to the sexp at point if it exists.
\"(foo* bar)\" --> \"(|foo*| bar)\"
- Expand to the list around point if it exists.
\"(|foo*| bar)\" --> \"(|foo bar*|)\"
- Expand to the sexp around point if it exists.
\"(|foo bar*|)\" --> \"|(foo bar)*|\""
  :strategies ((default . (bounds sexp-at-point))))

(brk-sontaku--define-expander linear
  "Expander for linear region expansion.

The algorithm is as follows in order:

- Expand at the mercy of the user-defined navigator as long as
expansion moves the point within the current line.
- Expand to the whole current line when the above condition is
no longer met.
- Expand to the previous line beginning or next line end
depending on which region end the point is located at.
(region beg --> prev line beg, region end --> next line end)"
  :strategies (((navigator-within-line) . (bounds normal-navigator))
               ((or (not (match-line-and-region-beg))
                    (not (match-line-and-region-end)))
                . (bounds line-around-region))
               (default . (bounds line-ahead))))

;;;; Sontaku mode

(defvar-keymap sontaku-mode-map
  :doc "Keymap for `sontaku-mode'."
  "M-f" #'brk-sontaku-forward
  "M-b" #'brk-sontaku-backward
  "C-c SPC" #'brk-sontaku-expand-region)

(defvar-keymap sontaku-navigation-repeat-map
  :doc "Repeat keymap for navigation commands in `sontaku-mode'."
  :repeat t
  "f" #'brk-sontaku-forward
  "M-f" #'brk-sontaku-forward
  "b" #'brk-sontaku-backward
  "M-b" #'brk-sontaku-backward)

(defvar-keymap sontaku-expansion-repeat-map
  :doc "Repeat keymap for expansion/contraction commands in `sontaku-mode'."
  :repeat (:enter (brk-sontaku-expand-region))
  "SPC" #'brk-sontaku-expand-region
  "+" #'brk-sontaku-expand-region
  "-" #'brk-sontaku-contract-region)

;;;###autoload
(define-minor-mode sontaku-mode
  "Minor mode to enable keybindings for Sontaku commands."
  :keymap sontaku-mode-map)

(provide 'brk-sontaku)
;;; brk-sontaku.el ends here
