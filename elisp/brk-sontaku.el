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

(defcustom brk-sontaku-default-state-machine-function
  #'brk-sontaku--emacs-lisp-mode-state-machine
  "A default state machine function called when expanding,
contracting, or moving."
  :type '(choice
          (const :tag "State machine for `emacs-lisp-mode'"
                 brk-sontaku--elisp-mode-state-machine)
          (const :tag "State machine for `org-mode'"
                 brk-sontaku--org-mode-state-machine)
          (function :tag "Custom state machine"))
  :group 'brk-sontaku)

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

(defun brk-sontaku--safe-scan-sexps (n &optional pt)
  "Safely scan and return the position N sexps away from PT.
If PT is not provided, use the current point.  Return nil if
the scan reaches the beginning or end of the accessible part
in the middle."
  (let ((pt (or pt (point))))
    (ignore-errors (scan-sexps pt n))))

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

;;;; Predicates & Unit Actions

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
        (over-bound-p (brk-sontaku--resolve-direction
                       direction
                       (lambda (pos bound) (> pos bound))
                       (lambda (pos bound) (< pos bound))))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point)))))
        moved)
    (cl-block nil
      (while t
        (let ((next-pos (+ (point) n)))
          (when (and bound
                     (funcall over-bound-p next-pos bound))
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
  "Return non-nil if char at PT is whitespace.
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
        (over-bound-p (brk-sontaku--resolve-direction
                       direction
                       (lambda (pos bound) (> pos bound))
                       (lambda (pos bound) (< pos bound))))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point)))))
        moved)
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
    (funcall skip-fn)))

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

(defun brk-sontaku--primitive-skip-sexp (direction)
  "Move across a sexp according to DIRECTION.
DIRECTION must be either \\='forward or \\='backward.

This is similar to `forward-sexp'/`backward-sexp', but it takes care of
some behavioral glitches (quirks?) they have.  See the implementation
for more details."
  (let ((skip-syntax-direction (brk-sontaku--resolve-direction
                                direction 'backward 'forward))
        (point-fn (brk-sontaku--resolve-direction
                   direction #'point (lambda () (1- (point))))))
    (if (brk-sontaku--skip-sexp direction)
        ;; Play it safe and skip back whitespace ahead of the point.
        (progn (brk-sontaku--skip-syntax " " skip-syntax-direction)
               (point))
      ;; Skip an expression prefix ahead if present.
      ;; In the example below, "*" is the point.
      ;; e.g., (foo *') --> 'forward --> (foo '*)
      (when (eq (brk-sontaku--syntax-char-after (funcall point-fn)) ?')
        (brk-sontaku--char-action direction) (point)))))

;; TODO: Complete this action.
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
- it goes beyond the current comment block.

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
             ;; "? " stands for whitespace syntax class.
             if (eq (brk-sontaku--syntax-char-after (funcall point-fn)) ? )
             do (cl-return nil)
             else do (funcall skip-fn)
             when (and bound (funcall over-bound-p (point) bound))
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

;;;;; Dispatch tables

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
  "An alist mapping unit names to the corresponding primitive action functions.")

(defconst brk-sontaku--pred-dispatch-table
  '((at-bob . bobp)
    (at-bol . bolp)
    (at-comment-sentense-beg . brk-sontaku--at-comment-sentence-beg-p)
    (at-comment-sentense-end . brk-sontaku--at-comment-sentence-end-p)
    (at-empty-line . brk-sontaku--at-empty-line-p)
    (at-eob . eobp)
    (at-eol . eolp)
    (at-first-comment-starter . brk-sontaku--at-first-comment-starter-p)
    (at-last-comment-ender . brk-sontaku--at-last-comment-ender-p)
    (at-outermost . brk-sontaku--at-outermost-p)
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
  "An alist mapping predicates to the corresponding predicate functions.")

;;;; Commands

;;;###autoload
(defun brk-sontaku-expand-region ()
  "Expand the active region according to the state machine
designed for the current major mode.

This command performs region expansion based on the state machine
registered for the current `major-mode'.  Each invocation expands
the region outward one step, following the strategies defined for
that mode.
They can be defined via `brk-sontaku-define-mode-state-machine'.

For whatever reason, if expansion is not possible, signal an error."
  (interactive)
  (let* ((orig-bounds (if (use-region-p)
                          (cons (region-beginning) (region-end))
                        (cons (point) (point))))
         (sm-fn (brk-sontaku--get-state-machine major-mode))
         (beg (save-excursion
                (funcall
                 (or sm-fn 'backward
                     brk-sontaku-default-state-machine-function 'backward))))
         (end (save-excursion
                (funcall
                 (or sm-fn 'forward
                     brk-sontaku-default-state-machine-function 'forward))))
         (target-bounds (cons beg end)))
    (if (brk-sontaku--interval-contains-p target-bounds orig-bounds 'proper)
        (progn
          (brk-sontaku--maybe-reset-region-history)
          (brk-sontaku--mark-region beg end replace-mark)
          (brk-sontaku--update-region-history beg end))
      (user-error "Cannot expand region further"))))

;;;###autoload
(defun brk-sontaku-contract-region (arg)
  "Contract current region according to `brk-sontaku--region-history'.
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
- \\='default symbol
- (in-string), (at-char \"@\"), (with-pred (lambda () (...)))
- a user-defined predicate function symbol: treat it as a function

Otherwise, it signals an error.
For the valid predicate keywords, see `brk-sontaku--pred-dispatch-table'."
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
     (pred
      `(,pred ,@(cdr pred-form)))
     ;; If it is a symbol/predicate name, assume it is a predicate to call.
     ((symbolp pred-form)
      `(funcall #',pred-form))
     ;; Otherwise, signal an error.
     (t (user-error "Invalid predicate form: %S" pred-form)))))

(defun brk-sontaku--compile-action-form (action-form direction)
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
At runtime, it is called and returns the last point after
applying all unit actions.
DIRECTION must be either \\='forward or \\='backward."
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
                            (setq to (,unit-action ,direction ,@(butlast rest))))
                       `(setq to (,unit-action ,direction ,@rest)))))
                  (_ (user-error "Invalid unit action syntax: %S" a)))))
            actions)
         to))))

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
named `brk-sontaku--MODE-state-machine' and registered in
`brk-sontaku--mode-state-machine-registry' under the MODE symbol.

The user-defined state machine functions play a role as an agency in:

- verifying the given predicates
- pinning down the corresponding unit action(s)
- running them according to DIRECTION
- returning the resulting point

Sontaku commands use them to look up the next point to move or expand to."
  (declare (debug (symbolp stringp body [":done" [&or symbolp form]]))
           (indent defun))
  (unless (brk-sontaku--valid-derived-major-mode-symbol-p mode)
    (user-error "brk-sontaku-define-mode-state-machine: `%s' is not a valid derived major mode" mode))
  (when (brk-sontaku--get-state-machine mode)
    (message "brk-sontaku-define-mode-state-machine: Redefining the state machine function for `%s'" mode))
  (let ((strategy-forms (cl-loop for (key val) on strategies by #'cddr
                                 when (eq key :strategy)
                                 collect val))
        (fn-name (intern (format "brk-sontaku--%s-state-machine" mode)))
        (fn-docstring (format "A state machine auto-generated for `%s'.\n
DIRECTION must be either \\='forward or \\='backward.\n\n%s"
                              mode docstring)))
    `(progn
       (brk-sontaku--register-state-machine ',mode ',fn-name)

       (defun ,fn-name (direction)
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

(brk-sontaku-define-mode-state-machine emacs-lisp-mode
  ""
  :strategy ((in-comment) . (unit sexp))
  :strategy ((in-string) . (unit sexp))
  :strategy ((at-empty-line) . (unit empty-line))
  :strategy (default . (unit sexp))
  :done 'at-outermost)

(brk-sontaku-define-mode-state-machine org-mode
  ""
  :strategy (default . (unit word))
  :done 'at-outermost)

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
;;     (or (brk-sontaku--move-within #'brk-sontaku--symbol-action direction limit)
;;         (brk-sontaku--move-within #'brk-sontaku--string-action direction limit)
;;         (brk-sontaku--move-within #'brk-sontaku--comment-action direction limit)
;;         (when (memq syntax-char syntax-classes)
;;           (let ((forward-sexp-function nil))
;;             (brk-sontaku--move-within #'brk-sontaku--primitive-skip-sexp
;;                                       direction limit)))
;;         (when (eq syntax-char ?.)
;;           (progn (forward-char) (point)))
;;         (brk-sontaku--move-within #'brk-sontaku--same-chars-and-syntax-action
;;                                   direction limit))))

;; TODO: add OOTB strategies for some popular major modes.
;; emacs-lisp-mode, org-mode.

;; TODO: add a default strategy that works as a fallback. (Maybe the one like puni?)
;; That can be used in `brk-sontaku-expand-region' in case no state machine-fn found
;; for the current mode.

(provide 'brk-sontaku)
;;; brk-sontaku.el ends here
