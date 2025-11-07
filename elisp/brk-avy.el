;;; brk-avy.el --- Extensions for avy -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-avy.el
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

(require 'avy)

;; https://karthinks.com/software/avy-can-do-anything/#look-up-the-documentation-for-a-symbol
(defun brk-avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

;; https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere
(defun brk-avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun brk-avy-action-copy-dwim (pt)
  "Copy the entire sexp at PT."
  (save-excursion
    (goto-char pt)
    (let* ((bounds (or (bounds-of-thing-at-point 'sexp)
                       (bounds-of-thing-at-point 'symbol)
                       (cons pt (progn (forward-sexp 1)
                                       (point)))))
           (beg (car bounds))
           (end (cdr bounds))
           (str (buffer-substring-no-properties beg end)))
      (kill-new str)
      (message "Copied: %s" (truncate-string-to-width str 80))))
  (let ((dat (ring-ref avy-ring 0)))
    (select-frame-set-input-focus (window-frame (cdr dat)))
    (select-window (cdr dat))
    (goto-char (car dat))))

(provide 'brk-avy)
;;; brk-avy.el ends here
