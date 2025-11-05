;;; brk-visual-replace.el --- A collection of personal tweaks for visual-replace -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-visual-replace.el
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

(require 'transient)

(transient-define-prefix brk-visual-replace-transient ()
  "Provide a menu of available actions on `visual-replace-mode-map'."
  ["Basic operation"
   ("a" "apply one repeat" visual-replace-apply-one-repeat)
   ("A" "apply one" visual-replace-apply-one)
   ("s" "substring match" visual-replace-substring-match)
   ("u" "undo" visual-replace-undo)
   ("y" "yank" visual-replace-yank)
   ("M-y" "yank-pop" visual-replace-yank-pop)]
  ["Toggle replace mode"
   ("c" "toggle case fold" visual-replace-toggle-case-fold)
   ("e" "toggle regexp" visual-replace-toggle-regexp)
   ("q" "toggle query" visual-replace-toggle-query)
   ("w" "toggle word" visual-replace-toggle-word)]
  ["Change scope"
   ("f" "switch to full scope" visual-replace-switch-to-full-scope)
   ("p" "switch to from-point scope" visual-replace-switch-to-from-point-scope)
   ("r" "switch to region scope" visual-replace-switch-to-region-scope)])

(provide 'brk-visual-replace)
;;; brk-visual-replace.el ends here
