;;; brk-fontset.el --- Custom fontset configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience, font
;; URL: https://github.com/brklntmhwk/elisp/brk-fontset.el
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

(defun brk-fontset--apply-fontset (characters fontset family &optional add-method)
  "Apply fontset configuration with strict type validation.
CHARACTERS is either a list of symbols (scripts/charsets)
or a list of (FROM . TO) cons ranges."
  (let ((font-spec (font-spec :family family)))
    (unless (listp characters)
      (error "Characters must be a list: %s" characters))

    (when (null characters)
      (message "Empty character list for font: %s" family)
      (cl-return-from brk-fontset--apply-fontset))

    (let ((first (car characters)))
      (cond
       ((symbolp first)
        (dolist (sym characters)
          (unless (symbolp sym)
            (error "Expected symbol in character list, got %S (type: %s)"
                   sym (type-of sym)))
          (set-fontset-font fontset sym font-spec nil add-method)))
       ((and (consp first) (integerp (car first)) (integerp (cdr first)))
        (dolist (range characters)
          (unless (and (consp range)
                       (integerp (car range))
                       (integerp (cdr range)))
            (error "Expected (FROM . TO) cons pair, got %S" range))
          (let ((from (car range))
                (to (cdr range)))
            (when (> from to)
              (error "Invalid range order: %d > %d" from to))
            (set-fontset-font fontset range font-spec nil add-method))))
       (t
        (error "Invalid character specification. Expected symbol or cons, got: %S (%s)"
               first (type-of first)))))))

(defun brk-fontset-apply-fontset-config (&optional fontsets skip-check)
  "Apply font configuration.
FONTSETS defaults to `brk-fontset-config-alist'.
SKIP-CHECK bypasses font availability checking."
  (let ((available-fonts (font-family-list))
        (fontsets (or fontsets brk-fontset-config-alist)))
    (cl-loop for (characters . plist) in fontsets
             for family = (plist-get plist :family)
             for fontset = (plist-get plist :fontset)
             for add-method = (plist-get plist :add)

             when (or skip-check
                      (member family available-fonts))
             do (brk-fontset--apply-fontset characters fontset family add-method)
             else collect family into missing-fonts
             finally (when missing-fonts
                       (message "Missing fonts: %s"
                                (string-join missing-fonts ", "))))))

(defcustom brk-fontset-config-alist
  '(((emoji)
     :family "Noto Color Emoji"
     :fontset t
     :add prepend)
    ((han)
     :family "Sarasa Mono SC"
     :fontset t)
    ((hangul)
     :family "Sarasa Mono K"
     :fontset t)
    ((kana)
     :family "Sarasa Mono J"
     :fontset t)
    ((symbol)
     :family "Symbola"
     :fontset t
     :add append))
  "Alist of fontset configuration."
  :type '(alist
          :key-type (choice (repeat symbol)
                            (repeat (cons integer integer)))
          :value-type (plist
                       :options ((string
                                  :tag "Name of a font family"
                                  :family)
                                 ((choice
                                   (const :tag "Default fontset" t)
                                   (string :tag "Name of a fontset"))
                                  :fontset)
                                 ((choice
                                   (const nil)
                                   (const prepend)
                                   (const append))
                                  :add))))
  :set (lambda (symbol value)
         (set symbol value)
         (when (or window-system (daemonp))
           (brk-fontset-apply-fontset-config value (daemonp)))))

(provide 'brk-fontset)
;;; brk-fontset.el ends here
