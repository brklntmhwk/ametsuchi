;;; brk-org-super-agenda.el --- Extensions for org-super-agenda -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-org-super-agenda.el
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

(require 'org-super-agenda)

;; https://github.com/akirak/emacs-config/commit/bb2441429d2527ac378ecced3db147b6ae7240a1
(org-super-agenda--defgroup
 datetree "Match items inside a date tree."
 :section-name "Date tree"
 :test (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
                                                  (org-reverse-datetree-date-child-p)))

;; https://github.com/akirak/emacs-config/commit/bb2441429d2527ac378ecced3db147b6ae7240a1
(org-super-agenda--def-auto-group
 reverse-parent "their parent heading in reverse order"
 :keyword :auto-reverse-parent
 :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
                                                      (when (org-up-heading-safe)
                                                        (org-get-heading 'notags 'notodo)))
 :key-sort-fn string>)

(provide 'brk-org-super-agenda)
;;; brk-org-super-agenda.el ends here
