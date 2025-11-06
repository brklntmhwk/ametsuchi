;;; brk-activities.el --- Extensions for activities -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-activities.el
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

(require 'activities)

;; https://github.com/alphapapa/activities.el/discussions/59
(defun brk-activities-local-buffer-p (buffer)
  "Returns non-nil if BUFFER is present in `activities-current'."
  (when (activities-current)
    (memq buffer
          (activities-tabs--tab-parameter
           'activities-buffer-list
           (activities-tabs--tab (activities-current))))))

(provide 'brk-activities)
;;; brk-activities.el ends here
