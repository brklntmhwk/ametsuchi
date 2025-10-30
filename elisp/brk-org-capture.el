;;; brk-org-capture.el --- A collection of personal tweaks for org-capture -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-org-capture.el
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

(require 'org)
(require 'ol)
(require 'org-capture)

(defun brk-org-capture-find-latest-datetree-entry ()
  "Return the position of today's entry in the latest yearly journal file.
If today's journal heading exists, place point at the end of the subtree.
If not, create it using `org-reverse-datetree-goto-date-in-file' before placing point."
  (require 'brk-directory)
  (require 'org-reverse-datetree)
  (let* ((journal-dir (file-name-as-directory (concat brk-directory-user-documents "journal/")))
         (year (format-time-string "%Y"))
         (latest-file (expand-file-name (format "%s.org" year) journal-dir)))
    (unless (file-exists-p latest-file)
      (with-temp-buffer (write-file latest-file))
      (message "Created new yearly journal file: %s" latest-file))
    (set-buffer (or (org-find-base-buffer-visiting latest-file)
                    (progn (org-capture-put :new-buffer t)
                           (find-file-noselect latest-file))))
    (org-capture-put-target-region-and-position)
    (widen)
    (org-reverse-datetree-goto-date-in-file)
    (cons latest-file (point))))

(provide 'brk-org-capture)
;;; brk-org-capture.el ends here
