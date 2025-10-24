;;; brk-dashboard.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; URL: https://github.com/brklntmhwk/lisp/brk-dashboard.el
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

(require 'dashboard)

;; TODO: Add actions and widget params.
(defun brk-dashboard-insert-my-agenda-section (list-size)
  "Insert LIST-SIZE number of agenda entries into the dashboard."
  (dashboard-insert-section
   "Appointment(s) for today"
   (brk-dashboard--my-agenda-items)
   list-size
   'my-agenda
   (dashboard-get-shortcut 'my-agenda)))

;; TODO: Add the action part and proceed to the end.
(defun brk-dashboard--my-agenda-items ()
  (require 'org-ql)
  (let* ((today (brk-dashboard--adjusted-today))
         (tomorrow (decoded-time-add today (make-decoded-time :day 1)))
         (files (org-agenda-files))
         (regexp (brk-dashboard--org-timestamp-regexp-for-decoded-dates
                  (list today tomorrow))))
    (if files
        (org-ql-select files
          `(and (regexp ,regexp)
                (not (done))
                (not (tags "ARCHIVE")))
          :action `(lambda ()
                     ())))))

(defun brk-dashboard--adjusted-today ()
  (let* ((now (decode-time))
         (hour (decoded-time-hour now))
         (adjusted-today
          (if (and org-extend-today-until
                   (< (hour org-extend-today-until)))
              (decoded-time-add now (make-decoded-time :day -1))
            (now))))
    (setf (decoded-time-hour adjusted-today) org-extend-today-until
          (decoded-time-minute adjusted-today) 0
          (decoded-time-second adjusted-today) 0)
    adjusted-today))

(defun brk-dashboard--org-timestamp-regexp-for-decoded-dates (decoded-dates)
  "Return a regexp that matches an active Org timestamp
whose date part equals any of the DECODED-DATES."
  (let ((date-strings (mapcar (lambda (dt)
                                (format-time-string "%F" (encode-time dt)))
                              decoded-dates)))
    (rx-to-string
     `(seq
       "<"
       (or ,@(mapcar #'regexp-quote date-strings))
       (optional
        (seq " " (*? (not (any ">")))))
       ">"))))

(provide 'brk-dashboard)
;;; brk-dashboard.el ends here
