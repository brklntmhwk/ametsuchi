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

(defun brk-dashboard-insert-my-agenda-section (list-size)
  "Insert LIST-SIZE number of agenda entries into the dashboard."
  (dashboard-insert-section
   (format "Today's Agenda (Extended today until: %02d:00)"
           org-extend-today-until)
   (brk-dashboard--my-agenda-items)
   list-size
   'my-agenda
   (dashboard-get-shortcut 'my-agenda)
   `(lambda (&rest _)
      (brk-dashboard--agenda-open-action ',el))
   (brk-dashboard--my-agenda-format el)))

(defun brk-dashboard--my-agenda-items ()
  (require 'org-ql)
  (let* ((today (brk-dashboard--adjusted-today))
         (tomorrow (decoded-time-add today (make-decoded-time :day 1)))
         (regexp (brk-dashboard--org-timestamp-regexp-for-decoded-dates
                  (list today tomorrow)))
         (files org-agenda-files))
    (when files
      (thread-last
        (org-ql-select files
          `(and (regexp ,regexp)
                (not (done))
                (not (tags "ARCHIVE")))
          :action `(lambda ()
                     (let* ((el (org-element-at-point-no-context))
                            (bound (org-element-end el))
                            (heading-marker (point-marker))
                            timestamps)
                       (save-excursion
                         (when (re-search-forward ,regexp bound t)
                           (push (org-timestamp-from-string (match-string 0))
                                 timestamps))
                         (thread-first
                           el
                           (org-element-put-property :hd-marker heading-marker)
                           (org-element-put-property :active-timestamp
                                                     (thread-last
                                                       timestamps
                                                       (seq-sort-by #'org-timestamp-to-time
                                                                    #'time-less-p)
                                                       (car))))))))
        (seq-sort-by (lambda (el)
                       (org-element-property :todo-keyword el))
                     #'string-lessp)
        (seq-sort-by (lambda (el)
                       (or (org-element-property :priority el)
                           org-priority-default))
                     #'<)
        (seq-sort-by (lambda (el)
                       (org-timestamp-to-time (org-element-property :active-timestamp el)))
                     #'time-less-p)))))

(defun brk-dashboard--adjusted-today ()
  "Return today's base time adjusted for `org-extend-today-until'.

If the current time is earlier than `org-extend-today-until', treat it as still
belonging to yesterday."
  (let* ((now (decode-time))
         (hour (decoded-time-hour now))
         (today
          (if (and org-extend-today-until
                   (< hour org-extend-today-until))
              (decoded-time-add now (make-decoded-time :day -1))
            now)))
    (setf (decoded-time-hour today) org-extend-today-until
          (decoded-time-minute today) 0
          (decoded-time-second today) 0)
    today))

(defun brk-dashboard--org-timestamp-regexp-for-decoded-dates (decoded-dates)
  "Return a regexp that matches an active Org timestamp
  whose date part equals any of the DECODED-DATES."
  (let ((date-strings (mapcar (lambda (dt)
                                (format-time-string "%F" (encode-time dt)))
                              decoded-dates)))
    (rx-to-string
     `(and "<"
           (or ,@date-strings)
           (zero-or-more (any " \tA-Za-z0-9:+-"))
           ">"))))

(defun brk-dashboard--my-agenda-format (el)
  (let* ((ts (org-element-property :active-timestamp el))
         (deadline (org-element-property :deadline el))
         (scheduled (org-element-property :scheduled el)))
    (concat
     (string-join
      (delq nil (list
                 ;; Use bare active timestamps for appointments only.
                 (when (and (not deadline) (not scheduled) ts)
                   (concat "["
                           (string #x1f4c5)
                           ": "
                           (brk-dashboard--org-timestamp-format ts)
                           "] "))
                 (when scheduled
                   (concat "["
                           (string #x1f4dd)
                           ": "
                           (brk-dashboard--org-timestamp-format scheduled)
                           "] "))
                 (when deadline
                   (concat "["
                           (string #x1f480)
                           ": "
                           (brk-dashboard--org-timestamp-format deadline)
                           "] ")))))
     (if-let* ((pri (org-element-property :priority el)))
         (propertize (format "#%s " (char-to-string pri))
                     'face 'org-priority)
       "")
     (if-let* ((kwd (org-element-property :todo-keyword el)))
         (concat (propertize kwd 'face (or (org-get-todo-face kwd) 'default))
                 " ")
       "")
     (propertize (org-link-display-format (org-element-property :raw-value el))
                 'face 'italic))))

(defun brk-dashboard--org-timestamp-format (ts)
  "Format any form of an Org timestamp with `org-extend-today-until' in mind."
  (let* ((time (org-timestamp-to-time ts))
         (filled-time (org-timestamp-to-time
                       (brk-dashboard--org-timestamp-fill ts)))
         (today (encode-time (brk-dashboard--adjusted-today)))
         (day-diff (- (time-to-days filled-time)
                      (time-to-days today)))
         (min-diff (/ (float-time (time-subtract time (current-time))) 60)))
    (cond
     ((< day-diff -1)
      (propertize (org-format-timestamp ts "%Y-%m-%d, %a")
                  'face 'org-imminent-deadline))
     ((= day-diff -1)
      (propertize "yesterday" 'face 'org-imminent-deadline))
     ((= day-diff 0)
      (if (= (org-element-property :hour-start ts)
             org-extend-today-until)
          (propertize "today"
                      'face 'org-scheduled-today)
        (if (< min-diff 0)
            (propertize (format "%s ago"
                                (org-duration-from-minutes (abs min-diff)))
                        'face 'org-imminent-deadline)
          (propertize (format "in %s" (org-duration-from-minutes min-diff))
                      'face 'org-upcoming-deadline))))
     ((= day-diff 1)
      (propertize "tomorrow" 'face 'org-date))
     ((> day-diff 1)
      (propertize (org-format-timestamp ts "%Y-%m-%d, %a")
                  'face 'org-default)))))

(defun brk-dashboard--org-timestamp-fill (ts)
  "Fill the time part of an Org timestamp with `org-extend-today-until' in mind."
  (let ((hour (or (org-element-property :hour-start ts)
                  org-extend-today-until))
        (min (or (org-element-property :minute-start ts)
                 0))
        (sec (or (org-element-property :second-start ts)
                 0)))
    (thread-first
      (copy-sequence ts)
      (org-element-put-property :hour-start hour)
      (org-element-put-property :minute-start min)
      (org-element-put-property :second-start sec))))

(defun brk-dashboard--agenda-open-action (el)
  (let ((marker (or (org-element-property :hd-marker el)
                                        ; `org-ql--add-markers' adds this prop and `org-ql-select' may use it internally.
                    (org-element-property :org-hd-marker el))))
    (when (and marker (markerp marker))
      (let* ((base-buffer (marker-buffer marker))
             (headline (with-current-buffer base-buffer
                         (org-with-point-at marker
                           (org-get-heading t t t t))))
             (indirect-buffer (org-get-indirect-buffer base-buffer headline)))
        (with-current-buffer indirect-buffer
          (goto-char marker)
          (org-narrow-to-subtree)
          (org-show-entry)
          (org-show-children))
        (pop-to-buffer indirect-buffer)))))

(provide 'brk-dashboard)
;;; brk-dashboard.el ends here
