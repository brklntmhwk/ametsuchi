;;; brk-org-agenda.el --- Extensions for org-agenda -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-org-agenda.el
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

(require 'org-agenda)
(require 'org-super-agenda)
(require 'brk-directory)

(defun brk-org-agenda--common-agenda-sections ()
  "Return a list of common agenda sections."
  `((agenda "" ((org-agenda-span 'day)
                (org-agenda-overriding-header "")
                (org-agenda-log-mode-items '(closed clock state))
                (org-super-agenda-groups
                 '((:name ""
                          :time-grid t)
                   (:name "◆ Today"
                          :date today
                          :deadline today
                          :scheduled today)
                   (:name "◆ Overdue"
                          :deadline past
                          :scheduled past)))))
    (agenda "" ((org-agenda-span 6)
                (org-agenda-overriding-header "")
                (org-agenda-log-mode-items '(closed clock))
                (org-agenda-start-day "+1d")
                (org-super-agenda-groups
                 '((:name ""
                          :time-grid t
                          :anything)))))))

(defun brk-org-agenda-custom-commands ()
  "Return personal custom commands for `org-agenda-custom-commands'."
  `(("@" "Private Agenda"
     ,(brk-org-agenda--common-agenda-sections)
     ((org-agenda-files
       (seq-filter
        (lambda (file)
          (and
           (not (string-match-p "/archive/" file))
           (not (string-match-p "/work/" file))))
        (directory-files-recursively brk-directory-user-documents "\\.org$")))
      (org-agenda-start-with-log-mode t)
      (org-agenda-block-separator nil)))
    ;; ("l" "Throwback Views"
    ;;  ((org-ql-block '(or (closed :from -30 :to today)
    ;;                      (clocked :from -30 :to today)
    ;;                      (ts :from -30 :to today))
    ;;                 ((org-ql-block-header "Over the last 30 days")
    ;;                  (org-ql-indent-levels t)
    ;;                  (org-super-agenda-groups
    ;;                   '((:auto-ts reverse)))))))
    ("t" "Miscellaneous Task Views"
     ((alltodo "" ((org-agenda-overriding-header "By Priority")
                   (org-super-agenda-groups
                    '((:name "Now!"
                             :priority "A")
                      (:name "Soon."
                             :priority "B")
                      (:name "Someday..."
                             :priority "C")
                      (:discard (:anything))))))
      (alltodo "" ((org-agenda-overriding-header "By Tag")
                   (org-super-agenda-groups
                    '((:discard (:tag ("ORDERED" "PROJECT")))
                      (:auto-tags)
                      (:discard (:anything))))))
      (alltodo "" ((org-agenda-overriding-header "By Effort")
                   (org-super-agenda-groups
                    '((:name "Sprint (x <= 30min)"
                             :effort< "30min")
                      (:name "Short Run (30min < x <= 3h)"
                             :effort< "3h")
                      (:name "Long Run (3h < x <= 3d)"
                             :effort< "3d")
                      (:name "Marathon (3d < x)"
                             :effort> "3d")
                      (:discard (:anything))))))
      (org-ql-block '(and (level 2 6)
                          (todo)
                          (ancestors (tags "PROJECT")))
                    ((org-ql-block-header "By Project")
                     (org-ql-indent-levels t)
                     (org-super-agenda-groups
                      '((:auto-outline-path)))))
      ;; https://github.com/alphapapa/org-ql/blob/master/examples.org#stuck-projects-block-agenda
      (org-ql-block '(and (tags "PROJECT")
                          (not (done))
                          (not (descendants (todo "READY")))
                          (not (descendants (scheduled))))
                    ((org-ql-block-header "Stuck Projects")
                     (org-ql-indent-levels t)
                     (org-super-agenda-groups
                      '((:auto-ts)))))))
    ("w" "Workplace Agenda"
     ,(brk-org-agenda--common-agenda-sections)
     ((org-agenda-files
       (directory-files-recursively (concat brk-directory-user-documents "work/") "\\.org$"))
      (org-agenda-block-separator nil)))))

(provide 'brk-org-agenda)
;;; brk-org-agenda.el ends here
