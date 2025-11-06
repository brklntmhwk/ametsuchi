;;; brk-consult.el --- Extensions for consult -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-consult.el
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

(require 'consult)
(require 'brk-activities)

;; https://github.com/alphapapa/activities.el/discussions/59
(defvar brk-consult-source-activities-buffer
  `(:name "Activities Buffers"
          :narrow (?a . "Activity")
          :category buffer
          :face consult-buffer
          :history buffer-name-history
          :default t
          :items ,(lambda ()
                    (consult--buffer-query
                     :predicate #'brk-activities-local-buffer-p
                     :sort 'visibility
                     :as #'buffer-name))
          :state ,#'consult--buffer-state))

;; Comply with the conventions consult brings up.
(consult--define-state tab)
(defun consult--tab-action (tab)
  "Open TAB."
  (tab-bar-switch-to-tab tab))
(defun consult--tab-preview ()
  "Create preview function for tabs."
  (let (current-tab)
    (lambda (action cand)
      (pcase action
        ('preview
         (setq current-tab (alist-get 'name (tab-bar--current-tab)))
         (tab-bar-switch-to-tab cand))
        ('preview-restore
         (when current-tab
           (tab-bar-switch-to-tab current-tab)))))))

(defvar brk-consult--tab-history nil)

(defvar brk-consult-source-tab-bar-tab
  `(:name "Tabs in Tab Bar"
          :narrow (?t . "Tab")
          :category tab
          :face font-lock-function-name-face
          :history brk-consult--tab-history
          ;; :action ,#'tab-bar-switch-to-tab
          :items ,(lambda ()
                    (let ((tabs (funcall tab-bar-tabs-function)))
                      (when (> (length tabs) 1)
                        (thread-last
                          tabs
                          (cl-remove-if (lambda (tab)
                                          (eq 'current-tab (car tab))))
                          (mapcar (lambda (tab)
                                    (alist-get 'name tab)))))))
          :state ,#'consult--tab-state))

(provide 'brk-consult)
;;; brk-consult.el ends here
