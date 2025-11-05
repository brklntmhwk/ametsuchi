;;; brk-org-anki.el --- Personal settings and extensions for org-anki -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-org-anki.el
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

(require 'org-anki)

(defvar brk-org-anki--decks '("Default")
  "Targetable Anki decks.")

(defvar brk-org-anki--deck-fetch-timer nil
  "Timer for fetching deck names after module load.")

(defun brk-org-anki--fetch-deck-names ()
  "Fetch Anki deck names via AnkiConnect.
Return a list of deck names, or nil if unavailable."
  (let ((url-request-method "POST")
        (url-request-data
         (encode-coding-string
          ;; https://git.sr.ht/~foosoft/anki-connect#codedecknamescode
          (json-encode '((action . "deckNames") (version . 6)))
          'utf-8))
        (url-request-extra-headers
         '(("Content-Type" . "application/json"))))
    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously org-anki-ankiconnnect-listen-address
                                        nil nil 3)
          (goto-char (point-min))
          ;; Skip HTTP headers
          (when (search-forward "\n\n" nil t)
            (let ((json-object-type 'alist)
                  (json-array-type 'list))
              (condition-case json-err
                  (alist-get 'result (json-read))
                (error
                 (message "org-anki: Failed to parse JSON: %s" json-err)
                 nil)))))
      (error
       (message "org-anki: AnkiConnect unavailable (%s)" (error-message-string err))
       nil))))

(defun brk-org-anki-refresh-decks ()
  "Refresh deck list from AnkiConnect."
  (interactive)
  (if-let ((decks (brk-org-anki--fetch-deck-names)))
      (progn
        (setq 'brk-org-anki--decks decks)
        (message "brk-org-anki: Updated decks: %s" decks))
    (message "brk-org-anki: Failed to fetch decks")))

(defun brk-org-anki--init ()
  "Initialize org-anki settings."
  (setq brk-org-anki--deck-fetch-timer
        (run-at-time "3 sec" nil #'brk-org-anki-refresh-decks)))

(when (and (not noninteractive) (not brk-org-anki--deck-fetch-timer))
  (brk-org-anki--init))

(provide 'brk-org-anki)
;;; brk-org-anki.el ends here
