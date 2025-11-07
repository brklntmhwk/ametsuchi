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

(require 'brk-directory)

(defun brk-org-capture--find-latest-datetree-entry ()
  "Return the position of today's entry in the latest yearly journal file.
If today's journal heading exists, place point at the end of the subtree.
If not, create it using `org-reverse-datetree-goto-date-in-file' before placing point."
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

(defun brk-org-capture--anki-deck-prompt ()
  (let* ((decks brk-org-anki--decks)
         (deck-options (string-join decks "|")))
    (format "%%^{Anki deck name?|%s}" deck-options)))

;;;###autoload
(defun brk-org-capture-generate-templates ()
  "Return my default Org-capture templates as a list."
  (require 'brk-org-anki)
  (let ((anki-deck-prompt (brk-org-capture--anki-deck-prompt)))
    `(("a" "Anki Basic" entry
       (file ,(concat brk-directory-user-documents "anki.org"))
       ,(concat
         "* %? %^g\n"
         ":PROPERTIES:\n"
         ":ANKI_NOTE_TYPE: Basic\n"
         ":ANKI_DECK: " anki-deck-prompt "\n"
         ":END:\n"
         "Answer")
       :empty-lines-after 1
       :prepend t)
      ("c" "Anki Cloze" entry
       (file ,(concat brk-directory-user-documents "anki.org"))
       ,(concat
         "* {{c1::%?}} %^g\n"
         ":PROPERTIES:\n"
         ":ANKI_NOTE_TYPE: Cloze\n"
         ":ANKI_DECK: " anki-deck-prompt "\n"
         ":END:\n")
       :empty-lines-after 1
       :prepend t)
      ("d" "Draft" entry
       (file "")
       "* %U %? :DRAFT:"
       :empty-lines-after 1
       :prepend t)
      ("i" "Interruption" entry
       (file "")
       "* %U %?"
       :clock-in t
       :clock-resume t)
      ("j" "Journal" entry
       (function brk-org-capture--find-latest-datetree-entry)
       "* %U %?")
      ("p" "Project" entry
       (file "")
       ,(concat
         "* PENDING %? :PROJECT:\n"
         ":LOGBOOK:\n"
         "- Created at      %U\n"
         ":END:")
       :empty-lines-after 1
       :prepend t)
      ("t" "Todo" entry
       (file "")
       ,(concat
         "* PENDING %?\nSCHEDULED: %t\n"
         ":LOGBOOK:\n"
         "- Created at      %U\n"
         ":END:")
       :empty-lines-after 1
       :prepend t))))

(provide 'brk-org-capture)
;;; brk-org-capture.el ends here
