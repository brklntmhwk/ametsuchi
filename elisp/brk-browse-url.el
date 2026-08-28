;;; brk-browse-url.el --- Extensions for browse-url -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ohma Togaki

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

(require 'brk-directory)
(eval-when-compile (require 'cl-lib))

(defvar brk-browse-url--browser-choice-file
  (concat brk-directory-data-home "browser-choice.el")
  "File to persist the user's browser choice.")

;; (SYMBOL-NAME EXECUTABLE-NAME BROWSE-FUNCTION PROGRAM-VARIABLE)
(defvar brk-browse-url--browser-definitions
  '((librewolf     "librewolf"     browse-url-firefox  browse-url-firefox-program)
    (firefox       "firefox"       browse-url-firefox  browse-url-firefox-program)
    (chromium      "chromium"      browse-url-chromium browse-url-chromium-program)
    (google-chrome "google-chrome" browse-url-chrome   browse-url-chrome-program)
    (brave         "brave"         browse-url-chrome   browse-url-chrome-program)
    (vivaldi       "vivaldi"       browse-url-chrome   browse-url-chrome-program)
    (xdg-open      "xdg-open"      browse-url-generic  browse-url-generic-program)
    (eww           nil             eww-browse-url      nil))
  "List of browsers with their respective Emacs handling functions.")

(defun brk-browse-url--discover-browsers ()
  "Discover available browsers from `brk-browse-url--browser-definitions'."
  (cl-loop for def in brk-browse-url--browser-definitions
           for exec = (nth 1 def)
           ;; The executable should be present on the system, or it's a built-in browser
           ;; (exec is nil).
           when (or (null exec) (executable-find exec))
           collect def))

(defun brk-browse-url--apply-browser-choice (browser-def)
  "Apply settings for BROWSER-DEF."
  (cl-destructuring-bind (_name exec fn var) browser-def
    (setq browse-url-browser-function fn)
    (when var
      (set var (or (and exec (executable-find exec)) exec)))))

(defun brk-browse-url--load-browser-choice ()
  "Load browser choice from the persistence file."
  (when (file-exists-p brk-browse-url--browser-choice-file)
    (with-temp-buffer
      (insert-file-contents brk-browse-url--browser-choice-file)
      (read (current-buffer)))))

(defun brk-browse-url--save-browser-choice (name)
  "Save the selected browser NAME to the persistence file."
  (with-temp-file brk-browse-url--browser-choice-file
    (let ((print-level nil)
          (print-length nil))
      (prin1 name (current-buffer)))))

;;;###autoload
(defun brk-browse-url-choose-browser ()
  "Choose a browser from available options.
It will be stored in `brk-browse-url--browser-choice-file' and persisted."
  (interactive)
  (let* ((browsers (brk-browse-url--discover-browsers))
         (names (mapcar (lambda (b) (symbol-name (car b))) browsers)))
    (if (null browsers)
        (message "No supported browsers found.")
      (let* ((choice (completing-read "Choose default browser: " names nil t))
             (selected (assq (intern choice) browsers)))
        (when selected
          (brk-browse-url--save-browser-choice (car selected))
          (brk-browse-url--apply-browser-choice selected)
          (message "Default browser set to: %s" choice))))))

;;;###autoload
(defun brk-browse-url-initialize-browser ()
  "Initialize browser configuration based on saved choice or auto-discovery."
  (let* ((saved-name (brk-browse-url--load-browser-choice))
         (available (brk-browse-url--discover-browsers))
         (target (or (assq saved-name available)
                     (cl-find-if (lambda (b) (nth 1 b)) available)
                     (car available))))
    (if target
        (brk-browse-url--apply-browser-choice target)
      (message "No supported browsers found."))))

(provide 'brk-browse-url)
;;; brk-browse-url.el ends here
