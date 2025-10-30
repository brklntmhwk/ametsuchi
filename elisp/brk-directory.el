;;; brk-directory.el --- A library to provide support for various directory patterns on various systems -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience, paths
;; URL: https://github.com/brklntmhwk/elisp/brk-directory.el
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

;; This library provides directory-related utilities and options
;; based on the XDG directory convention; all the user options
;; for directory paths default to the XDG ones.

;;; Code:
(require 'xdg)

(defun brk-directory--join (dir &rest paths)
  "Join DIR and PATHS into a single expanded path."
  (expand-file-name (mapconcat #'identity paths "/") dir))

(defgroup brk-directory nil
  "User directory paths abstraction."
  :group 'environment
  :prefix "brk-directory-")

(defcustom brk-directory-config-home
  (expand-file-name "emacs/" (xdg-config-home))
  "Directory for Emacs configuration files."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-cache-home
  (expand-file-name "emacs/" (xdg-cache-home))
  "Directory for Emacs cache files."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-data-home
  (expand-file-name "emacs/" (xdg-data-home))
  "Directory for Emacs data files."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-state-home
  (expand-file-name "emacs/" (xdg-state-home))
  "Directory for Emacs state files."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-user-desktop
  (or (xdg-user-dir "DESKTOP")
      "~/Desktop/")
  "User's desktop directory."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-user-documents
  (or (xdg-user-dir "DOCUMENTS")
      "~/Documents/")
  "User's documents directory."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-user-downloads
  (or (xdg-user-dir "DOWNLOADS")
      "~/Downloads/")
  "User's downloads directory."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-user-music
  (or (xdg-user-dir "MUSIC")
      "~/Music/")
  "User's music directory."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-user-pictures
  (or (xdg-user-dir "PICTURES")
      "~/Pictures/")
  "User's pictures directory."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-user-public
  (or (xdg-user-dir "PUBLIC")
      "~/Public/")
  "User's public directory."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-user-templates
  (or (xdg-user-dir "TEMPLATES")
      "~/Templates/")
  "User's templates directory."
  :type 'directory
  :group 'brk-directory)

(defcustom brk-directory-user-videos
  (or (xdg-user-dir "VIDEOS")
      "~/Videos/")
  "User's videos directory."
  :type 'directory
  :group 'brk-directory)

;;;###autoload
(defun brk-directory-describe ()
  "Display all user directory paths in a readable format."
  (interactive)
  (with-output-to-temp-buffer "*My Directories*"
    (printc (format "Config:    %s\n" brk-directory-config-home))
    (printc (format "Cache:     %s\n" brk-directory-config-home))
    (printc (format "Data:      %s\n" brk-directory-config-home))
    (printc (format "State:     %s\n" brk-directory-config-home))
    (printc (format "Desktop:   %s\n" brk-directory-config-home))
    (printc (format "Documents: %s\n" brk-directory-config-home))
    (printc (format "Downloads: %s\n" brk-directory-config-home))
    (printc (format "Music:     %s\n" brk-directory-config-home))
    (printc (format "Pictures:  %s\n" brk-directory-config-home))
    (printc (format "Public:    %s\n" brk-directory-config-home))
    (printc (format "Templates: %s\n" brk-directory-config-home))
    (printc (format "Videos:    %s\n" brk-directory-config-home))))

(provide 'brk-directory)
;;; brk-directory.el ends here
