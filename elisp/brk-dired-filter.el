;;; brk-dired-filter.el --- Customization of dired-filter -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ohma Togaki

;; Author: Ohma Togaki
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/brklntmhwk/elisp/brk-dired-filter.el
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

;;;###autoload
(defun brk-dired-filter-generate-default-groups ()
  "Generate default groups for `dired-filter-group-saved-groups'."
  (require 'mailcap)
  (mailcap-parse-mailcaps) ; reflect user's mailcap info
  (append '(("General directories"
             (directory . t)
             (name . "^[[:alnum:]]"))
            ("Dot directories"
             (directory . t)
             (or (name . "^\\.")
                 (name . "_deps")
                 (name . "node_modules")))
            ("Build artifacts"
             (directory . t)
             (or (name . "^_?build")
                 (name . "dist")
                 (name . "^result.*")))
            ("Dot or config files"
             (file . t)
             (or (dot-files)
                 (extension "conf" "toml" "yaml" "yml")))
            ("Data files"
             (file . t)
             (extension "csv" "json" "jsonc" "lock"))
            ("Code"
             (file . t)
             (extension "astro" "c" "clj" "css" "el" "hs"
                        "html" "js" "jsx" "nix" "py" "rs"
                        "scss" "ts" "tsx" "zig"))
            ("Org"
             (file . t)
             (extension "org" "org_archive"))
            ("Text documents"
             (file . t)
             (or (name . "COPYING")
                 (name . "LICENSE")
                 (name . "README")
                 (name . "TODO")
                 (extension "markdown" "md" "mdx" "mkd" "rst" "txt")))
            ("E-books and PDF" (extension "azw" "epub" "mobi"  "pdf"))
            ("Archives" (extension "bz2" "gz" "nar" "rar" "tar" "zip"))
            ("LaTeX" (extension "tex" "bib"))
            ("Executables" (executable)))
          ;; https://github.com/akirak/emacs-config/commit/f6224a515be5626aba0914f68bd4b3af7fcb0a79
          (thread-last
            mailcap-mime-extensions
            ;; Avoid confusing Typescript with Video Transport Stream.
            (cl-remove-if (lambda (x)
                            (string= ".ts" (car x))))
            (seq-group-by (lambda (x)
                            (let ((mime (cdr x)))
                              (pcase mime
                                ((rx bol "image/")
                                 "Image")
                                ((rx bol "video/")
                                 "Video")
                                ((rx bol "audio/")
                                 "Audio")))))
            (cl-remove-if (lambda (x)
                            (not (car x))))
            (mapcar (lambda (x)
                      `(,(car x)
                        (extension ,@(thread-last
                                       (cdr x)
                                       (mapcar #'car)
                                       (mapcar (lambda (s)
                                                 (substring s 1)))))))))))

(provide 'brk-dired-filter)
;;; brk-dired-filter.el ends here
