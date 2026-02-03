;;; early-init.el -*- lexical-binding: t; -*-

;;;; Startup ;;;;

;; Disable package.el in preparation for using emacs-twist instead.
(with-eval-after-load 'package
  (setopt package-enable-at-startup nil
          package-quickstart nil))

(advice-add 'x-apply-session-resources :override 'ignore)

;;;; Native Compile ;;;;

;; https://apribase.net/2024/07/09/emacs-eln-cache/
(when (boundp 'native-comp-eln-load-path)
  ;; Redirect the cache to a clean XDG location.
  (startup-redirect-eln-cache
   (expand-file-name "~/.local/share/emacs/eln-cache/")))

(with-eval-after-load 'comp
  (setopt native-comp-async-jobs-number 8
          native-comp-speed 1
          native-comp-always-compile t))

(with-eval-after-load 'warnings
  (setopt warning-suppress-types '((comp))))

;;;; UI Display Control ;;;;

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq use-dialog-box nil) ; fns.c
(setq frame-inhibit-implied-resize t) ; frame.c
