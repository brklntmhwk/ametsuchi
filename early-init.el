;;; early-init.el -*- lexical-binding: t; -*-

;;; Startup

;; Disable package.el in preparation for using emacs-twist instead.
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(advice-add 'x-apply-session-resources :override 'ignore)

;;; UI display control

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq use-dialog-box nil) ; fns.c
(setq frame-inhibit-implied-resize t) ; frame.c
