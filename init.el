;; init.el -*- lexical-binding: t; -*-

;;; Package manager

(use-package twist
	:ensure t
	:hook (emacs-startup-hook . twist-watch-mode)
	:bind
	(("<f12>" . twist-update)))

;;; Package configuration macro

(eval-when-compile
	(require 'use-package))

(eval-and-compile
	(setq-default use-package-ensure-function #'(lambda (&rest args) t)
								use-package-always-defer t
								use-package-hook-name-suffix nil
								use-package-verbose t
								use-package-expand-minimally nil))

(if init-file-debug
		(setq-default use-package-verbose t
									use-package-expand-minimally nil
									use-package-compute-statistics t)
	(setq-default use-package-verbose nil
								use-package-expand-minimally t))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Data management

;; XDG base directory
(require 'xdg)

(defvar emacs-config-home
	(concat (xdg-config-home) "/emacs/"))

(defvar emacs-cache-home
	(concat (xdg-cache-home) "/emacs/"))

(defvar emacs-data-home
	(concat (xdg-data-home) "/emacs/"))

(defvar emacs-state-home
	(concat (xdg-state-home) "/emacs/"))

(defvar emacs-pictures-dir
	(concat (xdg-user-dir "PICTURES") "/emacs/"))

(defvar emacs-documents-dir
	(concat (xdg-user-dir "DOCUMENTS") "/emacs/"))

;; custom

(use-package custom
	:custom
	(custom-file (locate-user-emacs-file "custom.el"))
	:config
	(load custom-file :no-error-if-file-is-missing))

;; no-littering

(use-package no-littering
	:ensure t
	:custom
	(no-littering-var-directory emacs-cache-home)
	(no-littering-etc-directory emacs-data-home))

;;; Global keybindings

;;;; Kill & yank

;; (global-set-key (kbd "C-c c") #'kill-ring-save)
;; (global-set-key (kbd "C-c x") #'kill-region)
;; (global-set-key (kbd "C-c v") #'yank)

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;;; Custom keymaps

(defvar toggle-prefix "<f8>"
	"Key prefix for `toggle-map'.")

(defvar-keymap toggle-map
	:doc "Keymap for common toggle actions."
	:prefix 'toggle-map-prefix
	"*" '("Light/dark theme" . modus-themes-toggle)
	"=" '("Calculator" . calc)
	"d" '("Debug on error" . toggle-debug-on-error)
	"f" '("Fill column indicator" . display-fill-column-indicator-mode)
	"h" '("Line highlight" . hl-line-mode)
	"l" '("Line numbers" . global-display-line-numbers-mode)
	"t" '("Truncate lines" . toggle-truncate-lines)
	"v" '("Variable pitch" . variable-pitch-mode)
	"w" '("Whitespace" . whitespace-mode)
	"x" '("Syntax checker" . flymake-mode))

(keymap-global-set toggle-prefix 'toggle-map-prefix)

(use-package repeat
	:hook (after-init-hook . repeat-mode))

;;; Library

;; Use macros only.
(eval-when-compile
	(require 'cl-lib))

;;; Performance enhancement

(use-package edebug
	:config
	;; Prevent `edebug' default bindings from interfering with those of `activities-map'.
	(setq edebug-inhibit-emacs-lisp-mode-bindings t))

(use-package gcmh
	:ensure t
	:custom
	(gcmh-idle-delay 'auto)
	(gcmh-high-cons-threshold (* 128 1024 1024))
	(gcmh-verbose init-file-debug)
	:hook
	(after-init-hook . gcmh-mode))

;;; Help

(use-package casual
	:ensure t
	:after transient
	:config
	(require 'casual-image)
	:bind
	((:map calc-mode-map
				 ("M-?" . casual-calc-tmenu))
	 (:map dired-mode-map
				 ("M-?" . casual-dired-tmenu))
	 (:map image-mode-map
				 ("M-?" . casual-image-tmenu))))

(use-package help
	:custom
	(help-window-keep-selected t))

(use-package helpful
	:ensure t
	:defer 1
	:bind
	(([remap describe-function] . helpful-callable)
	 ([remap describe-command] . helpful-command)
	 ([remap describe-key] . helpful-key)
	 ([remap describe-variable] . helpful-variable)
	 ([remap Info-goto-emacs-command-node] . helpful-function)
	 :map mode-specific-map
	 ("C-d" .  helpful-at-point)))

(use-package transient
	:custom
	(transient-history-file (concat (xdg-emacs-state-home) "transient/history.el"))
	(transient-values-file (concat (xdg-emacs-data-home) "transient/values.el"))
	(transient-levels-file (concat (xdg-emacs-data-home) "transient/levels.el"))
	:config
	(transient-define-prefix my/toggle-transient ()
		"Prefix for `toggle-map'"
		[("d" "Debug on error" toggle-debug-on-error)
		 ("f" "Fill column indicator" display-fill-column-indicator-mode)
		 ("h" "Line highlight" hl-line-mode)
		 ("l" "Line numbers" global-display-line-numbers-mode)
		 ("t" "Truncate lines" toggle-truncate-lines)
		 ("v" "Variable pitch" variable-pitch-mode)
		 ("w" "Whitespace" whitespace-mode)
		 ("x" "Syntax checker" flymake-mode)
		 ("*" "Light/dark theme" modus-themes-toggle)])
	(keymap-set toggle-map "?" '("Transient help" . my/toggle-transient)))

;; (use-package transient-posframe
;; 	:ensure t
;; 	:after transient
;; 	:custom
;; 	(transient-posframe-border-width 3)
;; 	:config
;; 	(transient-posframe-mode 1))

(use-package woman
	:custom
	(woman-fill-column 82)
	(woman-cache-filename (concat (emacs-cache-home) ".wmncach.el"))
	:bind
	(("<f1> M-m" . woman)))

;;; Window management

(use-package ace-window
	:ensure t
	:custom
	(aw-keys '(?e ?i ?a ?o ?k ?t ?n ?s ?h))
	(aw-scope 'frame)
	(aw-dispatch-when-more-than 1)
	:bind
	(("M-o" . ace-window)
	 (:map window-prefix-map
				 ("o" . ace-swap-window)))
	:config
	;; Use `setq' here because `aw-dispatch-alist' is implemented with `defvar' as of Jul 2025.
	(setq aw-dispatch-alist
				'((?b aw-switch-buffer-in-window "Select buffer")
					(?c aw-copy-window "Copy Window")
					(?f aw-split-window-vert "Split window fairly")
					(?j aw-switch-buffer-other-window "Select buffer in other window")
					(?m aw-move-window "Move window")
					(?v aw-split-window-vert "Split window vertically")
					(?w aw-swap-window "Swap windows")
					(?x aw-execute-command-other-window "Execute command in other window")
					(?z aw-split-window-horz "Split window horizontally")
					(?0 aw-delete-window "Delete window")
					(?1 delete-other-windows "Delete other windows")
					(?~ aw-transpose-frame "Transpose frame")
					(?? aw-show-dispatch-help))))

(use-package popper
	:ensure t
	:custom
	(popper-window-height 0.333)
	(popper-display-function #'popper-display-popup-at-bottom)
	(popper-mode-line '(:eval (propertize " POP ")))
	(popper-reference-buffers
	 '("Output\\*$"
		 "\\*Backtrace\\*"
		 "\\*Messages\\*$"
		 "^\\*Async Shell Command\\*$"
		 "^\\*Apropos\\*$"
		 "^\\*Compile-Log\\*$"
		 "^\\*eat.\\*$" eat-mode
		 "^\\*envrc\\*"
		 "^\\*eshell.*\\*$" eshell-mode
		 "^\\*Flymake diagnostics"
		 "^\\*Help.*\\*$" help-mode
		 "^\\*helpful.*\\*$" helpful-mode
		 "^\\*Shell Command Output\\*"
		 "^\\*Warnings\\*$"))
	:hook
	(after-init-hook . popper-mode)
	(popper-mode-hook . popper-echo-mode)
	:bind
	((:map window-prefix-map
				 :prefix-map popper-prefix-map 
				 :prefix "p"
				 ("t" . popper-toggle)
				 ("@" . popper-cycle)
				 ("~" . popper-toggle-type))
	 (:repeat-map popper-repeat-map
								("t" . popper-toggle)
								("@" . popper-cycle)
								("~" . popper-toggle-type))))

(use-package tab-bar
	:custom
	(tab-bar-auto-width-max '(320 25))
	(tab-bar-new-tab-choice "*scratch*")
	:bind
	((:map tab-bar-history-mode-map
				 :map tab-prefix-map
				 (">" . tab-bar-history-forward)
				 ("<" . tab-bar-history-back)))
	:hook (after-init-hook . tab-bar-history-mode))

(use-package window
	:custom
	(recenter-positions '(top middle bottom))
	(switch-to-buffer-obey-display-actions t)
	:bind
	([remap scroll-up-command] . my/scroll-half-window-height-forward)
	([remap scroll-down-command] . my/scroll-half-window-height-backward)
	:config
	(defun scroll-half-window-height ()
		(/ (window-body-height) 2))
	(defun my/scroll-half-window-height-forward (&optional arg)
		(interactive "P")
		(if (numberp arg)
				(pixel-scroll-up arg)
			(pixel-scroll-up (scroll-half-window-height))))
	(defun my/scroll-half-window-height-backward (&optional arg)
		(interactive "P")
		(if (numberp arg)
				(pixel-scroll-down arg)
			(pixel-scroll-down (scroll-half-window-height)))))

(use-package winner
	:custom
	(winner-dont-bind-my-keys t)
	:hook (window-setup-hook . winner-mode)
	:bind
	(:map window-prefix-map
				("<" . winner-undo)
				(">" . winner-redo))
	(:repeat-map winner-repeat-map
							 ("<" . winner-undo)
							 (">" . winner-redo)))

;;; Appearance

;; buffer.c

(setq-default buffer-file-coding-system 'utf-8 ; `undecided-unix' by default
							cursor-type 'bar ; t by default
							fill-column 85 ; 70 by default
							line-spacing 2  ; 1 by default
							tab-width 2 ; 8 by default
							indicate-empty-lines t ; nil by default
							indicate-buffer-boundaries 'left ; nil by default
							left-fringe-width 2 ; nil by default
							right-fringe-width 2 ; nil by default
							left-margin-width 2 ; 0 by default
							right-margin-width 2 ; 0 by default
							)

;; frame.c

(cl-pushnew '(internal-border-width . 16) default-frame-alist :test #'equal)

;; xdisp.c

(defconst my/base-frame-title-format
	'(" - GNU Emacs"
		(emacs-version (" " emacs-version))
		(system-name (" on " system-name))))

(defconst my/default-frame-title-format
	(cons '("%b")
				my/base-frame-title-format))

(setq-default bidi-inhibit-bpa t
							bidi-display-reordering 'left-to-right
							bidi-paragraph-direction 'left-to-right
							display-line-numbers-width 4
							frame-title-format my/default-frame-title-format
							scroll-conservatively 1)

(use-package fontaine
	:ensure t
	:custom
	(fontaine-presets
	 '((regular
			:default-family "Hackgen NF"
			:default-height 120
			:fixed-pitch-family "Hackgen NF"
			:fixed-pitch-height 1.0
			:variable-pitch-family "Noto Sans"
			:variable-pitch-height 1.2
			:line-spacing 1)
		 (medium
			:inherit regular
			:default-height 150)
		 (large
			:inherit regular
			:default-height 175)))
	:config
	(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
	(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(use-package frame
	:custom
	(window-divider-default-places t)
	(window-divider-default-bottom-width 5)
	(window-divider-default-right-width 5)
	:config
	(blink-cursor-mode -1))

(use-package modus-themes
	:ensure t
	:demand t
	:custom
	(modus-themes-headings
   '((1 . (variable-pitch bold 1.5))
     (2 . (variable-pitch rainbow semibold 1.4))
     (3 . (variable-pitch rainbow medium 1.3))
     (4 . (variable-pitch rainbow medium 1.2))
     (t . (1.1))))
	(modus-themes-common-palette-overrides
	 '((border-mode-line-active unspecified)
		 (border-mode-line-inactive unspecified)))
	(modus-vivendi-tinted-palette-overrides
	 '((bg-hl-line bg-dim)
		 (bg-mode-line-active bg-lavender)
		 (bg-mode-line-inactive bg-inactive)))
	(modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
	:init
	(defun my/modus-themes-custom-face ()
		(modus-themes-with-colors
			(custom-set-faces
			 ;; ace-window
			 `(aw-leading-char-face ((,c :height 2.0 :foreground ,blue-warmer)))
			 `(aw-minibuffer-leading-char-face ((,c :height 1.1 :foreground ,blue-warmer)))
			 ;; dired-filter
			 `(‎dired-filter-group-header‎ ((,c :background ,bg-lavender :box(:line-width 2 :color ,bg-lavender))))
			 ;; goggles
			 `(goggles-added ((,c :background ,bg-added-refine)))
			 `(goggles-changed ((,c :background ,bg-changed-refine)))
			 `(goggles-removed ((,c :background ,bg-removed-refine)))
			 ;; vertico-posframe
			 `(vertico-posframe-border-2 ((,c :background ,bg-added-refine)))
			 `(vertico-posframe-border-3 ((,c :background ,bg-added-fringe)))
			 ;; vundo
			 `(vundo-saved ((,c :foreground ,blue)))
			 `(vundo-last-saved ((,c :foreground ,blue-intense)))
			 `(vundo-highlight ((,c :foreground ,fg-changed)))
			 ;; Built-ins
			 `(header-line ((,c :background ,bg-dim :box (:line-width 4 :color ,bg-dim))))
			 `(mode-line-active ((,c :overline ,bg-lavender
															 :underline (:color ,bg-lavender :position t))))
			 `(mode-line-inactive ((,c :overline ,bg-inactive
																 :underline (:color ,bg-inactive :position t))))
			 `(tab-bar-tab ((,c :background ,bg-active :box (:line-width 5 :color ,bg-active))))
			 `(tab-bar-tab-inactive ((,c :background ,bg-inactive :box (:line-width 5 :color ,bg-inactive))))
			 `(scroll-bar ((,c :foreground ,border :background ,bg-dim)))
			 `(whitespace-line ((,c :background ,slate :foreground ,fg-main)))
			 `(whitespace-missing-newline-at-eof ((,c :background ,slate :foreground ,fg-main)))
			 `(whitespace-trailing ((,c :background ,slate :foreground ,fg-main))))))
	(add-hook 'modus-themes-after-load-theme-hook #'my/modus-themes-custom-face)
	:config
	(modus-themes-load-theme 'modus-vivendi-tinted))

;; Remember to do M-x and run `nerd-icons-install-fonts' to get the
;; font files.  Then restart Emacs to see the effect.
(use-package nerd-icons	:ensure t)

(use-package nerd-icons-completion
	:ensure t
	:after marginalia
	:config
	(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
	:ensure t
	:after corfu
	:config
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
	:ensure t
	:hook
	(dired-mode-hook . nerd-icons-dired-mode))

(use-package olivetti
	:ensure t
	:custom
	(olivetti-body-width 82)
	:hook
	((markdown-mode
		org-mode) . olivetti-mode))

(use-package page-break-lines
	:ensure t
	;; :hook (after-init-hook . global-page-break-lines-mode)
	:init (global-page-break-lines-mode 1)
	:config
	(dolist (mode '(compilation-mode-hook
									dashboard-mode-hook
									doc-mode-hook
									haskell-mode-hook
									help-mode-hook
									magit-mode-hook))
		(add-to-list 'page-break-lines-modes mode)))

;;; Lines

(setq-default header-line-format
							'("" header-line-indent
								(:eval (breadcrumb--header-line))
								" "
								(mode-line-misc-info mode-line-misc-info)))

(setq-default mode-line-format
							'("%e"
								mode-line-front-space
								mode-line-mule-info
								mode-line-modified
								"  "
								mode-line-buffer-identification
								(vc-mode vc-mode)
								"  "
								(:eval (if minions-mode
													 minions-mode-line-modes
												 mode-line-modes))
								"  "
								(mode-line-process ("  " mode-line-process))
								(current-input-method-title
								 (current-input-method-title " "))
								(global-mode-string global-mode-string)
								(mode-line-client mode-line-client)
								mode-line-position
								mode-line-end-spaces))

;; (use-package bindings
;; 	:custom
;; 	(mode-line-right-align-edge 'right-fringe))

(use-package breadcrumb
	:ensure t
	:custom
	(breadcrumb-project-crumb-separator " > ")
	:hook (after-init-hook . breadcrumb-mode))

(use-package hide-mode-line
	:ensure t
	:commands
	(hide-mode-line-mode
	 ‎turn-on-hide-mode-line-mode‎
	 ‎turn-off-hide-mode-line-mode‎))

(use-package minions
	:ensure t
	:custom
	(minions-mode-line-lighter "[...]")
	:bind
	("<f7>" . minions-minor-modes-menu)
	:hook (after-init-hook . minions-mode))

(use-package mlscroll
	:ensure t
	:custom
	(mlscroll-right-align nil)
	(mlscroll-alter-percent-position 'replace)
	(mlscroll-minimum-current-width 5)
	:config
	(if (daemonp)
			(add-hook 'server-after-make-frame-hook #'mlscroll-mode)
		(mlscroll-mode 1)))

(use-package moody
	:ensure t
	:custom
	(moody-mode-line-height 25)
	(moody-ribbon-background '(base :background))
	:hook
	(after-init-hook . (lambda ()
											 (moody-replace-mode-line-front-space)
											 (moody-replace-mode-line-buffer-identification)
											 (moody-replace-vc-mode))))

(use-package which-func
	:custom
	(which-func-unknown "⊥")
	(which-func-non-auto-modes
	 '(fundamental-mode
		 help-mode
		 org-mode
		 markdown-mode
		 nov-mode
		 pdf-view-mode
		 minibuffer-mode))
	:hook (after-init-hook . which-function-mode))

;;; Completion

(use-package cape
	:ensure t
	:custom
	(cape-dict-file
	 (concat (xdg-data-home) "cape/dict"))
	:functions
	(cape-capf-buster
	 cape-capf-super)
	:bind
	((:prefix-map cape-capf-prefix-map :prefix "M-p"
								("a" . cape-abbrev)
								("d" . cape-dabbrev)
								("f" . cape-file)
								("h" . cape-history)
								("k" . cape-keyword)
								("l" . cape-line)
								("r" . cape-rfc1345)
								("s" . cape-sgml)
								("w" . cape-dict)
								("&" . cape-sgml)
								("\\" . cape-tex)))
	:hook
	(eglot-managed-mode-hook . my/setup-cape-eglot-capf)
	(text-mode-hook . my/setup-cape-text-mode-capf)
	(prog-mode-hook . my/setup-cape-prog-mode-capf)
	:config
	(setq-default completion-at-point-functions
								(append (default-value 'completion-at-point-functions)
												(list #'cape-file #'cape-dabbrev)))
  (defun my/setup-cape-eglot-capf()
    (setq-local completion-at-point-functions
								(list (cape-capf-super
											 #'cape-file
											 (cape-capf-buster #'eglot-completion-at-point #'string-prefix-p)
											 #'cape-keyword
											 :with #'tempel-complete))))
  (defun my/setup-cape-prog-mode-capf()
    (add-hook 'completion-at-point-functions #'cape-file nil t))
  (defun my/setup-cape-text-mode-capf()
    (add-hook 'completion-at-point-functions #'cape-file nil t)
    (add-hook 'completion-at-point-functions #'cape-dict 10 t))
	(with-eval-after-load 'transient
		(transient-define-prefix my/cape-capf-transient ()
			"Prefix for cape capfs."
			[("a" "abbrev" cape-abbrev)
			 ("d" "dabbrev" cape-dabbrev)
			 ("f" "file" cape-file)
			 ("h" "history" cape-history)
			 ("k" "keyword" cape-keyword)
			 ("l" "line" cape-line)
			 ("r" "rfc1345" cape-rfc1345)
			 ("s" "elisp symbol" cape-elisp-symbol)
			 ("w" "dict" cape-dict)
			 ("&" "sgml" cape-sgml)
			 ("\\" "tex" cape-tex)])
		(keymap-set cape-capf-prefix-map "?" #'my/cape-capf-transient)))

(use-package completion-preview
	:if (version<= "30.1" emacs-version)
	:hook
	(corfu-mode-hook . completion-preview-mode)
	:bind
	(:map completion-preview-active-mode-map
				("TAB" . completion-preview-complete)
				("C-e" . completions-preview-insert)))

(use-package consult
	:ensure t
	:custom
	(consult-bookmark-narrow
	 '((?e "Eww" eww-bookmark-jump)
		 (?f "File" bookmark-default-handler)
		 (?h "Help" help-bookmark-jump)
		 (?i "Info" Info-bookmark-jump)
		 (?o "Org headings" org-bookmark-heading-jump)))
	:bind
	(([remap bookmark-jump] . consult-bookmark)
	 ([remap goto-line] . consult-goto-line)
	 ([remap switch-to-buffer] . consult-buffer)
	 ([remap project-switch-to-buffer] . consult-project-buffer)
	 ([remap yank-pop] . consult-yank-pop)
	 (:map goto-map
				 ("m" . consult-mark)
				 ("M" . consult-global-mark)
				 ("o" . consult-outline))
	 (:map search-map
				 ("f" . consult-fd)
				 ("g" . consult-git-grep)
				 ("k" . consult-keep-lines)
				 ("l" . consult-line)
				 ("L" . consult-line-multi)
				 ("r" . consult-ripgrep)
				 ("u" . consult-focus-lines))
	 (:map isearch-mode-map
				 ("M-e" . consult-isearch-history)
				 ("M-s e" . consult-isearch-history))
	 (:map mode-specific-map
				 ("k" . consult-kmacro)))
	:hook (completion-list-mode-hook . consult-preview-at-point-mode))

(use-package consult-dir
	:ensure t
	:after vertico
	:bind
	((:map ctl-x-map
				 ("C-d" . consult-dir))
	 (:map vertico-map
				 ("C-x C-d" . consult-dir)
				 ("C-x C-j" . consult-dir-jump-file)))
	:config
	((add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)))

(use-package consult-gh
	:ensure t
	:if (executable-find "gh")
	:after consult
	:custom
	;; (consult-gh-default-clone-directory "path/to/directory")
	(consult-gh-issue-maxnum 50)
	(consult-gh-repo-maxnum 50)
	(consult-gh-show-preview t)
	(consult-gh-preview-key "C-o")
	;; (consult-gh-prioritize-local-folder 'suggest)
	(consult-gh-default-interactive-command #'consult-gh-transient)
	:bind
	(:map ctl-x-map
				("M-g" . consult-gh))
	:config
	((add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
	 (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
	 (consult-gh-enable-default-keybindings)))

(use-package consult-gh-embark
	:ensure t
	:after consult-gh
	:config
	(consult-gh-embark-mode 1))

(use-package consult-imenu
	:after consult
	:bind
	(([remap imenu] . consult-imenu))
	(:map goto-map
				("I" . consult-imenu-multi)))

(use-package consult-xref
	:after xref
	:functions
	(consult-xref)
	:init
	(setq xref-show-xrefs-function #'consult-xref)
	(setq xref-show-definitions-function #'consult-xref))

(use-package corfu
	:ensure t
	:custom
	(corfu-cycle t)
	(corfu-preview-current nil)
	(corfu-min-width 20)
	(corfu-scroll-margin 5)
	(corfu-quit-at-boundary nil)
	:hook
	((comint-mode-hook
		eshell-mode-hook
		prog-mode-hook
		text-mode-hook) . corfu-mode)
	(minibuffer-setup-hook . my/corfu-enable-in-minibuffer)
	:bind
	(:map corfu-map
				("SPC" . corfu-insert-separator)
				("TAB" . corfu-next)
				([tab] . corfu-next)
				("S-TAB" . corfu-previous)
				([backtab] . corfu-previous)
				("C-e" . corfu-complete))
	:config
	(defun my/corfu-enable-in-minibuffer ()
		"Enable Corfu in the minibuffer if `completion-at-point' is bound."
		(when (where-is-internal #'completion-at-point (list (current-local-map)))
			(corfu-mode 1))))

(use-package corfu-history
	:after corfu
	:hook (corfu-mode-hook . corfu-history-mode)
	:config
	(with-eval-after-load 'savehist
		(add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-popupinfo
	:after corfu
	:custom
	(corfu-popupinfo-delay '(1.5 . 0.5))
	(corfu-popupinfo-max-height 15)
	:hook (corfu-mode-hook . corfu-popupinfo-mode))

(use-package embark
	:ensure t
	:defer 2
	:custom
	(embark-indicators
	 '(embark-minimal-indicator
		 embark-highlight-indicator
		 embark-isearch-highlight-indicator))
	:bind
	((:map mode-specific-map
				 ("." . embark-act)
				 ("," . embark-dwim)
				 ("*" . embark-act-all))
	 (:map minibuffer-mode-map
				 ("C-<" . embark-become)
				 ("C-SPC" . embark-select))
	 (:map help-map
				 ("b" . embark-bindings))))
;; :init
;; (setopt prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
	:after (consult embark)
	:hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark-org
	:bind
	((:map embark-org-link-map
				 ("l" . org-insert-link))
	 (:map embark-org-src-block-map
				 ("e" . org-edit-special))))

(use-package indent
	:custom
	(tab-always-indent 'complete))

(use-package marginalia
	:ensure t
	:hook (after-init-hook . marginalia-mode))

(use-package minibuffer
	:custom
	(completion-cycle-threshold 3))

(use-package orderless
	:ensure t
	:custom
	(completion-styles '(orderless basic))
	(completion-category-defaults nil)
	(completion-category-overrides nil))

(use-package vertico
	:ensure t
	:custom
	(vertico-count 20)
	(vertico-resize nil)
	:hook (after-init-hook . vertico-mode))

(use-package vertico-directory
	:after vertico
	:bind
	(:map vertico-map
				("RET" . vertico-directory-enter)
				("DEL" . vertico-directory-delete-char)
				("M-DEL" . vertico-directory-delete-word))
	:hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
	:after vertico
	:custom
	(vertico-multiform-categories
	 '((embark-keybinding grid)
		 (jinx grid)))
	(vertico-multiform-commands
	 '((consult-buffer (:not posframe))
		 (consult-line (:not posframe))
		 (consult-ripgrep (:not posframe))
		 (t posframe)))
	:config
	(vertico-multiform-mode 1))

;; (consult-line
;;  posframe
;;  (vertico-posframe-poshandler . posframe-poshandler-frame-bottom-center)
;;  (vertico-posframe-fallback-mode . vertico-buffer-mode))

(use-package vertico-posframe
	:ensure t
	:custom
	(vertico-posframe-border-width 3)
	(vertico-posframe-min-width 80)
	(vertico-posframe-width nil)
	(vertico-posframe-parameters
	 '((left-fringe . 10)
		 (right-fringe . 10)))
	;; (vertico-posframe-poshandler 'posframe-poshandler-frame-center)
	:hook (vertico-mode-hook . vertico-posframe-mode))

;;; Editing

(use-package abbrev
	:custom
	(save-abbrevs nil)
	:hook
	((git-commit-mode-hook
		vc-git-log-edit-mode-hook
		markdown-mode-hook
		org-mode-hook) . abbrev-mode)
	:config
	(define-abbrev-table 'global-abbrev-table
		'(("fixme" "FIXME")
			("tbd" "TBD")
			("wip" "WIP")
			("teh" "the")
			("afaik" "As far as I know")
			("btw" "By the way")
			("imo" "In my opinion")
			("imho" "In my humble opinion"))))

(use-package avy
	:ensure t
	:custom
	(avy-dispatch-alist
	 '((?c . avy-action-copy)
		 (?l . avy-action-ispell)
		 (?m . avy-action-mark)
		 (?r . avy-action-teleport)
		 (?w . avy-action-kill-move)
		 (?W . avy-action-kill-stay)
		 (?y . avy-action-yank)
		 (?Y . avy-action-yank-line)
		 (?z . avy-action-zap-to-char)))
	(avy-keys '(?e ?i ?a ?o ?k ?t ?n ?s ?h))
	(avy-style 'pre)
	(avy-styles-alist '((avy-goto-char-timer . at-full)))
	(avy-all-windows t)
	(avy-single-candidate-jump nil)
	(avy-timeout-seconds 0.5)
	:bind
	(("M-j" . avy-goto-char-timer)
	 ("M-J" . avy-goto-char-in-line)
	 (:map goto-map
				 ("j" . avy-goto-char-timer)
				 ("J" . avy-goto-char-in-line)
				 ("l" . avy-goto-end-of-line)
				 ("w" . avy-goto-whitespace-end))
	 (:map isearch-mode-map
				 ("M-j" . avy-isearch)))
	:config
	(with-eval-after-load 'helpful
		;; https://karthinks.com/software/avy-can-do-anything/#look-up-the-documentation-for-a-symbol
		(defun my/avy-action-helpful (pt)
			(save-excursion
				(goto-char pt)
				(helpful-at-point))
			(select-window
			 (cdr (ring-ref avy-ring 0)))
			t)
		(setf (alist-get ?H avy-dispatch-alist) #'my/avy-action-helpful))

	(with-eval-after-load 'embark
		;; https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere
		(defun my/avy-action-embark (pt)
			(unwind-protect
					(save-excursion
						(goto-char pt)
						(embark-act))
				(select-window
				 (cdr (ring-ref avy-ring 0))))
			t)
		(setf (alist-get ?. avy-dispatch-alist) #'my/avy-action-embark)))

;; (use-package avy-migemo
;;   :ensure t)

(use-package deadgrep
	:ensure t
	:custom
	(deadgrep-display-buffer-function 'pop-to-buffer)
	(deadgrep-extra-arguments
	 '("--no-config"
		 "--hidden"
		 "--ignore-file=.gitignore"
		 "--iglob=!.git"
		 "--sort=modified"))
	:bind
	((:map search-map
				 ("d" . deadgrep))))

(use-package delsel
	:hook (after-init-hook . delete-selection-mode))

(use-package elec-pair
	:hook (after-init-hook . electric-pair-mode))

(use-package grugru
	:ensure t
	:bind
	(:map mode-specific-map
				("@ <right>" . grugru-forward)
				("@ <left>" . grugru-backward)
				("@ >" . grugru-forward)
				("@ <" . grugru-backward)
				("@ SPC" . grugru-select))
	(:repeat-map grugru-repeat-map
							 ("<right>" . grugru-forward)
							 ("<left>" . grugru-backward)
							 (">" . grugru-forward)
							 ("<" . grugru-backward)
							 ("SPC" . grugru-select))
	:config
	(grugru-default-setup)
	(grugru-define-global 'symbol '("yes" "no"))
	(grugru-define-global 'symbol '("true" "false"))
	(grugru-define-multiple
		((nix-mode rust-mode)
		 (non-alphabet "==" "!="))
		(nix-mode
		 (symbol "fetchurl" "fetchGit" "fetchTarball" "fetchClosure")
		 (symbol "mkShell" "mkShellNoCC"))
		(rust-mode
		 (non-alphabet "&&" "||")
		 (non-alphabet "+=" "-=")
		 (non-alphabet "*=" "/=" "%=")
		 (non-alphabet "&=" "|=" "^=")
		 (non-alphabet "<" "<=" ">" ">=")
		 (non-alphabet ">>=" "<<=")
		 (symbol "const" "let" "static"))))

(use-package hippie-exp
	:custom
	(hippie-expand-try-functions-list
	 '(try-complete-file-name-partially
		 try-complete-file-name
		 try-expand-dabbrev
		 try-expand-dabbrev-visible
		 try-expand-dabbrev-from-kill
		 try-expand-dabbrev-all-buffers))
	:bind
	([remap dabbrev-expand] . hippie-expand))

(use-package isearch
	:custom
	(isearch-allow-scroll t)
	(isearch-lazy-count t))

(use-package jinx
	:ensure t
	:hook
	((git-commit-mode-hook
		vc-git-log-edit-mode-hook
		markdown-mode-hook
		org-mode-hook) . jinx-mode)
	:bind
	(([remap ispell-word] . jinx-correct)
	 ("C-M-$" . jinx-correct-nearest)))

(use-package link-hint
	:ensure t
	:bind
	((:map mode-specific-map
				 ("l o" . link-hint-open-link)
				 ("l c" . link-hint-copy-link))))

(use-package markdown-mode
	:ensure t
	:custom
	(markdown-fontify-code-blocks-natively t)
	:mode
	(("\\.markdown\\'"
		"\\.md\\'"
		"\\.mdoc\\'"
		"\\.mdx\\'") . markdown-mode)
	("README\\.md\\'" . gfm-mode)
	:hook
	(markdown-mode-hook . dprint-on-save-mode)
	:bind
	(:map markdown-mode-map
				:map mode-specific-map
				("'" . markdown-edit-code-block)))

(use-package move-dup
	:ensure t
	:bind
	(("M-P" . move-dup-move-lines-up)
	 ("M-N" . move-dup-move-lines-down)
	 ("C-M-p" . move-dup-duplicate-up)
	 ("C-M-n" . move-dup-duplicate-down))
	:hook (after-init-hook . global-move-dup-mode))

(use-package pixel-scroll
	:if (>= emacs-major-version 29)
	:custom
	(pixel-scroll-precision-interpolate-page t)
	(pixel-scroll-precision-use-momentum t)
	(pixel-scroll-precision-momentum-seconds 0.5)
	(pixel-scroll-precision-initial-velocity-factor 0.000375)
	(pixel-scroll-precision-large-scroll-height 100)
	:hook (after-init-hook . pixel-scroll-precision-mode))

(use-package puni
	:ensure t
	:hook (after-init-hook . puni-global-mode)
	:bind
	((:map puni-mode-map
				 ([remap mark-sexp] . puni-mark-sexp-at-point)
				 ([remap transpose-sexps] . puni-transpose))
	 (:map mode-specific-map
				 ("SPC" . puni-expand-region)
				 ("<" . puni-wrap-angle)
				 ("{" . puni-wrap-curly)
				 ("^" . puni-splice))
	 (:repeat-map puni-region-repeat-map
								("SPC" . puni-expand-region)
								("<right>" . puni-expand-region)
								("<left>" . puni-contract-region)))
	:config
	(mapc (lambda (k) (keymap-unset puni-mode-map k))
				'("C-M-a" "C-M-e" "C-M-f" "C-M-b")))

(use-package replace
	:bind
	(:map mode-specific-map
				("o" . occur)))

(use-package savehist
	:hook (after-init-hook . savehist-mode))

(use-package saveplace
	:hook (after-init-hook . save-place-mode))

(use-package separedit
	:ensure t
	:custom
	(separedit-default-mode 'markdown-mode)
	:bind
	(:map mode-specific-map
				("'" . separedit)))

(use-package simple
	:custom
	(read-extended-command-predicate 'command-completion-default-include-p)
	(kill-whole-line t)
	(line-number-mode nil))

(use-package string-inflection
	:ensure t
	:bind
	((:map mode-specific-map
				 ("-" . string-inflection-all-cycle))
	 (:repeat-map string-inflection-repeat-map
								("-" . string-inflection-all-cycle))))

(use-package text-mode
	:custom
	(text-mode-ispell-word-completion nil))

(use-package tempel
	:ensure t
	:custom
	(tempel-path (list (expand-file-name "templates/*.eld" user-emacs-directory)))
	:bind
	(("M-+" . tempel-complete)
	 ("M-*" . tempel-insert)
	 (:map tempel-map
				 ("TAB" . tempel-next)
				 ([tab] . tempel-next)
				 ("S-TAB" . tempel-previous)
				 ([backtab] . tempel-previous)
				 ("M-RET". tempel-done))))

(use-package visual-replace
	:ensure t
	:custom
	(visual-replace-default-to-full-scope t)
	(visual-replace-keep-initial-position t)
	:hook
	(after-init-hook . visual-replace-global-mode)
	:bind
	([remap query-replace] . visual-replace)
	(:map mode-specific-map
				("r" . visual-replace))
	:config
	(with-eval-after-load 'transient
		(transient-define-prefix my/visual-replace-mode-transient ()
			"Prefix for `visual-replace-mode-map'."
			["Basic operation"
			 ("a" "apply one repeat" visual-replace-apply-one-repeat)
			 ("A" "apply one" visual-replace-apply-one)
			 ("s" "substring match" visual-replace-substring-match)
			 ("u" "undo" visual-replace-undo)
			 ("y" "yank" visual-replace-yank)
			 ("M-y" "yank-pop" visual-replace-yank-pop)]
			["Toggle replace mode"
			 ("c" "toggle case fold" visual-replace-toggle-case-fold)
			 ("e" "toggle regexp" visual-replace-toggle-regexp)
			 ("q" "toggle query" visual-replace-toggle-query)
			 ("w" "toggle word" visual-replace-toggle-word)]
			["Change scope"
			 ("f" "switch to full scope" visual-replace-switch-to-full-scope)
			 ("p" "switch to from-point scope" visual-replace-switch-to-from-point-scope)
			 ("r" "switch to region scope" visual-replace-switch-to-region-scope)])
		(keymap-set visual-replace-mode-map "?" #'my/visual-replace-mode-transient)))

(use-package vundo
	:ensure t
	:custom
	(vundo-glyph-alist vundo-unicode-symbols)
	(vundo-popup-timeout 3.0)
	(vundo-window-max-height 15)
	:bind
	(("C-z" . vundo)
	 (:map vundo-mode-map
				 ("C-e" . vundo-confirm)))
	:hook (after-init-hook . vundo-popup-mode))

;;; Visual aid

(use-package colorful-mode
	:ensure t
	:custom
	(colorful-use-prefix t)
	(colorful-only-strings 'only-prog)
	(css-fontify-colors nil)
	:config
	(global-colorful-mode 1)
	(add-to-list 'global-colorful-modes 'helpful-mode))

(use-package display-fill-column-indicator
	:hook
	((prog-mode-hook
		text-mode-hook) . display-fill-column-indicator-mode))

(use-package display-line-numbers
	:custom
	(display-line-numbers-type t)
	(display-line-numbers-grow-only t)
	(display-line-numbers-width-start t)
	:hook
	(display-line-numbers-mode-hook . header-line-indent-mode)
	:init
	(global-display-line-numbers-mode 1)
	:config
	(defun my/display-line-numbers-fixed-width ()
		(when (< display-line-numbers-width 5)
			(setq display-line-numbers-width 5)))
	(add-hook 'display-line-numbers-mode-hook #'my/display-line-numbers-fixed-width)
	(dolist (mode '(dashboard-mode-hook
									dired-mode-hook
									eat-mode-hook
									eshell-mode-hook
									;; org-mode-hook
									shell-mode-hook
									term-mode-hook
									vterm-mode-hook))
		(add-hook mode (lambda () (display-line-numbers-mode 0)))))

(use-package goggles
	:ensure t
	:custom
	(goggles-pulse-delay 0.05)
	(goggles-pulse-iterations 15)
	:hook
	((prog-mode-hook
		text-mode-hook) . goggles-mode))

(use-package hl-line
	:custom
	(hl-line-sticky-flag nil)
	(global-hl-line-sticky-flag nil)
	:hook
	((prog-mode-hook
		text-mode-hook
		dired-mode-hook) . hl-line-mode))

(use-package hl-todo
	:ensure t
	:custom
	(hl-todo-highlight-punctuation ":")
	:hook
	((org-mode-hook
		prog-mode-hook) . hl-todo-mode)
	:bind
	((:map hl-todo-mode-map
				 :map mode-specific-map
				 ("t <right>" . hl-todo-next)
				 ("t <left>" . hl-todo-previous)
				 ("t >" . hl-todo-next)
				 ("t <" . hl-todo-previous)
				 ("t o" . hl-todo-occur))
	 (:repeat-map hl-todo-repeat-map
								("<right>" . hl-todo-next)
								("<left>" . hl-todo-previous)
								(">" . hl-todo-next)
								("<" . hl-todo-previous))))

(use-package paren
	:custom
	(show-paren-context-when-offscreen t)
	:hook (after-init-hook . show-paren-mode))

(use-package rainbow-delimiters
	:ensure t
	:hook
	((clojure-mode-hook
		emacs-lisp-mode-hook
		haskell-mode-hook
		lisp-mode-hook) . rainbow-delimiters-mode))

(use-package valign
	:ensure t
	:custom
	(valign-fancy-bar t)
	:hook ((markdown-mode-hook org-mode-hook) . valign-mode))

(use-package visual-fill-column
	:ensure t
	:custom
	(visual-fill-column-center-text t)
	(visual-fill-column-extra-text-width '(5 . 0))
	(visual-fill-column-width 100))

(use-package whitespace
	:custom
	(whitespace-line-column fill-column)
	(whitespace-space-regexp "\\(\u3000+\\)")
	(whitespace-style '(face
											trailing
											tabs
											spaces
											empty
											missing-newline-at-eof))
	:config
	(global-whitespace-mode t))

(use-package whitespace-cleanup-mode
	:commands whitespace-cleanup-mode
	:config
	(global-whitespace-cleanup-mode 1))

;;; File & directory management

(setq create-lockfiles nil)

(use-package activities
	:ensure t
	:custom
	(activities-bookmark-warnings t)
	:bind
	((:map ctl-x-map
				 :prefix-map activities-map
				 :prefix "C-a"
				 ("l" . activities-list)
				 ("g" . activities-revert)
				 ("RET" . activities-switch)
				 ("C-d" . activities-define)
				 ("C-k" . activities-kill)
				 ("C-n" . activities-new)
				 ("C-a" . activities-resume)
				 ("C-s" . activities-suspend)))
	:hook (after-init-hook . activities-tabs-mode)
	:config
	(with-eval-after-load 'consult
		(defun my/activities-local-buffer-p (buffer)
			"Returns non-nil if BUFFER is present in `activities-current'."
			(when (activities-current)
				(memq buffer (activities-tabs--tab-parameter 'activities-buffer-list (activities-tabs--tab (activities-current))))))
		(defvar my-consult--source-activities-buffer
			`(:name "Activities Buffers"
							:narrow ?a
							:category buffer
							:face consult-buffer
							:history buffer-name-history
							:default t
							:items ,(lambda () (consult--buffer-query
																	:predicate #'my/activities-local-buffer-p
																	:sort 'visibility
																	:as #'buffer-name))
							:state ,#'consult--buffer-state))
		(add-to-list 'consult-buffer-sources 'my-consult--source-activities-buffer)))

(use-package dashboard
	:ensure t
	:custom
	(dashboard-startup-banner (concat (emacs-pictures-dir) "dashboard-banner.png"))
	(dashboard-banner-logo-title "Welcome to Ametsuchi.")
	;; (dashboard-init-info nil)
	(dashboard-center-content t)
	(dashboard-vertically-center-content t)
	(dashboard-page-separator "\n\f\f\n")
	;; (dashboard-set-navigator t)
	(dashboard-set-footer nil)
	(dashboard-set-file-icons t)
	(dashboard-set-heading-icons t)
	(dashboard-show-shortcuts t)
	(dashboard-projects-backend 'project-el)
	(dashboard-items
	 '((agenda . 10)
		 (projects . 5)
		 (bookmarks . 10)
		 (recents . 15)))
	(dashboard-item-shortcuts
	 '((agenda . "a")
		 (bookmarks . "s")
		 (projects . "j")
		 (recents . "r")
		 (registers . "e")))
	:bind
	(("<f5>" . my/home)
	 (:map dashboard-mode-map
				 ("?" . my/dashboard-mode-transient)))
	:hook
	;; (window-setup-hook . dashboard-open)
	(after-init-hook . dashboard-refresh-buffer)
	(server-after-make-frame-hook . dashboard-refresh-buffer)
	(dashboard-mode-hook . (lambda ()
													 (setq-local frame-title-format nil)))
	:config
	(defun my/home ()
		(interactive)
		(delete-other-windows)
		(dashboard-refresh-buffer))
	(mapc (lambda (k) (keymap-unset dashboard-mode-map k))
				'("j" "k" "{" "}"))
	(with-eval-after-load 'transient
		(transient-define-prefix my/dashboard-mode-transient ()
			"Prefix for dashboard launch menu."
			[("m" "Mastodon" mastodon)
			 ("o" "OpenStreetMap" osm)
			 ;; ("g" "GitHub CLI" consult-gh)
			 ;; ("c" "Calibre" calibredb)
			 ("?" "Gptel" gptel-menu)
			 ("e" "Mu4e" mu4e)]))
	(dashboard-setup-startup-hook))

(use-package dired
	:commands dired
	:custom
	(dired-recursive-copies 'always)
	(dired-recursive-deletes 'always)
	(delete-by-moving-to-trash t)
	(dired-dwim-target t)
	(dired-listing-switches "-alh")
	:hook
	(dired-mode-hook . dired-hide-details-mode))

(use-package dired-collapse
	:ensure t
	:after dired
	:hook
	(dired-mode-hook . dired-collapse-mode))

(use-package dired-filter
	:ensure t
	:after dired
	:custom
	(dired-filter-group-saved-groups
	 '(("default"
			("General directories"
			 (directory . t)
			 (name . "^[[:alnum:]]"))
			("Dot directories"
			 (directory . t)
			 (name . "^\\."))
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
			("Executables" (executable))
			("Images"
			 (extension "avif" "bmp" "ico" "jpeg" "jpg" "gif"
									"png" "raw" "svg" "tiff" "webp" "xcf")))))
	:hook
	((dired-mode-hook . dired-filter-mode)
	 (dired-mode-hook . dired-filter-group-mode)))

(use-package dired-hacks-utils
	:ensure t
	:after dired
	:bind
	(:map dired-mode-map
				("n" . dired-hacks-next-file)
				("p" . dired-hacks-previous-file)))

(use-package dired-open-with
	:ensure t
	:after dired
	:bind
	((:map dired-mode-map
				 ("M-RET" . dired-open-with))))

(use-package dired-ranger
	:ensure t
	:after dired
	:bind
	((:map dired-mode-map
				 :prefix-map dired-ranger-map
				 :prefix "r"
				 ("c" . dired-ranger-copy)
				 ("x" . dired-ranger-move)
				 ("y" . dired-ranger-paste))))

(use-package dired-subtree
	:ensure t
	:after dired
	:custom
	(dired-subtree-use-backgrounds nil)
	:bind
	(:map dired-mode-map
				("i" . dired-subtree-insert)
				("SPC" . dired-subtree-toggle)
				("b" . dired-subtree-remove)))

(use-package envrc
	:ensure t
	:if (executable-find "direnv")
	:hook (after-init-hook . envrc-global-mode))

(use-package files
	:custom
	(auto-mode-case-fold nil)
	(backup-by-copying t)
	(backup-directory-alist
	 `(("." . ,(locate-user-emacs-file "backup/"))))
	(delete-old-versions t)
	(require-final-newline t)
	(version-control t)
	(view-read-only t)
	:config
	(with-eval-after-load 'embark
		(defun my/find-file-vertically (file)
			"Open FILE in a new vertically split window."
			(select-window (split-window-right))
			(find-file file))
		(keymap-set embark-file-map "M-RET" #'my/find-file-vertically)))

(use-package project
	:config
	(defun my/project-try-nix-store (dir)
		(save-match-data
			(when (string-match (rx bol "/nix/store/" (+ (not "/")) "/")
													dir)
				(list 'nix-store (match-string 0 dir)))))
	(add-hook 'project-find-functions #'my/project-try-nix-store)
	(cl-defmethod project-root ((project (head nix-store)))
		(cadr project)))

(use-package recentf
	:custom
	(recentf-max-saved-items 300)
	(recentf-auto-cleanup 'never)
	(recentf-exclude
	 '("/tmp/"
		 "/nix/store/"))
	:hook
	(after-init-hook . recentf-mode))

(use-package startup
	:custom
	(inhibit-default-init t)
	(inhibit-startup-echo-area-message t)
	(inhibit-startup-screen t)
	(initial-buffer-choice
	 (lambda () (get-buffer-create "*dashboard*")))
	(initial-scratch-message nil)
	(initial-major-mode 'fundamental-mode))

;;; Development

(use-package aggressive-indent
	:ensure t
	:hook (emacs-lisp-mode-hook . aggressive-indent-mode))

(use-package compile)

(use-package dumb-jump
	:ensure t
	:custom
	(dumb-jump-selector 'completing-read)
	:hook
	(xref-backend-functions . dumb-jump-xref-activate))

(use-package eglot
	:defer 3
	:custom
	(eglot-autoshutdown t)
	(eglot-code-action-indications nil)
	(eglot-confirm-server-edits nil)
	(eglot-extend-to-xref t)
	:hook
	(eglot-managed-mode-hook . my/setup-eglot-buffer)
	:bind
	(:map eglot-mode-map
				:map mode-specific-map
				("L a" . eglot-code-actions)
				("L i" . eglot-code-actions-inline)
				("L o" . eglot-code-actions-organize-imports)
				("L q" . eglot-code-actions-quickfix)
				("L R" . eglot-reconnect)
				("L r" . eglot-rename)
				("L Q" . eglot-shutdown))
	:config
	(defun my/setup-eglot-buffer ()
		(if (eglot-managed-p)
				(add-hook 'before-save-hook #'eglot-format-buffer nil t)
			(remove-hook 'before-save-hook #'eglot-format-buffer t)))

	(dolist (entry '((just-mode . ("just-lsp"))
									 ((nix-ts-mode nix-mode) . ("nil"))
									 (zig-ts-mode . ("zls"))))
    (cl-pushnew entry eglot-server-programs :test #'equal)))

(use-package eglot-booster
	:ensure t
	:after eglot
	:hook (eglot-managed-mode-hook . eglot-booster-mode))

(use-package eglot-tempel
	:ensure t
	:after (eglot tempel)
	:hook (eglot-managed-mode-hook . eglot-tempel-mode))

(use-package eldoc
	:custom
	(eldoc-echo-area-use-multiline-p nil)
	:hook (after-init-hook . global-eldoc-mode))

(use-package elysium
	:ensure t
	:after gptel
	:config
	(with-eval-after-load 'gptel-transient
		(transient-append-suffix 'gptel-menu '(-1 -1)
			["Elysium"
			 ("q" "Elysium query" elysium-query)])))

(use-package flymake
	:bind
	((:map flymake-mode-map
				 :map goto-map
				 ("M-n" . flymake-goto-next-error)
				 ("M-p" . flymake-goto-prev-error))
	 (:repeat-map flymake-mode-repeat-map
								("n" . flymake-goto-next-error)
								("p" . flymake-goto-prev-error))))

(use-package reformatter
	:ensure t
	:config
	(reformatter-define dprint
		:program "dprint"
		:args (list "fmt" "--stdin" (buffer-file-name)))
	(reformatter-define stylua
		:program "stylua"
		:args (list "-" "--indent-type=Spaces" "--indent-width=2"))
	(reformatter-define nixfmt
		:program "nixfmt"
		:args (list "-"))
	(reformatter-define yamlfmt
		:program "yamlfmt"
		:args (list "-")))

(use-package repl-toggle
	:ensure t
	:custom
	(rtog/goto-buffer-fun #'pop-to-buffer))

(use-package treesit
	:custom
	(treesit-font-lock-level 4))

;;;; Language-specific modes

(use-package astro-ts-mode
	:ensure t
	:mode "\\.astro\\'"
	:hook (astro-ts-mode-hook . eglot-ensure))

(use-package dockerfile-ts-mode
	:mode
	"\\.dockerfile\\'"
	"[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'"
	:hook
	(dockerfile-ts-mode-hook . eglot-ensure)
	(dockerfile-ts-mode-hook . dprint-on-save-mode))

(use-package just-mode
	:ensure t
	:mode "/justfile\\'")

(use-package lua-ts-mode
	:mode "\\.lua\\'"
	:hook
	(lua-ts-mode-hook . eglot-ensure)
	(lua-ts-mode-hook . stylua-on-save-mode))

(use-package nix-ts-mode
	:ensure t
	:mode "\\.nix\\'"
	:hook
	(nix-ts-mode-hook . eglot-ensure)
	(nix-ts-mode-hook . nixfmt-on-save-mode))

(use-package nix-mode
	:ensure t
	:commands nix-repl)

(use-package rust-ts-mode
	:ensure t
	:mode "\\.rs\\'"
	:hook
	(rust-ts-mode-hook . eglot-ensure))

(use-package toml-ts-mode
	:mode "\\.toml\\'"
	:hook
	(toml-ts-mode-hook . eglot-ensure)
	(toml-ts-mode-hook . dprint-on-save-mode))

(use-package typescript-ts-mode
	:mode "\\.ts$" "\\.mts\\'"
	:hook (typescript-ts-mode-hook . eglot-ensure))

(use-package web-mode
	:ensure t
	:custom
	(web-mode-enable-front-matter-block t)
	(web-mode-enable-current-element-highlight t)
	:mode
	"\\.html?\\'"
	"\\.mdx\\'"
	:hook
	(web-mode-hook . eglot-ensure))

(use-package yaml-ts-mode
	:mode "\\.ya?ml\\'"
	:hook
	(yaml-ts-mode-hook . eglot-ensure)
	(yaml-ts-mode-hook . yamlfmt-on-save-mode))

(use-package zig-ts-mode
	:ensure t
	:mode "\\(?:\\.z\\(?:ig\\|on\\)\\)\\'"
	:hook
	(zig-ts-mode-hook . eglot-ensure))

;;; Version control system

(use-package diff-hl
	:ensure t
	:custom
	(diff-hl-command-prefix (kbd "C-x v"))
	(diff-hl-draw-borders t)
	:hook
	(after-init-hook . global-diff-hl-mode)
	(dired-mode-hook . diff-hl-dired-mode)
	:config
	(with-eval-after-load 'magit
		(add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
		(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package forge
  :ensure t
	:after magit)

(use-package magit
	:ensure t
	:custom
	(magit-save-repository-buffers 'dontask)
	(magit-define-global-key-bindings nil)
	:bind
	((:map ctl-x-map
				 :prefix-map magit-prefix-map
				 :prefix "g"
				 ("s" . magit-status)
				 ("d" . magit-dispatch)
				 ("f" . magit-file-dispatch))))

(use-package magit-delta
	:ensure t
	:if (executable-find "delta")
	:after magit
	:hook (magit-mode . magit-delta-mode))

(use-package magit-todos
	:ensure t
	:after magit
	:custom
	(magit-todos-exclude-globs '(".git/"))
	:hook (magit-mode-hook . magit-todos-mode))

;;; Email

(use-package mu4e
	:ensure t)

;;; Terminal

(use-package eat
	:ensure t
	:custom
	;; (eat-enable-shell-prompt-annotation nil)
	(eat-kill-buffer-on-exit t)
	:bind
	((:map project-prefix-map
				 ("t" . eat-preject))))

(use-package exec-path-from-shell
	:ensure t
	:if (eq system-type 'darwin)
	:config
	(when (or window-system
						(daemonp))
		(exec-path-from-shell-initialize)))

;;; Org

(use-package org
	:defer 5
	:custom
	;; Org files management
	(org-default-notes-file (concat (emacs-documents-dir) "inbox.org"))
	;; Agenda-related stuff
	(org-enforce-todo-dependencies t)
	(org-extend-today-until 4)
	(org-use-effective-time t)
	(org-log-done 'time)
	(org-tags-exclude-from-inheritance '("crypt"))
	(org-track-ordered-property-with-tag t)
	;; General workflow
	(org-cycle-separator-lines 0)
	(org-imenu-depth 6)
	(org-use-speed-commands t)
	(org-special-ctrl-a/e t)
	(org-special-ctrl-k t)
	;; Appearance
	(org-ellipsis "  ")
	(org-fontify-done-headline t)
	(org-fontify-quote-and-verse-blocks t)
	(org-fontify-whole-heading-line t)
	(org-image-actual-width t)
	(org-pretty-entities t)
	(org-startup-folded 'content)
	(org-startup-indented nil)
	(org-startup-truncated nil)
	(org-tags-column -80)
	;; Misc
	(org-bookmark-names-plist nil)
	:hook (org-mode-hook . my/org-mode-setup)
	:config
	(defun my/org-mode-setup ()
		(setq-local tab-width 8)))

(use-package dslide
	:ensure t
	:after org
	:custom
	(dslide-header nil)
	(dslide-header-author nil)
	(dslide-header-date nil)
	(dslide-header-email nil)
	(dslide-breadcrumb-separator nil)
	:hook
	(dslide-start-hook . hide-mode-line-mode))

(use-package org-agenda
	:after org
	:custom
	;; (org-agenda-compact-blocks t)
	;; (org-agenda-custom-commands
	;;  '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))))
	;; (org-agenda-dim-blocked-tasks nil)
	(org-agenda-files (lambda ()
											(directory-files-recursively
											 emacs-documents-dir "\\.org\\'")))
	(org-agenda-inhibit-startup t)
	(org-agenda-restore-windows-after-quit t)
	;; (org-agenda-use-tag-inheritance '(todo search agenda))
	(org-agenda-window-setup 'current-window)
	:bind
	(:map mode-specific-map
				("a" . org-agenda))
	:hook (org-agenda-finalize-hook . hl-line-mode))

(use-package org-super-agenda
	:ensure t
	:after org-agenda
	:custom
	(org-super-agenda-groups
	 '((:name "Timeline"
						:time-grid t)))
	:config
	(org-super-agenda-mode 1))

(use-package org-anki
	:ensure t
	:after org
	:custom
	(org-anki-default-deck "Default")
	(org-anki-default-note-type "Basic")
	(org-anki-inherit-tags nil)
	:bind
	(:map org-mode-map
				:prefix-map org-anki-map
				:prefix "C-c C-x k"
				("b" . org-anki-browse-entry)
				("s" . org-anki-sync-entry)
				("S" . org-anki-sync-all)
				("u" . org-anki-update-all)
				("U" . org-anki-update-dir)))

(use-package org-appear
	:ensure t
	:hook (org-mode-hook . org-appear-mode))

(use-package org-autolist
	:ensure t
	:hook (org-mode-hook . org-autolist-mode))

(use-package ob
	:after org
	:custom
	(org-confirm-babel-evaluate nil)
	(org-babel-load-languages
	 '((clojure . t)
		 (emacs-lisp . t)
		 (haskell . t)
		 (julia . t)
		 (latex . t)
		 (lua . t)
		 (python . t)
		 (ocaml . t)
		 (shell . t)
		 (sql . t)
		 (sqlite . t))))

(use-package org-bookmark-heading
	:ensure t
	:after org
	:custom
	(org-bookmark-heading-make-ids t))

(use-package org-capture
	:after org
	:custom
	(org-capture-templates
	 `(("i" "Inbox" entry
			(file "")
			"* %?\n%U\n\n  %i")))
	:bind
	(:map mode-specific-map
				("c" . org-capture)))

(use-package org-clock
	:after org
	:custom
	(org-clock-auto-clockout-timer (* 60 20))
	(org-clock-history-length 30)
	(org-clock-in-resume t)
	(org-clock-mode-line-total 'today)
	(org-clock-out-remove-zero-time-clocks t)
	(org-clock-persist t)
	(org-clock-persist-query-resume t)
	(org-clock-persist-query-save t))

(use-package org-clock-convenience
	:ensure t
	:bind
	(:map org-agenda-mode-map
				("M-]" . org-clock-convenience-timestamp-up)
				("M-[" . org-clock-convenience-timestamp-down)))

(use-package org-crypt
	:custom
	(org-crypt-key nil)
	:hook (org-mode-hook . my/org-encrypt-entries)
	:config
	(defun my/org-encrypt-entries ()
		(add-hook 'before-save-hook #'org-encrypt-entries nil t)))

(use-package org-download
	:ensure t
	:after org
	:custom
	(org-download-image-dir emacs-pictures-dir)
	;; (org-download-edit-cmd "swappy %s")
	;; (org-download-screenshot-method "grimblast copysave")
	(org-download-timestamp "%Y%m%d_%H%M%S_")
	:hook (dired-mode-hook . org-download-enable))

(use-package org-element
	:after org
	:init
	(setq org-element-cache-persistent nil))

(use-package org-habit
	:after org-agenda
	:custom
	(org-habit-graph-column 35)
	(org-habit-following-days 7)
	(org-habit-preceding-days 14)
	(org-habit-show-done-always-green t))

(use-package org-id
	:after org
	:custom
	(org-id-link-to-org-use-id t)
	(org-id-search-archives nil))

(use-package org-journal
	:ensure t
	:after org
	:custom
	(org-journal-dir (concat (emacs-documents-dir) "journal/"))
	(org-journal-prefix-key nil)
	(org-journal-date-format "%a, %d %b")
	(org-journal-time-format "")
	(org-journal-time-prefix "")
	(org-journal-file-format "%Y.org")
	(org-journal-enable-agenda-integration t)
	;; (org-journal-enable-encryption t)
	(org-journal-file-type 'yearly)
	(org-journal-file-header "#+TITLE: %Y Journal\n\n")
	:bind
	((:map mode-specific-map
				 ("j" . org-journal-new-entry))
	 (:map org-journal-mode-map
				 :map mode-specific-map
				 ("n" . org-journal-next-entry)
				 ("p" . org-journal-previous-entry)
				 ("j" . org-journal-new-entry)
				 ("s" . org-journal-search))))

(use-package ol
	:custom
	(org-link-keep-stored-after-insertion t)
	:bind
	(:map mode-specific-map
				("l" . org-store-link)))

(use-package org-make-toc
	:ensure t
	:custom
	(org-make-toc-insert-custom-ids t)
	:hook (org-mode-hook . org-make-toc-mode))

(use-package org-modern
	:ensure t
	:after org
	:custom
	(org-modern-hide-stars t)
	:hook
	(org-mode-hook . org-modern-mode)
	(org-agenda-finalize . org-modern-agenda))

(use-package org-nix-shell
	:ensure t
	:hook (org-mode-hook . org-nix-shell-mode))

(use-package org-pomodoro
	:ensure t
	:after org-agenda
	:commands (org-pomodoro)
	:custom
	(org-pomodoro-length 25)
	(org-pomodoro-short-break-length 5)
	(org-pomodoro-long-break-length 20))

(use-package org-ql
	:ensure t
	:after org
	:bind
	(:map org-mode-map
				("C-c C-x /" . org-ql-sparse-tree)))

(use-package org-recur
	:ensure t
	:hook
	((org-mode-hook . org-recur-mode)
	 (org-agenda-mode-hook . org-recur-agenda-mode)))

(use-package org-refile
	:after org
	:custom
	(org-log-refile nil)
	(org-outline-path-complete-in-steps nil)
	(org-refile-allow-creating-parent-nodes 'confirm)
	(org-refile-targets
	 '((nil :maxlevel . 99)
		 (org-agenda-files :maxlevel . 99)))
	(org-refile-use-outline-path 'file))

(use-package org-src
	:custom
	(org-src-tab-acts-natively t)
	(org-src-window-setup 'split-window-below))

(use-package org-super-links
	:ensure t
	:bind
	((:map org-mode-map
				 :prefix-map org-super-links-map
				 :prefix "C-c C-x s"
				 ;; ("d" . org-super-links-delete-link)
				 ("l" . org-super-links-insert-link)
				 ("s" . org-super-links-store-link))))

(use-package org-transclusion
	:ensure t
	:after org
	;; :custom
	;; (org-transclusion-add-all-on-activate t)
	:hook (org-mode-hook . org-transclusion-mode))

(use-package org-web-tools
	:ensure t
	:after org
	:bind
	((:map org-mode-map
				 :prefix-map org-web-tools-map
				 :prefix "C-c C-x w"
				 ("y" . org-web-tools-insert-link-for-url)
				 ("Y" . org-web-tools-insert-web-page-as-entry))))

;;; Miscellaneous

(use-package calc
	:custom
	(calc-kill-line-numbering nil))

(use-package calendar
	:custom
	(calendar-mark-holidays-flag t)
	(calendar-date-style 'european))

(use-package calibredb
	:ensure t
	:custom
	(calibredb-root-dir (concat (emacs-documents-dir) "calibre/"))
	(calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
	;; (calibredb-device-dir "/TODO")
	(calibredb-download-dir nil)
	(calibredb-format-nerd-icons t)
	(calibredb-library-alist
	 '((calibredb-root-dir (name . "Calibre"))))
	(calibredb-virtual-library-default-name "Library")
	(calibredb-virtual-library-alist nil))

(use-package epkg
	:custom
	(epkg-repository (concat (emacs-data-home) "epkgs/")))

(use-package epkg-marginalia
	:ensure t
	:after (epkg marginalia)
	:config
	(cl-pushnew 'epkg-marginalia-annotate-package
							(alist-get 'package marginalia-annotator-registry)))

(use-package go-translate
	:ensure t
	:custom
	(gt-langs '(en es ja))
	(gt-polyglot-p t)
	(gt-default-translator
	 (gt-translator
		:taker (list
						(gt-taker :pick nil :if 'selection)
						;; (gt-taker :text 'buffer :pick 'paragraph)
						(gt-taker :text 'word))
		:engines (gt-google-engine)
		;; :engines (list
		;; 					(gt-google-engine :if 'word)
		;; 					(gt-deepl-engine :if 'not-word))
		:render (gt-buffer-render)))
	:bind
	((:map mode-specific-map
				 ("q" . gt-do-translate))))

(use-package gptel
	:ensure t
	:custom
	(gptel-default-mode 'org-mode)
	(gptel-use-header-line t)
	(gptel-log-level 'info)
	:bind
	(:map mode-specific-map
				("\\" . gptel-menu))
	:config
	(setq gptel-model 'gemma3:4b
				gptel-backend (gptel-make-ollama "Ollama"
												:host "localhost:11434"
												:stream t
												:models '(deepseek-r1:8b
																	gemma3:4b
																	llama3.2:3b
																	mistral:7b
																	phi4:14b
																	qwen3:4b))))

(use-package gptel-org
	:after gptel
	:bind
	(:map org-mode-map
				("C-c C-x `'" . gptel-org-set-topic)
				("C-c C-x =" . gptel-org-set-properties)))

(use-package gptel-quick
	:ensure t
	:after (gptel)
	:config
	(with-eval-after-load 'embark
		(keymap-set embark-general-map "?" #'gptel-quick)))

(use-package gptel-rewrite
	:after gptel
	:custom
	(‎gptel-rewrite-default-action‎ 'dispatch)
	:config
	(with-eval-after-load 'embark
		(keymap-set embark-region-map "!" #'gptel-rewrite)))

(use-package mastodon
	:ensure t)

(use-package nov
	:ensure t
	:mode ("\\.epub\\'" . nov-mode)
	:custom
	(nov-text-width 82)
	;; File name where last reading places are saved to and restored from.
	(nov-save-place-file (concat (emacs-cache-home) "nov-places"))
	:hook (nov-mode-hook . my/nov-mode-setup)
	:config
	;; https://github.com/akirak/emacs-config?tab=readme-ov-file#nov
	(defun my/nov-mode-setup ()
		(when (require 'olivetti nil t)
			(olivetti-mode 1)
			(setq-local nov-text-width nil)
			(add-hook 'olivetti-expand-hook
								#'nov-render-document
								nil t))))

(use-package osm
	:ensure t
	:custom
	(osm-search-language "en,ja")
	(osm-tile-directory emacs-cache-home)
	:bind
	(("<f6>" . osm)
	 (:map osm-mode-map
				 ([remap previous-line] . osm-up)
				 ([remap next-line] . osm-down)
				 ([remap backward-char] . osm-left)
				 ([remap forward-char] . osm-right)
				 ("g" . osm-goto)
				 ("h" . osm-home)
				 ("j" . osm-jump)
				 ("s" . osm-search)
				 ("v" . osm-server)
				 ("x" . osm-gpx-show)
				 ("X" . osm-gpx-hide))))

(use-package pdf-tools
	:ensure t
	:magic ("%PDF" . pdf-view-mode))

(use-package prodigy
  :ensure t)

;; (use-package trashed
;;   :ensure t
;;   :commands (trashed)
;;   :config
;;   (setq trashed-action-confirmer 'y-or-n-p)
;;   (setq trashed-use-header-line t)
;;   (setq trashed-sort-key '("Date deleted" . t))
;;   (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))
