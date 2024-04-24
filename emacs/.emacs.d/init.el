;; Config file

;;;
;;; TODO: use tree-sitter when emacs 29+ start being available on macOS

;; Setup MELPA
(require 'package)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(add-to-list 'load-path (expand-file-name "settings" (file-name-directory load-file-name)))
(add-to-list 'exec-path "~/.local/bin")

;;;
;;; Custom options
;;;

(toggle-frame-maximized)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(show-paren-mode 1)
(electric-pair-mode t)

(global-hl-line-mode)
(set-face-attribute hl-line-face nil :underline nil)

(setq-default c-basic-offset 4
	          tab-width 4
	          indent-tabs-mode nil)

;(set-frame-parameter (selected-frame) 'alpha-background 80)
;(add-to-list 'default-frame-alist '(alpha-background . 80))
;(set-frame-parameter (selected-frame) 'alpha 99)
;(add-to-list 'default-frame-alist 'alpha 99)
;;;
;;; Custom keybindings
;;;

(global-set-key (kbd "M-s") 'replace-string)
(global-set-key (kbd "C-c r") 'revert-buffer)


;;;
;;; Platform-specific
;;;
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;;;
;;; Basic package options ----------------------
;;;

(use-package delight
  :ensure t)

(use-package windmove
  :ensure t
  :bind (("M-<left>" . windmove-left)
	 ("M-<right>" . windmove-right)
	 ("M-<up>" . windmove-up)
	 ("M-<down>" . windmove-down)))
	  
(use-package move-text
  :ensure t
  :bind (("M-S-<up>" . move-text-up)
         ("M-S-<down>" . move-text-down)))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind (("M-s-<down>" . mc/mark-next-like-this)
         ("M-s-<up>" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("M-s-c M-s-c" . mc/edit-lines)))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package forge
  :ensure t
  :requires magit
  :after magit)

(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  :bind (("C-c C-r" . recentf-open-files)))

(use-package emmet-mode
  :ensure t
  :hook (html-mode web-mode))

(use-package which-key
  :ensure t)

(use-package counsel
  :ensure t
  :delight
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  :bind
  (("M-x" . counsel-M-x)))

(use-package smex
  :ensure t
  :after counsel
  :init
  (smex-initialize))

(use-package auto-fill-mode
  :hook ((markdown-mode LaTeX-mode ConTeXt-mode) . auto-fill-mode))


(use-package projectile
  :ensure t
  :delight '(:eval (concat " proj[" (projectile-project-name) "]"))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-enable-caching t)
  (projectile-global-mode))

(use-package yasnippet
  :defer t
  :ensure t
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package yas-minor-mode
  :after yasnippet
  :hook (rust-mode c++-mode python-mode typescript-mode org-mode org))

(use-package eldoc
  :ensure t
  :delight " doc")

(use-package editorconfig
  :ensure t
  :delight " EC"
  :config
  (editorconfig-mode 1))

;;; TODO: keybinding for ag-kill-buffers
(use-package ag
  :ensure t)

(use-package paredit
  :hook ((scheme-mode emacs-lisp-mode) . paredit-mode)
  :ensure t)

;;;
;;; Completions, language servers, et al --------------
;;;

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;;(eldoc-mode +1)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (tide-hl-identifier-mode +1))

(use-package flycheck
  ; only for tide
  :ensure t
  :hook (typescript-mode))

(use-package tide
  ;; use tide because eglot does not work well with typescript-language-server
  ;; TODO: this does not seem to work?
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide-mode)
	 (web-mode . (lambda ()
		       (when (string-equal "tsx" (file-name-extension buffer-file-name))
			 (setup-tide-mode))))
	 (before-save . tide-format-before-save)))

(use-package company
  :ensure t
  :delight
  :hook ((c++-mode rust-mode python-mode typescript-mode js2-mode) . company-mode))

(use-package flymake
  :ensure t
  :hook ((c++-mode rust-mode python-mode) . flymake-mode)
  :bind (("C-c ! l" . flymake-show-buffer-diagnostics)))

(use-package eglot
  :ensure t
  :hook (((rust-mode python-mode go-mode) . eglot-ensure)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(require 'arthurmco-languages)
(require 'arthurmco-org)
(require 'arthurmco-roam)
(require 'arthurmco-dash)
(require 'arthurmco-visual)
(require 'arthurmco-mail)
(require 'arthurmco-utils)

;;; Mastodon
(use-package mastodon
  :config
  (setq mastodon-instance-url "https://mas.to/"
        mastodon-active-user "arthurmco")
  :ensure t)
