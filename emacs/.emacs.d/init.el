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

(add-to-list 'load-path (expand-file-name "settings" (file-name-directory load-file-name)))

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

;;;
;;; Custom keybindings
;;;

(global-set-key (kbd "M-s") 'replace-string)
(global-set-key (kbd "C-c r") 'revert-buffer)


;;;
;;; Custom themes
;;;

(defmacro download-theme (name)
  `(use-package ,name
     :if window-system
     :ensure t))

(download-theme solarized-theme)

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

(use-package rainbow-mode
  :ensure t
  :hook (css-mode scss-mode))

(use-package emmet-mode
  :ensure t
  :hook (html-mode web-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"
         "\\.phtml\\'"
         "\\.tpl\\'"
         "\\.tsx\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"))

(use-package rainbow-delimiters
  :ensure t
  :hook (c++-mode scheme-mode rust-mode js2-mode racket-mode typescript-mode))

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
  :hook ((markdown-mode LaTeX-mode) . auto-fill-mode))

(use-package rust-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

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
  :hook (rust-mode c++-mode python-mode typescript-mode org-mode))

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

(use-package restclient
  :ensure t)

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :after geiser
  :ensure t)

(use-package nim-mode
  :ensure t)

;;;
;;; Completions, language servers, et al --------------
;;;

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;(eldoc-mode +1)
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
  :hook (c++-mode rust-mode python-mode typescript-mode js2-mode))

(use-package flymake
  :ensure t
  :hook (c++-mode rust-mode python-mode)
  :bind (("C-c ! l" . flymake-show-buffer-diagnostics)))

(use-package eglot
  :ensure t
  :hook (((rust-mode python-mode) . eglot-ensure)))

;;;
;;; writing (org and LaTeX) --------------
;;;

(use-package tex
  :ensure auctex
  :init
  (setq-default TeX-master nil)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t))

(require 'arthurmco-org)


(defconst default-org-roam-template
  '("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t))

(use-package org-roam
  :ensure
  :custom
  (org-roam-directory (file-truename "~/roam"))
  :init
  (setq org-roam-v2-ack t)
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
        (list default-org-roam-template
              '("a" "arthurverse things")
              `("ad" "arthurverse default" plain
                "%?"
                :if-new (file+head "book/%<%Y%m%d>-${slug}.org"
                                   ,(concat "#+title: ${title}\n"
                                            "#+filetags: :arthurverse:\n"
                                            "#+bibliography: book-research.bib\n"))
                :unnarrowed t)
              `("ac" "arthurverse character" plain
                "%?"
                :if-new (file+head "book/char/${slug}.org"
                                   ,(concat "#+title: ${title}\n"
                                            "#+filetags: :arthurverse: :character:\n"))
                :unnarrowed t)))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :ensure t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;;; Custom functions

(defun arthurmco/query-on-wiktionary (message)
  "Query a certain term on wiktionary"
  (interactive "sTerm: ")
  (shell-command
   (format "xdg-open https://en.wiktionary.org/w/index.php?search=%s"
           (string-replace " " "+" message))))
    
