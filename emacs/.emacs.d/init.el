;; Config file

;; Setup MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(toggle-frame-maximized)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
(electric-pair-mode t)

(use-package projectile
  :ensure t
  :init
  (require 'projectile)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode)
  (setq projectile-enable-caching t))


(defun setup-c-indentation ()
  "Configure indentation right on C modes"
  (setq-default c-basic-offset 4
		        tab-width 4
		        indent-tabs-mode nil)
  (c-set-offset 'arglist-intro '+))

(defun setup-custom-keys ()
  (global-set-key (kbd "M-s") 'replace-string)
  (global-set-key (kbd "C-c r") 'revert-buffer)

  ;; delete non needed whitespace
  ;; this might be in a before-save-hook, but I want to do only when I want
  ;; and markdown uses trailing whitespaces for non-new-paragraph-newline
  (global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
  (global-set-key (kbd "C-c ! l") 'flymake-show-diagnostics-buffer))

(defun setup-highlight ()
  (global-hl-line-mode)
  (set-face-attribute hl-line-face nil :underline nil))

(defun setup-mode-hooks ()
;;  (add-hook 'c-mode-hook #'lsp)
;;  (add-hook 'php-mode-hook #'lsp)
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (add-hook 'latex-mode-hook 'auto-fill-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)) ; 100mb

(setup-c-indentation)
(setup-highlight)
(show-paren-mode 1)

(require 'use-package)

(use-package emmet-mode
  :ensure t
  :hook ((html-mode . emmet-mode)))

(use-package windmove
  :ensure t
  :bind (("M-<left>" . windmove-left)          ; move to left window
         ("M-<right>" . windmove-right)        ; move to right window
         ("M-<up>" . windmove-up)              ; move to upper window
         ("M-<down>" . windmove-down) ; move to lower window
))

(use-package move-text
  :ensure t
  :bind (("M-S-<up>" . move-text-up)
         ("M-S-<down>" . move-text-down)))

(use-package idomenu
  :ensure t
  :init (ido-grid-mode 1)
  :config (ido-mode t))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :bind (("M-s-<down>" . mc/mark-next-like-this)
         ("M-s-<up>" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("M-s-c M-s-c" . mc/edit-lines)))

(use-package web-mode
  :mode ("\\.html\\'"
         "\\.phtml\\'"
         "\\.tpl\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"))

(use-package rjsx-mode
  :mode ("\\.jsx\\'"))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'"))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package forge
  :after magit)

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :after geiser)


(use-package python-mode
  :ensure t)

(use-package indent-guide
  :ensure t
  :after python-mode
  :hook ((python-mode . indent-guide-mode)))

(use-package yasnippet                  ; Snippets
  :ensure t
  :config
  (setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t)

(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  :bind (("\C-c C-r" . recentf-open-files)))

;(use-package org
;  :ensure t
;  :bind (("\C-c l" . org-store-link)
;         ("\C-c a" . org-agenda))
;  :config
;  (setq org-log-done t))

(use-package hl-todo
  :ensure t
  :bind (("\C-c h p" . hl-todo-previous)
         ("\C-c h n" . hl-todo-next)
         ("\C-c h o" . hl-todo-occur)
         ("\C-c h i" . hl-todo-insert)))


(use-package smart-mode-line
  :ensure t
  :config (sml/setup))

(use-package rainbow-delimiters
  :ensure t
  :hook ((c++-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (js2-mode . rainbow-delimiters-mode)))

(setq lsp-keymap-prefix "C-c s")

(use-package which-key
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (require 'lsp-ido)
  
  ;; required by lsp-mode to work nicely
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000) ; 100mb
  (which-key-mode)

  (setq lsp-clients-clangd-executable "/run/current-system/sw/bin/clangd")
  
  :hook ((c++-mode . lsp)
         (js2-mode . lsp)
         (lsp . #'lsp-enable-which-key-integration)
         (rust-mode . lsp)
         (typescript-mode . lsp))
  
  :config 
  (setq lsp-clients-clangd-args '("-j=8" "-background-index" "-log=error"))
  :bind (("\C-c s" . lsp-command-map)
         ("\C-c f" . lsp-format-buffer))
  :commands lsp)

;; Colors emacs compilation buffer, so I do not see those bunch of ^[[32m's when using cmake
(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))


(use-package direnv
 :config
 (direnv-mode))

(setup-mode-hooks)
(setup-custom-keys)

