;;; arthurmco-visual.el --- things that change how Emacs looks

;;;
;;; Custom themes
;;;

(defmacro download-theme (name)
  `(use-package ,name
     :if window-system
     :ensure t))

(download-theme solarized-theme)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-mode
  :ensure t
  :hook (css-mode scss-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((c++-mode scheme-mode rust-mode js2-mode racket-mode typescript-mode) . rainbow-delimiters-mode))

(use-package anzu
  :ensure t
  :config  
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(provide 'arthurmco-visual)
