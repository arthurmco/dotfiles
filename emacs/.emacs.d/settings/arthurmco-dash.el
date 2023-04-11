;;; arthurmco-dash.el --- dashboard and initial buffer customizations

(use-package dashboard
  :ensure t
  :init
  (add-hook 'emacs-startup-hook 'dashboard-refresh-buffer)
  :config
  (setq dashboard-set-init-info t)
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
                          (registers . 5)
                          (bookmarks . 5)))
  (setq dashboard-banner-logo-title "TUTSMACS")
  (setq dashboard-footer-messages
        '("'Trabalhando e relaxando' -- Jailson Mendes"
          "please use [[https://github.com/viglioni/lauremacs]] instead"
          "Rewrite it in Rust!"
          "apt remove vim"))  
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(provide 'arthurmco-dash)
