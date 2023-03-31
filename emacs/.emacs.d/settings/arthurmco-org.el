;;; arthurmco-org.el --- customizations for org-mode

(defun arthurmco/org-mode-font-fixes ()
  (let* ((normal-height (/ 1 arthurmco/org-mode-default-scale))
         (drawer-height (* normal-height 1.1)))
    (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch :height drawer-height)
    (set-face-attribute 'org-tag nil :inherit 'fixed-pitch :height drawer-height)
    (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch :height drawer-height)
    (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch :height normal-height))
  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.4)
                  (org-level-4 . 1.3)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face))))


(use-package org
  :init
  (setq arthurmco/org-mode-default-scale 1.25)
  :ensure t
  :hook ((org-shiftup-final . windmove-up)
         (org-shiftdown-final . windmove-down)
         (org-shiftleft-final . windmove-left)
         (org-shiftright-final . windmove-right))
  :config
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t        
        org-image-actual-width '(300))
  (arthurmco/org-mode-font-fixes)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (python . t)
     (shell . t))))

(use-package org-appear
  :after org
  :ensure t
  :hook (org-mode . org-appear-mode))

(defun org-superstar-init ()
  (org-superstar-mode 1))

(use-package org-superstar
  :after org
  :ensure t
  :hook (org-mode . org-superstar-init))


(defun arthurmco/allow-exclusive-org-mode-default ()
  (if (equal mixed-pitch-mode nil)
      (face-remap-reset-base 'default)
      (face-remap-add-relative 'default :height arthurmco/org-mode-default-scale :weight 'regular)))


(use-package mixed-pitch
  :init
  :ensure t
  :after org
  :hook
  ;; If you want it in all text modes:
  (org-mode . mixed-pitch-mode)
  (mixed-pitch-mode . arthurmco/allow-exclusive-org-mode-default)
  
  :config
  (setq fill-column 100)  
  (set-face-attribute 'variable-pitch nil :font "TeX Gyre Pagella" :height 130))


(use-package olivetti  
  :ensure t
  :after org
  :hook
  (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 70))

(provide 'arthurmco-org)
