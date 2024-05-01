;;; arthurmco-languages.el --- customizations for language mode support

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

(use-package rust-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package go-mode
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

(use-package nix-mode
  :ensure t)

(use-package ob-nix
  :ensure t)


(use-package tex
  :ensure auctex
  :config
  (TeX-engine-set "luatex")
  :init
  (setq-default TeX-master nil)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t))

(use-package poetry
 :ensure t)

(use-package mermaid-mode
  :init
  (setq mermaid-mmdc-location "/home/arthurmco/.nix-profile/bin/mmdc")
  :ensure t)

(provide 'arthurmco-languages)

