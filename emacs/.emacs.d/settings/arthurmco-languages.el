;;; arthurmco-languages.el --- customizations for language mode support

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"
         "\\.phtml\\'"
         "\\.tpl\\'"
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

