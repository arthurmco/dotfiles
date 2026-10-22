;;; -*- lexical-binding: t -*-
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

(defun arthurmco/org-mode-tikz-preview-setup ()
  (setq org-preview-latex-process-alist
        '((dvisvgm :programs ("latex" "dvisvgm")
                   :description "dvi > svg"
                   :message "you need to install the programs: latex and dvisvgm."
                   :image-input-type "dvi"
                   :image-output-type "svg"
                   :image-size-adjust (1.7 . 1.5)
                   :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                   :image-converter ("dvisvgm %f -n -b min -c %S --currentcolor -o %O"))))
  (setq org-latex-packages-alist
        '(("" "tikz" t)
          ("" "tikz-cd" t)))
  (setq org-latex-create-formula-image-program 'dvisvgm))

;;; Alerts estilo GitHub dentro de #+begin_quote

(defface arthurmco/org-alert-note-face
  '((((background dark)) :foreground "#4493f8")
    (t :foreground "#0969da"))
  "Face para quotes com [!NOTE].")

(defface arthurmco/org-alert-tip-face
  '((((background dark)) :foreground "#3fb950")
    (t :foreground "#1a7f37"))
  "Face para quotes com [!TIP].")

(defface arthurmco/org-alert-important-face
  '((((background dark)) :foreground "#ab7df8")
    (t :foreground "#8250df"))
  "Face para quotes com [!IMPORTANT].")

(defface arthurmco/org-alert-warning-face
  '((((background dark)) :foreground "#d29922")
    (t :foreground "#9a6700"))
  "Face para quotes com [!WARNING].")

(defface arthurmco/org-alert-caution-face
  '((((background dark)) :foreground "#f85149")
    (t :foreground "#d1242f"))
  "Face para quotes com [!CAUTION].")

(with-eval-after-load 'solarized-palettes
  (let ((c (lambda (name)
             (alist-get name solarized-gruvbox-dark-color-palette-alist))))
    (set-face-attribute 'arthurmco/org-alert-note-face nil      :foreground (funcall c 'blue-l))
    (set-face-attribute 'arthurmco/org-alert-tip-face nil       :foreground (funcall c 'green-l))
    (set-face-attribute 'arthurmco/org-alert-important-face nil :foreground (funcall c 'violet-l))
    (set-face-attribute 'arthurmco/org-alert-warning-face nil    :foreground (funcall c 'yellow-l))
    (set-face-attribute 'arthurmco/org-alert-caution-face nil    :foreground (funcall c 'red-l))))

(defconst arthurmco/org-alert-faces
  '(("NOTE"      . arthurmco/org-alert-note-face)
    ("TIP"       . arthurmco/org-alert-tip-face)
    ("IMPORTANT" . arthurmco/org-alert-important-face)
    ("WARNING"   . arthurmco/org-alert-warning-face)
    ("CAUTION"   . arthurmco/org-alert-caution-face)))

(defun arthurmco/org-alert-face (type)
  (alist-get (upcase type) arthurmco/org-alert-faces nil nil #'string=))

(defun arthurmco/org-alert-matcher (limit)
  "Acha o próximo quote com callout antes de LIMIT.
Grupo 0: bloco inteiro; 1: o marcador [!TIPO]; 2: o TIPO."
  (let ((case-fold-search t)
        found)
    (while (and (not found)
                (re-search-forward
                 (concat "^[ \t]*#\\+begin_quote\\b.*\n"
                         "\\(?:[ \t]*\n\\)*"   ; linhas em branco opcionais
                         "[ \t]*\\(\\[!\\(NOTE\\|TIP\\|IMPORTANT\\|WARNING\\|CAUTION\\)\\]\\)[ \t]*$")
                 limit t))
      (let ((beg (match-beginning 0))
            (mb1 (match-beginning 1)) (me1 (match-end 1))
            (mb2 (match-beginning 2)) (me2 (match-end 2)))
        (when (re-search-forward "^[ \t]*#\\+end_quote\\b.*$" nil t)
          (set-match-data (list beg (match-end 0) mb1 me1 mb2 me2))
          (setq found t))))
    found))

(defun arthurmco/org-alert-setup ()
  (setq-local font-lock-multiline t)
  (font-lock-add-keywords
   nil
   '((arthurmco/org-alert-matcher
      (0 (arthurmco/org-alert-face (match-string 2)) prepend)
      (1 '(:weight bold) prepend)))
   'append))


(use-package ob-restclient
  :ensure t)

(use-package ob-mermaid
  :ensure t
  :init
  (setq ob-mermaid-cli-path "/home/arthurmco/.nix-profile/bin/mmdc"))

(use-package org
  :init
  (setq arthurmco/org-mode-default-scale 1.25)
  :ensure t
  :hook ((org-shiftup-final . windmove-up)
         (org-shiftdown-final . windmove-down)
         (org-shiftleft-final . windmove-left)
         (org-shiftright-final . windmove-right)
         (org-mode . arthurmco/org-alert-setup))
  :config
  (setq org-startup-indented t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-latex-compiler "xelatex"
        org-preview-latex-default-process 'dvipng
        org-image-actual-width '(400))
  (arthurmco/org-mode-font-fixes)
  ; (arthurmco/org-mode-tikz-preview-setup)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)
     (mermaid . t)
     (python . t)
     (nix . t)
     (restclient . t)
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


(use-package ox-gfm
  :ensure t
  :after org)

(use-package htmlize
  :after org
  :ensure t)

(use-package olivetti  
  :ensure t
  :after org
  :hook
  (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 85))


(provide 'arthurmco-org)
