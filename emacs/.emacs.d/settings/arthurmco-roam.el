;;; arthurmco-roam.el --- customizations for org-roam and org-roam-ui

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

(provide 'arthurmco-roam)
