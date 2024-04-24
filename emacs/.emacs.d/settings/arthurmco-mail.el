;;; arthurmco-mail.el --- mailboard and initial buffer customizations

(use-package mu4e
  :ensure nil
  :defer t
  :load-path "/nix/store/khgrb7610g4jw0n2yc5hg17wyzq94kaa-mu-1.8.14/share/emacs/site-lisp/mu4e/"
  :init
  ;; pass to store passwords
  ;; file auth looks for is ~/.password-store/<smtp.host.tld>:<port>/<name>
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (setq auth-source-debug t)
  (setq auth-source-do-cache nil)
  (setq user-mail-address "arthurmco@gmail.com"
        user-full-name "Arthur Mendes"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user "arthurmco@gmail.com"
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-debug-info t
        message-send-mail-function 'smtpmail-send-it)
  
  :config
  (require 'org-mu4e)

  (setq mu4e-change-filenames-when-moving t)
  
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.local/share/mail/arthurmco@gmail.com")

  (setq mu4e-drafts-folder "/[Gmail]/Rascunhos")
  (setq mu4e-sent-folder "/[Gmail]/E-mails enviados")
  (setq mu4e-refile-folder "/INBOX")
  (setq mu4e-trash-folder "/Trash")

  (setq mu4e-compose-signature "Arthur Mendes")
 

  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-keep-self-cc nil)
  ;; moving messages renames files to avoid errors
  (setq mu4e-change-filenames-when-moving t)
  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)
  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)
  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
  ;; Don't ask for a 'context' upon opening mu4e

  (setq mu4e-context-policy 'pick-first)
  ;; Don't ask to quit... why is this the default?
  (setq mu4e-confirm-quit nil))


(use-package org-mime
  :ensure t
  :init
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author toc
                                  :with-toc nil)))

(provide 'arthurmco-mail)
  
