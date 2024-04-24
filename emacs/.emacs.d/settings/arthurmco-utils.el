;;; arthurmco-utils.el --- Custom functions useful for me

(defun arthurmco/query-on-wiktionary (message)
  "Query a certain term on wiktionary"
  (interactive "sTerm: ")
  (shell-command
   (format "xdg-open \"https://en.wiktionary.org/w/index.php?search=%s\""
           (string-replace " " "+" message))))
    

(defun arthurmco/query-portuguese-lemma (word)
  "Query a term on a portuguese dictionary"
  (interactive "sPalavra: ")
  (shell-command
   (format "xdg-open \"https://michaelis.uol.com.br/busca?r=0&f=0&t=0&palavra=%s\""
           (string-replace " " "+" word))))


(defun arthurmco/query-synonym (word)
  "Query a term on a portuguese dictionary"
  (interactive "sPalavra: ")
  (shell-command
   (format "xdg-open \"https://www.sinonimos.com.br/busca.php?q=%s\""
           (string-replace " " "+" word))))


(defun arthurmco/--get-current-path-relative-to-project ()
  (let ((current-file (buffer-file-name))
        (current-project (projectile-project-root)))
    (if current-project
        (s-replace current-project "" current-file)
      current-file)))

(defun arthurmco/copy-current-path-relative-to-project ()
  (interactive)
  (let ((path (arthurmco/--get-current-path-relative-to-project)))
    (when path
      (message "copied <%s>" path)
      (kill-new path))))



(provide 'arthurmco-utils)
