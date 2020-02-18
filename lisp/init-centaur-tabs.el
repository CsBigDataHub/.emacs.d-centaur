
(use-package centaur-tabs
  :demand
  :after evil
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'over)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("s-<left>" . centaur-tabs-backward)
  ("s-<right>" . centaur-tabs-forward)
  ("s-<up>" . centaur-tabs-forward-group)
  ("s-<down>" . centaur-tabs-backward-group)
  ("s-1" . centaur-tabs-select-visible-tab)
  ("s-2" . centaur-tabs-select-visible-tab)
  ("s-3" . centaur-tabs-select-visible-tab)
  ("s-4" . centaur-tabs-select-visible-tab)
  ("s-5" . centaur-tabs-select-visible-tab)
  ("s-6" . centaur-tabs-select-visible-tab)
  ("s-7" . centaur-tabs-select-visible-tab)
  ("s-8" . centaur-tabs-select-visible-tab)
  ("s-9" . centaur-tabs-select-visible-tab)
  ("s-<" . centaur-tabs-move-current-tab-to-left)
  ("s->" . centaur-tabs-move-current-tab-to-right)
  ("H-t t t" . centaur-tabs-counsel-switch-group)
  ("H-t t p" . centaur-tabs-group-by-projectile-project)
  ("H-t t g" . centaur-tabs-group-buffer-groups)
  (:map evil-normal-state-map
   ("g t" . centaur-tabs-forward)
   ("g T" . centaur-tabs-backward)))

(provide 'init-centaur-tabs)
