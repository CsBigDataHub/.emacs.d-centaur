;;init-shr.el --- SHR enhancements.     -*- lexical-binding: t -*-
(use-package shrface
  :after shr
  ;; :straight (shrface :type git :host github :repo "chenyanming/shrface")
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t)
  (setq nov-shr-rendering-functions shr-external-rendering-functions)
  (setq shrface-bullets-bullet-list '("☯" "☢" "❀" "◉" "⚫" "○" "✸" "✿" "~"))
  (setq shrface-item-bullet "➤")
  ;; eww support
  (with-eval-after-load 'eww
    (add-hook 'eww-after-render-hook 'shrface-mode))

  ;; nov support
  (with-eval-after-load 'nov
    (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))) ; reset nov-shr-rendering-functions, in case of the list get bigger and bigger
    (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
    (add-hook 'nov-mode-hook 'shrface-mode))

  ;; mu4e support
  (with-eval-after-load 'mu4e
    (add-hook 'mu4e-view-mode-hook 'shrface-mode))

  (with-eval-after-load 'nov
    (define-key nov-mode-map (kbd "<tab>") 'org-cycle)
    (define-key nov-mode-map (kbd "S-<tab>") 'org-shifttab)
    (define-key nov-mode-map (kbd "C-j") 'outline-next-visible-heading)
    (define-key nov-mode-map (kbd "C-k") 'outline-previous-visible-heading))

  (with-eval-after-load 'eww
    (define-key eww-mode-map (kbd "<tab>") 'org-cycle)
    (define-key eww-mode-map (kbd "S-<tab>") 'org-shifttab)
    (define-key eww-mode-map (kbd "C-j") 'outline-next-visible-heading)
    (define-key eww-mode-map (kbd "C-k") 'outline-previous-visible-heading))
  )


(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  )

(provide 'init-shr)
