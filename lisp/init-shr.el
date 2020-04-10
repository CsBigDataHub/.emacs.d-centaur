;;init-shr.el --- SHR enhancements.     -*- lexical-binding: t -*-
(use-package shrface
  :after shr
  :straight (shrface :type git :host github :repo "chenyanming/shrface")
  :config
  (setq nov-shr-rendering-functions shr-external-rendering-functions)
  (setq shrface-bullets-bullet-list '("☯" "☢" "❀" "◉" "⚫" "○" "✸" "✿" "~"))
  )


(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  )

(provide 'init-shr)
