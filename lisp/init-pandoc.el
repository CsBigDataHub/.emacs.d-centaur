
(use-package pandoc-mode
  :hook
  (markdown-mode . pandoc-mode)
  (pandoc-mode-hook . pandoc-load-default-settings)
  )

(use-package ox-pandoc
  :after ox
  )

(provide 'init-pandoc)
