
(use-package pandoc-mode
  :hook
  (markdown-mode . pandoc-mode)
  (pandoc-mode-hook . pandoc-load-default-settings)
  )

(use-package ox-pandoc
  :after ox
  )

(eval-after-load 'org
  '(require 'ox-pandoc))

(provide 'init-pandoc)
