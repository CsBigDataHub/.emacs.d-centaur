(use-package kubernetes
  :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :after kubernetes)

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode)
  :config
  ;;(setq k8s-search-documentation-browser-function 'browse-url-firefox)
  (setq k8s-site-docs-url "https://kubernetes.io/docs/reference/generated/kubernetes-api/")
  (setq k8s-site-docs-version "v1.13")
  )

(add-hook 'k8s-mode-hook 'yas-minor-mode)

(use-package kubernetes-tramp)

(use-package kubernetes-helm)

(use-package kubel)

(use-package kubel-evil)

(provide 'init-kubernetes)
