(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-redraw-frequency 300
        kubernetes-poll-frequency 300))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :after kubernetes-overview)

;; (with-eval-after-load 'kubernetes-overview
;;   (load-library "kubernetes-evil"))
(eval-after-load 'kubernetes-overview
  '(require 'kubernetes-evil))

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode)
  :config
  ;;(setq k8s-search-documentation-browser-function 'browse-url-firefox)
  (setq k8s-site-docs-url "https://kubernetes.io/docs/reference/generated/kubernetes-api/")
  (setq k8s-site-docs-version "v1.18")
  )

(add-hook 'k8s-mode-hook 'yas-minor-mode)

(use-package kubernetes-tramp)

(use-package kubernetes-helm)

(use-package kubel)

(use-package kubel-evil)

(provide 'init-kubernetes)
