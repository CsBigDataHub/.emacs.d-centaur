;; init-kubernetes.el --- Initialize kubernetes configurations.	-*- lexical-binding: t -*-

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  ;; Emacs hangs if this updates too frequently.  Use "g" to update.
  (setq kubernetes-redraw-frequency 300
        kubernetes-poll-frequency 300))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :after (kubernetes-overview evil)
  :demand t
  :commands (kubernetes-overview))

;; (with-eval-after-load 'kubernetes-overview
;;   (load-library "kubernetes-evil"))
;; (eval-after-load 'kubernetes-overview
;;   '(require 'kubernetes-evil))

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode)
  :config
  (setq k8s-search-documentation-browser-function 'browse-url-chrome)
  (setq k8s-site-docs-url "https://kubernetes.io/docs/reference/generated/kubernetes-api/")
  (setq k8s-site-docs-version "v1.18")
  )

(add-hook 'k8s-mode-hook 'yas-minor-mode)

(use-package kubernetes-tramp)

(use-package kubernetes-helm)

(use-package kubel
  :config
  (setq kubel-use-namespace-list 'on))

(use-package kubel-evil
  :after (kubel evil)
  :demand t
  :commands (kubel))

;; (eval-after-load 'kubel
;;   '(require 'kubel-evil))

(provide 'init-kubernetes)
