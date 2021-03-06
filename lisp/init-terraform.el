;;init-terraform.el --- Initialize terraform configurations.	-*- lexical-binding: t -*-


(use-package terraform-mode
  :mode "\\.tf$"
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  )

(use-package terraform-doc)

(use-package company-terraform
  :after company
  :hook (terraform-mode . company-terraform-init))

;; included in lsp-mode file
;;(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))

;; included in lsp-mode file
;; TODO  get Terraform lsp from -- https://github.com/juliosueiras/terraform-lsp
;;(lsp-register-client
;; (make-lsp-client :new-connection (lsp-stdio-connection '("~/.emacs.d/lsp/terraform-lsp" "-enable-log-file"))
;;                  :major-modes '(terraform-mode)
;;                  :server-id 'terraform-ls))

(add-hook 'terraform-mode-hook #'lsp)

(provide 'init-terraform)
