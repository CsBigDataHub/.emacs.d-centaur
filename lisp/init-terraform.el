(use-package terraform-mode
  :mode "\\.tf$"
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  )

(use-package terraform-doc)

(use-package company-terraform
  :after company
  :hook (terraform-mode . company-terraform-init))

;; TODO -look in to it lat
;;(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))

;; TODO -look in to it lat
;;(lsp-register-client
;; (make-lsp-client :new-connection (lsp-stdio-connection '("~/.emacs.d/custom-el-scripts/lsp/terraform-lsp/terraform-lsp" "-enable-log-file"))
;;                  :major-modes '(terraform-mode)
;;                  :server-id 'terraform-ls))

(add-hook 'terraform-mode-hook #'lsp)

(provide 'init-terraform)
