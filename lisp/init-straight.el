(setq straight-recipes-gnu-elpa-use-mirror t
      straight-repository-branch           "develop"
      straight-check-for-modifications     'live
      straight-vc-git-default-clone-depth  1)

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
;;(setq straight-use-package-by-default t)

(use-package point-history
  :straight (point-history :type git :host github :repo "blue0513/point-history")
  :hook (after-init . point-history-mode)
  :config
  (point-history-mode t)
  :init (gsetq point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*"))

(use-package ivy-point-history
  :straight (ivy-point-history :type git :host github :repo "SuzumiyaAoba/ivy-point-history")
  :bind ("C-c C-/" . ivy-point-history))

(use-package reformatter
  :straight (reformatter :type git :host github :repo "purcell/reformatter.el"))

(reformatter-define xml-format
  :program "xmllint"
  :args '("--format" "-")
  :mode nil)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :straight
  (eshell-toggle :type git :host github :repo "4DA/eshell-toggle")
  :bind
  ("<f12>" . eshell-toggle)
  ("s-~" . eshell-toggle))

(use-package gitignore-templates
  :straight (gitignore-templates :type git :host github :repo "CsBigDataHub/gitignore-templates.el"))

(use-package number-to-word
  :straight (number-to-word :type git :host github :repo "xuchunyang/number-to-word"))

;; commenting this until terraform pull request is accepted
;; https://github.com/AndreaCrotti/yasnippet-snippets/pull/363
;; Getting Terraform snippets from my repo with straight in init-straight.el
;; https://github.com/CsBigDataHub/yasnippet-snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet")
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :straight (yasnippet-snippets :type git :host github :repo "CsBigDataHub/yasnippet-snippets")))

(use-package ivy-yasnippet
  :straight (ivy-yasnippet :type git :host github :repo "CsBigDataHub/ivy-yasnippet"))

(use-package auto-yasnippet
  :straight (auto-yasnippet :type git :host github :repo "abo-abo/auto-yasnippet"))
;;; yas-snippet

(use-package lsp-latex
  :straight (lsp-latex :type git :host github :repo "ROCKTAKEY/lsp-latex")
  :config
  (require 'lsp-latex)
  (setq lsp-latex-texlab-executable "/Users/ckoneru/.emacs.d/lsp/texlab")
  (with-eval-after-load "tex-mode"
    (add-hook 'tex-mode-hook 'lsp)
    (add-hook 'latex-mode-hook 'lsp)))

;; Java properties
(use-package properties-mode
  :straight (properties-mode :type git :host github :repo "iquiw/properties-mode")
  :mode "\\.properties\\'")

(when sys/linuxp
  (use-package org-mime
    :straight (org-mime :type git :host github :repo "org-mime/org-mime")))

(provide 'init-straight)
