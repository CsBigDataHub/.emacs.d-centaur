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

;; Not using it actively
;; (use-package lsp-latex
;;   :straight (lsp-latex :type git :host github :repo "ROCKTAKEY/lsp-latex")
;;   :config
;;   (require 'lsp-latex)
;;   (setq lsp-latex-texlab-executable "/Users/ckoneru/.emacs.d/lsp/texlab")
;;   (with-eval-after-load "tex-mode"
;;     (add-hook 'tex-mode-hook 'lsp)
;;     (add-hook 'latex-mode-hook 'lsp)))

;; Java properties
(use-package properties-mode
  :straight (properties-mode :type git :host github :repo "iquiw/properties-mode")
  :mode "\\.properties\\'")

(use-package clipboard2org
  :straight (clipboard2org :type git :host github :repo "itf/clipboard2org")
  :bind ("C-S-y" . clipboard2org))

(when (image-type-available-p 'svg)
  (use-package imgur
    :straight (imgur
               :type git
               :host github
               :repo "myuhe/imgur.el")))

(when (image-type-available-p 'svg)
  (use-package meme
    :straight (meme
               :type git
               :host github
               :repo "larsmagne/meme")))
(when sys/macp
  (use-package apples-mode
    :straight (apples-mode
               :type git
               :host github
               :repo "rprimus/apples-mode")
    :config
    (add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . apples-mode))))

(provide 'init-straight)
