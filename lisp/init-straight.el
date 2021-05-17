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
  :straight (ivy-yasnippet :type git :host github :repo "mkcms/ivy-yasnippet"))

(which-key-add-key-based-replacements
  "M-m y" "yas-prefix")

(bind-keys*
 ("M-m y y" . ivy-yasnippet))

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
  :bind (:map org-mode-map
         ("C-S-y" . clipboard2org-paste)))

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

(use-package org-pandoc-import
  :straight (org-pandoc-import
             :type git
             :host github
             :repo "tecosaur/org-pandoc-import"
             :files ("*.el" "filters" "preprocessors")))

(use-package org-table-wrap-functions
  :straight (org-table-wrap-functions
             :type git
             :host github
             :repo "analyticd/org-table-wrap-functions"))


(use-package org-timed-alerts
  :after (org alert)
  :straight (org-timed-alerts
             :type git
             :host github
             :repo "legalnonsense/org-timed-alerts")
  :custom
  (org-timed-alerts-alert-function #'alert)
  (org-timed-alerts-tag-exclusions nil)
  (org-timed-alerts-default-alert-props nil)
  (org-timed-alerts-warning-times '(-15 -7 -3))
  (org-timed-alerts-agenda-hook-p t)
  (org-timed-alert-final-alert-string "IT IS %alert-time\n\n%todo %headline")
  (org-timed-alert-warning-string (concat "%todo %headline\n at %alert-time\n "
                                          "it is now %current-time\n "
                                          "*THIS IS YOUR %warning-time MINUTE WARNING*"))
  :config
  (add-hook 'org-mode-hook #'org-timed-alerts-mode))

(use-package ob-pwsh
  :straight (ob-pwsh
             :type git
             :host github
             :repo "CsBigDataHub/ob-pwsh")
  )

(use-package restclient-jq
  :after (restclient ob-restclient)
  :demand t
  :straight (restclient-jq
             :type git
             :host github
             :repo "pashky/restclient.el"
             :files ("restclient-jq.el"))
  :config
  (eval-after-load 'restclient
    '(require 'restclient-jq))
  (eval-after-load 'ob-restclient
    '(require 'restclient-jq)))

(provide 'init-straight)
