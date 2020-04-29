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

;;lang-tools
(use-package langtool
  :straight (langtool :type git :host github :repo "redguardtoo/Emacs-langtool")
  :config
  (when sys/macp
    (progn
      ;; place the language-tool directory in $HOME
      (setq langtool-language-tool-jar
            "/usr/local/Cellar/languagetool/4.9.1/libexec/languagetool-commandline.jar")
      (setq langtool-java-bin "/Users/ckoneru/.sdkman/candidates/java/8.0.232.j9-adpt/bin/java")
      (setq langtool-bin "/usr/local/bin/languagetool")
      (setq langtool-default-language "en-US")
      (setq langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
      (setq langtool-mother-tongue "en")
      )
    ))

;; hydra for langtool check
(defhydra hydra-langtool (:color pink
                          :hint nil)
  "
_c_: check    _n_: next error
_C_: correct  _p_: prev error _d_: done checking
"
  ("n"  langtool-goto-next-error)
  ("p"  langtool-goto-previous-error)
  ("c"  langtool-check)
  ("C"  langtool-correct-buffer)
  ("d"  langtool-check-done :color blue)
  ("q" nil "quit" :color blue))
(bind-key "C-c h l t" 'hydra-langtool/body)


(when sys/linuxp
  (use-package org-mime
    :straight (org-mime :type git :host github :repo "org-mime/org-mime")))

(provide 'init-straight)
