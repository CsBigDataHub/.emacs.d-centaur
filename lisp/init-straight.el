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

(provide 'init-straight)
