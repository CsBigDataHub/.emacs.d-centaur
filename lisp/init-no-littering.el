;; init-no-littering.el --- Initialize no-littering configurations.	-*- lexical-binding: t -*-

(use-package no-littering               ; Keep .emacs.d clean
  :config (progn
            (require 'recentf)
            (add-to-list 'recentf-exclude no-littering-etc-directory)
            (add-to-list 'recentf-exclude no-littering-var-directory)
            (setq create-lockfiles nil
                  delete-old-versions t
                  kept-new-versions 6
                  kept-old-versions 2
                  version-control t)))
(provide 'init-no-littering)
