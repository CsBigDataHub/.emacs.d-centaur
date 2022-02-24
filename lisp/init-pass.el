;;init-pass.el --- Initialize password-store configurations.	-*- lexical-binding: t -*-

;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
;; (require 'epa-file)
;; (epa-file-enable)
;; Enable encryption/decryption of .gpg files
(use-package epa-file
  :ensure nil
  :defer t
  :after epg
  :commands epa-file-enable
  :config  (progn
             ;; Unfortunately there is bug in gpg which disabled this
             ;; ~/.gnupg/gpg-agent.conf should contain:
             ;; allow-emacs-pinentry
             ;; allow-loopback-pinentry
             ;; Restart with:
             ;; gpgconf --reload gpg-agent
             ;; (setq epa-pinentry-mode 'loopback)

             (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
             (epa-file-name-regexp-update))
  :hook (after-init . epa-file-enable))

(use-package password-generator)

(use-package pass)

(use-package password-store)


(provide 'init-pass)
