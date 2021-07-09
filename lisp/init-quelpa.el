(use-package quelpa
  :demand t
  :config
  (setq quelpa-update-melpa-p nil)
  (quelpa-self-upgrade))

(use-package quelpa-use-package
  :after
  (quelpa)
  :demand t
  :config
  (quelpa-use-package-activate-advice))


;; my-personal-config
(use-package company-fuzzy
  :quelpa
  (company-fuzzy
   :fetcher github
   :repo "jcs-elpa/company-fuzzy"
   :commit "2f70e6fe78c45d0cbbb22b1d994f90aa9c78553b"
   )
  :init
  (setq company-fuzzy-sorting-backend 'flx)
  (setq company-fuzzy-prefix-ontop nil)
  (with-eval-after-load 'company
    (global-company-fuzzy-mode t)))

(provide 'init-quelpa)
