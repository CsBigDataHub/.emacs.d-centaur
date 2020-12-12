;;init-image-plus.el --- Initialize image+ configurations.	-*- lexical-binding: t -*-

(use-package image+
  :init
  (add-hook 'image-mode-hook
            '(lambda ()
               (require 'image+)
               (imagex-sticky-mode)))

  :bind (:map image-mode-map
         ("=" . imagex-sticky-zoom-in)
         ("-" . imagex-sticky-zoom-out)
         ("m" . imagex-sticky-maximize)
         ("g" . imagex-sticky-restore-original)
         ("S" . imagex-sticky-save-image)
         ("r" . imagex-sticky-rotate-right)
         ("l" . imagex-sticky-rotate-left)
         ("/" . imagex-sticky-binding/body))

  :config
  (defhydra imagex-sticky-binding () ;; global-map "C-x C-l"
    "Manipulating Image"
    ("+" imagex-sticky-zoom-in "zoom in")
    ("-" imagex-sticky-zoom-out "zoom out")
    ("M" imagex-sticky-maximize "maximize")
    ("O" imagex-sticky-restore-original "restore original")
    ("S" imagex-sticky-save-image "save file")
    ("r" imagex-sticky-rotate-right "rotate right")
    ("l" imagex-sticky-rotate-left "rotate left")))

(eval-after-load 'image+ '(imagex-global-sticky-mode 1))

(provide 'init-image-plus)
