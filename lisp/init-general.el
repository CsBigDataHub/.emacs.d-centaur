(use-package general
  :config
  (general-evil-setup t)
  :init
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

;; use `SPC' as leader key
(general-create-definer my-spc-leader-def
  :prefix "SPC"
  :states '(normal visual))

(my-spc-leader-def
  "bl" 'ibuffer
  "bb" 'counsel-switch-buffer
  "bs" 'save-buffer
  "bk" 'my/kill-this-buffer
  "ff" 'counsel-find-file
  "fz" 'counsel-fzf
  "fp" 'counsel-projectile
  "fd" 'counsel-fd
  "dd" 'dired
  "uu" 'undo-tree-visualize
  "wo" 'other-window
  "w0" 'delete-window
  "w1" 'delete-other-windows
  "w2" 'my/split-below-and-move
  "w3" 'my/split-right-and-move
  "ww1" 'eyebrowse-switch-to-window-config-0
  "ww1" 'eyebrowse-switch-to-window-config-1
  "ww2" 'eyebrowse-switch-to-window-config-2
  "ww3" 'eyebrowse-switch-to-window-config-3
  "ww4" 'eyebrowse-switch-to-window-config-4
  "ww5" 'eyebrowse-switch-to-window-config-5
  "ww6" 'eyebrowse-switch-to-window-config-6
  "ww7" 'eyebrowse-switch-to-window-config-7
  "ww8" 'eyebrowse-switch-to-window-config-8
  "ww9" 'eyebrowse-switch-to-window-config-9
  "5o" 'other-frame
  "5f" 'find-file-other-frame
  "5b" 'switch-to-buffer-other-frame
  "50" 'delete-frame
  "51" 'delete-other-frames
  "5d" 'dired-other-frame
  "gg" 'magit-status
  "zz" 'zop-to-char
  "za" 'zop-up-to-char
  "mc" 'my/open-config
  "tt" 'treemacs
  "rr" 'quickrun
  "rc" 'quickrun-compile-only
  "SPC" 'counsel-M-x)

(provide 'init-general)
