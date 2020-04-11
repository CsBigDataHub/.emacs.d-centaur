(use-package general
  :config
  (general-evil-setup t)
  :init
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

;; use `SPC' as leader key
(general-create-definer my-spc-leader-def
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :states '(insert emacs normal hybrid motion visual operator)
  )

(my-spc-leader-def
  "b" '(:ignore t :which-key "buffer")
  "bl" 'ibuffer
  "bb" 'counsel-switch-buffer
  "bs" 'save-buffer
  "bk" 'my/kill-this-buffer
  "f" '(:ignore t :which-key "counsel")
  "ff" 'counsel-find-file
  "fz" 'counsel-fzf
  "fp" 'counsel-projectile
  "fr" 'counsel-recentf
  "fg" 'counsel-git
  "dd" 'dired
  "uu" 'undo-tree-visualize
  "v" '(:ignore t :which-key "VC")
  "v=" 'vc-diff
  "v+" 'vc-update
  "v[" 'diff-hl-previous-hunk
  "v]" 'diff-hl-next-hunk
  "vB" 'browse-at-remote
  "vD" 'vc-root-diff
  "vL" 'vc-print-root-log
  "vP" 'vc-push
  "vd" 'vc-dir
  "vh" 'vc-region-history
  "vl" 'vc-print-log
  "vc" 'vc-revert
  "vv" 'vc-next-action
  "v SPC" 'diff-hl-mark-hunk
  "w" '(:ignore t :which-key "window")
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
  "5" '(:ignore t :which-key "other-frame")
  "5o" 'other-frame
  "5f" 'find-file-other-frame
  "5b" 'switch-to-buffer-other-frame
  "50" 'delete-frame
  "51" 'delete-other-frames
  "5d" 'dired-other-frame
  "g" '(:ignore t :which-key "magit")
  "gg" 'magit-status
  "gi" 'magit-init
  "gc" 'magit-clone
  "z" '(:ignore t :which-key "zop-zoom")
  "za" 'zop-up-to-char
  "zz" 'zop-to-char
  "zx" 'zoom-window-zoom
  "mc" 'my/open-config
  "tt" 'treemacs
  "r" '(:ignore t :which-key "run")
  "rr" 'quickrun
  "rc" 'quickrun-compile-only
  ".." 'imenu
  "C-a" 'mark-whole-buffer
  "SPC" 'counsel-M-x)

(provide 'init-general)
