;;; init-linux.el --- Initialize key-bindings for linux.	-*- lexical-binding: t -*-

(require 'init-const)

(when sys/linuxp
  (bind-keys*
   ("<f12>" . eshell-toggle)
   ("H-<left>" . centaur-tabs-backward)
   ("H-<right>" . centaur-tabs-forward)
   ("H-<up>" . centaur-tabs-forward-group)
   ("H-<down>" . centaur-tabs-backward-group)
   ("H-1" . centaur-tabs-select-visible-tab)
   ("H-2" . centaur-tabs-select-visible-tab)
   ("H-3" . centaur-tabs-select-visible-tab)
   ("H-4" . centaur-tabs-select-visible-tab)
   ("H-5" . centaur-tabs-select-visible-tab)
   ("H-6" . centaur-tabs-select-visible-tab)
   ("H-7" . centaur-tabs-select-visible-tab)
   ("H-8" . centaur-tabs-select-visible-tab)
   ("H-9" . centaur-tabs-select-visible-tab)
   ("H-<" . centaur-tabs-move-current-tab-to-left)
   ("H->" . centaur-tabs-move-current-tab-to-right)
   ("H-r" . revert-this-buffer)
   ("s-c" . kill-ring-save)
   ("s-x" . kill-region)
   ("C-S-v" . yank)
   ("s-H-n" . make-frame-command)
   ("s-w" . delete-frame)
   ("s-b" . switch-to-buffer)
   ("s-k" . my/kill-this-buffer)
   ("s-0" . delete-window)
   ("s-H-o" . other-window)
   ("H-M-s-SPC" . major-mode-hydra)
   ))


(when sys/linuxp
  (progn
    (menu-bar-mode 1)
    (setq display-time-day-and-date t)
    (display-time-mode +1)
    )
  )

(provide 'init-linux)
