;;; init-linux.el --- Initialize key-bindings for linux.	-*- lexical-binding: t -*-


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
   ("s-H-o" . other-window)
   ))

(when sys/linuxp
  (add-hook 'mu4e-headers-mode (lambda () (isolate-add-mode -1)))
  ;; TODO - disable isolate delete mode for mu4e-headers-mode
  ;; (add-hook 'mu4e-headers-mode (lambda () (if 'mu4e-headers-mode (remove-hook 'activate-mark-hook #'activate-mark-hook@set-transient-map 'local))))
  )
(when sys/linuxp
  (progn
    (menu-bar-mode 1)
    )
  )
(provide 'init-linux)
