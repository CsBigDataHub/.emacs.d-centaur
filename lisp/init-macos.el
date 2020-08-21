;;; init-macos.el --- Initialize key-bindings for MacOs.	-*- lexical-binding: t -*-

(when sys/macp
  (bind-keys*
   ("<f13>" . treemacs)
   ("<C-M-s-268632064>" . major-mode-hydra) ;; 268632064 is number for SPACE key locater when used with this combination
   ("C-M-s-SPC" . major-mode-hydra) ;; 268632064 is number for SPACE key locater when used with this combination
   ))
(when sys/macp
  (progn
    (setq mac-right-command-modifier 'hyper) ;; mac right command key as hyper, This is when logitech keyboards are used.
    (setq display-time-day-and-date t)
    (display-time-mode +1)
    )
  )
(provide 'init-macos)
