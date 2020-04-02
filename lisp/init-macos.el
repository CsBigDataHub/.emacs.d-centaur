;;; init-macos.el --- Initialize key-bindings for MacOs.	-*- lexical-binding: t -*-

(when sys/macp
  (bind-keys*
   ("<f13>" . treemacs)
   ))
(when sys/macp
  (progn
    (setq display-time-day-and-date t)
    (display-time-mode +1)
    )
  )
(provide 'init-macos)
