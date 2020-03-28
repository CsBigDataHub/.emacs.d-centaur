;;; init-macos.el --- Initialize key-bindings for MacOs.	-*- lexical-binding: t -*-

(when sys/macp
  (bind-keys*
   ("<f13>" . treemacs)
   ))

(provide 'init-macos)
