;;; init-macos.el --- Initialize key-bindings for MacOs.	-*- lexical-binding: t -*-

(require 'init-const)

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
    (setq locate-command "mdfind")
    (display-time-mode +1)
    ;; (add-to-list 'woman-manpath
    ;;              "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/share/man")
    ;; (add-to-list 'woman-manpath
    ;;              "/Applications/Xcode.app/Contents/Developer/usr/share/man")
    ;; (add-to-list 'woman-manpath
    ;;              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/share/man")
    (use-package ob-applescript)
    (setq trash-directory "~/.Trash")
    )
  )
(provide 'init-macos)
