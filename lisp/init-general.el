;;;init-general.el --- Initialize key-bindings for general.	-*- lexical-binding: t -*-

(require 'init-const)

(use-package general
  :config
  (general-evil-setup t)
  :init
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

;; use `SPC' as leader key

(when sys/macp
  (general-create-definer my-spc-leader-def
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :states '(insert emacs normal hybrid motion visual operator)
    ))

(when sys/linuxp
  (general-create-definer my-spc-leader-def
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "H-SPC"
    :states '(insert emacs normal hybrid motion visual operator)
    ))

(my-spc-leader-def
  "uu" 'vundo
  "mc" 'my/open-config
  ".." 'imenu
  "C-a" 'mark-whole-buffer
  "P" 'projectile-command-map
  "p" 'persp-key-map
  "SPC" 'counsel-M-x)

(defmacro general-global-spc-menu-definer (def infix-key &rest body)
  "Create a definer named general-global-DEF wrapping global-definer.
The prefix map is named 'my-DEF-map'."
  `(progn
     (general-create-definer ,(intern (concat "general-global-spc-" def))
       :wrapping my-spc-leader-def ;; defined above
       :prefix-map (quote ,(intern (concat "my-" def "-spc-map")))
       :infix ,infix-key
       :wk-full-keys nil
       "" '(:ignore t :which-key ,def))
     (,(intern (concat "general-global-spc-" def))
      ,@body)))

(general-global-spc-menu-definer
 "buffer" "b"
 "l" 'ibuffer
 "b" 'counsel-switch-buffer
 "s" 'save-buffer
 "k" 'my/kill-this-buffer
 "p"  'previous-buffer
 "n"  'next-buffer
 "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
       :which-key "messages-buffer")
 "S" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
       :which-key "scratch-buffer")
 "D" '((lambda () (interactive) (switch-to-buffer "*dashboard*"))
       :which-key "dashboard-buffer")
 "TAB" '((lambda () (interactive) (switch-to-buffer nil))
         :which-key "other-buffer")
 )

(general-global-spc-menu-definer
 "company" "c"
 "f" 'company-files
 "s" 'company-ispell
 "y" 'ivy-yasnippet)

(general-global-spc-menu-definer
 "dumb-jump/dired" "d"
 "d" 'dired
 "j" 'dired-jump
 "J" 'dired-jump-other-window
 "g" 'dumb-jump-go
 "b" 'dumb-jump-back
 )

(general-global-spc-menu-definer
 "counsel-file" "f"
 "c" '(:ignore t :which-key "copy")
 "cc" 'my/put-current-filename-to-clipboard
 "cp" 'my/put-current-path-to-clipboard
 "dd" 'counsel-fd-dired-jump
 "df" 'counsel-fd-file-jump
 "f" 'counsel-find-file
 "v" 'vlf
 "z" 'counsel-fzf
 "p" 'counsel-projectile
 "r" 'counsel-recentf
 "g" 'counsel-git
 "l" 'counsel-locate
 "1" 'flycheck-list-errors
 )

(general-global-spc-menu-definer
 "execute" "e"
 "e" 'my/vterm-execute-region-or-current-line
 "E" 'my/eshell-execute-current-line)

(general-global-spc-menu-definer
 "tab-bar/treemacs" "t"
 "2" 'tab-new
 "1" 'tab-close-other
 "0" 'tab-close
 "o" 'tab-next
 "m" 'tab-move
 "r" 'tab-rename
 "\r" 'tab-bar-select-tab-by-name
 "b" 'switch-to-buffer-other-tab
 "f" 'find-file-other-tab
 "\C-f" 'find-file-other-tab
 "T" 'prot-tab-tab-bar-toggle
 "s" 'prot-tab-select-tab-dwim
 "t" 'treemacs
 "e" 'treemacs-display-current-project-exclusively
 )

(general-global-spc-menu-definer
 "org-roam-map" "n"
 "nn" 'notdeft
 "nh" 'notdeft-mode-hydra/body
 "de" 'deft
 "dd" 'my/toggle-org-roam-directory
 "l" 'org-roam-buffer-toggle
 "f" 'org-roam-node-find
 "g" 'org-roam-graph
 "i" 'org-roam-node-insert
 "s" 'org-roam-ui-mode
 )

(general-global-spc-menu-definer
 "VC" "v"
 "=" 'vc-diff
 "+" 'vc-update
 "[" 'diff-hl-previous-hunk
 "]" 'diff-hl-next-hunk
 "A" 'vc-annotate
 "B" 'browse-at-remote
 "D" 'vc-root-diff
 "L" 'vc-print-root-log
 "P" 'vc-push
 "R" 'vc-refresh-state
 "b" 'magit-blame
 "c" 'vc-revert
 "d" 'vc-dir
 "h" 'vc-region-history
 "l" 'vc-print-log
 "p" 'git-messenger:popup-message
 "v" 'vc-next-action
 "SPC" 'diff-hl-mark-hunk
 )

(general-global-spc-menu-definer
 "window" "w"
 "o" 'other-window
 "s" 'ace-swap-window
 "0" 'delete-window
 "1" 'delete-other-windows
 "2" 'my/split-below-and-move
 "3" 'my/split-right-and-move
 "t" 'transpose-frame
 )

(general-global-spc-menu-definer
 "other-frame" "5"
 "o" 'other-frame
 "f" 'find-file-other-frame
 "b" 'switch-to-buffer-other-frame
 "0" 'delete-frame
 "1" 'delete-other-frames
 "d" 'dired-other-frame
 )

(general-global-spc-menu-definer
 "magit" "g"
 "g" 'magit-status
 "G" 'magit-dispatch
 "f" 'magit-file-dispatch
 "i" 'magit-init
 "c" 'magit-clone
 "t" 'magit-todos-list
 "Cr" 'my/magit-copy-remote-url-to-kill-ring
 "Cb" 'my/magit-copy-branch-name-to-kill-ring
 )

(general-global-spc-menu-definer
 "zoom-zop" "z"
 "a" 'zop-up-to-char
 "z" 'zop-to-char
 "x" 'zoom-window-zoom
 )

(general-global-spc-menu-definer
 "run" "r"
 "e" 'embrace-commander
 "rr" 'quickrun
 "rc" 'quickrun-compile-only
 )

(general-global-spc-menu-definer
 "Hydra" "h"
 "C" 'hydra-change-case/body
 "h" 'my-hydra/body
 "b" 'bm-hydra/body
 "M" 'my/hydra-macro/body
 "m" 'my/hydra-multiple-cursors/body
 "w" 'ace-window-hydra/body
 "t" 'hydra-transpose/body)

(provide 'init-general)
