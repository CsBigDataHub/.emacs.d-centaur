;;;init-general.el --- Initialize key-bindings for general.	-*- lexical-binding: t -*-

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
  "uu" 'undo-tree-visualize
  "mc" 'my/open-config
  "tt" 'treemacs
  "te" 'treemacs-display-current-project-exclusively
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
 "z" 'counsel-fzf
 "p" 'counsel-projectile
 "r" 'counsel-recentf
 "g" 'counsel-git
 "l" 'counsel-locate
 )

(general-global-spc-menu-definer
 "org-roam-map" "n"
 "nn" 'notdeft
 "nh" 'notdeft-mode-hydra/body
 "de" 'deft
 "dd" 'my/toggle-org-roam-directory
 "l" 'org-roam
 "f" 'org-roam-find-file
 "g" 'org-roam-graph
 "i" 'org-roam-insert
 "s" 'org-roam-server-mode
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
 "w1" 'eyebrowse-switch-to-window-config-0
 "w1" 'eyebrowse-switch-to-window-config-1
 "w2" 'eyebrowse-switch-to-window-config-2
 "w3" 'eyebrowse-switch-to-window-config-3
 "w4" 'eyebrowse-switch-to-window-config-4
 "w5" 'eyebrowse-switch-to-window-config-5
 "w6" 'eyebrowse-switch-to-window-config-6
 "w7" 'eyebrowse-switch-to-window-config-7
 "w8" 'eyebrowse-switch-to-window-config-8
 "w9" 'eyebrowse-switch-to-window-config-9
 "w." 'eyebrowse-last-window-config
 "w," 'eyebrowse-switch-to-window-config
 "w;" 'eyebrowse-rename-window-config
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
 "rr" 'quickrun
 "rc" 'quickrun-compile-only
 )

(provide 'init-general)
