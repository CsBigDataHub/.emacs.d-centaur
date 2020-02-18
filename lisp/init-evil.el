
(use-package evil
 ;; :init ((setq evil-disable-insert-state-bindings t)
 ;;        )  ;;full blown emacs in insert mode
  ;;:config
  ;;(evil-mode 1)
  )
(setq evil-disable-insert-state-bindings t)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)

(use-package evil-matchit             ; vi-% for more than {[""]}
  :init
  (global-evil-matchit-mode 1))

;;(use-package evil-surround            ; Exactly like tpopes vim-surround
;;  :init
;;  (global-evil-surround-mode))

;; Treat underscores '_' part of the words
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(evil-set-initial-state 'ibuffer-mode 'emacs)
(evil-set-initial-state 'bookmark-bmenu-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'wgrep-change-to-wgrep-mode 'emacs)

(use-package evil-numbers
  )

(global-set-key (kbd "C-c C-i +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c C-i -") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "C-c C-i +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c C-i -") 'evil-numbers/dec-at-pt)

;; See Doc at https://github.com/edkolev/evil-lion
(use-package evil-lion
  :config
  (evil-lion-mode)
  :bind (:map evil-normal-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)
         :map evil-visual-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)))

(use-package evil-exchange
  )

(use-package evil-goggles
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces)
  )

;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; my-personal-config
;;(use-package linum-relative
;;  :config
;;  (linum-relative-global-mode 1)
;;  (setq linum-relative-backend 'display-line-numbers-mode)
;;  )
     ;;; my-personal-config-end-here

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))


(use-package evil-mc-extras
  :config
  (global-evil-mc-extras-mode 1))


(provide 'init-evil)