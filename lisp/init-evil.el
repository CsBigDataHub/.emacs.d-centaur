;; init-evil.el --- Initialize evil configurations.	-*- lexical-binding: t -*-

(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t) ;;full blown emacs in insert mode
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  )
;;(setq evil-disable-insert-state-bindings t) ;;full blown emacs in insert mode
;;(setq evil-want-keybinding nil)
;;(require 'evil)
;;(evil-mode 1)

(use-package evil-matchit             ; vi-% for more than {[""]}
  :init
  (global-evil-matchit-mode 1))

;; Exactly like tpopes vim-surround but replacing with isolate which give more features
(use-package evil-surround
  :after general
  :init
  (global-evil-surround-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  )

(use-package embrace
  :commands embrace-commander
  :hook ((LaTeX-mode . embrace-LaTeX-mode-hook)
         (org-mode . embrace-org-mode-hook)
         (emacs-lisp-mode-hook . embrace-emacs-lisp-mode-hook)
         (ruby-mode . embrace-ruby-mode-hook))
  :init
  (setq embrace-semantic-units-alist '((?w . er/mark-word)
                                       (?s . er/mark-symbol)
                                       (?S . er/mark-symbol-with-prefix)
                                       (?a . er/mark-text-sentence)
                                       (?A . er/mark-text-paragraph)
                                       (?n . er/mark-next-accessor)
                                       (?m . er/mark-method-call)
                                       (?Q . er/mark-inside-quotes)
                                       (?q . er/mark-outside-quotes)
                                       (?P . er/mark-inside-pairs)
                                       (?p . er/mark-outside-pairs)
                                       (?c . er/mark-comment)
                                       (?u . er/mark-url)
                                       (?e . er/mark-email)
                                       (?d . er/mark-defun))))


(use-package evil-embrace
  :after evil-surround
  :config
  (evil-embrace-enable-evil-surround-integration))

(defun activate-mark-hook@set-transient-map ()
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "a" #'embrace-commander)
     map)
   #'region-active-p))

(add-hook 'activate-mark-hook #'activate-mark-hook@set-transient-map)

;; Treat underscores '_' part of the words
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; Disable evil-mode in few modes
(evil-set-initial-state 'ibuffer-mode 'emacs)
(cond ((eq system-type 'gnu/linux) (evil-set-initial-state 'mu4e-main-mode 'emacs)))
(cond ((eq system-type 'gnu/linux) (evil-set-initial-state 'mu4e-headers-mode 'emacs)))
(evil-set-initial-state 'bookmark-bmenu-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs)
(evil-set-initial-state 'vc-mode 'emacs)
(evil-set-initial-state 'diff-mode 'emacs)
(evil-set-initial-state 'vc-git-command 'emacs)
(evil-set-initial-state 'vc-git-log-edit-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'helpful-mode 'emacs)
(evil-set-initial-state 'wgrep-change-to-wgrep-mode 'emacs)
(evil-set-initial-state 'flycheck-error-list-mode 'emacs)
(evil-set-initial-state 'imenu-list-major-mode 'emacs)
(evil-set-initial-state 'view-mode 'emacs)
(evil-set-initial-state 'pass-mode 'emacs)
(add-to-list 'evil-insert-state-modes 'view-mode)
(evil-set-initial-state 'calendar-mode 'emacs)
(evil-set-initial-state 'docker-image-mode 'emacs)
(evil-set-initial-state 'docker-network-mode 'emacs)
(evil-set-initial-state 'docker-container-mode 'emacs)
(evil-set-initial-state 'docker-volume-mode 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'notdeft-mode 'emacs)
(evil-set-initial-state 'lsp-ui-flycheck-list-mode 'emacs)
(evil-set-initial-state 'magrant-machine-mode 'emacs)
(evil-set-initial-state 'magrant-box-mode 'emacs)
(evil-set-initial-state 'flymake-diagnostics-buffer-mode 'emacs)

;; (add-hook 'flymake-diagnostics-buffer-mode-hook
;;           (lambda ()
;;             (evil-insert-state)))

(use-package evil-numbers)

(global-set-key (kbd "C-c M-i +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c M-i -") 'evil-numbers/dec-at-pt)

(evil-define-key '(normal visual) 'global (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
(evil-define-key '(normal visual) 'global (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)
(evil-define-key '(normal visual) 'global (kbd "C-<kp-add>") 'evil-numbers/inc-at-pt-incremental)
(evil-define-key '(normal visual) 'global (kbd "C-<kp-subtract>") 'evil-numbers/dec-at-pt-incremental)

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

(use-package evil-exchange)

(use-package evil-goggles
  :init
  (evil-goggles-mode)
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse nil)
  (setq evil-goggles-duration 0.100) ;; default is 0.200

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  ;; (evil-goggles-use-diff-faces)
  (custom-set-faces
   ;; '(evil-goggles-default-face ((t (:inherit 'highlight)))) ;; default is to inherit 'region
   ;; run `M-x list-faces-display` in a fresh emacs to get a list of faces on your emacs
   '(evil-goggles-delete-face ((t (:inherit 'menu))))
   '(evil-goggles-paste-face ((t (:inherit 'custom-saved))))
   '(evil-goggles-yank-face ((t (:inherit 'custom-invalid)))))
  )

;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
;; NOTES: do not need this using hydra
;; (use-package evil-org
;;   :after org
;;   :hook (org-mode . (lambda () evil-org-mode))
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))


(use-package evil-mc-extras
  :config
  (global-evil-mc-extras-mode 1))

(use-package evil-snipe
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'visible
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  :config
  ;; and disable in specific modes
  (push 'Info-mode evil-snipe-disabled-modes)
  (push 'treemacs-mode evil-snipe-disabled-modes)
  (push 'calc-mode evil-snipe-disabled-modes)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;;for shell-pop
(evil-define-key 'insert vterm-mode-map (kbd "<f9>")      #'shell-pop) ;;Added personally
(evil-define-key 'normal vterm-mode-map (kbd "<f9>")      #'shell-pop) ;;Added personally

(add-hook 'vterm-mode-hook
          (lambda ()
            (evil-insert-state)))


;; (add-hook 'evil-mode-hook (lambda()
;;                             (local-unset-key (kbd "M-."))))

;; to replace with `xref-find-definitions'
;; https://emacs.stackexchange.com/questions/29684/conditional-key-binding-evil-vs-slime-conflict-for-m
(define-key evil-normal-state-map (kbd "M-.")
  `(menu-item "" evil-repeat-pop :filter
              ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))


(evil-define-key 'normal lsp-mode-map (kbd "M-.") #'lsp-find-definition)

;; https://blog.meain.io/2020/emacs-highlight-yanked/
(defun my/evil-yank-advice (orig-fn beg end &rest args)
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(advice-add 'evil-yank :around 'my/evil-yank-advice)

(provide 'init-evil)
