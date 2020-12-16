;;init-window-conf.el --- Initialize eyebrowse configurations.	-*- lexical-binding: t -*-

;; using `tab-bar' in place of `eyebrowse'
;; (use-package eyebrowse
;;   :demand
;;   :config
;;   (eyebrowse-mode)
;;   )
;; (use-package winds
;;   :demand
;;   :custom
;;   (winds-default-ws 1)
;;   (winds-default-cfg 1)
;;   )

(use-package tab-bar
  :ensure nil
  :init
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  :config
  (tab-bar-mode -1)
  (tab-bar-history-mode -1) ;; This can be handled by `winner-mode'. so disabling
  (defun set-name-if-in-project ()
    (format "%s"
            (if (projectile-project-p)
                (format "[%s] - %s" (projectile-project-name) (tab-bar-tab-name-current))
              (tab-bar-tab-name-current)
              )
            )
    )
  (setq tab-bar-tab-name-function 'set-name-if-in-project)

  (defun prot-tab--tab-bar-tabs ()
    "Return a list of `tab-bar' tabs, minus the current one."
    (mapcar (lambda (tab)
              (alist-get 'name tab))
            (tab-bar--tabs-recent)))

  (defun prot-tab-tab-bar-toggle ()
    "Toggle `tab-bar' presentation."
    (interactive)
    (if (bound-and-true-p tab-bar-mode)
        (progn
          (setq tab-bar-show nil)
          (tab-bar-mode -1))
      (setq tab-bar-show t)
      (tab-bar-mode 1)))

  (defun prot-tab-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (prot-tab--tab-bar-tabs)))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t))))))

  :bind (("<C-tab>" . tab-bar-select-tab-by-name)
         ("C-x t t" . prot-tab-tab-bar-toggle)
         ("C-x t s" . prot-tab-select-tab-dwim)
         ))

(provide 'init-window-conf)
