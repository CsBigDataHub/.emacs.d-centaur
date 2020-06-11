(add-to-list 'load-path "~/.emacs.d/lisp/notdeft/")

(require 'notdeft-autoloads)

(when sys/macp
  (setq notdeft-directories '("/Users/ckoneru/GitRepos/my-org-notes/"
                              "/Users/ckoneru/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/")))

(setq notdeft-extension "org")
(setq notdeft-secondary-extensions '("md" "txt"))

(setq notdeft-xapian-program "~/.emacs.d/lisp/notdeft/xapian/notdeft-xapian")

(defun run-local-variables-mode-hooks ()
  "Run hooks for `major-mode' with locals set.
Like `run-mode-hooks', but run later, with any buffer and
directory local variables set."
  (run-hooks (intern (concat (symbol-name major-mode)
                             "-local-variables-hook"))))
(add-hook 'hack-local-variables-hook 'run-local-variables-mode-hooks)

;; A variable determining whether to enable minor mode.
(defcustom notdeft-note-mode-auto-enable nil
  "Whether to enable NotDeft Note minor mode for a buffer."
  :type 'boolean
  :safe 'booleanp)
(make-variable-buffer-local 'notdeft-note-mode-auto-enable)

;; Define a hook for conditionally enabling the NotDeft minor mode.
(defun default-notdeft-hook ()
  "Conditionally enable `notdeft-note-mode'.
 when the buffer local variable
`notdeft-note-mode-auto-enable' is set to a non-nil value."
  (when notdeft-note-mode-auto-enable
    (notdeft-note-mode 1)))

;; Have Org mode files respect the flag. A hook like this should be
;; set for all NotDeft note file types, and no others.
(add-hook 'org-mode-local-variables-hook 'default-notdeft-hook)

(defun my-notdeft-add-directory-local-variables ()
  "Add `notdeft-note-mode-auto-enable' flag.
Add it for all `notdeft-directories'."
  (interactive)
  (dolist (dir notdeft-directories)
    (make-directory dir t)
    (let ((default-directory dir))
      (add-dir-local-variable nil 'notdeft-note-mode-auto-enable t))))

;; Extras for Org mode.
(eval-after-load 'org (lambda () (require 'notdeft-org)))

;; Add global bindings for NotDeft. To do that, bind a custom keymap
;; that inherits from NotDeft's, one that we can use to override and
;; add to the predefined set of bindings.
(require 'notdeft-global)
(defvar my-notdeft-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(a) (d) (l) (v)]
      'my-notdeft-add-directory-local-variables)
    (define-key map [(l)] 'notdeft-org-link-existing-note) ;; l for link
    (define-key map [(n)] 'notdeft-org-link-new-file) ;; n for new
    (define-key map [(s)] 'org-store-link) ;; s for store
    (define-key map [(S)] 'notdeft-org-store-deft-link) ;; s for store
    (define-key map [(*)] 'notdeft-org-open-heading-as-query)
    (set-keymap-parent map 'notdeft-global-map)
    map)
  "Custom keymap for accessing NotDeft functionality.

\\{my-notdeft-global-map}")
(fset 'my-notdeft-global-map my-notdeft-global-map)
(global-set-key (kbd "C-c d n") 'my-notdeft-global-map)

;; Add Org-specific bindings that are also usable in a NotDeft buffer.
(eval-after-load 'notdeft
  (lambda ()
    (define-key notdeft-mode-map (kbd "C-c S")
      'notdeft-org-store-deft-link)))

(require 'hydra nil t)
(when (featurep 'hydra)
  ;; Augment `notdeft-mode' bindings with a hydra.
  (autoload 'notdeft-mode-hydra/body "notdeft-mode-hydra" nil t)
  (eval-after-load 'notdeft
    (lambda ()
      (define-key notdeft-mode-map (kbd "C-c d n h")
        'notdeft-mode-hydra/body)))

  ;; Augment the global NotDeft keymap with a hydra also.
  (autoload 'notdeft-global-hydra/body "notdeft-global-hydra" nil t)
  (define-key my-notdeft-global-map [(h)] 'notdeft-global-hydra/body))

(provide 'init-notdeft)
