;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2010-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Python configurations.
;;

;;; Code:

;; Python Mode
;; Install: pip install pyflakes autopep8
(use-package python
  :ensure nil
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  ;; :mode-hydra ;; moved to init-lsp-hydra.el
  ;; (python-mode
  ;;  (:title "Python Commands")
  ;;  (
  ;;   "Buffer"
  ;;   (("d" lsp-describe-thing-at-point)
  ;;    ("bf" lsp-format-buffer)
  ;;    ("bb" blacken-buffer "black format")
  ;;    ("ip" lsp-info-under-point)
  ;;    ("m" lsp-ui-imenu)
  ;;    ("q" nil "quit"))
  ;;   "Errors"
  ;;   (("e" hydra-flycheck/body)
  ;;    ("lf" lsp-ui-flycheck-list))
  ;;   "Refactor"
  ;;   (("rs" lsp-rename))
  ;;   "Find"
  ;;   (("fd" lsp-ui-peek-find-definitions)
  ;;    ("fi" lsp-ui-peek-find-implementation)
  ;;    ("fr" lsp-ui-peek-find-references)
  ;;    ("fs" lsp-ui-peek-find-workspace-symbol))
  ;;   "Go-to/Jump"
  ;;   (("gi" lsp-goto-implementation)
  ;;    ("gt" lsp-goto-type-definition)
  ;;    ("jn" lsp-ui-peek-jump-forward)
  ;;    ("jp" lsp-ui-peek-jump-backward))
  ;;   ))
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  ;; (with-eval-after-load 'exec-path-from-shell
  ;;   (exec-path-from-shell-copy-env "PYTHONPATH"))

  ;; Live Coding in Python
  (use-package live-py-mode))

;;; my-personal-config
(use-package virtualenvwrapper
  ;; Automatically switch python venv
  :hook (projectile-after-switch-project . venv-projectile-auto-workon)
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location "~/.virtualenvs/")
  (setq projectile-switch-project-action 'venv-projectile-auto-workon)
  (setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
  )

(use-package blacken
  :after (python)
  :init
  (setq-default blacken-fast-unsafe t)
  (setq-default blacken-line-length 80)
  (add-hook 'python-mode-hook #'blacken-mode))

(use-package pip-requirements
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package py-autopep8)


(use-package pyenv-mode
  :if
  (executable-find "pyenv")
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  ;; (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ;; ("C-x p e" . pyenv-activate-current-project)
  )

(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

;; NOTES: This hook is breaking doom-mode-line at init if `pyenv global systems`
;; (add-hook 'after-init-hook 'pyenv-init)

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

(add-hook 'projectile-after-switch-project-hook 'pyenv-activate-current-project)

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs/")
  :config
  ;; (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package python-mode
  :ensure nil
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . yas-minor-mode))


;;; my-personal-config end

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
