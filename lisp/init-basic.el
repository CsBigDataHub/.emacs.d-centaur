;; init-basic.el --- Better default configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2020 Vincent Zhang

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
;; Better defaults.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Personal information
(setq user-full-name centaur-full-name
      user-mail-address centaur-mail-address)

;; Key Modifiers
(with-no-warnings
  (cond
   (sys/win32p
    ;; make PC keyboard's Win key or other to type Super or Hyper
    ;; (setq w32-pass-lwindow-to-system nil)
    (setq w32-lwindow-modifier 'super     ; Left Windows key
          w32-apps-modifier 'hyper)       ; Menu/App key
    (w32-register-hot-key [s-t]))
   (sys/mac-port-p
    ;; Compatible with Emacs Mac port
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
    (bind-keys ([(super a)] . mark-whole-buffer)
               ([(super c)] . kill-ring-save)
               ([(super l)] . goto-line)
               ([(super q)] . save-buffers-kill-emacs)
               ([(super s)] . save-buffer)
               ([(super v)] . yank)
               ([(super w)] . delete-frame)
               ([(super z)] . undo)))))

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; Environment
;;(when (or sys/mac-x-p sys/linux-x-p)
;;  (use-package exec-path-from-shell
;;    :init
;;    (setq exec-path-from-shell-check-startup-files nil
;;          exec-path-from-shell-variables '("PATH" "MANPATH")
;;          exec-path-from-shell-arguments '("-l"))
;;    (exec-path-from-shell-initialize)
;;    :config ;;my-personal config added
;;    (exec-path-from-shell-copy-env "LC_ALL")
;;    (exec-path-from-shell-copy-env "LANG")
;;    (exec-path-from-shell-copy-env "LC_TYPE")
;;    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
;;    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
;;    (exec-path-from-shell-copy-env "SHELL")
;;    (exec-path-from-shell-copy-env "JAVA_HOME")
;;    (defun set-exec-path-from-shell-PATH ()
;;      "Sets the exec-path to the same value used by the user shell"
;;      (let ((path-from-shell
;;             (replace-regexp-in-string
;;              "[[:space:]\n]*$" ""
;;              (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
;;        (setenv "PATH" path-from-shell)
;;        (setq exec-path (split-string path-from-shell path-separator))))
;;
;;    ;; call function now
;;    (set-exec-path-from-shell-PATH)
;;
;;    (cond ((display-graphic-p)
;;           ;; A known problem with GUI Emacs on MacOS: it runs in an isolated
;;           ;; environment, so envvars will be wrong. That includes the PATH
;;           ;; Emacs picks up. `exec-path-from-shell' fixes this. This is slow
;;           ;; and benefits greatly from compilation.
;;           (setq exec-path
;;                 (or (eval-when-compile
;;                       (when (require 'exec-path-from-shell nil t)
;;                         (setq exec-path-from-shell-check-startup-files nil)
;;                         (nconc exec-path-from-shell-variables '("GOPATH" "GOROOT" "PYTHONPATH"))
;;                         (exec-path-from-shell-initialize)
;;                         exec-path))
;;                     exec-path))))
;;    ))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-variables '("PATH" "MANPATH")
          exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY" "GRADLE_HOME" "GROOVY_HOME" "JAVA_HOME" "MAVEN_HOME" "SBT_HOME" "SCALA_HOME" "WORKON_HOME" "PYENV_ROOT"))
    (exec-path-from-shell-initialize)))

;; Start server
(use-package server
  :ensure nil
  :if centaur-server
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(when emacs/>=27p
  (use-package so-long
    :ensure nil
    :hook (after-init . global-so-long-mode)
    :config (setq so-long-threshold 400)))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode 'text-mode
              fill-column 120
              tab-width 4
              indent-tabs-mode nil)     ; Permanently indent with spaces, never with TABs

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; Fullscreen
(when (display-graphic-p)
  (add-hook 'window-setup-hook #'fix-fullscreen-cocoa)
  (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
             ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
             ("S-s-<return>" . toggle-frame-fullscreen)
             ("M-S-<return>" . toggle-frame-fullscreen)))

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
