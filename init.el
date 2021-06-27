;;; init.el --- A Fancy and Fast Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2006-2021 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d
;; Version: 5.9.0
;; Keywords: .emacs.d centaur

;;
;;                          `..`
;;                        ````+ `.`
;;                    /o:``   :+ ``
;;                .+//dho......y/..`
;;                `sdddddhysso+h` ``
;;                  /ddd+`..` +. .`
;;                 -hos+    `.:```
;;               `./dddyo+//osso/:`
;;             `/o++dddddddddddddod-
;;            `// -y+:sdddddsddsy.dy
;;                /o   `..```h+`y+/h+`
;;                .s       `++``o:  ``
;;                        `:- `:-
;;
;;   CENTAUR EMACS - Enjoy Programming & Writing

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
;; Centaur Emacs - A Fancy and Fast Emacs Configuration.
;;


;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Speed up startup
(defvar centaur-gc-cons-threshold (if (display-graphic-p) 64000000 1600000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar centaur-gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold centaur-gc-cons-upper-limit
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold centaur-gc-cons-threshold
                  gc-cons-percentage 0.1)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (add-hook 'minibuffer-setup-hook
                      (lambda ()
                        "Enlarge gc cons threshold while entering minibuffer."
                        (setq gc-cons-threshold centaur-gc-cons-upper-limit)))
            (add-hook 'minibuffer-exit-hook
                      (lambda ()
                        "Recover gc cons threshold while exiting minibuffer."
                        (setq gc-cons-threshold centaur-gc-cons-threshold)))))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

;; Preferences
;; (require 'init-env)

(require 'init-basic)
(require 'init-hydra)

(require 'init-org)
(require 'init-ui)
(require 'init-edit)
(require 'init-ivy)
(require 'init-company)
;; (require 'init-yasnippet)

(require 'init-bookmark)
(require 'init-calendar)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-persp)
(require 'init-window)
(require 'init-treemacs)

(require 'init-eshell)
(require 'init-shell)

(require 'init-markdown)
(require 'init-reader)

(require 'init-docker)
(require 'init-utils)

;; Programming
(require 'init-vcs)
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-lsp-hydra)

(require 'init-prog)
(require 'init-elisp)
;;(require 'init-c)
(require 'init-go)
(require 'init-rust)
(require 'init-python)
;;(require 'init-ruby)
;;(require 'init-dart)
;;(require 'init-elixir)
(require 'init-web)

;;my-personal
(require 'init-bm)
(require 'init-centaur-tabs)
;; (require 'init-deadgrep)
;; (require 'init-emacro)
(require 'init-window-conf)
(require 'init-evil)
(require 'init-engine)
;; (require 'init-fira-code-mode)
(require 'init-general)
(require 'init-gradle)
(require 'init-hippie-expand)
(require 'init-image-plus)
(require 'init-kubernetes)
;; (require 'init-lisp-clojure)
(require 'init-my-cust-fun)
(require 'init-my-personal)
(require 'init-no-littering)
(require 'init-pandoc)
(require 'init-reveal-js)
(require 'init-straight)
(require 'init-notdeft)
;; (require 'init-quelpa)
(require 'init-terraform)
(require 'init-smartparens)
(require 'init-shr)
(require 'init-plantuml)
(require 'init-pass)
(require 'init-multi-compile)
(require 'init-multi-scratch)
(require 'init-savekill)

( cond ((eq system-type 'darwin) (require 'init-macos)))
( cond ((eq system-type 'gnu/linux) (require 'init-linux)))
;; ( cond ((eq system-type 'gnu/linux) (require 'init-mail)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
