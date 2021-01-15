;; init-lisp-clojure.el --- Initialize configurations for lisp and clojure.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2021 Chetan Koneru

;; Author: Chetan Koneru
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
;; Lisp and Clojure configurations.
;;

;;; Code:

;;; Lisp configuration

(defvar inferior-lisp-program  "sbcl --dynamic-space-size 1024")

;; common lisp
(use-package lisp-mode
  :ensure nil
  :mode (("\\.cl\\'" . lisp-mode)
         ("\\.lisp\\'" . lisp-mode)
         ("\\.sbclrc\\'" . lisp-mode)))

(use-package slime-company
  :commands slime-company)

(use-package slime
  :commands (slime slime-lisp-mode-hook slime-mode)
  :hook
  (slime-repl-mode-hook . rainbow-delimiters-mode)
  (slime-repl-mode-hook . smartparens-mode)
  :custom
  (inferior-lisp-program "sbcl --dynamic-space-size 1024")
  (slime-net-coding-system 'utf-8-unix)
  (slime-complete-symbol*-fancy t)
  (slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  :config
  (setq slime-contribs
        '(slime-fancy slime-asdf slime-quicklisp slime-cl-indent))

  (slime-setup '(slime-fancy slime-asdf slime-quicklisp slime-company))
  )

;; <return> or <enter> key in repl is not company-complete-selection
;; Need to explicitly add it.
;; https://stackoverflow.com/questions/38120498/key-binding-in-slime-emacs
;; https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
(defun my-slime-repl-mode-keybindings ()
  "For use in `slime-mode-hook' and 'slime-repl-mode-hook."
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "<return>") #'company-complete-selection)
    )
  ) ;; end of defun my-slime-mode-keybindings()
(add-hook 'slime-repl-mode-hook     #'my-slime-repl-mode-keybindings)

;; (defun my-start-sly()
;;   "Start sly when opening a Lisp file."
;;   (unless (sly-connected-p)
;;     (save-excursion (sly))))

;; (use-package sly
;;   :hook
;;   (text-mode . turn-on-auto-fill)
;;   (lisp-mode . turn-on-auto-fill)
;;   ;; (sly-mode . my-start-sly)
;;   :bind (:map sly-mode-map
;;          ("C-c C-e" . sly-load-file)
;;          ("C-c C-k" . sly-compile-and-load-file)
;;          ("TAB"     . sly-mrepl-indent-and-complete-symbol)
;;          ("C-c i"   . sly-inspect)
;;          ("C-c C-s" . sly-selector))
;;   :init
;;   (setq sly-contribs '(sly-fancy
;;                        sly-indentation
;;                        sly-autodoc
;;                        sly-sbcl-exts
;;                        sly-scratch))
;;   :config
;;   (sly-setup '(sly-fancy sly-asdf sly-quicklisp))
;;   (setq sly-autodoc-use-multiline t
;;         sly-complete-symbol*-fancy t
;;         sly-kill-without-query-p t
;;         sly-repl-history-remove-duplicates t
;;         sly-repl-history-trim-whitespaces t
;;         sly-net-coding-system 'utf-8-unix))

;; (use-package sly-macrostep
;;   :after sly)

;; (use-package sly-repl-ansi-color
;;   :config (push 'sly-repl-ansi-color sly-contribs))

;; (use-package sly-quicklisp
;;   :config
;;   (push 'sly-quicklisp sly-contribs))

;; (use-package sly-asdf
;;   :config (push 'sly-asdf sly-contribs))

(use-package common-lisp-snippets
  :defer t
  :after (yasnippet))

(provide 'init-lisp-clojure)
