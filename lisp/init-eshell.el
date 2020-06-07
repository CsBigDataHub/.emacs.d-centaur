;; init-eshell.el --- Initialize eshell configurations.	-*- lexical-binding: t -*-

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
;; Eshell configurations.
;;

;;; Code:

;; Emacs command shell
(use-package eshell
  :ensure nil
  :defines eshell-prompt-function
  :functions eshell/alias
  :bind (:map eshell-mode-map
         ("C-c e e" . my/eshell-insert-file-at-point)
         ("C-c e f" . my/eshell-find-file-at-point)
         ("C-c e >" . my/eshell-complete-redirect-to-buffer)
         ("C-c e p" . my/eshell-file-parent-dir)
         ("C-c e c" . my/eshell-kill-save-file-at-point)
         ("C-c e o" . my/eshell-put-last-output-to-buffer)
         )
  :hook (eshell-mode . (lambda ()
                         (bind-key "C-l" 'eshell/clear eshell-mode-map)
                         (bind-key "C-r" 'counsel-esh-history eshell-mode-map) ;; my-personal-conf
                         ;; Aliases
                         (eshell/alias "f" "find-file $1")
                         (eshell/alias "fo" "find-file-other-window $1")
                         (eshell/alias "d" "dired $1")
                         (eshell/alias "l" "ls -lFh")
                         (eshell/alias "ll" "ls -l")
                         (eshell/alias "la" "ls -lAFh")
                         (eshell/alias "lr" "ls -tRFh")
                         (eshell/alias "lrt" "ls -lFcrt")
                         (eshell/alias "lsa" "ls -lah")
                         (eshell/alias "lt" "ls -ltFh")))
  :config
  (with-no-warnings
    ;; For compatibility
    (unless (fboundp 'flatten-tree)
      (defalias 'flatten-tree #'eshell-flatten-list))

    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))

    (defun eshell/emacs (&rest args)
      "Open a file (ARGS) in Emacs.  Some habits die hard."
      (if (null args)
          ;; If I just ran "emacs", I probably expect to be launching
          ;; Emacs, which is rather silly since I'm already in Emacs.
          ;; So just pretend to do what I ask.
          (bury-buffer)
        ;; We have to expand the file names or else naming a directory in an
        ;; argument causes later arguments to be looked for in that directory,
        ;; not the starting directory
        (mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))
    (defalias 'eshell/e #'eshell/emacs)
    (defalias 'eshell/ec #'eshell/emacs)

    (defun eshell/ebc (&rest args)
      "Compile a file (ARGS) in Emacs. Use `compile' to do background make."
      (if (eshell-interactive-output-p)
          (let ((compilation-process-setup-function
                 (list 'lambda nil
                       (list 'setq 'process-environment
                             (list 'quote (eshell-copy-environment))))))
            (compile (eshell-flatten-and-stringify args))
            (pop-to-buffer compilation-last-buffer))
        (throw 'eshell-replace-command
               (let ((l (eshell-stringify-list (flatten-tree args))))
                 (eshell-parse-command (car l) (cdr l))))))
    (put 'eshell/ebc 'eshell-no-numeric-conversions t)

    (defun eshell-view-file (file)
      "View FILE.  A version of `view-file' which properly rets the eshell prompt."
      (interactive "fView file: ")
      (unless (file-exists-p file) (error "%s does not exist" file))
      (let ((buffer (find-file-noselect file)))
        (if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class)
                'special)
            (progn
              (switch-to-buffer buffer)
              (message "Not using View mode because the major mode is special"))
          (let ((undo-window (list (window-buffer) (window-start)
                                   (+ (window-point)
                                      (length (funcall eshell-prompt-function))))))
            (switch-to-buffer buffer)
            (view-mode-enter (cons (selected-window) (cons nil undo-window))
                             'kill-buffer)))))

    (defun eshell/less (&rest args)
      "Invoke `view-file' on a file (ARGS).

\"less +42 foo\" will go to line 42 in the buffer for foo."
      (while args
        (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
            (let* ((line (string-to-number (match-string 1 (pop args))))
                   (file (pop args)))
              (eshell-view-file file)
              (forward-line line))
          (eshell-view-file (pop args)))))
    (defalias 'eshell/more #'eshell/less))

  ;;  Display extra information for prompt
  (use-package eshell-prompt-extras
    :after esh-opt
    :defines eshell-highlight-prompt
    :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
    :init (setq eshell-highlight-prompt nil
                eshell-prompt-function #'epe-theme-lambda))

  ;; Fish-like history autosuggestions
  (use-package esh-autosuggest
    :defines ivy-display-functions-alist
    :bind (:map eshell-mode-map
           ([remap eshell-pcomplete] . completion-at-point))
    :hook ((eshell-mode . esh-autosuggest-mode)
           (eshell-mode . eshell-setup-ivy-completion))
    :init (defun eshell-setup-ivy-completion ()
            "Setup `ivy' completion in `eshell'."
            (setq-local ivy-display-functions-alist
                        (remq (assoc 'ivy-completion-in-region
                                     ivy-display-functions-alist)
                              ivy-display-functions-alist))))

  ;; `eldoc' support
  (use-package esh-help
    :init (setup-esh-help-eldoc))

  ;; `cd' to frequent directory in `eshell'
  (use-package eshell-z
    :hook (eshell-mode . (lambda () (require 'eshell-z)))))

;;; my-personal from https://protesilaos.com/dotemacs/

(declare-function ffap-file-at-point 'find-file-at-point)

(defun my/eshell-insert-file-at-point ()
  "Insert (cat) contents of file at point."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (progn
          (end-of-buffer)
          (insert (concat "cat " file))
          (eshell-send-input))
      (user-error "No file at point"))))

(defun my/eshell-kill-save-file-at-point ()
  "Add to kill-ring the absolute path of file at point."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (kill-new (concat (eshell/pwd) "/" file))
      (user-error "No file at point"))))

(defun my/eshell-find-file-at-point ()
  "Run `find-file' for file at point (ordinary file or dir).
Recall that this will produce a `dired' buffer if the file is a
directory."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (find-file file)
      (user-error "No file at point"))))

(defun my/eshell-file-parent-dir ()
  "Open `dired' with the parent directory of file at point."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (dired (file-name-directory file))
      (user-error "No parent dir for file to jump to"))))

(defun my/eshell-complete-redirect-to-buffer ()
  "Complete the syntax for appending to a buffer via `eshell'."
  (interactive)
  (insert
   (concat " >>> #<"
           (format "%s"
                   (read-buffer-to-switch "Switch to buffer: "))
           ">")))

(defun my/eshell-put-last-output-to-buffer ()
  "Produce a buffer with output of last `eshell' command."
  (interactive)
  (let ((eshell-output (kill-region (eshell-beginning-of-output)
                                    (eshell-end-of-output))))
    (with-current-buffer (get-buffer-create  "*last-eshell-output*")
      (erase-buffer)
      (yank)           ; TODO do it with `insert' and `delete-region'?
      (switch-to-buffer-other-window (current-buffer)))))

(use-package esh-module
  :ensure nil
  :config
  (setq eshell-modules-list             ; Needs review
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-tramp
          eshell-unix)))

(use-package em-dirs
  :ensure nil
  :after esh-mode
  :config
  (setq eshell-cd-on-directory t))

(use-package em-tramp
  :ensure nil
  :after esh-mode
  :config
  (setq password-cache t)
  (setq password-cache-expiry 600))

(use-package em-hist
  :ensure nil
  :after esh-mode
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t))

;;https://www.reddit.com/r/emacs/comments/gksqhl/emacs_eshell_demo/fqtveuc?utm_source=share&utm_medium=web2x
(defun eshell/in-term (prog &rest args)
  (switch-to-buffer
   (apply #'make-term (format "in-term %s %s" prog args) prog nil args))
  (term-mode)
  (term-char-mode))

;;; my-personal ends here

(provide 'init-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eshell.el ends here
