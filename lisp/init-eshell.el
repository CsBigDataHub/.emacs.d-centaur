;; init-eshell.el --- Initialize eshell configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
  :custom
  (eshell-visual-commands '("ssh" "less" "top" "man"))
  (eshell-visual-options '(("git" "commit" "log")))
  :hook (eshell-mode . (lambda ()
                         (bind-key "C-l" 'eshell/clear eshell-mode-map)
                         (bind-key "C-r" 'counsel-esh-history eshell-mode-map) ;; my-personal-conf
                         ;; Aliases
                         (eshell/alias "f" "find-file $1")
                         (eshell/alias "fo" "find-file-other-window $1")
                         (eshell/alias "d" "dired $1")
                         (eshell/alias "l" "ls -lFh")
                         (eshell/alias "ll" "ls -ltrah")
                         (eshell/alias "la" "ls -lAFh")
                         (eshell/alias "lr" "ls -tRFh")
                         (eshell/alias "lrt" "ls -lFcrt")
                         (eshell/alias "lsa" "ls -lah")
                         (eshell/alias "lt" "ls -ltFh")))
  :config
  ;; (with-eval-after-load 'exec-path-from-shell
  ;;   (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY" "GRADLE_HOME" "GROOVY_HOME" "JAVA_HOME" "MAVEN_HOME" "SBT_HOME" "SCALA_HOME" "WORKON_HOME" "PYENV_ROOT")))
  ;; (setq eshell-visual-commands '("ssh" "less" "top" "man")
  ;;       ;; eshell-visual-subcommands '(("git" "log" "l" "diff" "show"))
  ;;       eshell-visual-options '(("git" "commit" "log"))
  ;;       )
  (with-no-warnings
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
    :custom-face
    (epe-pipeline-host-face ((t (:foreground "tomato1"))))
    (epe-pipeline-time-face ((t (:foreground "DarkOrchid1"))))
    :init (setq eshell-highlight-prompt nil
                eshell-prompt-function #'epe-theme-pipeline))

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

(when sys/linuxp
  (add-hook 'eshell-mode-hook (lambda ()
                                (eshell/alias "ad" "sudo apt update")
                                (eshell/alias "au" "sudo apt upgrade")
                                (eshell/alias "ai" "sudo apt install $1"))))

;;; my-personal from https://protesilaos.com/dotemacs/

(declare-function ffap-file-at-point 'find-file-at-point)

(defun my/eshell-insert-file-at-point ()
  "Insert (cat) contents of file at point."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (progn
          (goto-char (point-max))
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

(defun prot-eshell--command-prompt-output ()
  "Capture last command prompt and its output."
  (let ((beg (save-excursion
               (goto-char (eshell-beginning-of-input))
               (goto-char (point-at-bol)))))
    (when (derived-mode-p 'eshell-mode)
      (buffer-substring-no-properties beg (eshell-end-of-output)))))

(defun my/eshell-put-last-output-to-buffer ()
  "Produce a buffer with output of last `eshell' command."
  (interactive)
  (let ((eshell-output (prot-eshell--command-prompt-output)))
    (with-current-buffer (get-buffer-create  "*last-eshell-output*")
      (goto-char (point-max))
      (unless (eq (point-min) (point-max))
        (insert (format "\n%s\n\n" "* * *")))
      (goto-char (point-at-bol))
      (insert eshell-output)
      (switch-to-buffer-other-window (current-buffer)))))

(defun prot-eshell-find-subdirectory-recursive ()
  "Recursive `eshell/cd' to subdirectory.
This command has the potential for infinite recursion: use it
wisely or prepare to call `eshell-interrupt-process'."
  (interactive)
  (let* ((dir (abbreviate-file-name (eshell/pwd)))
         (contents (directory-files-recursively dir ".*" t nil nil))
         (dirs (cl-remove-if-not (lambda (x)
                                   (or (file-directory-p x)
                                       (string-match-p "\\.git" x)))
                                 contents))
         (selection (completing-read
                     (format "Find sub-dir from %s: "
                             (propertize dir 'face 'success))
                     dirs nil t)))
    (insert selection)
    (eshell-send-input)))

(defun prot-eshell-root-dir ()
  "Switch to the root directory of the present project."
  (interactive)
  (let ((root (or (vc-root-dir)
                  (locate-dominating-file "." ".git"))))
    (if root
        (progn
          (insert root)
          (eshell-send-input))
      (user-error "Cannot find a project root here"))))

(defun my/eshell-execute-current-line ()
  "Insert text of current line or selected in eshell and execute."
  (interactive)
  (eval-when-compile (require 'subr-x))
  (eval-when-compile (require 'eshell))
  (if (use-region-p)
      (setq min (region-beginning)
            max (region-end))
    (setq min (point-at-bol)
          max (point-at-eol)))
  (let ((command (string-trim (buffer-substring min max))))
    (let ((buf (current-buffer)))
      (unless (get-buffer eshell-buffer-name)
        (eshell))
      (display-buffer eshell-buffer-name t)
      (switch-to-buffer-other-window eshell-buffer-name)
      (goto-char (point-max))
      (eshell-kill-input)
      (insert command)
      (eshell-send-input)
      (goto-char (point-max))
      (switch-to-buffer-other-window buf)
      )))

(global-set-key (kbd "C-x E e") 'my/eshell-execute-current-line)

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

(add-hook 'eshell-mode-hook (lambda()
                              (local-unset-key (kbd "M-s"))))

(use-package em-hist
  :ensure nil
  :after esh-mode
  :bind (:map eshell-mode-map
         ("C-c e e" . my/eshell-insert-file-at-point)
         ("C-c e f" . my/eshell-find-file-at-point)
         ("C-c e >" . my/eshell-complete-redirect-to-buffer)
         ("C-c e p" . my/eshell-file-parent-dir)
         ("C-c e c" . my/eshell-kill-save-file-at-point)
         ("C-c e o" . my/eshell-put-last-output-to-buffer)
         ("M-s i" . eshell-next-matching-input)
         )
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 10000)
  (setq eshell-save-history-on-exit t))

(use-package pcomplete-extension
  :after pcomplete)

(use-package pcmpl-git
  :after pcomplete)

(use-package pcmpl-pip
  :after pcomplete)

(use-package pcmpl-args
  :after pcomplete)

(use-package pcmpl-homebrew
  :after pcomplete)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :demand t ;; Install if not already installed.
  :custom-face
  (eshell-syntax-highlighting-shell-command-face ((t (:foreground "spring green"))))
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

;;https://www.reddit.com/r/emacs/comments/gksqhl/emacs_eshell_demo/fqtveuc?utm_source=share&utm_medium=web2x
;; (defun eshell/in-term (prog &rest args)
;;   (switch-to-buffer
;;    (apply #'make-term (format "in-term %s %s" prog args) prog nil args))
;;   (term-mode)
;;   (term-char-mode))

;; WORKAROUND: https://github.com/zwild/eshell-prompt-extras/issues/32
;; (defvar my-ansi-escape-re
;;   (rx (or ?\233 (and ?\e ?\[))
;;       (zero-or-more (char (?0 . ?\?)))
;;       (zero-or-more (char ?\s ?- ?\/))
;;       (char (?@ . ?~))))

;; (defun my-nuke-ansi-escapes (beg end)
;;   (save-excursion
;;     (goto-char beg)
;;     (while (re-search-forward my-ansi-escape-re end t)
;;       (replace-match ""))))

;; (defun my-eshell-nuke-ansi-escapes ()
;;   (my-nuke-ansi-escapes eshell-last-output-start eshell-last-output-end))

;; ( add-hook 'eshell-output-filter-functions 'my-eshell-nuke-ansi-escapes t)

;; Save command history when commands are entered
(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

(add-hook 'eshell-post-command-hook
          '(lambda () (setenv "TERM" "dumb")))

(setenv "PAGER" "cat")

;; https://gist.github.com/gregsexton/dd2d6c304d06fc3e6833
;; Company backend for completing eshell history

(defun company-eshell-history (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
                 (let ((word (company-grab-word)))
                   (save-excursion
                     (eshell-bol)
                     (and (looking-at-p (s-concat word "$")) word)))))
    (candidates (remove-duplicates
                 (->> (ring-elements eshell-history-ring)
                      (remove-if-not (lambda (item) (s-prefix-p arg item)))
                      (mapcar 's-trim))
                 :test 'string=))
    (sorted t)))

(defun my-company-eshell-mode-company-hook ()
  (set (make-local-variable 'company-backends) '((company-files)
                                                 (company-dabbrev
                                                  company-eshell-history
                                                  company-capf
                                                  ))))

(add-hook 'eshell-mode-hook 'my-company-eshell-mode-company-hook)

;;; my-personal ends here

(provide 'init-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eshell.el ends here
