;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Vincent Zhang

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
;; Version control systems.
;;

;;; Code:

(require 'init-const)

;; Git
(use-package magit
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup)
         (:map magit-status-mode-map
          ("q" . magit-quit-session)
          )
         ("C-c M-g" . magit-file-popup))
  :init (setq magit-diff-refine-hunk t)
  :config
  (setq-default magit-diff-refine-hunk 'all)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (when sys/macp
    (setq magit-git-executable "/usr/local/bin/git"))
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  (when (fboundp 'transient-append-suffix)
    ;; Add switch: --tags
    (transient-append-suffix 'magit-fetch
      "-p" '("-t" "Fetch all tags" ("-t" "--tags"))))

  ;; Exterminate Magit buffers
  (with-no-warnings
    (defun my-magit-kill-buffers (&rest _)
      "Restore window configuration and kill all Magit buffers."
      (interactive)
      (let ((buffers (magit-mode-get-buffers)))
        (magit-restore-window-configuration)
        (mapc (lambda (buf)
                (with-current-buffer buf
                  (if (and magit-this-process
                           (eq (process-status magit-this-process) 'run))
                      (bury-buffer buf)
                    (kill-buffer buf))))
              buffers)))
    (setq magit-bury-buffer-function #'my-magit-kill-buffers))

  ;; Access Git forges from Magit
  (when (executable-find "cc")
    (use-package forge
      :demand
      :init (setq forge-topic-list-columns
                  '(("#" 5 t (:right-align t) number nil)
                    ("Title" 60 t nil title  nil)
                    ("State" 6 t nil state nil)
                    ("Updated" 10 t nill updated nil)))))

  ;; Show TODOs in magit
  (when emacs/>=25.2p
    (use-package magit-todos
      :init
      (setq magit-todos-nice (if (executable-find "nice") t nil))
      (magit-todos-mode 1))))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook (before-revert . (lambda ()
                           (when (bound-and-true-p git-timemachine-mode)
                             (user-error "Cannot revert the timemachine buffer")))))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string popuped-message
                                :left-fringe 8
                                :right-fringe 8
                                :internal-border-width 1
                                :internal-border-color (face-foreground 'font-lock-comment-face))
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-delete buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "diff")
    :color pink :quit-key "q")
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :bind (:map smerge-mode-map
         ("C-c m" . smerge-mode-hydra/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (hydra-smerge/body))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("B" . browse-at-remote)))

;; Git related modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

;; my-personal

(defun my/git-commit-search-message-history ()
  "Search and insert commit message from history.
     http://xenodium.com/m-r-history-search-in-git-commit-mode/"
  (interactive)
  (insert (completing-read "History: "
                           ;; Remove unnecessary newlines from beginning and end.
                           (mapcar (lambda (text)
                                     (string-trim text))
                                   (ring-elements log-edit-comment-ring)))))

;;(bind-key "M-r" #'my/git-commit-search-message-history git-commit-mode-map)
;;(add-to-list 'savehist-additional-variables log-edit-comment-ring)
;;(savehist-mode +1)

(when sys/macp
  (setq magit-repository-directories
        `(("~/.emacs.d/" . 0)
          ("~/GitRepos/" . 1)
          ("~/GitRepos/team-loki/" . 1)
          ("~/GitRepos/my-projects/" . 1)
          ("~/IdeaProjects/" . 1))))

;; C-c C-a to amend without any prompt
;; This code is inbuild by magit so Instead you should be using the built in Extend Commit command: `c e'
;;(defun magit-just-amend ()
;;  (interactive)
;;  (save-window-excursion
;;    (magit-refresh
;;     (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))
;;
;;(eval-after-load "magit"
;;  '(define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend))

;; full screen magit-status

;; (defadvice magit-status (around magit-fullscreen activate)
;;   (window-configuration-to-register :magit-fullscreen)
;;   ad-do-it
;;   (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;;(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;Emacs comes with a version control interface called "VC", see (emacs)Version Control. It is enabled be default, and if you don’t use it in addition to Magit, then you should disable it to keep it from performing unnecessary work:

;; enable line below if you are seeing `Args out of range' error
;; (setq vc-handled-backends nil) ;;had to disable this to get git-gutter working

;;You can also disable its use for Git but keep using it when using another version control system:

;;(setq vc-handled-backends (delq 'Git vc-handled-backens)) ;;had to disable this to get git-gutter working

;;(use-package gitignore-templates)

;; my-personal


(provide 'init-vcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
