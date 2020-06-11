;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

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
;; Org configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)

(use-package org
  ;; :ensure nil
  :ensure org-plus-contrib ;; My-personal-config
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org")
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook ((org-mode . (lambda ()
                       "Beautify org symbols."
                       (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  ;; To speed up startup, don't put to init section
  (when sys/macp
    (setq org-agenda-files
          '("/Users/ckoneru/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/"
            "/Users/ckoneru/GitRepos/my-org-notes/")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?) "  " nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t)

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (xwidget-webkit-browse-url (concat "file://" file))
              (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
                (when (buffer-live-p buf)
                  (and (eq buf (current-buffer)) (quit-window))
                  (pop-to-buffer buf)))))
          org-file-apps))

  ;; Add gfm/md backends
  (use-package ox-gfm)
  (add-to-list 'org-export-backends 'md)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Prettify UI
  (use-package org-bullets
    ;;:if (char-displayable-p ?⚫)
    :hook (org-mode . org-bullets-mode)
    ;;:init (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫"))
    ;; My-personal-config
    :init (setq org-bullets-bullet-list '("☯" "☢" "❀" "◉" "⚫" "○" "✸" "✿" "~"))
    ;;:config
    ;;(setq org-bullets-bullet-list '("☯" "☢" "❀" "◉" "○" "✸" "✿" "~"))
    ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    ;;; my-personal-config-end-here
    )

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (char-displayable-p ?⯀)
                    '("⯀" "⯀" "⯀" "⯀")
                  '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (restclient . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
           ("C-c M-o" . org-mime-htmlize)
           :map org-mode-map
           ("C-c M-o" . org-mime-org-buffer-htmlize)))

  ;; Preview
  (use-package org-preview-html
    :diminish)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
           ("C-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-agenda-mode-map
           ("P" . org-pomodoro))))

;;;;; My personal modifications
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
   (("s-Y" . org-download-screenshot)
    ("s-y" . org-download-yank)))
  :config
  (if (memq window-system '(mac ns))
      (setq org-download-screenshot-method "screencapture -i %s")
    (setq org-download-screenshot-method "maim -s %s"))
  (defun my-org-download-method (link)
    "This is a helper function for org-download.
It creates a folder in the root directory (~/.org/img/) named after the
org filename (sans extension) and puts all images from that file in there.
Inspired by https://github.com/daviderestivo/emacs-config/blob/6086a7013020e19c0bc532770e9533b4fc549438/init.el#L701"
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          ;; Create folder name with current buffer name, and place in root dir
          (dirname (concat "./images/"
                           (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name))))))

      ;; Add timestamp to filename
      (setq filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename)))
      ;; Create folder if necessary
      (unless (file-exists-p dirname)
        (make-directory dirname))
      (expand-file-name filename-with-timestamp dirname)))
  (setq org-download-method 'my-org-download-method))

(setq org-startup-align-all-table t)
;;(add-hook 'org-mode-hook (lambda ()
;;                           "Beautify Org Checkbox Symbol"
;;                           (push '("[ ]" .  "☐") prettify-symbols-alist)
;;                           (push '("[X]" . "☑" ) prettify-symbols-alist)
;;                           (push '("[-]" . "❍" ) prettify-symbols-alist)
;;                           (push '("#+BEGIN_SRC" . "⟾" ) prettify-symbols-alist)
;;                           (push '("#+END_SRC" . "⟽" ) prettify-symbols-alist)
;;                           (push '("#+BEGIN_EXAMPLE" . "⟾" ) prettify-symbols-alist)
;;                           (push '("#+END_EXAMPLE" . "⟽" ) prettify-symbols-alist)
;;                           (push '("#+BEGIN_QUOTE" . "⟾" ) prettify-symbols-alist)
;;                           (push '("#+END_QUOTE" . "⟽" ) prettify-symbols-alist)
;;                           (push '("#+begin_quote" . "⟾" ) prettify-symbols-alist)
;;                           (push '("#+end_quote" . "⟽" ) prettify-symbols-alist)
;;                           (push '("#+begin_example" . "⟾" ) prettify-symbols-alist)
;;                           (push '("#+end_example" . "⟽" ) prettify-symbols-alist)
;;                           (push '("#+begin_src" . "⟾" ) prettify-symbols-alist)
;;                           (push '("#+end_src" . "⟽" ) prettify-symbols-alist)
;;                           (prettify-symbols-mode)))
;;

;;(use-package org-download
;;  :config
;;  (require 'org-download)
;;  ;; Drag and drop to Dired
;;  (add-hook 'dired-mode-hook 'org-download-enable)
;;  (setq org-download-method 'directory)
;;  (setq-default org-download-image-dir "./img")
;;  (setq org-download-screenshot-method "screencapture –I %s")
;;  (setq org-download-screenshot-file "./img/tmp.png")
;;  )
;;;; my latex function
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("my-latex-fun"
                 "\\documentclass{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage{natbib}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{geometry}
\\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;;;; my latex function

(use-package org-web-tools)

(use-package ob-restclient
  )

(use-package doct
  ;; :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

;; for org-protocol if using mac go though - it is a little outdated
;;                    https://blog.aaronbieber.com/2016/11/24/org-capture-from-anywhere-on-your-mac.html
;;                    But use - https://github.com/aaronbieber/org-protocol-handler

;; for linux go through -
;;                    https://cestlaz.github.io/post/using-emacs-70-org-protocol/

(global-set-key (kbd "C-c c c") 'org-capture)

;; (when sys/macp
;;   (setq org-capture-templates
;;         '(
;;           ("n" "📖  Note" entry (file+headline "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/notes.org" "Notes")
;;            "* Note %? %^g \n%T")
;;           ("l" "🌐   Link" entry (file+headline "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/links.org" "Links")
;;            "* %a %^g\n %?\n %T\n %i" :prepend t :empty-lines 1)
;;           ("t" "✔   To-Do-Item" entry (file+headline "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/todos.org" "To Do Items")
;;            "* TODO %?\n  %i\n  %a" :prepend t)
;;           )))

(when sys/macp
  (setq org-capture-templates
        (doct `(
                ("🌎 Website As Entry"
                 :keys "w"
                 :type plain
                 :file "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/webentries.org"
                 :prepend t
                 :empty-lines-after 2
                 :headline "Web-Entry"
                 :template ""
                 :hook ,(defun my/website-to-org-entry ()
                          "Convert clipboard's URL content to org entry."
                          (org-web-tools-insert-web-page-as-entry (org-get-x-clipboard 'PRIMARY)))
                 )
                ("✔ To-Do-Item"
                 :keys "t"
                 :file "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/todos.org"
                 :template "* TODO %?\n  %i\n  %a"
                 :prepend t
                 :empty-lines-after 2
                 :headline "TODO ITEMS"
                 )
                ("🌐 Link"
                 :keys "l"
                 :file "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/links.org"
                 :template "* %a %^g\n %?\n %T\n %i"
                 :prepend t
                 :empty-lines-after 2
                 :headline "Links"
                 )
                ("📖 Note"
                 :keys "n"
                 :file "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/notes.org"
                 :template "* Note %? %^g \n%T"
                 :prepend t
                 :empty-lines-after 2
                 :headline "Notes"
                 )
                )
              )))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(use-package noflet)

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture)))

(require 'org-protocol)

(setq org-html-html5-fancy t
      org-html-wrap-src-lines t
      org-html-doctype "html5")

;; Minted is a latex package used to wrap text
;; this is used when exporting org file to pdf
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))

;;;;; my personal modification end here
;; org-roam

(when sys/macp
  (setq org-roam-directory-alist '("~/GitRepos/my-org-notes/"
                                   "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/")))

(when sys/linuxp
  (setq org-roam-directory-alist '("~/Documents/my-org-notes/")))

(defun my/toggle-org-roam-directory ()
  "cycles through a list of directories and configures org-roam
  directory"
  (interactive)
  (setq org-roam-directory-alist (append (cdr
                                          org-roam-directory-alist) (cons (car org-roam-directory-alist)
                                          ())))
  (setq org-roam-directory (car org-roam-directory-alist))
  (org-roam-db-build-cache)
  (message "org-roam-directory now '%s'" (car
                                          org-roam-directory-alist)))

(when (and emacs/>=26p (executable-find "cc"))
  (use-package org-roam
    :diminish
    :custom
    (org-roam-directory (car org-roam-directory-alist))
    (org-roam-completion-system 'ivy)
    :hook (after-init . org-roam-mode)
    :bind (:map org-roam-mode-map
           (("C-c n l" . org-roam)
            ("C-c n f" . org-roam-find-file)
            ("C-c n d" . my/toggle-org-roam-directory)
            ("C-c n g" . org-roam-graph))
           :map org-mode-map
           (("C-c n i" . org-roam-insert)))))

(use-package company-org-roam
  :config
  (push 'company-org-roam company-backends))

(use-package org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8181
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))

(use-package org-special-block-extras
  :hook (org-mode . org-special-block-extras-mode))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
