;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-

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
;; Org configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

(use-package org
  :ensure nil
  ;; :ensure org-plus-contrib ;; My-personal-config
  :autoload (org-dynamic-block-define)
  :custom-face (org-ellipsis ((t (:foreground nil))))
  ;; :pretty-hydra
  ;; ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
  ;;   :color blue :quit-key "q")
  ;;  ("Basic"
  ;;   (("a" (hot-expand "<a") "ascii")
  ;;    ("c" (hot-expand "<c") "center")
  ;;    ("C" (hot-expand "<C") "comment")
  ;;    ("e" (hot-expand "<e") "example")
  ;;    ("E" (hot-expand "<E") "export")
  ;;    ("h" (hot-expand "<h") "html")
  ;;    ("l" (hot-expand "<l") "latex")
  ;;    ("n" (hot-expand "<n") "note")
  ;;    ("o" (hot-expand "<q") "quote")
  ;;    ("v" (hot-expand "<v") "verse"))
  ;;   "Head"
  ;;   (("i" (hot-expand "<i") "index")
  ;;    ("A" (hot-expand "<A") "ASCII")
  ;;    ("I" (hot-expand "<I") "INCLUDE")
  ;;    ("H" (hot-expand "<H") "HTML")
  ;;    ("L" (hot-expand "<L") "LaTeX"))
  ;;   "Source"
  ;;   (("s" (hot-expand "<s") "src")
  ;;    ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
  ;;    ("y" (hot-expand "<s" "python :results output") "python")
  ;;    ("p" (hot-expand "<s" "perl") "perl")
  ;;    ("r" (hot-expand "<s" "ruby") "ruby")
  ;;    ("S" (hot-expand "<s" "sh") "sh")
  ;;    ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
  ;;   "Misc"
  ;;   (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
  ;;    ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
  ;;    ("P" (progn
  ;;           (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
  ;;           (hot-expand "<s" "perl")) "Perl tangled")
  ;;    ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("s-?" . my-org-hydra/body)
         ("C-'" . nil) ;; disable org-agenda-cycle keymap
         ("M-;" .  org-comment-dwim-2) ;; moved to here from init-edit.el
         ;; ("<" . (lambda ()
         ;;          "Insert org template."
         ;;          (interactive)
         ;;          (if (or (region-active-p) (looking-back "^\s*" 1))
         ;;              (org-hydra/body)
         ;;            (self-insert-command 1))))
         )
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (when centaur-prettify-org-symbols-alist
                         (if prettify-symbols-alist
                             (push centaur-prettify-org-symbols-alist prettify-symbols-alist)
                           (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)))
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; HACK: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :config
  ;; For hydra
  ;; (defun hot-expand (str &optional mod)
  ;;     "Expand org template.

  ;; STR is a structure template string recognised by org like <s. MOD is a
  ;; string with additional parameters to add the begin line of the
  ;; structure element. HEADER string includes more parameters that are
  ;; prepended to the element after the #+HEADER: tag."
  ;;     (let (text)
  ;;       (when (region-active-p)
  ;;         (setq text (buffer-substring (region-beginning) (region-end)))
  ;;         (delete-region (region-beginning) (region-end)))
  ;;       (insert str)
  ;;       (if (fboundp 'org-try-structure-completion)
  ;;           (org-try-structure-completion) ; < org 9
  ;;         (progn
  ;;           ;; New template expansion since org 9
  ;;           (require 'org-tempo nil t)
  ;;           (org-tempo-complete-tag)))
  ;;       (when mod (insert mod) (forward-line))
  ;;       (when text (insert text))))

  ;; To speed up startup, don't put to init section
  (when sys/macp
    (setq org-agenda-files
          '("/Users/ckoneru/GitRepos/my-org-agenda-files/")))
  (setq org-modules nil                 ; Faster loading
        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))

        ;; Agenda styling
        org-agenda-files `(,centaur-org-directory)
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────"

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-replace-disputed-keys t ;;https://irreal.org/blog/?p=1562
        org-ellipsis (if (and (display-graphic-p) (char-displayable-p ?⏷)) "\t⏷" nil)
        org-pretty-entities t
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-imenu-depth 4
        org-hide-emphasis-markers t)

  ;; Make windmove work in Org mode:
  ;; https://orgmode.org/manual/Conflicts.html#Conflicts
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (centaur-webkit-browse-url (concat "file://" file) t)))
          org-file-apps))

  ;; Add gfm/md backends
  (use-package ox-gfm)
  (add-to-list 'org-export-backends 'md)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; this is to replace pretty-hydra and hot-expand
  (use-package company-org-block
    :custom
    (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
    :hook ((org-mode . (lambda ()
                         (setq-local company-backends '(company-org-block))
                         (company-mode +1)))))

  ;; Prettify UI
  (if emacs/>=27p
      (use-package org-modern
        :hook ((org-mode . org-modern-mode)
               (org-agenda-finalize . org-modern-agenda)
               (org-modern-mode . (lambda ()
                                    "Adapt `org-modern-mode'."
                                    ;; Disable Prettify Symbols mode
                                    (setq prettify-symbols-alist nil)
                                    (prettify-symbols-mode -1)))))
    (progn
      (use-package org-superstar
        :if (and (display-graphic-p) (char-displayable-p ?◉))
        :hook (org-mode . org-superstar-mode)
        :init (setq org-superstar-headline-bullets-list '("◉""○""◈""◇""⁕")))
      (use-package org-fancy-priorities
        :diminish
        :hook (org-mode . org-fancy-priorities-mode)
        :init (setq org-fancy-priorities-list
                    (if (and (display-graphic-p) (char-displayable-p ?🅐))
                        '("🅐" "🅑" "🅒" "🅓")
                      '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (perl       . t)
      (python     . t)
      (ruby       . t)
      (js         . t)
      (css        . t)
      (sass       . t)
      (C          . t)
      (java       . t)
      (plantuml   . t))
    "Alist of org ob languages.")

  ;; ob-sh renamed to ob-shell since 26.1.
  (cl-pushnew '(shell . t) load-language-alist)

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-alist))

  ;; Install: npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

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

  ;; Add graphical view of agenda
  (use-package org-timeline
    :hook (org-agenda-finalize . org-timeline-insert-timeline))

  (when emacs/>=27p
    ;; Auto-toggle Org LaTeX fragments
    (use-package org-fragtog
      :diminish
      :hook (org-mode . org-fragtog-mode))

    ;; Preview
    (use-package org-preview-html
      :diminish
      :bind (:map org-mode-map
             ("C-c C-h" . org-preview-html-mode))
      :init (when (featurep 'xwidget-internal)
              (setq org-preview-html-viewer 'xwidget))))

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
           ("s-<f7>" . org-tree-slide-mode)
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
    :init (setq org-tree-slide-header nil
                org-tree-slide-slide-in-effect t
                org-tree-slide-heading-emphasis nil
                org-tree-slide-cursor-init t
                org-tree-slide-modeline-display 'outside
                org-tree-slide-skip-done nil
                org-tree-slide-skip-comments t
                org-tree-slide-skip-outline-level 3))

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-mode-map
           ("C-c C-x m" . org-pomodoro))
    :init
    (with-eval-after-load 'org-agenda
      (bind-keys :map org-agenda-mode-map
        ("K" . org-pomodoro)
        ("C-c C-x m" . org-pomodoro)))))

;;;;; My personal modifications

(use-package org-appear
  :after org
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
   (("s-Y" . org-download-screenshot)
    ("s-y" . org-download-yank)))
  :config
  (if (memq window-system '(mac ns))
      (setq org-download-screenshot-method "screencapture -i %s")
    ;; (setq org-download-screenshot-method "maim -s %s")
    (setq org-download-screenshot-method "flameshot gui --raw > %s")
    )
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
        (make-directory dirname t))
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
;;;; http://pragmaticemacs.com/emacs/org-mode-basics-v-exporting-your-notes/
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

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
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


;; https://github.com/sprig/org-capture-extension/issues/72
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

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
                ("🌎 Protocol-Capture"
                 :keys "p"
                 :file "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/protocol-capture.org"
                 :template "* %^{Title}\nSource: [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t
                 :empty-lines-after 2
                 :headline "Protocol-Capture"
                 )
                ("🔗 Protocol-Link"
                 :keys "L"
                 :file "~/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/protocol-link.org"
                 :template "* %? [[%:link][%:description]] \nCaptured On: %U"
                 :prepend t
                 :empty-lines-after 2
                 :headline "Protocol-Link"
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

;; (require 'org-protocol)

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

;;;; ox-beamer - Beamer export
(use-package ox-beamer
  :ensure nil
  :commands (org-beamer-export-as-latex
             org-beamer-export-to-latex
             org-beamer-export-to-pdf)
  :config
  (progn
    ;; allow for export=>beamer by placing
    ;; #+LaTeX_CLASS: beamer in org files
    (add-to-list 'org-latex-classes
                 '("beamer"
                   "\\documentclass[presentation]{beamer}"
                   ("\\section{%s}"        . "\\section*{%s}")
                   ("\\subsection{%s}"     . "\\subsection*{%s}")
                   ("\\subsubsection{%s}"  . "\\subsubsection*{%s}")))))

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

(when (executable-find "cc")
  (use-package org-roam
    :diminish
    :custom
    (org-roam-directory (car org-roam-directory-alist))
    (org-roam-completion-system 'ivy)
    :hook (after-init . org-roam-db-autosync-enable)
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n j" . org-roam-dailies-capture-today))
    :init
    ;; (setq org-roam-v2-ack t)
    :config
    ;; (require 'org-roam-protocol)
    ;; (setq-default org-roam-capture-templates
    ;;               '(("d" "default" plain "%?" :if-new
    ;;                  (file+head "${slug}.org" "#+title: ${title}")
    ;;                  :unnarrowed t)))
    ;; https://orgmode-exocortex.com/2021/07/22/configure-org-roam-v2-to-update-database-only-when-idle/
    ;; (with-eval-after-load "org-roam"
    ;;   ;; queue for files that will be updated in org-roam-db when emacs is idle
    ;;   (setq org-roam-db-update-queue (list))
    ;;   ;; save the original update function;
    ;;   (setq orig-update-file (symbol-function 'org-roam-db-update-file))
    ;;   ;; then redefine the db update function to add the filename to a queue
    ;;   (defun org-roam-db-update-file (&optional file-path)
    ;;     ;; do same logic as original to determine current file-path if not passed as arg
    ;;     (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
    ;;     (message "org-roam: scheduling update of %s" file-path)
    ;;     (if (not (memq file-path org-roam-db-update-queue))
    ;;         (push file-path org-roam-db-update-queue)))

    ;;   ;; this function will be called when emacs is idle for a few seconds
    ;;   (defun org-roam-db-idle-update-files ()
    ;;     ;; go through queued filenames one-by-one and update db
    ;;     ;; if we're not idle anymore, stop. will get rest of queue next idle.
    ;;     (while (and org-roam-db-update-queue (current-idle-time))
    ;;       ;; apply takes function var and list
    ;;       (apply orig-update-file (list (pop org-roam-db-update-queue)))))

    ;;   ;; we'll only start updating db if we've been idle for this many seconds
    ;;   (run-with-idle-timer 5 t #'org-roam-db-idle-update-files))
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory))
    )
  ;; (use-package company-org-roam
  ;;   :config
  ;;   (push 'company-org-roam company-backends))
  )

(use-package org-ql)

;; (use-package org-super-agenda
;;   ;; TODO: Get config from org-super-agenda/examples.org
;;   )

(when sys/linuxp
  (use-package org-mime))

(setq org-M-RET-may-split-line '((item . nil)))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("Tasks.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)


;; (use-package org-sidebar)

;; (use-package org-special-block-extras
;;   :hook (org-mode . org-special-block-extras-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;notification;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example https://christiantietze.de/posts/2019/12/emacs-notifications/

;; (use-package alert
;;   :config
;;   (when sys/macp
;;     (setq alert-default-style 'notifier)))

;; simpler code for `org-alert' package
;; https://raw.githubusercontent.com/jakecoble/org-alert/master/org-alert.el
;;(use-package org-alert
;;  :demand t
;; :init
;; (org-alert-enable)
;;  )

;; (use-package org-wild-notifier
;;   :after (org alert)
;;   :custom
;;   (org-wild-notifier-alert-time '(120 60 30 10 5 1))
;;   (org-wild-notifier-keyword-whitelist '("TODO"))
;;   (org-wild-notifier--day-wide-events t)
;;   (org-wild-notifier-keyword-blacklist '("CANCELED" "DONE" "ABORDED" "HAVE" "GIVEN" "CONSUMED" "LOST"))
;;   (org-wild-notifier-notification-title "Org Wild Reminder!")
;;   :init
;;   (org-wild-notifier-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; adding as a fail safe if above does not alert
;; (require 'appt)
;;
;; (setq appt-time-msg-list nil)    ;; clear existing appt list
;; (setq appt-display-interval '5)  ;; warn every 5 minutes from t - appt-message-warning-time
;; (setq
;;  appt-message-warning-time '15  ;; send first warning 15 minutes before appointment
;;  appt-display-mode-line nil     ;; don't show in the modeline
;;  appt-display-format 'window)   ;; pass warnings to the designated window function
;; (setq appt-disp-window-function (function ct/appt-display-native))
;;
;; (appt-activate 1)                ;; activate appointment notification
;;                                         ; (display-time) ;; Clock in modeline
;;
;; (defun ct/send-notification (title msg)
;;   (let ((notifier-path (executable-find "alerter")))
;;     (start-process
;;      "Appointment Alert"
;;      "*Appointment Alert*" ; use `nil` to not capture output; this captures output in background
;;      notifier-path
;;      "-message" msg
;;      "-title" title
;;      "-sender" "org.gnu.Emacs"
;;      "-activate" "org.gnu.Emacs")))
;; (defun ct/appt-display-native (min-to-app new-time msg)
;;   (ct/send-notification
;;    (format "Appointment in %s minutes" min-to-app) ; Title
;;    (format "%s" msg)))                             ; Message/detail text
;;
;;
;; ;; Agenda-to-appointent hooks
;; (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
;; (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
;; (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;notification;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when emacs/>=27p
 (use-package org-roam-ui
  :init
  (when (featurep 'xwidget-internal)
    (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
