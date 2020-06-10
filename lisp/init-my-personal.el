;; init-my-personal.el --- Initialize personal configurations.	-*- lexical-binding: t -*-
;; to speed up tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)

;;https://emacs.stackexchange.com/questions/17170/how-to-auto-copy-when-a-region-is-selected
;;When emacs 25.1 is released (supposedly the next stable version after 24.5),
;;the x-select-enable-primary variable name will be deprecated and
;;select-enable-primary must be used instead (removal of that x- prefix).
;;Similarly x-select-enable-clipboard will be deprecated in favor of select-enable-clipboard
(setq select-enable-primary nil
	  select-enable-clipboard t
	  select-active-regions t)

;;Keyboard tweaks
;;(setq mac-command-modifier 'meta) ; make cmd key do Meta
;;(setq mac-option-modifier 'super) ; make opt key do Super
;;(setq mac-control-modifier 'control) ; make Control key do Control
(when sys/macp
  (setq ns-function-modifier 'hyper))  ; make Fn key do Hyper
;;(setq user-full-name "")

;;Text selected with the mouse is automatically copied to clipboard.
(setq mouse-drag-copy-region t)

;; C-n, next line, inserts newlines when at the end of the buffer
(setq next-line-add-newlines t)

;;saves the buffers whenever emacs loses the window focus.
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(setq require-final-newline t)

(global-visual-line-mode 1)

;;Restart emacs from emacs
(use-package restart-emacs
  :ensure t
  :config (defalias 'emacs-restart #'restart-emacs)
  :bind* (("C-x M-c" . restart-emacs)))

;; Open Large file
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

;; Enhance fuzzy matching
(use-package flx
  :ensure t)

;;(when (<= 26 emacs-major-version)
;;(setq-default
;; display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
;;) ; show relative numbers on the side

(use-package format-all)

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode))

;; to join lines
(global-set-key (kbd "H-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ;; or "forward"

;; font scaling
(use-package default-text-scale
  :config
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease))

(use-package fontify-face)

(use-package indent-tools
  :bind
  ("C-c h i" . indent-tools-hydra/body))

(use-package logview
  :mode ("syslog\\(?:\\.[0-9]+\\)" "\\.log\\(?:\\.[0-9]+\\)?\\'")
  :config
  (setq datetime-timezone 'UTC
        logview-additional-level-mappings '(("ICP"
                                             (error       "ERROR")
                                             (warning     "WARNING")
                                             (information "INFO")
                                             (debug       "DEBUG")
                                             (trace       "TRACE")))
        logview-additional-timestamp-formats '(("easyloggingpp" (java-pattern . "HH:mm:ss,SSS")))
        logview-additional-submodes '(("ICP" . ((format . "TIMESTAMP LEVEL ")
                                                (levels . "ICP")
                                                (timestamp . ("easyloggingpp"))))))
  )

(use-package hl-anything
  :diminish hl-highlight-mode
  :commands hl-highlight-mode
  )

(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode)
  :config (setq ws-butler-keep-whitespace-before-point nil))

(use-package visual-regexp
  :bind (:map mode-specific-map
         :prefix-map visual-regexp-prefix-map
         :prefix "r"
         ("r" . vr/query-replace)
         ("R" . vr/replace)
         ))

(use-package visual-regexp-steroids
  :after visual-regexp
  )

;;; Query exchange
;; Inspired from http://www.emacswiki.org/emacs/QueryExchange and definition of
;; `query-replace-regexp' from replace.el
(defun query-exchange (string-1 string-2 &optional delimited start end)
  "Exchange string-1 and string-2 interactively.
The user is prompted at each instance like query-replace. Exchanging
happens within a region if one is selected."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query replace"
                   (if current-prefix-arg " word" "")
                   " regexp"
                   (if (and transient-mark-mode mark-active) " in region" ""))
           t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           ;; These are done separately here
           ;; so that command-history will record these expressions
           ;; rather than the values they had this time.
           (if (and transient-mark-mode mark-active)
               (region-beginning))
           (if (and transient-mark-mode mark-active)
               (region-end)))))
  (perform-replace
   (concat "\\(" string-1 "\\)\\|" string-2)
   '(replace-eval-replacement replace-quote
                              (if (match-string 1) string-2 string-1))
   t t delimited nil nil start end))

(use-package color-identifiers-mode
  :hook ((after-init . global-color-identifiers-mode)))

;;melpa issue
(use-package ranger
  :config (setq ranger-width-preview 0.5))

(use-package highlight-numbers
  :hook '(after-init-hook prog-mode-hook text-mode-hook org-mode-hook))

(use-package mmm-mode
  :commands mmm-mode
  :config
  (require 'mmm-auto))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(setq-default indicate-empty-lines t)


(use-package copyit)

(use-package reveal-in-osx-finder)

(use-package sort-words)

(use-package package-lint)

(use-package isolate
  :demand
  :config
  (progn
    (defun activate-mark-hook@set-transient-map ()
      (unless (progn
                (derived-mode-p 'mu4e-headers-mode)
                (derived-mode-p 'magit-mode))
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (define-key map "a" #'isolate-quick-add)
           (define-key map "A" #'isolate-long-add)
           (define-key map "d" #'isolate-quick-delete)
           (define-key map "D" #'isolate-long-delete)
           (define-key map "c" #'isolate-quick-change)
           (define-key map "C" #'isolate-long-change)
           map)
         #'region-active-p)))

    (add-hook 'activate-mark-hook #'activate-mark-hook@set-transient-map)
    ))

(use-package counsel-fd
  :bind (("C-c c d d" . counsel-fd-dired-jump)
         ("C-c c d f" . counsel-fd-file-jump)))

(use-package gcmh
  :config
  (gcmh-mode 1)
  )

(use-package region-convert
  :bind
  ("C-c C" . region-convert)
  )

;;M-x swap-regions [select the first region] C-M-c [select the second region] C-M-c
(use-package swap-regions)

(use-package highlight-symbol
  :bind
  (("M-g h" . highlight-symbol)
   ("M-p" . highlight-symbol-prev)
   ("M-n" . highlight-symbol-next))
  :config
  (set-face-attribute 'highlight-symbol-face nil
                      :background "default"
                      :foreground "#FA009A")
  (setq highlight-symbol-idle-delay 0)
  (setq highlight-symbol-on-navigation-p t)
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))

(use-package adoc-mode
  :mode "\\.adoc\\'"
  :config
  (add-hook 'adoc-mode-hook
            #'(lambda ()
                (auto-fill-mode -1)
                (visual-line-mode 1)
                (visual-fill-column-mode 1))))

(use-package highlight2clipboard
  :config
  (highlight2clipboard-mode 1))

(use-package ox-clip
  :bind ("C-x c c" . ox-clip-formatted-copy))

(use-package org-cliplink
  :after org
  :bind
  ("C-x c l" . org-cliplink)
  )

(use-package deft
  :commands (deft)
  :init (setq deft-directory "~/GitRepos/my-org-notes/"
              deft-recursive t
              deft-text-mode 'org-mode
              deft-use-filename-as-title t
              deft-use-filter-string-for-filename t
              deft-extensions '("org" "md"))
  )


(use-package ox-hugo
  :after ox)

(use-package toml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.t[o]ml\\'" . toml-mode)))

(use-package manage-minor-mode)

(use-package zoom-window
  :bind ("C-x C-z" . zoom-window-zoom)
  :custom
  (zoom-window-mode-line-color "DarkGreen")
  (zoom-window-use-persp t)
  :hook (find-file . (lambda () (setq my-solaire-mode solaire-mode)))
  :init
  (zoom-window-setup)
  (defvar-local my-solaire-mode nil)
  (advice-add #'zoom-window-zoom :before #'turn-off-solaire-mode)
  (advice-add #'zoom-window--restore-mode-line-face :after
              (lambda ()
                (if my-solaire-mode
                    (turn-on-solaire-mode)
                  (turn-off-solaire-mode)))))

;; Golden Ratio
(use-package golden-ratio                 ; Auto resize windows
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t)
  :config
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down)))

  (setq golden-ratio-exclude-buffer-names '("*Org Select*", "*vterm-1*")
        golden-ratio-exclude-modes '(messages-buffer-mode
                                     fundamental-mode
                                     ediff-mode
                                     calendar-mode
                                     wget-mode
                                     calc-mode
                                     calc-trail-mode
                                     )
        golden-ratio-recenter t)
  )

(add-to-list 'golden-ratio-extra-commands 'ace-window)

(use-package loccur
  :bind
  (("M-s M-l" . loccur-current)
   ("M-s M-L" . loccur)
   ("M-s C-l" . loccur-previous-match)))

;;(setq inhibit-startup-message t)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;groovy and jenkins
(use-package groovy-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\)\\'" . groovy-mode))
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
  (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode)))

(use-package groovy-imports)
(use-package flycheck-gradle)

(setq lsp-server-install-dir (concat user-emacs-directory "lsp/")
      lsp-groovy-server-file (concat lsp-server-install-dir "groovy-language-server/build/libs/groovy-language-server-all.jar")
      )

;;;groovy and jenkins

;;; for Scala - metals
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )
;;; for Scala - metals

;;;ElMacro
(use-package elmacro
  :config
  (elmacro-mode))

(use-package transpose-mark)

(use-package company-emoji)

(add-to-list 'company-backends 'company-emoji)
;;adding company-emoji to magit mode
;;(add-hook 'git-commit-setup-hook (lambda () (set (make-local-variable 'company-backends) '((company-capf :with company-dabbrev company-files company-emoji)))))

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)

;; my-personal-config

;; (use-package dimmer
;;   :unless noninteractive
;;   :custom
;;   (dimmer-fraction 0.4)
;;   :init
;;   (dimmer-mode t))

(use-package adaptive-wrap
  :config
  (adaptive-wrap-prefix-mode))

(use-package copy-as-format)

(use-package transpose-frame)


(use-package reformatter)

(reformatter-define xml-format
  :program "xmllint"
  :args '("--format" "-")
  :mode nil)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-window-side 'right)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("<f12>" . eshell-toggle)
  ("s-~" . eshell-toggle))

(use-package gitignore-templates)


(use-package read-aloud
  :config
  (when sys/macp
    (setq read-aloud-engine "say"))
  (when sys/linuxp
    ;;install from http://manpages.ubuntu.com/manpages/bionic/man1/flite.1.html
    ;; or https://learn.adafruit.com/speech-synthesis-on-the-raspberry-pi/speak-easier-flite
    ;; also look in to http://manpages.ubuntu.com/manpages/bionic/man1/eBook-speaker.1.html
    (setq read-aloud-engine "flite"))
  )

(use-package string-inflection)

;; Added this to build pdf tools
(setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig:/usr/local/Cellar/zlib/1.2.11/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")

(use-package pdf-tools
  :config
  (pdf-tools-install :no-query)
  (require 'pdf-tools)
  (setq-default pdf-view-display-size 'fit-page)
  (bind-keys :map pdf-view-mode-map
    ("\\" . hydra-pdftools/body)
    ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
    ("g"  . pdf-view-first-page)
    ("G"  . pdf-view-last-page)
    ("l"  . image-forward-hscroll)
    ("h"  . image-backward-hscroll)
    ("j"  . pdf-view-next-page)
    ("k"  . pdf-view-previous-page)
    ("e"  . pdf-view-goto-page)
    ("u"  . pdf-view-revert-buffer)
    ("al" . pdf-annot-list-annotations)
    ("ad" . pdf-annot-delete)
    ("aa" . pdf-annot-attachment-dired)
    ("am" . pdf-annot-add-markup-annotation)
    ("at" . pdf-annot-add-text-annotation)
    ("y"  . pdf-view-kill-ring-save)
    ("i"  . pdf-misc-display-metadata)
    ("s"  . pdf-occur)
    ("b"  . pdf-view-set-slice-from-bounding-box)
    ("r"  . pdf-view-reset-slice))
  ;; TODO - wait until it gets published in melpa
  ;;(use-package org-pdftools
  ;;:config
    ;;;;(setq org-pdftools-root-dir /path/you/store/pdfs)
  ;;(with-eval-after-load 'org
  ;;(org-link-set-parameters "pdftools"
  ;;:follow #'org-pdftools-open
  ;;:complete #'org-pdftools-complete-link
  ;;:store #'org-pdftools-store-link
  ;;:export #'org-pdftools-export)
  ;;(add-hook 'org-store-link-functions 'org-pdftools-store-link)))
  ;;
  ;;(use-package org-noter-pdftools
  ;;:after (org-noter))
  )

(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))

(setq pdf-view-use-scaling t)

;;Added this to disable linum in pdf-tools
(add-hook 'pdf-view-mode-hook (lambda() (linum-relative-mode -1)))
(add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
;;my-personal-config

(provide 'init-my-personal)
