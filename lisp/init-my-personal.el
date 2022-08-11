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
      mouse-drag-copy-region t
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
  :config (defalias 'emacs-restart #'restart-emacs)
  :bind (("C-x M-c" . restart-emacs)))

;; Open Large file
(use-package vlf
  :config
  (require 'vlf-setup))

;; Enhance fuzzy matching
;; (use-package flx)

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

;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets) ;; or "forward"

;; (use-package fontify-face)

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

;; (use-package visual-regexp
;;   :bind (:map mode-specific-map
;;          :prefix-map visual-regexp-prefix-map
;;          :prefix "r"
;;          ("r" . vr/query-replace)
;;          ("R" . vr/replace)
;;          ))

;; (use-package visual-regexp-steroids
;;   :after visual-regexp
;;   )

(use-package color-identifiers-mode
  :hook ((after-init . global-color-identifiers-mode)))

;;melpa issue
;; (use-package ranger
;;   :config (setq ranger-width-preview 0.5))

;; (use-package highlight-numbers
;;   :hook '(after-init-hook prog-mode-hook text-mode-hook org-mode-hook))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(setq-default indicate-empty-lines t)

;; ** Functions
;; - copyit-variable
;; - copyit-file-pathname
;; - copyit-file-content
;; - copyit-file-exif-information
;; - copyit-file-as-data-uri
;; - copyit-ssh
;;
;; ** Customize
;; - copyit-binary-file-copy-method
;; - copyit-ssh-directory-path
;; - copyit-copy-bare-string

(use-package copyit)

(when sys/macp
  (use-package reveal-in-osx-finder))

(use-package sort-words)

(use-package package-lint)

(use-package counsel-fd
  :bind (("C-c c d d" . counsel-fd-dired-jump)
         ("C-c c d f" . counsel-fd-file-jump))
  :config
  (ivy-set-actions
   'counsel-fd-file-jump
   `(("D" ,(lambda (x)
             (dired (or (file-name-directory x) default-directory)))
      "open in dired")))
  )

(use-package openwith
  :config
  (setq openwith-associations
        (cond
         ((string-equal system-type "darwin")
          '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
             "open" (file))
            ("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "open" ("-a" "VLC" file))))
         ((string-equal system-type "gnu/linux")
          '(("\\.\\(mp4\\|mkv\\|m4v\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "xdg-open" (file))))))
  (openwith-mode +1))

;; (use-package gcmh
;;   :config
;;   (gcmh-mode 1)
;;   )

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

;; (use-package deft
;;   :commands (deft)
;;   :init (setq deft-directory "~/GitRepos/my-org-notes/"
;;               deft-recursive t
;;               deft-text-mode 'org-mode
;;               deft-use-filename-as-title t
;;               deft-use-filter-string-for-filename t
;;               deft-extensions '("org" "md"))
;;   )


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
;; (use-package golden-ratio                 ; Auto resize windows
;;   :diminish golden-ratio-mode
;;   :init
;;   (golden-ratio-mode 1)
;;   (setq golden-ratio-auto-scale t)
;;   :config
;;   (setq golden-ratio-extra-commands
;;         (append golden-ratio-extra-commands
;;                 '(evil-window-left
;;                   evil-window-right
;;                   evil-window-up
;;                   evil-window-down)))

;;   (setq golden-ratio-exclude-buffer-names '("*Org Select*", "*vterm-1*")
;;         golden-ratio-exclude-modes '(messages-buffer-mode
;;                                      fundamental-mode
;;                                      ediff-mode
;;                                      calendar-mode
;;                                      wget-mode
;;                                      calc-mode
;;                                      calc-trail-mode
;;                                      )
;;         golden-ratio-recenter t)
;;   )

;; (add-to-list 'golden-ratio-extra-commands 'ace-window)
;; (add-to-list 'golden-ratio-extra-commands 'evil-window-next)

;;(setq inhibit-startup-message t)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;groovy and jenkins
(use-package groovy-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\)\\'" . groovy-mode))
  ;; (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))
  ;; (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode)))
  ;; (with-eval-after-load 'exec-path-from-shell
  ;;   (exec-path-from-shell-copy-envs '("JAVA_HOME" "GROOVY_HOME")))
  )

(use-package jenkinsfile-mode
  :preface
  (defun my-company-jenkinsfile-mode-company-hook ()
    (set (make-local-variable 'company-backends) '((company-files)
                                                   (company-dabbrev
                                                    company-dabbrev-code
                                                    company-keywords
                                                    company-capf
                                                    company-yasnippet))))
  :config
  (add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . jenkinsfile-mode))
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . jenkinsfile-mode))
  (add-hook 'jenkinsfile-mode 'my-company-jenkinsfile-mode-company-hook)
  ;; notation below can be used instead of preface
  ;; (add-hook 'jenkinsfile-mode-hook
  ;;           '(lambda ()
  ;;              (set (make-local-variable 'company-backends)
  ;;                   '((company-dabbrev company-dabbrev-code company-keywords company-files company-capf :with company-yasnippet)))))
  )

(use-package groovy-imports)
(use-package flycheck-gradle)

(use-package shell-command+
  :bind ("M-!" . shell-command+))

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

(defun emoji-text-mode-hook-setup ()
  ;; make `company-backends' local is critcal
  ;; or else, you will have completion in every major mode, that's very annoying!
  (make-local-variable 'company-backends)

  (add-to-list 'company-backends 'company-emoji)
  )

(add-hook 'text-mode-hook 'emoji-text-mode-hook-setup)

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

(use-package super-save
  :config
  (super-save-mode +1))

;; (use-package adaptive-wrap
;;   :config
;;   (adaptive-wrap-prefix-mode))

(use-package copy-as-format)

(use-package transpose-frame)


(use-package reformatter)

(reformatter-define xml-format
  :program "xmllint"
  :args '("--format" "-")
  :mode nil)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 2)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-window-side 'right)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("<f12>" . eshell-toggle)
  ("s-~" . eshell-toggle))

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

(use-package mvn)

;; (use-package skeletor)

(use-package vagrant-tramp)

(use-package magrant)

(use-package emojify
  :hook
  ;; (after-init . global-emojify-mode)
  (org-mode . emojify-mode)
  (markdown-mode . emojify-mode)
  )

(use-package savekill)

(use-package jq-format)

(use-package disk-usage)

(use-package regex-tool)

(use-package scratch
  :config
  (defun prot/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (let* ((mode major-mode)
           (string (format "Scratch buffer for: %s\n\n" mode))
           (region (with-current-buffer (current-buffer)
                     (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end)))
                     ""))
           (text (concat string region)))
      (when scratch-buffer
	    (save-excursion
          (insert text)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
	    (forward-line 2))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  (add-hook 'scratch-create-buffer-hook #'prot/scratch-buffer-setup)
  :bind ("C-c S" . scratch))

(use-package selected
  :commands selected-minor-mode
  :init
  (setq selected-org-mode-map (make-sparse-keymap))
  :bind (:map selected-keymap
         ("/" . undo-in-region)
         ("C" . hydra-change-case/body)
         ("c" . capitalize-region)
         ("l" . downcase-region)
         ("D" . delete-duplicate-lines)
         ("m" . apply-macro-to-region-lines)
         ("q" . selected-off)
         ("s" . sort-lines)
         ("u" . upcase-region)
         ("w" . count-words-region)
         :map selected-org-mode-map
         ("t" . org-table-convert-region)
         ("-" . org-ctrl-c-minus))
  :config (selected-global-mode)
  (setq selected-minor-mode-override t))

(use-package counsel-jq)

;; disable use it later
;;Ispell for camel-case
;; wucuo is a fast wrapper around flyspell designed to keep your workflow snappy.
;; flyspell can be notoriously sluggish when used in the conventional way.
;; wucuo gets around flyspell’s shortcomings by only calling flyspell in
;; certain circumstances, such as when a buffer is saved to disk, or after a specified time interval.
;; Wucuo uses Flyspell API. So the good news is your configuration for flyspell still works.
;; (use-package wucuo
;;   :after flyspell
;;   :hook
;;   ((prog-mode text-mode) . wucuo-start)
;;   :custom
;;   ;; How wucuo operates.
;;   ;; "fast": run `flyspell-region' in `after-save-hook' over visible region.
;;   ;; "normal": run `flyspell-buffer' in `after-save-hook'.
;;   (wucuo-flyspell-start-mode "fast")
;;   ;; How many seconds wucuo waits before running spell-check.
;;   (wucuo-update-interval 2)
;;   :init
;;   ;; Disable flyspell-mode before using wucuo
;;   (when (bound-and-true-p flyspell-mode)
;;     (flyspell-mode -1))
;;   (when (bound-and-true-p flyspell-prog-mode)
;;     (flyspell-prog-mode -1))
;;   ;; Tell wucuo to ignore certain majore modes.
;;   (setq wucuo-spell-check-buffer-predicate
;;         (lambda ()
;;           (not (memq major-mode
;;                      '(dired-mode
;;                        log-edit-mode
;;                        compilation-mode
;;                        help-mode
;;                        profiler-report-mode
;;                        speedbar-mode
;;                        gud-mode
;;                        calc-mode
;;                        Info-mode))))))

;; Added this to build pdf tools
(when sys/macp
  (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/libffi/3.4.2/lib/pkgconfig:/usr/local/Cellar/zlib/1.2.12/lib/pkgconfig:/usr/local/lib/pkgconfig")
  )

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
;; (add-hook 'pdf-view-mode-hook (lambda() (linum-relative-mode -1)))
;; (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
(dolist (modes '(org-mode-hook
                 term-mode-hook
                 vterm-mode-hook
                 pdf-view-mode-hook
                 shell-mode-hook
                 treemacs-mode-hook
                 eshell-mode-hook))
  (add-hook modes (lambda() (display-line-numbers-mode -1))))
;;my-personal-config

(provide 'init-my-personal)
