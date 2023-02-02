;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq centaur-logo (expand-file-name
                    (if (display-graphic-p) "emacs-head-color.png" "gnu-head.png")
                    user-emacs-directory))                         ; Logo file or nil (official logo)
(setq centaur-full-name "Chetan Koneru")           ; User full name
(setq centaur-mail-address "kchetan.hadoop@gmail.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
(setq centaur-server t)                      ; Enable `server-mode' or not: t or nil
(setq centaur-icon t)                         ; Display icons or not: t or nil
;; (setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china, netease, ustc, tencent or tuna
(setq centaur-theme 'auto)                        ; Color theme: auto, random, default, classic, colorful, dark, light, day or night
(setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
(setq centaur-dashboard t)                   ; Display dashboard at startup or not: t or nil
(setq centaur-restore-frame-geometry t)      ; Restore the frame's geometry at startup: t or nil
(setq centaur-lsp 'eglot)                      ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save-ignore-modes '(json-mode python-mode ruby-mode)) ; Ignore format on save for some languages
;; (setq centaur-tree-sitter nil)                 ; Enable tree-sitter or not: t or nil. Only available in 29+.
;; (setq centaur-chinese-calendar nil)            ; Use Chinese calendar or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications
;; (setq centaur-player t)                        ; Enable players or not: t or nil
;; (setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'
;; (setq centaur-benchmark-init t); Enable initialization benchmark or not: t or nil ;; removed in c2b53bc5c26ae49b79b74b48c448f8d50ef421f2 in seagle0128

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Jetbrains Mono" "Cascadia Code" "Fira Code"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 140)
                                                      (sys/win32p 110)
                                                      (t 100))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)


;; Misc.
(require 'solar)

(setq confirm-kill-emacs 'y-or-n-p
      visible-bell t
      calendar-latitude 45.018280
      calendar-longitude -93.473892
      calendar-standard-time-zone-name "CST"
      calendar-daylight-time-zone-name "CDT"
      centaur-auto-themes '((:sunrise . modus-vivendi)
                            (:sunset  . modus-vivendi)))


(put 'downcase-region 'disabled nil)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))


;; Supress "ad-handle-definition: x got redefined" warnings
(setq ad-redefinition-action 'accept)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-package-archives 'melpa)
 '(centaur-theme 'auto)
 '(doom-themes-treemacs-theme "doom-colors" nil nil "Customized with use-package doom-themes")
 '(ispell-choices-win-default-height 3)
 '(lsp-java-jdt-download-url
   "https://download.eclipse.org/jdtls/milestones/1.0.0/jdt-language-server-1.0.0-202104151857.tar.gz")
 '(lsp-ui-doc-border "#5B6268")
 '(magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:")
 '(network-stream-use-client-certificates t)
 '(native-comp-async-report-warnings-errors 'silent)
 '(org-web-tools-pandoc-sleep-time 0.5)
 '(safe-local-variable-values
   '((add-to-list 'browse-at-remote-remote-type-domains
                  '("github.com" . "github"))
     (magit-todos-exclude-globs "*.elc" "elpa/**" "straight/**" "*.md" "*.org")))
 '(vterm-max-scrollback 10000)
 '(windmove-default-keybindings '([ignore] hyper))
 '(word-wrap-by-category t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(cfrs-border-color ((t (:background "#a8a8a8"))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:foreground "#51afef" :background nil))))
 '(diff-hl-delete ((t (:background nil))))
 '(diff-hl-insert ((t (:background nil))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
 '(epe-pipeline-host-face ((t (:foreground "tomato1"))))
 '(epe-pipeline-time-face ((t (:foreground "DarkOrchid1"))))
 '(eshell-syntax-highlighting-shell-command-face ((t (:foreground "spring green"))))
 '(evil-goggles-delete-face ((t (:inherit 'menu))))
 '(evil-goggles-paste-face ((t (:inherit 'custom-saved))))
 '(evil-goggles-yank-face ((t (:inherit 'custom-invalid))))
 '(flycheck-posframe-face ((t (:foreground "#98be65"))))
 '(flycheck-posframe-info-face ((t (:foreground "#98be65"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#ff6c6b") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#ECBE7B") :inherit lsp-headerline-breadcrumb-path-face)))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ff6c6b"))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ECBE7B"))))
 '(lsp-ui-peek-highlight ((t (:inherit lsp-ui-peek-header :background "gridColor" :foreground "Red" :box 1))))
 '(lsp-ui-peek-selection ((t (:background "systemGrayColor" :foreground "Green" :weight bold))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:background "#23272e" :extend t))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(pulse-highlight-face ((t (:inherit region))))
 '(pulse-highlight-start-face ((t (:inherit region))))
 '(region ((t (:extend t :background "gray30" :foreground "#ffffff"))))
 '(symbol-overlay-default-face ((t (:inherit (region bold)))))
 '(ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
 '(ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
 '(ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
 '(ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
 '(ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
 '(ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
 '(ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
 '(ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
 '(ztreep-header-face ((t (:inherit diff-header))))
 '(ztreep-leaf-face ((t (:inherit diff-index))))
 '(ztreep-node-face ((t (:inherit font-lock-variable-name-face)))))

;;; custom.el ends here
