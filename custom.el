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
(setq centaur-theme 'default)                        ; Color theme: auto, random, default, classic, colorful, dark, light, day or night
(setq centaur-dashboard t)                    ; Use dashboard at startup or not: t or nil
(setq centaur-restore-frame-geometry t)      ; Restore the frame's geometry at startup: t or nil
;; (setq centaur-lsp 'eglot)                      ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save-ignore-modes '(json-mode)) ; Ignore format on save for some languages
;; (setq centaur-chinese-calendar nil)            ; Use Chinese calendar or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications
;; (setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'
(setq centaur-benchmark-init t)                ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("JetBrains Mono" "Cascadia Code SemiLight" "Fira Code" "Source Code Pro" "SF Mono" "Hack"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (sys/mac-x-p 170)
                                                    (sys/win32p 110)
                                                    (t 120))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Symbola" "icons-in-terminal" "Apple Symbols" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

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
      calendar-daylight-time-zone-name "CDT")

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-package-archives 'melpa)
 '(centaur-theme 'default)
 '(doom-themes-treemacs-theme "doom-colors")
 '(magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:")
 '(safe-local-variable-values
   '((add-to-list 'browse-at-remote-remote-type-domains
                  '("github.com" . "github"))
     (magit-todos-exclude-globs "*.elc" "elpa/**" "straight/**" "*.md" "*.org"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
