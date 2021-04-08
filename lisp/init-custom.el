;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

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
;; Customization.
;;

;;; Code:

(defgroup centaur nil
  "Centaur Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/CsBigDataHub/.emacs.d-centaur"))

(defcustom centaur-logo (expand-file-name
                         (if (display-graphic-p) "logo.png" "banner.txt")
                         user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :group 'centaur
  :type 'string)

(defcustom centaur-full-name "Vincent Zhang"
  "Set user full name."
  :group 'centaur
  :type 'string)

(defcustom centaur-mail-address "seagle0128@gmail.com"
  "Set user email address."
  :group 'centaur
  :type 'string)

(defcustom centaur-org-directory (expand-file-name "~/org/")
  "Set org directory."
  :group 'centaur
  :type 'string)

(defcustom centaur-proxy "127.0.0.1:1087"
  "Set network proxy."
  :group 'centaur
  :type 'string)

(defcustom centaur-server t
  "Enable `server-mode' or not."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-icon (display-graphic-p)
  "Display icons or not."
  :group 'centaur
  :type 'boolean)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom centaur-package-archives-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    `(,(cons 'melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "org"   (concat proto "://orgmode.org/elpa/")) ;; my-personal
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))
               ,(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/"))
               ))
      ,(cons 'emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
      ,(cons 'netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
      ,(cons 'ustc
             `(,(cons "gnu"   (concat proto "://mirrors.ustc.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.ustc.edu.cn/elpa/melpa/"))))
      ,(cons 'tencent
             `(,(cons "gnu"   (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
      ,(cons 'tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))
  "The package archives group list."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom centaur-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'centaur
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value centaur-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    centaur-package-archives-alist)))

(defcustom centaur-theme-alist
  '((default . doom-one)
    (pro     . doom-monokai-pro)
    (dark    . doom-dark+)
    (light   . doom-one-light)
    (warm    . doom-solarized-light)
    (cold    . doom-city-lights)
    (day     . doom-tomorrow-day)
    (night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom centaur-auto-themes '(("6:00"  . doom-solarized-light)
				                 ("19:00" . doom-solarized-dark))
  "List of themes mapped to the time they should be loaded.

The keywords `:sunrise' and `:sunset' can be used for the time
if `calendar-latitude' and `calendar-longitude' are set.
For example:
  '((:sunrise . doom-solarized-light)
    (:sunset  . doom-solarized-dark))"
  :group 'centaur
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(when (boundp 'ns-system-appearance)
  (defcustom centaur-system-themes '((light . doom-one-light)
				                     (dark  . doom-one))
    "List of themes related the system appearance. It's only available on macOS."
    :group 'centaur
    :type '(alist :key-type (symbol :tag "Appearance")
                  :value-type (symbol :tag "Theme"))))

(defcustom centaur-theme 'default
  "The color theme."
  :group 'centaur
  :type `(choice (const :tag "Auto" auto)
                 (const :tag "Random" random)
                 ,(if (boundp 'ns-system-appearance)
                      '(const :tag "System" system)
                    "")
                 ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    centaur-theme-alist)
                 symbol))

(defcustom centaur-completion-style 'minibuffer
  "Completion display style."
  :group 'centaur
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom centaur-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-lsp 'lsp-mode
  "Set language server."
  :group 'centaur
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes.
"
  :group 'centaur
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom centaur-chinese-calendar nil
  "Use Chinese calendar or not."
  :group 'centaur
  :type 'boolean)

(defcustom centaur-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-" . ?←)
    ("->" . ?→)
    ("->>" . ?↠)
    ("=>" . ?⇒)
    ("map" . ?↦)
    ("/=" . ?≠)
    ("!=" . ?≠)
    ("==" . ?≡)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("=<<" . (?= (Br . Bl) ?≪))
    (">>=" . (?≫ (Br . Bl) ?=))
    ("<=<" . ?↢)
    (">=>" . ?↣)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("not" . ?¬))
  "Alist of symbol prettifications.
Nil to use font supports ligatures."
  :group 'centaur
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom centaur-prettify-org-symbols-alist
  '(("[ ]" . ?☐)
    ("[X]" . ?☑)
    ("[-]" . ?⛝)

    ("#+ARCHIVE:" . ?📦)
    ("#+AUTHOR:" . ?👤)
    ("#+CREATOR:" . ?💁)
    ("#+DATE:" . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:" . ?📧)
    ("#+OPTIONS:" . ?⛭)
    ("#+SETUPFILE:" . ?⛮)
    ("#+TAGS:" . ?🏷)
    ("#+TITLE:" . ?📓)

    ("#+BEGIN_SRC" . ?⏠)
    ("#+END_SRC" . ?⏡)
    ;; ("#+BEGIN_SRC" . ?✎)
    ;; ("#+END_SRC" . ?□)
    ("#+BEGIN_QUOTE" . ?»)
    ("#+END_QUOTE" . ?«)
    ("#+HEADERS" . ?☰)
    ("#+RESULTS:" . ?💻)

    ("#+archive:" . ?📦)
    ("#+author:" . ?👤)
    ("#+creator:" . ?💁)
    ("#+date:" . ?📆)
    ("#+description:" . ?⸙)
    ("#+email:" . ?🖂)
    ("#+options:" . ?⛭)
    ("#+setupfile:" . ?⛮)
    ("#+tags:" . ?🏷)
    ("#+title:" . ?🕮)

    ("#+begin_src" . ?⏠)
    ("#+end_src" . ?⏡)
    ("#+begin_quote" . ?»)
    ("#+end_quote" . ?«)
    ("#+headers" . ?☰)
    ("#+results:" . ?💻)
    )
  "Alist of symbol prettifications for `org-mode'."
  :group 'centaur
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
