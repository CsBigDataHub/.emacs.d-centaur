;;; init-linux.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; mu4econfig-setup.el
;;; Commentary: Emacs Startup File --- initialization for Emacs
;;
(add-to-list 'load-path (expand-file-name "/usr/local/share/emacs/site-lisp/mu4e"))
(require 'mu4e)
(require 'org-mime)

(require 'org-mu4e)
(setq org-mu4e-convert-to-html t)

(require 'smtpmail)
(use-package mu4e-alert
  :ensure t)

(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

;;need this for hash access
(require 'subr-x)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; default
(setq mu4e-maildir "~/Maildir")

(setq mu4e-get-mail-command "offlineimap"
      ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
      mu4e-view-prefer-html t
      mu4e-headers-include-related t
      mu4e-headers-skip-duplicates t
      mu4e-update-interval 180
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed t)

;; (setq mu4e-html2text-command "html2text -utf8 -width 72") ;;using the default mu4e-shr2text

;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)
;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

(setq
 user-mail-address "hadoopchetan@gmail.com"
 user-full-name  "hadoop chetan"
 mu4e-compose-signature
 (concat
  "Hadoop Chetan\n"
  "this is my hadoop gmail\n"))

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;;(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].Important"   . ?I)
         ("/[Gmail].All Mail"    . ?a)))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "hadoopchetan@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(add-hook 'message-send-hook
          (lambda ()
            (unless (yes-or-no-p "Sure you want to send this?")
              (signal 'quit nil))))

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Then, mark the file(s) in dired you would like to attach and press
;; C-c RET C-a, and you’ll be asked whether to attach them to an existing
;; message, or create a new one.

(provide 'init-mail)
