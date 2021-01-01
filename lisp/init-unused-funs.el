;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
;; (defun my/delete-this-file ()
;;   "Delete the current file, and kill the buffer."
;;   (interactive)
;;   (unless (buffer-file-name)
;;     (error "No file is currently being edited"))
;;   (when (yes-or-no-p (format "Really delete '%s'?"
;;                              (file-name-nondirectory buffer-file-name)))
;;     (delete-file (buffer-file-name))
;;     (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
;; (defun my/rename-this-file-and-buffer (new-name)
;;   "Renames both current buffer and file it's visiting to NEW-NAME."
;;   (interactive "sNew name: ")
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (unless filename
;;       (error "Buffer '%s' is not visiting a file!" name))
;;     (progn
;;       (when (file-exists-p filename)
;;         (rename-file filename new-name 1))
;;       (set-visited-file-name new-name)
;;       (rename-buffer new-name))))

;; (defun my/dos2unix (buffer)
;;   "Convert BUFFER from DOS file format to UNIX."
;;   (interactive "*b")
;;   (shell-command (format "dos2unix %s" (file-truename buffer))))





;; (defun my/upcase-backward-word (arg)
;;   (interactive "p")
;;   (upcase-word (- arg)))

;; (defun my/downcase-backward-word (arg)
;;   (interactive "p")
;;   (downcase-word (- arg)))

;; (defun my/capitalize-backward-word (arg)
;;   (interactive "p")
;;   (capitalize-word (- arg)))

;;(global-set-key (kbd "C-M-u")	 'upcase-backward-word)
;;(global-set-key (kbd "C-M-l")	 'downcase-backward-word)
;;(global-set-key (kbd "M-c")	 'capitalize-backward-word)

;; (defun my/kill-word-at-point ()
;;   (interactive)
;;   (let ((char (char-to-string (char-after (point)))))
;;     (cond
;;      ((string= " " char) (delete-horizontal-space))
;;      ((string-match "[\t\n -@\[-`{-~],.、。" char) (kill-word 1))
;;      (t (forward-char) (backward-word) (kill-word 1)))))

;;(global-set-key (kbd "M-d")  'kill-word-at-point)

;; (defun my/backward-kill-word-or-region (&optional arg)
;;   (interactive "p")
;;   (if (region-active-p)
;;       (call-interactively #'kill-region)
;;     (backward-kill-word arg)))

;;(global-set-key (kbd "C-w")  'backward-kill-word-or-region)


;; (defun my/xah-search-current-word ()
;;   "Call `isearch' on current word or text selection."
;;   ;;“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
;;   ;;URL `http://ergoemacs.org/emacs/modernization_isearch.html'
;;   ;;Version 2015-04-09"
;;   (interactive)
;;   (let ( $p1 $p2 )
;;     (if (use-region-p)
;;         (progn
;;           (setq $p1 (region-beginning))
;;           (setq $p2 (region-end)))
;;       (save-excursion
;;         (skip-chars-backward "-_A-Za-z0-9")
;;         (setq $p1 (point))
;;         (right-char)
;;         (skip-chars-forward "-_A-Za-z0-9")
;;         (setq $p2 (point))))
;;     (setq mark-active nil)
;;     (when (< $p1 (point))
;;       (goto-char $p1))
;;     (isearch-mode t)
;;     (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

;;(global-set-key (kbd "<f8>") 'my/xah-search-current-word)


;;In emacs, the following commands lets you delete whitespaces around cursor.
;;
;;    delete-blank-lines 【Ctrl+x Ctrl+o】
;;    just-one-space 【Alt+Space】
;;    delete-indentation 【Alt+^】
;;    delete-horizontal-space 【Alt+\】
;;    fixup-whitespace
;;    cycle-spacing (emacs 24.4)
;;
;;Here's a command xah-shrink-whitespaces that combine most of them into one.




;; (defun my/move-line-down ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines 1))
;;     (forward-line)
;;     (move-to-column col)))

;; (defun my/move-line-up ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines -1))
;;     (move-to-column col)))

;;(global-set-key (kbd "<C-S-down>") 'my/move-line-down)
;;(global-set-key (kbd "<C-S-up>") 'my/move-line-up)



;; (defun my/clean-buffer-formatting ()
;;   "Indent and clean up the buffer"
;;   (interactive)
;;   (indent-region (point-min) (point-max))
;;   (whitespace-cleanup))

;; (global-set-key "\C-cn" 'my/clean-buffer-formatting)

;; by default,
;; highlight trailing whitespace
;; and show form-feed chars as horizontal rules


;;(defun my/text-formatting-hooks ()
;;  (turn-on 'auto-fill)) ; turn on automatic hard line wraps
;;
;;(add-hook 'text-mode-hook
;;          'my/text-formatting-hooks)


;; (defun my/sk/insert-date (prefix)
;;   "Insert the current date. With prefix-argument, write out the day and month name."
;;   (interactive "P")
;;   (let ((format (cond
;;                  ((not prefix) "%Y-%m-%d")
;;                  ((equal prefix '(4)) "%A, %d %B %Y")
;;                  ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
;;     (insert (format-time-string format))))

;; (bind-keys*
;;  ("M-m g D" . my/sk/insert-date))


;; (defun my/copy-current-file-path ()
;;   "Add current file path to kill ring. Limits the filename to project root if possible."
;;   (interactive)
;;   (kill-new buffer-file-name))

;; (bind-keys*
;;  ("M-m g y" . my/copy-current-file-path))

;; (defun my/move-text-internal (arg)
;;   (cond
;;    ((and mark-active transient-mark-mode)
;;     (if (> (point) (mark))
;;         (exchange-point-and-mark))
;;     (let ((column (current-column))
;;           (text (delete-and-extract-region (point) (mark))))
;;       (forward-line arg)
;;       (move-to-column column t)
;;       (set-mark (point))
;;       (insert text)
;;       (exchange-point-and-mark)
;;       (setq deactivate-mark nil)))
;;    (t
;;     (let ((column (current-column)))
;;       (beginning-of-line)
;;       (when (or (> arg 0) (not (bobp)))
;;         (forward-line)
;;         (when (or (< arg 0) (not (eobp)))
;;           (transpose-lines arg)
;;           (when (and (eval-when-compile
;;                        '(and (>= emacs-major-version 24)
;;                              (>= emacs-minor-version 3)))
;;                      (< arg 0))
;;             (forward-line -1)))
;;         (forward-line -1))
;;       (move-to-column column t)))))

;; (defun my/move-text-down (arg)
;;   "Move region (transient-mark-mode active) or current line
;;   arg lines down."
;;   (interactive "*p")
;;   (my/move-text-internal arg))
;; (defun my/move-text-up (arg)
;;   "Move region (transient-mark-mode active) or current line
;;   arg lines up."
;;   (interactive "*p")
;;   (my/move-text-internal (- arg)))

;; (bind-keys*
;;  ("M-m [ e" . my/move-text-up)
;;  ("M-m ] e" . my/move-text-down))

;; (defun move-file (new-location)
;;   "Write this file to NEW-LOCATION, and delete the old one."
;;   (interactive (list (expand-file-name
;;                       (if buffer-file-name
;;                           (read-file-name "Move file to: ")
;;                         (read-file-name "Move file to: "
;;                                         default-directory
;;                                         (expand-file-name (file-name-nondirectory (buffer-name))
;;                                                           default-directory))))))
;;   (when (file-exists-p new-location)
;;     (delete-file new-location))
;;   (let ((old-location (expand-file-name (buffer-file-name))))
;;     (message "old file is %s and new file is %s"
;;              old-location
;;              new-location)
;;     (write-file new-location t)
;;     (when (and old-location
;;                (file-exists-p new-location)
;;                (not (string-equal old-location new-location)))
;;       (delete-file old-location))))
;; (use-package ws-butler
;;   :hook ((text-mode prog-mode) . ws-butler-mode)
;;   :config (setq ws-butler-keep-whitespace-before-point nil))


;;; Query exchange
;; Inspired from http://www.emacswiki.org/emacs/QueryExchange and definition of
;; `query-replace-regexp' from replace.el
;; (defun query-exchange (string-1 string-2 &optional delimited start end)
;;   "Exchange string-1 and string-2 interactively.
;; The user is prompted at each instance like query-replace. Exchanging
;; happens within a region if one is selected."
;;   (interactive
;;    (let ((common
;;           (query-replace-read-args
;;            (concat "Query replace"
;;                    (if current-prefix-arg " word" "")
;;                    " regexp"
;;                    (if (and transient-mark-mode mark-active) " in region" ""))
;;            t)))
;;      (list (nth 0 common) (nth 1 common) (nth 2 common)
;;            ;; These are done separately here
;;            ;; so that command-history will record these expressions
;;            ;; rather than the values they had this time.
;;            (if (and transient-mark-mode mark-active)
;;                (region-beginning))
;;            (if (and transient-mark-mode mark-active)
;;                (region-end)))))
;;   (perform-replace
;;    (concat "\\(" string-1 "\\)\\|" string-2)
;;    '(replace-eval-replacement replace-quote
;;                               (if (match-string 1) string-2 string-1))
;;    t t delimited nil nil start end))


;; (use-package mmm-mode
;;   :commands mmm-mode
;;   :config
;;   (require 'mmm-auto))



;; (use-package region-convert
;;   :bind
;;   ("C-c C" . region-convert)
;;   )

;;M-x swap-regions [select the first region] C-M-c [select the second region] C-M-c
;; (use-package swap-regions)


;; (use-package loccur
;;   :bind
;;   (("M-s M-l" . loccur-current)
;;    ("M-s M-L" . loccur)
;;    ("M-s C-l" . loccur-previous-match)))


;; (use-package dimmer
;;   :unless noninteractive
;;   :custom
;;   (dimmer-fraction 0.4)
;;   :init
;;   (dimmer-mode t))
