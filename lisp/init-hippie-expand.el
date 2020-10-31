(use-package dabbrev
  :commands (dabbrev-expand dabbrev-completion)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t))

(defun my-try-expand-company (old)
  (unless company-candidates
    (company-auto-begin))
  (if (not old)
      (progn
        (he-init-string (he-lisp-symbol-beg) (point))
        (if (not (he-string-member he-search-string he-tried-table))
            (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list
              (and (not (equal he-search-string ""))
                   company-candidates))))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

;; The actual expansion function
(defun my-try-expand-by-dict (old)
  ;; old is true if we have already attempted an expansion
  (unless (bound-and-true-p ispell-minor-mode)
    (ispell-minor-mode 1))

  ;; english-words.txt is the fallback dicitonary
  (if (not ispell-alternate-dictionary)
      (setq ispell-alternate-dictionary (file-truename (expand-file-name (concat user-emacs-directory "misc/english-words.txt")))))
  (let ((lookup-func (if (fboundp 'ispell-lookup-words)
                         'ispell-lookup-words
                       'lookup-words)))
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (he-string-member he-search-string he-tried-table))
          (setq he-tried-table (cons he-search-string he-tried-table)))
      (setq he-expand-list
            (and (not (equal he-search-string ""))
                 (funcall lookup-func (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
    (if (null he-expand-list)
        (if old (he-reset-string))
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)
    ))

(use-package hippie-exp
  :after dabbrev
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-list-all-buffers
          try-expand-list
          try-expand-line-all-buffers
          try-expand-line
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          my-try-expand-by-dict
          my-try-expand-company))
  (setq hippie-expand-verbose nil)
  :bind ("M-/" . hippie-expand))

;;https://emacs.stackexchange.com/a/30704
(defun dabbrev-complation-at-point ()
  (dabbrev--reset-global-variables)
  (let* ((abbrev (dabbrev--abbrev-at-point))
         (candidates (dabbrev--find-all-expansions abbrev t))
         (bnd (bounds-of-thing-at-point 'symbol)))
    (list (car bnd) (cdr bnd) candidates)))

;;https://oremacs.com/2017/10/04/completion-at-point/
(defun org-completion-symbols ()
  (when (looking-back "=[a-zA-Z]+")
    (let (cands)
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "=\\([a-zA-Z]+\\)=" nil t)
            (cl-pushnew
             (match-string-no-properties 0) cands :test 'equal))
          cands))
      (when cands
        (list (match-beginning 0) (match-end 0) cands)))))

(defun ora-cap-filesystem ()
  (let (path)
    (when (setq path (ffap-string-at-point))
      (when (string-match "\\`file:\\(.*\\)\\'" path)
        (setq path (match-string 1 path)))
      (let ((compl (all-completions path #'read-file-name-internal)))
        (when compl
          (let* ((str (car compl))
                 (offset
                  (let ((i 0)
                        (len (length str)))
                    (while (and (< i len)
                                (equal (get-text-property i 'face str)
                                       'completions-common-part))
                      (cl-incf i))
                    i)))
            (list (- (point) offset) (point) compl)))))))

(add-to-list 'completion-at-point-functions '(dabbrev-complation-at-point
                                              dabbrev-completion
                                              dabbrev-expand
                                              org-completion-symbols
                                              ora-cap-filesystem
                                              ))

(autoload 'ffap-file-at-point "ffap")
(defun complete-path-at-point+ ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))

(add-hook 'completion-at-point-functions
          #'complete-path-at-point+
          'append)

;; https://emacs.stackexchange.com/questions/15276/how-do-i-write-a-simple-completion-at-point-functions-function
;; NOTES: could not get this working -start-
;; (defvar words (split-string (with-temp-buffer
;;                               (insert-file-contents-literally (expand-file-name (concat user-emacs-directory "misc/aspell-en-sorted.txt")))
;;                               (buffer-string))
;;                             "\n"))

;; (defun words-completion-at-point ()
;;   (let ((bounds (bounds-of-thing-at-point 'word)))
;;     (when bounds
;;       (list (car bounds)
;;             (cdr bounds)
;;             words
;;             :exclusive 'no
;;             :company-docsig #'identity
;;             :company-doc-buffer (lambda (cand)
;;                                   (company-doc-buffer (format "'%s' is defined in '~/.emacs.d/misc/apsell-en-sorted.txt'" cand)))
;;             :company-location (lambda (cand)
;;                                 (with-current-buffer (find-file-noselect (expand-file-name (concat user-emacs-directory "misc/aspell-en-sorted.txt")))
;;                                   (goto-char (point-min))
;;                                   (cons (current-buffer) (search-forward cand nil t))))))))

;; (add-hook 'completion-at-point-functions #'words-completion-at-point 'append)
;; NOTES: could not get this working -end-

;; https://emacs.stackexchange.com/questions/54741/using-company-ispell-with-large-text-dictionary

;; (setq ispell-complete-word-dict
;;       (expand-file-name (concat user-emacs-directory "misc/english-words.txt")))

;; (defun my-generic-ispell-company-complete-setup ()
;;   ;; Only apply this locally.
;;   (make-local-variable 'company-backends)
;;   (setq company-backends (list 'company-ispell))

;;   (when ispell-complete-word-dict
;;     (let*
;;         (
;;          (has-dict-complete
;;           (and ispell-complete-word-dict (file-exists-p ispell-complete-word-dict)))
;;          (has-dict-personal
;;           (and ispell-personal-dictionary (file-exists-p ispell-personal-dictionary)))
;;          (is-dict-outdated
;;           (and
;;            has-dict-complete has-dict-personal
;;            (time-less-p
;;             (nth 5 (file-attributes ispell-complete-word-dict))
;;             (nth 5 (file-attributes ispell-personal-dictionary))))))

;;       (when (or (not has-dict-complete) is-dict-outdated)
;;         (with-temp-buffer

;;           ;; Optional: insert personal dictionary, stripping header and inserting a newline.
;;           (when has-dict-personal
;;             (insert-file-contents ispell-personal-dictionary)
;;             (goto-char (point-min))
;;             (when (looking-at "personal_ws\-")
;;               (delete-region (line-beginning-position) (1+ (line-end-position))))
;;             (goto-char (point-max))
;;             (unless (eq ?\n (char-after))
;;               (insert "\n")))

;;           ;; (call-process "aspell" nil t nil "-d" "en_US" "dump" "master")
;;           ;; Case insensitive sort is important for the lookup.
;;           ;; (let ((sort-fold-case t))
;;           ;;   (sort-lines nil (point-min) (point-max)))
;;           (write-region nil nil ispell-complete-word-dict))))))

;; Enable this in appropriate modes.

;; (add-hook 'org-mode-hook (lambda () (my-generic-ispell-company-complete-setup)))
;; (add-hook 'rst-mode-hook (lambda () (my-generic-ispell-company-complete-setup)))
;; (add-hook 'markdown-mode-hook (lambda () (my-generic-ispell-company-complete-setup)))

;; https://emacs.stackexchange.com/questions/42508/how-to-use-an-ispell-dictionary-in-company-mode/42526#42526
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(defun text-mode-hook-setup ()
  ;; make `company-backends' local is critcal
  ;; or else, you will have completion in every major mode, that's very annoying!
  (make-local-variable 'company-backends)

  ;; company-ispell is the plugin to complete words
  (add-to-list 'company-backends 'company-ispell)

  ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
  ;;  but I prefer hard code the dictionary path. That's more portable.
  (setq company-ispell-dictionary (file-truename "~/.emacs.d/misc/english-words.txt"))
  ;; (setq company-ispell-dictionary (file-truename "~/.emacs.d/misc/apsell-en-sorted.txt"))
  )

(add-hook 'text-mode-hook 'text-mode-hook-setup)

;; https://endlessparentheses.com/ispell-and-org-mode.html
(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_example" . "^#\\+end_example"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE" . "^#\\+END_EXAMPLE"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  )
(add-hook 'org-mode-hook #'endless/org-ispell)

;; https://emacs.stackexchange.com/questions/54754/how-to-change-the-company-complete-backend-based-on-the-current-syntaxG
(defun my-in-comment-p (pos)
  "Check whether the code at POS is comment by comparing font face."
  (let* ((fontfaces (get-text-property pos 'face)))
    (if (not (listp fontfaces))
        (setq fontfaces (list fontfaces)))
    (delq nil
          (mapcar #'(lambda (f)
                      ;; learn this trick from flyspell
                      (or (eq f 'font-lock-comment-face)
                          (eq f 'font-lock-comment-delimiter-face)))
                  fontfaces))))

(eval-after-load 'company-ispell
  '(progn
     ;; use company-ispell in comment when coding
     (defadvice company-ispell-available (around company-ispell-available-hack activate)
       (cond
        ((and (derived-mode-p 'prog-mode)
              (or (not (company-in-string-or-comment)) ; respect advice in `company-in-string-or-comment'
                  (not (my-in-comment-p (point))))) ; auto-complete in comment only
         (setq ad-return-value nil))
        (t
         ad-do-it)))))

(provide 'init-hippie-expand)
