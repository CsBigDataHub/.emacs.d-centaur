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
          try-expand-all-abbrevs))
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

(provide 'init-hippie-expand)
