;;; -*- lexical-binding: t; -*-
;; Diff last two kills
(defun diff-last-two-kills ()
  "Write the last two kills to temporary files and diff them."
  (interactive)
  (let ((old "/tmp/old-kill") (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (diff old new "-u" t)))

;;----------------------------------------------------------------------------
;;https://oremacs.com/2014/12/23/upcase-word-you-silly/
;;----------------------------------------------------------------------------
(defadvice upcase-word (before upcase-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))

(defadvice downcase-word (before downcase-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))

(defadvice capitalize-word (before capitalize-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))

;; Never understood why Emacs doesn't have this function, either.
;;
(defun my/move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn	(copy-file filename newname 1)	(delete-file filename)	(set-visited-file-name nil)))))

(defun my/put-current-path-to-clipboard ()
  "copy current buffer or file path to clipboard"
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (expand-file-name file-path))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (expand-file-name dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

(defun my/put-current-filename-to-clipboard ()
  "copy current buffer or file name to clipboard"
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (file-name-nondirectory file-path))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (file-name-nondirectory dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

(defun my/put-current-filename-with-line-to-clipboard ()
  "copy current file name or buffer name with a line to clipboard"
  (interactive)
  (let ((file-path buffer-file-name)
        (dir-path default-directory))
    (cond (file-path
           (kill-new (format "%s:%s"
                             (file-name-nondirectory file-path)
                             (count-lines (point-min) (point))))
           (message "This file path is on the clipboard!"))
          (dir-path
           (kill-new (file-name-nondirectory dir-path))
           (message "This directory path is on the clipboard!"))
          (t
           (error-message-string "Fail to get path name.")))))

(defun my/kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

;;eval-and-replace
;; https://emacsredux.com/blog/2013/06/21/eval-and-replace/

(defun my/eval-and-replace (beginning end)
  "Replace the preceding sexp or region with its value."
  (interactive "r")
  (if (region-active-p)
      (delete-region beginning end)
    (backward-kill-sexp))
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(add-hook 'org-mode-hook
          (lambda ()
            (defun my/insert-org-screenshot ()
              "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
              (interactive)
              (setq filename
                    (concat
                     (make-temp-name
                      (concat (file-name-nondirectory (buffer-file-name))
                              "_imgs/"
                              (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
              (unless (file-exists-p (file-name-directory filename))
				(make-directory (file-name-directory filename)))
                                        ; take screenshot
              (if (eq system-type 'darwin)
                  (call-process "screencapture" nil nil nil "-i" filename))
              (if (eq system-type 'gnu/linux) ; changed from `input' to `flameshot'
                  (call-process "gnome-screenshot" nil nil nil "-a" "-f" filename))
                                        ; insert into file if correctly taken
              (if (file-exists-p filename)
                  (insert (concat "[[file:" filename "]]")))
              (org-display-inline-images)
              )))

(add-hook 'markdown-mode-hook
          (lambda ()
            (defun my/insert-md-screenshot ()
              "Take a screenshot into a time stamped unique-named file in the
same directory as the markdown-mode-buffer and insert a link to this file."
              (interactive)
              (setq filename
                    (concat
                     (make-temp-name
                      (concat (file-name-nondirectory (buffer-file-name))
                              "_imgs/"
                              (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
              (unless (file-exists-p (file-name-directory filename))
                (make-directory (file-name-directory filename)))
                                        ; take screenshot
              (if (eq system-type 'darwin)
                  (call-process "screencapture" nil nil nil "-i" filename))
              (if (eq system-type 'gnu/linux)
                  (call-process "gnome-screenshot" nil nil nil "-a" "-f" filename))
                                        ; changed from `input' to `flameshot'
                                        ; insert into file if correctly taken
              (if (file-exists-p filename)
                  (insert (concat "![" filename "](" filename ")")))
              (markdown-toggle-inline-images)
              )))

(when (or sys/linux-x-p sys/linuxp)
  (setq org-download-screenshot-method "flameshot gui --raw > %s"))

(when sys/macp
  (setq-default org-download-screenshot-method "screencapture -i %s"))

(require 'ido)
(defun xah-insert-date ()
  "Insert current date time.
Insert date in this format: yyyy-mm-dd.
If `universal-argument' is called first, prompt for a format to use.
If there's text selection, delete it first.

URL `http://xahlee.info/emacs/emacs/elisp_insert-date-time.html'
version 2020-09-07"
  (interactive)
  (let (($style
         (if current-prefix-arg
             (string-to-number
              (substring
               (ido-completing-read
                "Style:"
                '(
                  "1 → 2018-04-12 Thursday"
                  "2 → 20180412224611"
                  "3 → 2018-04-12T22:46:11-07:00"
                  "4 → 2018-04-12 22:46:11-07:00"
                  "5 → Thursday, April 12, 2018"
                  "6 → Thu, Apr 12, 2018"
                  "7 → April 12, 2018"
                  "8 → Apr 12, 2018"
                  )) 0 1))
           0
           )))
    (when (use-region-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((= $style 0)
       ;; "2016-10-10"
       (format-time-string "%Y-%m-%d"))
      ((= $style 1)
       ;; "2018-04-12 Thursday"

       (format-time-string "%Y-%m-%d %A"))
      ((= $style 2)
       ;; "20180412224015"
       (replace-regexp-in-string ":" "" (format-time-string "%Y%m%d%T")))
      ((= $style 3)
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z")))
       ;; "2018-04-12T22:45:26-07:00"
       )
      ((= $style 4)
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z")))
       ;; "2018-04-12 22:46:11-07:00"
       )
      ((= $style 5)
       (format-time-string "%A, %B %d, %Y")
       ;; "Thursday, April 12, 2018"
       )
      ((= $style 6)
       (format-time-string "%a, %b %d, %Y")
       ;; "Thu, Apr 12, 2018"
       )
      ((= $style 7)
       (format-time-string "%B %d, %Y")
       ;; "April 12, 2018"
       )
      ((= $style 8)
       (format-time-string "%b %d, %Y")
       ;; "Apr 12, 2018"
       )
      (t
       (format-time-string "%Y-%m-%d"))))))


(defun my/xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
    This command calls `yank', and if repeated, call `yank-pop'."
  ;;
  ;;When `universal-argument' is called first with a number arg, paste that many times.
  ;;
  ;;URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
  ;;Version 2017-07-25"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes ($i (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

(global-set-key (kbd "C-y") 'my/xah-paste-or-paste-previous)

(defun my/xah-delete-blank-lines ()
  "Delete all newline around cursor.

      URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
      Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
    (skip-chars-backward "\n")
    (setq $p3 (point))
    (skip-chars-forward "\n")
    (setq $p4 (point))
    (delete-region $p3 $p4)))

(defun my/xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.

        Shrink any neighboring space tab newline characters to 1 or none.
        If cursor neighbor has space/tab, toggle between 1 or 0 space.
        If cursor neighbor are newline, shrink them to just 1.
        If already has just 1 whitespace, delete it.

        URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
        Version 2018-04-02T14:38:04-07:00"
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (delete-horizontal-space)
        (progn (delete-horizontal-space)
               (insert " "))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (goto-char $p2)
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(defun my/window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun my/scroll-up-half ()
  (interactive)
  (scroll-up (my/window-half-height)))

(defun my/scroll-down-half ()
  (interactive)
  (scroll-down (my/window-half-height)))

(global-set-key (kbd "H-d") 'my/scroll-up-half)
(global-set-key (kbd "H-u") 'my/scroll-down-half)
;;Scrolling 4 lines without moving the point
;;(global-set-key (kbd "M-n")  (lambda () (interactive) (scroll-up   4)) )
;;(global-set-key (kbd "M-p")  (lambda () (interactive) (scroll-down 4)) )


(defun my/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(bind-keys*
 ("M-m g R" . my/rename-current-buffer-file))

;;if you're windened, narrow to the region, if you're narrowed, widen;
;;bound to C-x n
(defun my/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
    Intelligently means: region, org-src-block, org-subtree, or defun,
    whichever applies first.
    Narrowing to org-src-block actually calls `org-edit-src-code'.

    With prefix P, don't widen, just narrow even if buffer is already
    narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;; (define-key endless/toggle-map "n" #'my/narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
(global-set-key (kbd "C-x n N") 'my/narrow-or-widen-dwim)

;; https://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(global-set-key (kbd "C-x x s") 'endless/ispell-word-then-abbrev)

;;; Tell ispell.el that ’ can be part of a word.
(setq ispell-local-dictionary-alist
      `((nil "[[:alpha:]]" "[^[:alpha:]]"
             "['\x2019]" nil ("-B") nil utf-8)))

;;; Don't send ’ to the subprocess.
(defun endless/replace-apostrophe (args)
  (cons (replace-regexp-in-string
         "’" "'" (car args))
        (cdr args)))
(advice-add #'ispell-send-string :filter-args
            #'endless/replace-apostrophe)

;;; Convert ' back to ’ from the subprocess.
(defun endless/replace-quote (args)
  (if (not (derived-mode-p 'org-mode))
      args
    (cons (replace-regexp-in-string
           "'" "’" (car args))
          (cdr args))))
(advice-add #'ispell-parse-output :filter-args
            #'endless/replace-quote)

(defun fixup-json ()
  "Re-indent json buffers with broken literal strings. Needs jsonpp installed (available using homebrew)"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "sed -e ':a' -e 'N' -e '$!ba' -e 's/\\n/ /g' | jsonpp"  nil t))


(defun my/open-config ()
  "Opens the configuration file from anywhere"
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))


(defun my/goto-closest-number ()
  (interactive)
  (let ((closest-behind (save-excursion (search-backward-regexp "[0-9]" nil t)))
        (closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
    (push-mark)
    (goto-char
     (cond
      ((and (not closest-ahead) (not closest-behind)) (error "No numbers in buffer"))
      ((and closest-ahead (not closest-behind)) closest-ahead)
      ((and closest-behind (not closest-ahead)) closest-behind)
      ((> (- closest-ahead (point)) (- (point) closest-behind)) closest-behind)
      ((> (- (point) closest-behind) (- closest-ahead (point))) closest-ahead)
      (t closest-ahead)))))

(defun my/split-below-and-move ()
  (interactive)
  (split-window-below)
  (other-window 1))
(defun my/split-right-and-move ()
  (interactive)
  (split-window-right)
  (other-window 1))

(bind-keys
 ("C-x 2" . my/split-below-and-move)
 ("C-x 3" . my/split-right-and-move))


(defun my/other-window-down ()
  "Scrolls down in adjoining window"
  (interactive)
  (other-window 1)
  (scroll-up-command)
  (other-window 1))

(defun my/other-window-up ()
  "Scrolls up in adjoining window"
  (interactive)
  (other-window 1)
  (scroll-down-command)
  (other-window 1))

(bind-keys*
 ("M-m g ]" . my/other-window-down)
 ("M-m g [" . my/other-window-up))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)


(defun my/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(bind-keys*
 ("M-m g B" . my/browse-current-file))


(defun my/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun my/toggle-frame-fullscreen-non-native ()
  "Toggle full screen non-natively. Uses the `fullboth' frame paramerter
   rather than `fullscreen'. Useful to fullscreen on OSX w/o animations."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
           (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
             (if (eq (frame-parameter nil 'maximized) 'maximized)
                 'maximized)
           'fullboth)))))

(pretty-hydra-define+ ace-window-hydra()
  (
   "Split Windows"
   (("V" my/split-right-and-move "my/split-right-and-move")
    ("H" my/split-below-and-move "my/split-below-and-move")
    ("F" my/toggle-frame-fullscreen-non-native "my-fullscreen")
    ("o" my/rotate-windows "rotate-windows")
    ("X" delete-other-windows "delete-other-windows"))
   "winner"
   (("U" (progn
           (winner-undo)
           (setq this-command 'winner-undo)) "winner-undo")
    ("R" winner-redo "winner-redo")
    ("Z" zoom-window-zoom "zoom-window-zoom"))
   )
  )

(defun my/incs (s &optional num)
  (let* ((inc (or num 1))
         (new-number (number-to-string (+ inc (string-to-number s))))
         (zero-padded? (s-starts-with? "0" s)))
    (if zero-padded?
        (s-pad-left (length s) "0" new-number)
      new-number)))

(defun my/change-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
              (looking-back "[0-9]"))
    (my/goto-closest-number))
  (save-excursion
    (while (looking-back "[0-9]")
      (forward-char -1))
    (re-search-forward "[0-9]+" nil)
    (replace-match (my/incs (match-string 0) arg) nil nil)))

(defun my/subtract-number-at-point (arg)
  (interactive "p")
  (my/change-number-at-point (- arg)))

(defun my/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun my/align-ampersand (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

(defun my/align-quote-space (start end)
  "Align columns by quote and space"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\).*\\s-\"" 1 0 t))

(defun my/align-equals (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)=" 1 0 t))

(defun my/align-comma (start end)
  "Align columns by comma"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)," 1 1 t))

(defun my/align-dot (start end)
  "Align columns by dot"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\\." 1 1 t))

(defun my/align-colon (start end)
  "Align columns by equals sign"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\):" 1 0 t))

;; https://blog.lambda.cx/posts/emacs-align-columns/
(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END
                "\\(\\s-*\\)\\S-+" 1 1 t))

(bind-keys*
 ("M-m g A SPC" . my/align-whitespace)
 ("M-m g A &"   . my/align-ampersand)
 ("M-m g A ,"   . my/align-comma)
 ("M-m g A \""  . my/align-quote-space)
 ("M-m g A ."   . my/align-dot)
 ("M-m g A ="   . my/align-equals)
 ("M-m g A :"   . my/align-colon)
 ("M-m g A A"   . align-regexp))

(which-key-add-key-based-replacements
  "M-m g" "custom-function")

(which-key-add-key-based-replacements
  "M-m g A" "align-prefix")

(pretty-hydra-define align-hydra (:title (pretty-hydra-title "Align your code" 'faicon "align-justify" :v-adjust -0.1)
                                  :color amaranth :quit-key "q")
  ("Align"
   (
    ("SPC" my/align-whitespace "Align at <SPC>")
    ("&" my/align-ampersand "Align at &")
    ("," my/align-comma "Align at ,")
    ("\"" my/align-quote-space "Align at \"")
    ("." my/align-dot "Align at .")
    ("=" my/align-equals "Align at =")
    (":" my/align-colon "Align at :")
    ("A" align-regexp "Align at A")
    ))
  )

(defun counsel-goto-recent-directory ()
  "Open recent directory with dired"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection :action 'dired)))

(defun my/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(bind-keys*
 ("M-m g K" . my/delete-current-buffer-file))

;; Transpose words forward
(defun my/transpose-words-forward ()
  "Transpose words forward"
  (interactive)
  (forward-word 1)
  (forward-char 1)
  (transpose-words 1)
  (backward-word 1))
;; Transpose words backward
(defun my/transpose-words-backward ()
  "Transpose words backward"
  (interactive)
  (transpose-words 1)
  (backward-word 1))

(bind-keys*
 ("M-m [ w" . my/transpose-words-backward)
 ("M-m ] w" . my/transpose-words-forward))


;; Transpose chars forward
(defun my/transpose-chars-forward ()
  "Transpose chars forward"
  (interactive)
  (forward-char 1)
  (transpose-chars 1)
  (backward-char 1))
;; Transpose chars backward
(defun my/transpose-chars-backward ()
  "Transpose chars backward"
  (interactive)
  (transpose-chars 1)
  (backward-char 1))


(bind-keys*
 ("M-m [ c" . my/transpose-chars-backward)
 ("M-m ] c" . my/transpose-chars-forward))


(defun my/duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun my/duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (my/duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

(defun my/duplicate-line-or-region (&optional num)
  "Duplicate the current line or region if active"
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (my/duplicate-region num beg end)))
  (my/duplicate-current-line num))

(bind-keys*
 ("M-m g d" . my/duplicate-line-or-region))

(defun my/join-line ()
  "Join the current line with the next line"
  (interactive)
  (forward-line)
  (delete-indentation))

(bind-keys
 ("C-S-j" . my/join-line))

(defun my/select-inside-line ()
  "Select the current line"
  (interactive)
  (my/smarter-move-beginning-of-line 1)
  (set-mark (line-end-position))
  (exchange-point-and-mark))

(defun my/select-around-line ()
  "Select line including the newline character"
  (interactive)
  (my/select-inside-line)
  (forward-line 1)
  (my/smarter-move-beginning-of-line 1))

(bind-keys*
 ("M-m i l" . my/select-inside-line)
 ("M-m a l" . my/select-around-line))

(defun my/replace-snake-case-with-camel-case (arg)
  "Change snake case to camel case"
  (interactive "p")
  (if (> arg 0)
      (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (let ((case-fold-search nil))
    (while (not (= arg 1))
      (search-forward-regexp "\\b_[a-z]")
      (forward-char -2)
      (delete-char 1)
      (capitalize-word 1)
      (setq arg (1- arg)))))


(bind-keys*
 ("M-m g C" . my/replace-snake-case-with-camel-case))

(defun my/snakeify-current-word ()
  (interactive)
  (er/mark-word)
  (let* ((beg (region-beginning))
         (end (region-end))
         (current-word (buffer-substring-no-properties beg end))
         (snakified (snake-case current-word)))
    (replace-match current-word snakified nil beg end)))

(bind-keys*
 ("M-m g _" . my/snakeify-current-word))

(defun xah-change-file-line-ending-style (@files @style)
  "Change current file or dired marked file's newline convention.

When called non-interactively, *style is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2016-10-16"
  (interactive
   (list
    (if (eq major-mode 'dired-mode )
        (dired-get-marked-files)
      (list (buffer-file-name)))
    (ido-completing-read "Line ending:" '("Linux/MacOSX/Unix" "MacOS9" "Windows") "PREDICATE" "REQUIRE-MATCH")))
  (let* (
         ($codingSystem
          (cond
           ((equal @style "Linux/MacOSX/Unix") 'unix)
           ((equal @style "MacOS9") 'mac)
           ((equal @style "Windows") 'dos)
           (t (error "code logic error 65327. Expect one of it." )))))
    (mapc
     (lambda (x) (xah-convert-file-coding-system x $codingSystem))
     @files)))

(defun xah-convert-file-coding-system (@fpath @coding-system)
  "Convert file's encoding.
 *fpath is full path to file.
 *coding-system is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

If the file is already opened, it will be saved after this command.

URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2015-07-24"
  (let ($buffer
        ($bufferOpened-p (get-file-buffer @fpath)))
    (if $bufferOpened-p
        (with-current-buffer $bufferOpened-p
          (set-buffer-file-coding-system @coding-system)
          (save-buffer))
      (progn
        (setq $buffer (find-file @fpath))
        (set-buffer-file-coding-system @coding-system)
        (save-buffer)
        (kill-buffer $buffer)))))

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2019-11-24"
  (interactive)
  (require 'dired-aux)
  (if (eq major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "-" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to underscore _.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2017-01-02"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired.")))

(progn
  (require 'dired )
  (define-key dired-mode-map (kbd "_") 'xah-dired-rename-space-to-underscore)
  (define-key dired-mode-map (kbd "-") 'xah-dired-rename-space-to-hyphen)
  ;;
  )

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char.
Works on current block or selection.

   cat dog cow - becomes -  cat
                            dog
                            cow

URL `http://ergoemacs.org/emacs/emacs_space_to_newline.html'
Version 2017-08-19"
  (interactive)
  (let* ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "move")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil t)
          (replace-match "\n" ))))))

(defun xah-quote-lines ()
  "Change current text block's lines to quoted lines with comma or other separator char.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

For example,

 cat
 dog
 cow

becomes

 \"cat\",
 \"dog\",
 \"cow\",

or

 (cat)
 (dog)
 (cow)

If the delimiter is any left bracket, the end delimiter is automatically the matching bracket.

URL `http://ergoemacs.org/emacs/emacs_quote_lines.html'
Version 2017-01-08"
  (interactive)
  (let* (
         $p1
         $p2
         ($quoteToUse
          (read-string
           "Quote to use:" "\"" nil
           '(
             ""
             "\""
             "'"
             "("
             "{"
             "["
             )))
         ($separator
          (read-string
           "line separator:" "," nil
           '(
             ""
             ","
             ";"
             )))
         ($beginQuote $quoteToUse)
         ($endQuote
          ;; if begin quote is a bracket, set end quote to the matching one. else, same as begin quote
          (let (($syntableValue (aref (syntax-table) (string-to-char $beginQuote))))
            (if (eq (car $syntableValue ) 4) ; ; syntax table, code 4 is open paren
                (char-to-string (cdr $syntableValue))
              $quoteToUse
              ))))
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "NOERROR")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (skip-chars-forward "\t ")
        (insert $beginQuote)
        (goto-char (point-max))
        (insert $endQuote)
        (goto-char (point-min))
        (while (re-search-forward "\n\\([\t ]*\\)" nil "NOERROR" )
          (replace-match
           (concat $endQuote $separator (concat "\n" (match-string 1)) $beginQuote) "FIXEDCASE" "LITERAL"))
        ;;
        ))))


(defun xah-make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is in this format
 x.html~2018-05-15_133429~
 The last part is hour, minutes, seconds.
in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done.

URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2018-05-15"
  (interactive)
  (let (($fname (buffer-file-name))
        ($date-time-format "%Y-%m-%d_%H%M%S"))
    (if $fname
        (let (($backup-name
               (concat $fname "~" (format-time-string $date-time-format) "~")))
          (copy-file $fname $backup-name t)
          (message (concat "Backup saved at: " $backup-name)))
      (if (string-equal major-mode "dired-mode")
          (progn
            (mapc (lambda ($x)
                    (let (($backup-name
                           (concat $x "~" (format-time-string $date-time-format) "~")))
                      (copy-file $x $backup-name t)))
                  (dired-get-marked-files))
            (message "marked files backed up"))
        (user-error "buffer not file nor dired")))))

(defun xah-make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `xah-make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (if (buffer-file-name)
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))


(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if @fname is given, open that.
URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04 2021-02-16"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                       (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(defun xah-show-in-file-manager ()
  "Show current file in desktop.
 (Mac Finder, File Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.
URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2020-11-20 2021-01-18"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) default-directory)))
    (cond
     ((string-equal system-type "windows-nt")
      (shell-command (format "PowerShell -Command Start-Process Explorer -FilePath %s" (shell-quote-argument default-directory)))
      ;; todo. need to make window highlight the file
      )
     ((string-equal system-type "darwin")
      (if (eq major-mode 'dired-mode)
          (let (($files (dired-get-marked-files )))
            (if (eq (length $files) 0)
                (shell-command (concat "open " (shell-quote-argument (expand-file-name default-directory ))))
              (shell-command (concat "open -R " (shell-quote-argument (car (dired-get-marked-files )))))))
        (shell-command
         (concat "open -R " (shell-quote-argument $path)))))

     ((string-equal system-type "gnu/linux")
      (let (
            (process-connection-type nil)
            (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                 "/usr/bin/gvfs-open"
                               "/usr/bin/xdg-open")))
        (start-process "" nil openFileProgram (shell-quote-argument $path)))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      ))))

(defun xah-open-in-vscode ()
  "Open current file or dir in vscode.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2020-02-13"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) (expand-file-name default-directory ) )))
    (message "path is %s" $path)
    (cond
     ((string-equal system-type "darwin")
      ;; (shell-command (format "open -a Visual\\ Studio\\ Code.app \"%s\"" $path)))
      (shell-command (format "open -a VSCodium.app \"%s\"" $path)))
     ((string-equal system-type "windows-nt")
      (shell-command (format "Code \"%s\"" $path)))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "code \"%s\"" $path))))))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
on Microsoft Windows, it starts cross-platform PowerShell pwsh. You need to have it installed.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2020-11-21 2021-01-18"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (let ((process-connection-type nil))
      (shell-command (concat "PowerShell -Command Start-Process pwsh -WorkingDirectory " (shell-quote-argument default-directory)))
      ;;
      ))
   ((string-equal system-type "darwin")
    (shell-command (concat "open -a iterm " (shell-quote-argument (expand-file-name default-directory )))))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory))))))

(when sys/macp
  (defun xah-open-in-textedit ()
    "Open the current file or `dired' marked files in Mac's TextEdit.
This command is for macOS only.

this is great for spell checking! just open it in TextEdit, and you get all misspelled words highlighted automatically. In emacs, it's 10 times slower and doesn't work well.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2017-11-21"
    (interactive)
    (let* (
           ($file-list
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name))))
           ($do-it-p (if (<= (length $file-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files? "))))
      (when $do-it-p
        (cond
         ((string-equal system-type "darwin")
          (mapc
           (lambda ($fpath)
             (shell-command
              (format "open -a TextEdit.app \"%s\"" $fpath))) $file-list)))))))

(defun xah-html-open-in-chrome-browser ()
  "Open the current file or `dired' marked files in Google Chrome browser.
Work in Windows, macOS, linux.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-10"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (format "open -a /Applications/Google\\ Chrome.app \"%s\"" $fpath)))
         $file-list))
       ((string-equal system-type "windows-nt")
        ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
        (let ((process-connection-type nil))
          (mapc
           (lambda ($fpath)
             (start-process "" nil "powershell" "start-process" "chrome" $fpath ))
           $file-list)))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath)
           (shell-command (format "google-chrome-stable \"%s\"" $fpath)))
         $file-list))))))

(defun xah-html-open-link-in-chrome ()
  "Open url under cursor in Google Chrome.
Work in Windows, macOS, linux.
Version 2019-11-10"
  (interactive)
  (let* (($inputStr
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ($p0 $p1 $p2
                      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (setq $p0 (point))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a Google\\ Chrome.app \"%s\"" $path)))
     ((string-equal system-type "windows-nt")
      ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
      (let ((process-connection-type nil))
        (start-process "" nil "powershell" "start-process" "chrome" $path )))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "google-chrome-stable \"%s\"" $path))))))


(defun xah-html-open-link-in-firefox (&optional @fullpath)
  "open url under cursor in Firefox browser.
Work in Windows, macOS. 2019-11-09 linux not yet.
Version 2019-11-09"
  (interactive)
  (let ($path)
    (if @fullpath
        (progn (setq $path @fullpath))
      (let (($inputStr
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (let ($p0 $p1 $p2
                         ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                 (setq $p0 (point))
                 (skip-chars-backward $pathStops)
                 (setq $p1 (point))
                 (goto-char $p0)
                 (skip-chars-forward $pathStops)
                 (setq $p2 (point))
                 (goto-char $p0)
                 (buffer-substring-no-properties $p1 $p2)))))
        (setq $path (replace-regexp-in-string
                     "^file:///" "/"
                     (replace-regexp-in-string
                      ":\\'" "" $inputStr)))))
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a 'Firefox.app' \"%s\"" $path)))
     ((string-equal system-type "windows-nt")
      ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
      (let ((process-connection-type nil))
        (start-process "" nil "powershell" "start-process" "firefox" $path )))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "firefox \"%s\"" $path))))))

(when sys/linuxp
  (defun xah-html-open-in-brave ()
    "Open the current file or `dired' marked files in Brave browser.
If the file is not saved, save it first.
Version 2019-11-10"
    (interactive)
    (let* (
           ($file-list
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name))))
           ($do-it-p (if (<= (length $file-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files? "))))
      (when $do-it-p
        (cond
         ((string-equal system-type "darwin")
          (mapc
           (lambda ($fpath)
             (shell-command (format "open -a 'Brave Browser.app' \"%s\"" $fpath)))
           $file-list))
         ((string-equal system-type "windows-nt")
          ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
          (let ((process-connection-type nil))
            (mapc
             (lambda ($fpath)
               (start-process "" nil "powershell" "start-process" "brave" $fpath ))
             $file-list)))
         ((string-equal system-type "gnu/linux")
          (mapc
           (lambda ($fpath)
             (shell-command (format "brave \"%s\"" $fpath)))
           $file-list)))))))

(when sys/linuxp
  (defun xah-html-open-link-in-brave (&optional @fullpath)
    "open url under cursor in Brave browser.
Work in Mac OS only
Version 2019-02-17"
    (interactive)
    (let ($path)
      (if @fullpath
          (progn (setq $path @fullpath))
        (let (($inputStr
               (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let ($p0 $p1 $p2
                           ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                   (setq $p0 (point))
                   (skip-chars-backward $pathStops)
                   (setq $p1 (point))
                   (goto-char $p0)
                   (skip-chars-forward $pathStops)
                   (setq $p2 (point))
                   (goto-char $p0)
                   (buffer-substring-no-properties $p1 $p2)))))
          (setq $path (replace-regexp-in-string
                       "^file:///" "/"
                       (replace-regexp-in-string
                        ":\\'" "" $inputStr)))))
      (cond
       ((string-equal system-type "darwin")
        (shell-command (format "open -a 'Brave Browser.app' \"%s\"" $path)))
       ((string-equal system-type "windows-nt")
        ;; "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" 2019-11-09
        (let ((process-connection-type nil))
          (start-process "" nil "powershell" "start-process" "brave" $path )))
       ((string-equal system-type "gnu/linux")
        (shell-command (format "brave \"%s\"" $path)))))))

(when sys/macp
  (defun xah-open-in-safari ()
    "Open the current file or `dired' marked files in Mac's Safari browser.

If the file is not saved, save it first.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2018-02-26"
    (interactive)
    (let* (
           ($file-list
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name))))
           ($do-it-p (if (<= (length $file-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files? "))))
      (when $do-it-p
        (cond
         ((string-equal system-type "darwin")
          (mapc
           (lambda ($fpath)
             (when (buffer-modified-p )
               (save-buffer))
             (shell-command
              (format "open -a Safari.app \"%s\"" $fpath))) $file-list)))))))

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17"
  (interactive)
  (let* (
         ($inputStr
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ($p0 $p1 $p2
                      ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (setq $p0 (point))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (let (($x (xahsite-url-to-filepath $path)))
              (if (string-match "^http" $x )
                  (browse-url $x)
                (find-file $x)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "#" $path )
            (let (
                  ( $fpath (substring $path 0 (match-beginning 0)))
                  ( $fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char 1)
                    (search-forward $fractPart ))
                (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char 1)
                      (forward-line (1- $line-num)))
                  (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
                  (find-file $path ))))))))))

(defun dired-copy-file-path-as-kill ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (dired-copy-filename-as-kill 0))

(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(defvar dired-compress-files-alist
  '(("\\.tar\\.gz\\'" . "tar -c %i | gzip -c9 > %o")
    ("\\.zip\\'" . "zip %o -r --filesync %i"))
  "Control the compression shell command for `dired-do-compress-to'.

Each element is (REGEXP . CMD), where REGEXP is the name of the
archive to which you want to compress, and CMD the the
corresponding command.

Within CMD, %i denotes the input file(s), and %o denotes the
output file. %i path(s) are relative, while %o is absolute.")

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2018-12-23"
  (interactive)
  (let ($sort-by $arg)
    (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" )))
    (cond
     ((equal $sort-by "name") (setq $arg "-Alh"))
     ((equal $sort-by "date") (setq $arg "-Alh -t"))
     ((equal $sort-by "size") (setq $arg "-Alh -S"))
     ((equal $sort-by "dir") (setq $arg "-Alh --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other $arg )))


(defun directory-is-empty-p (directory-name)
  "check if directory is empty"
  (null (directory-files directory-name nil
                         directory-files-no-dot-files-regexp t)))

(defun dired-mark-empty-dirs ()
  "Mark Empty Folders in Dired"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (let ((curr-dir))
      (save-excursion
        (dired-go-to-first)

        (while (not (eobp))
          (setq curr-dir (dired-file-name-at-point))
          (cond ((or (null curr-dir)
                     (string= curr-dir ".")
                     (string= curr-dir ".."))
                 ;; do nothing here
                 )
                ((file-directory-p curr-dir)
                 (when (directory-is-empty-p curr-dir)
                   (dired-mark 1)
                   (dired-previous-line 1))))
          (dired-next-line 1))))))

(defun dired-get-size ()
  "get size of marked files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

;;Hydra for Dired
(pretty-hydra-define my/hydra-dired (:title (pretty-hydra-title "DIRED" 'faicon "folder-open-o" :v-adjust -0.1)
                                     :foreign-keys run :color amaranth :quit-key "q")
  ("Actions"
   (("g" revert-buffer "refresh")        ;; read all directories again (refresh)
    ("l" dired-do-redisplay "redisplay")   ;; relist the marked or singel directory
    ("C" dired-do-copy "copy")
    ("D" dired-do-delete "delete")
    ("G" dired-do-chgrp "chgrp")
    ("M" dired-do-chmod "chmod")
    ("+" dired-create-directory "mkdir")
    ("R" dired-do-rename "mv")
    ("z" dired-do-compress-to "compress-to")
    ("Z" dired-do-compress "compress")
    ("-" xah-dired-rename-space-to-hyphen "SPC to -")
    ("_" xah-dired-rename-space-to-underscore "SPC to _")
    ("P" peep-dired "peep")
    ("<return>" dired-find-file "find file"))
   "Rsync/Symlink"
   (("y" ora-dired-rsync "Copy big file")
    ("r" dired-rsync "rsync")
    ("S" dired-do-symlink "symlink")
    ("Y" dired-do-relsymlink "rel-symlink"))
   "File Actions"
   (("O" dired-display-file "view other")
    ("o" dired-find-file-other-window "open other window")
    ("v" dired-view-file "view")      ;; q to exit, s to search, = gets line #
    ("i" dired-maybe-insert-subdir "insert subdir")
    ("w" dired-kill-subdir "delete subdir"))
   "Filter/Sort"
   (("n" dired-narrow "filter")
    ("A" dired-do-find-regexp "find regex")
    ("(" dired-hide-details-mode "details")
    (")" dired-omit-mode "omit")
    ("s" xah-dired-sort "sort")
    ("Q" dired-do-find-regexp-and-replace "replace regex"))
   "Clipboard"
   (("fc" dired-copy-filename-as-kill "copy name")
    ("fp" dired-copy-file-path-as-kill "copy path"))
   "diff"
   (("e" ora-ediff-files "ediff marked")
    ("d" ztree-dired-diff-toggle "zdiff panes"))
   "Info"
   (("??" dired-summary "info")
    ("?/" dired-get-size "marked file size")
    ("I" dired-git-info-mode "git info")
    (">" dired-subtree-toggle "tree")
    ("<" dired-subtree-cycle "tree cycle"))
   "Mark"
   (("E" dired-mark-extension "mark exten.")
    ("m" dired-mark "mark")
    ("@" dired-mark-files-regexp "mark regex")
    ("F" dired-do-find-marked-files "open marked files")
    ("t" dired-toggle-marks "toggle marks")
    ("U" dired-unmark-all-marks "unmark all")
    ("u" dired-unmark "unmark")
    ("0" dired-mark-empty-dirs "mark empty dirs"))
   ))

;;(define-key dired-mode-map "." 'my/hydra-dired/body)




(defhydra my/hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" my/hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" my/hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" my/hydra-ibuffer-sort/body :color blue)
  ("/" my/hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra my/hydra-ibuffer-mark (:color teal :columns 5
                                 :after-exit (my/hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" my/hydra-ibuffer-main/body "back" :color blue))

(defhydra my/hydra-ibuffer-action (:color teal :columns 4
                                   :after-exit
                                   (if (eq major-mode 'ibuffer-mode)
                                       (my/hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra my/hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" my/hydra-ibuffer-main/body "back" :color blue))

(defhydra my/hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" my/hydra-ibuffer-main/body "back" :color blue))

;;(define-key ibuffer-mode-map "." 'my/hydra-ibuffer-main/body)
(add-hook 'ibuffer-hook #'my/hydra-ibuffer-main/body)

(defhydra my/hydra-markdown-mode (:hint nil :foreign-keys run)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

Lists             C-c C-x    _m_: insert item

Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link        _q_: quit

"
  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("q" nil :color blue)
  )


;; (define-key markdown-mode-map (kbd "M-<f9>") 'my/hydra-markdown-mode/body)

(defhydra my/hydra-smartparens (:hint nil)
  "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)

  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)

  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))

(define-key emacs-lisp-mode-map (kbd "M-<f9>") 'my/hydra-smartparens/body)

(defhydra my/hydra-macro (:hint nil :color pink :pre
                          (when defining-kbd-macro
                            (kmacro-end-macro 1)))
  "
         ^Create-Cycle^                          ^Basic^           ^Insert^              ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────╯
            ^[_i_] cycle-ring-previous^          [_e_] execute    [_n_] insert counter   [_b_] name      [_'_] previous
             ^^↑^^                               [_d_] delete     [_t_] set counter      [_K_] key       [_,_] last
 [_j_] start ←   → [_l_] end                     [_o_] edit       [_a_] add counter      [_x_] register  [_V_] view
             ^^↓^^                               [_r_] region     [_f_] set format       [_B_] defun
            ^[_k_] cycle-ring-next^              [_m_] step
            ^^   ^^                              [_s_] swap                                              [_q_] quit
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat :color red)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("V" kmacro-view-macro :color blue)
  ("q" nil :color blue))

(bind-keys*
 ("C-c h M" . my/hydra-macro/body))

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                            :post (setq which-key-inhibit nil)
                            :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump                               _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week                                _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(bind-keys*
 ("C-c h o A" . hydra-org-agenda/body))

(defun my/package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/exit ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun clipboard/set (astring)
  "Copy a string to clipboard"
  (with-temp-buffer
    (insert astring)
    (clipboard-kill-region (point-min) (point-max))))

(defun eshell/copy-pwd ()
  "Copy current directory to clipboard "
  (clipboard/set (eshell/pwd)))

(defun eshell/copy-fpath (fname)
  "Copy file name with full path to clipboard "
  (let ((fpath (concat (eshell/pwd) "/" fname)))
    (clipboard/set fpath)
    (concat "Copied path: " fpath)))

(defun my/magit-copy-branch-name-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))

(defun my/magit-copy-remote-url-to-kill-ring ()
  "Show the current remote url in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((url
         (replace-regexp-in-string "com:" "com/"
                                   (replace-regexp-in-string "^git@" "https://"
                                                             (magit-git-string "remote" "get-url" "--push"
                                                                               (magit-get-remote))))))
    (if url
        (progn (kill-new url)
               (message "%s" url))
      (user-error "There is not current remote URL in path. Is this buffer part of a Git repo?"))))



(global-set-key (kbd "C-c h t")
                (pretty-hydra-define hydra-transpose (:title (pretty-hydra-title "Transpose" 'faicon "exchange" :v-adjust -0.1)
                                                      :foreign-keys run :color amaranth :quit-key "q")
                  (
                   "Transpose"
                   (("c" transpose-chars "characters")
                    ("w" transpose-words "words")
                    ("o" org-transpose-words "Org mode words")
                    ("l" transpose-lines "lines")
                    ("s" transpose-sentences "sentences")
                    ("p" transpose-paragraphs "paragraphs")
                    ("r" transpose-regions "regions")
                    ("m" transpose-mark "mark")
                    ("t" org-table-transpose-table-at-point "Org mode table"))
                   )))

(require 'iimage)
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))

;; Rendering plantuml
(defun my/plantuml-render-buffer ()
  (interactive)
  (message "PLANTUML Start rendering")
  (shell-command (concat "java -jar ~/GitRepos/plantuml.jar "
                         buffer-file-name))
  (message (concat "PLANTUML Rendered:  " (buffer-name))))

;; Image reloading
(defun my/reload-image-at-point ()
  (interactive)
  (message "reloading image at point in the current buffer...")
  (image-refresh (get-text-property (point) 'display)))

;; Image resizing and reloading
(defun my/resize-image-at-point ()
  (interactive)
  (message "resizing image at point in the current buffer123...")
  (let* ((image-spec (get-text-property (point) 'display))
         (file (cadr (member :file image-spec))))
    (message (concat "resizing image..." file))
    (shell-command (format "convert -resize %d %s %s "
                           (* (window-width (selected-window)) (frame-char-width))
                           file file))
    (reload-image-at-point)))

(defun my/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'my/kill-this-buffer)

(defun my/dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

(defun my/remove-control-M ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

(defun flush-kill-lines (regex)
  "Flush lines matching REGEX and append to kill ring.  Restrict to \
region if active. http://xenodium.com/fishing-with-emacs/"
  (interactive "sFlush kill regex: ")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (point) (mark))
        (goto-char 0))
      (while (search-forward-regexp regex nil t)
        (move-beginning-of-line nil)
        (kill-whole-line)))))

(defun markdown-to-html ()
  "Compiles the current file to HTML using Pandoc."
  (interactive)
  (let ((output-dir (read-directory-name "Output directory: "))
        (input-file (file-name-nondirectory buffer-file-name)))
    (setq output-file (concat output-dir "/" (file-name-sans-extension input-file) ".html"))
    (shell-command-on-region
     (point-min) (point-max)
     (concat "pandoc -f markdown+smart -t html5-smart --self-contained --highlight-style=pygments --standalone --mathjax -c ~/.emacs.d/custom-el-scripts/github-pandoc.css -o " output-file " " input-file))))

(defun markdown-to-pdf ()
  "Compiles the current file to PDF using Pandoc."
  (interactive)
  (let ((output-dir (read-directory-name "Output directory: "))
        (input-file (file-name-nondirectory buffer-file-name)))
    (setq output-file (concat output-dir "/" (file-name-sans-extension input-file) ".pdf"))
    (shell-command-on-region
     (point-min) (point-max)
     (concat "pandoc -f markdown+smart --toc --chapters --number-sections --variable papersize:a4paper --variable documentclass:article --variable colorlinks:blue -o " output-file " " input-file))))

(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(defun convert-yaml-yml-buffer-region-to-json ()
  "Convert the current buffer's selected region from yaml to json format and save it with the current buffer's file name but with .json extension."
  (interactive)
  (let ((output-dir (read-directory-name "Output directory: "))
        (input-file (file-name-nondirectory buffer-file-name)))
    (setq output-file (concat output-dir (file-name-sans-extension input-file) "-buffer-region.json"))
    (shell-command-on-region
     (region-beginning) (region-end)
     (concat "yq r -j - | jq  > " (shell-quote-argument output-file)))
    ))

(defun xah-check-parens-balance ()
  "Check if there are unbalanced parentheses/brackets/quotes in current bufffer or selection.
If so, place cursor there, print error to message buffer.

URL `http://ergoemacs.org/emacs/emacs_check_parens_balance.html'
Version 2018-07-03"
  (interactive)
  (let* (
         ($bracket-alist
          '( (?“ . ?”) (?‹ . ?›) (?« . ?») (?【 . ?】) (?〖 . ?〗) (?〈 . ?〉) (?《 . ?》) (?「 . ?」) (?『 . ?』) (?{ . ?}) (?\[ . ?\]) (?\( . ?\))))
         ;; regex string of all pairs to search.
         ($bregex
          (let (($tempList nil))
            (mapc
             (lambda (x)
               (push (char-to-string (car x)) $tempList)
               (push (char-to-string (cdr x)) $tempList))
             $bracket-alist)
            (regexp-opt $tempList )))
         $p1
         $p2
         ;; each entry is a vector [char position]
         ($stack '())
         ($char nil)
         $pos
         $is-closing-char-p
         $matched-open-char
         )
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (point-min) $p2 (point-max)))

    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (progn
          (goto-char 1)
          (while (re-search-forward $bregex nil "move")
            (setq $pos (point))
            (setq $char (char-before))
            (progn
              (setq $is-closing-char-p (rassoc $char $bracket-alist))
              (if $is-closing-char-p
                  (progn
                    (setq $matched-open-char
                          (if $is-closing-char-p
                              (car $is-closing-char-p)
                            (error "logic error 64823. The char %s has no matching pair."
                                   (char-to-string $char))))
                    (if $stack
                        (if (eq (aref (car $stack) 0) $matched-open-char )
                            (pop $stack)
                          (push (vector $char $pos) $stack ))
                      (progn
                        (goto-char $pos)
                        (error "First mismtach found. the char %s has no matching pair."
                               (char-to-string $char)))))
                (push (vector $char $pos) $stack ))))
          (if $stack
              (progn
                (goto-char (aref (car $stack) 1))
                (message "Mismtach found. The char %s has no matching pair." $stack))
            (print "All brackets/quotes match.")))))))

(defun convert-yaml-yml-buffer-to-json ()
  "Convert the current buffer's content from yaml to json format and save it with the current buffer's file name but with .json extension."
  (interactive)
  (let ((output-dir (read-directory-name "Output directory: "))
        (input-file (file-name-nondirectory buffer-file-name)))
    (setq output-file (concat output-dir (file-name-sans-extension input-file) ".json"))
    (shell-command
     (concat "yq r " input-file " -j | jq . > " (shell-quote-argument output-file)))
    ))

(when sys/macp
  (defun formatted-copy ()
    "Export region to HTML, and copy it to the clipboard."
    (interactive)
    (save-window-excursion
      (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
             (html (with-current-buffer buf (buffer-string))))
        (with-current-buffer buf
          (shell-command-on-region
           (point-min)
           (point-max)
           "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
        (kill-buffer buf)))))

(when sys/macp
  (global-set-key (kbd "C-x c w") 'formatted-copy))

(defun toggle-html-export-on-save ()
  "Enable or disable export HTML when saving current buffer."
  (interactive)
  (when (not (eq major-mode 'org-mode))
    (error "Not an org-mode file!"))
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn (remove-hook 'after-save-hook 'org-html-export-to-html t)
             (message "Disabled org html export on save"))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (set-buffer-modified-p t)
    (message "Enabled org html export on save")))

(defun my/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(define-key process-menu-mode-map (kbd "C-k") 'my/delete-process-at-point)

(defun delete-process-interactive ()
  (interactive)
  (let ((pname (ido-completing-read "Process Name: "
                                    (mapcar 'process-name (process-list)))))

    (delete-process (get-process pname))))

(defun my-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(defun xah-cycle-letter-case (arg)
  "Cycle the letter case of the selected region or the current word.
Cycles from 'lower' -> 'Capitalize' -> 'UPPER' -> 'lower' -> ..
        C-u M-x xah-cycle-letter-case -> Force convert to upper case.
    C-u C-u M-x xah-cycle-letter-case -> Force convert to lower case.
C-u C-u C-u M-x xah-cycle-letter-case -> Force capitalize."
  (interactive "p")
  (let (p1 p2
           (deactivate-mark nil)
           (case-fold-search nil))
    (if (use-region-p)
        (setq p1 (region-beginning)
              p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds)
              p2 (cdr bds))))

    (cl-case arg
      (4  (put this-command 'next-state "UPPER"))      ; Force convert to upper case
      (16 (put this-command 'next-state "lower"))      ; Force convert to lower case
      (64 (put this-command 'next-state "Capitalize")) ; Force capitalize
      (t (when (not (eq last-command this-command))
           (save-excursion
             (goto-char p1)
             (cond
              ;; lower -> Capitalize
              ((looking-at "[[:lower:]]")            (put this-command 'next-state "Capitalize"))
              ;; Capitalize -> UPPER
              ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'next-state "UPPER"))
              ;; Default: UPPER -> lower
              (t                                     (put this-command 'next-state "lower")))))))

    (cl-case (string-to-char (get this-command 'next-state)) ; `string-to-char' returns first character in string
      (?U (upcase-region p1 p2)
          ;; UPPER -> lower
          (put this-command 'next-state "lower"))
      (?l (downcase-region p1 p2)
          ;; lower -> Capitalize
          (put this-command 'next-state "Capitalize"))
      ;; Capitalization is a better Option here than upcasing the initials
      ;; because (upcase-initials "abc") -> "Abc" (good)
      ;;         (upcase-initials "ABC") -> "ABC" (not what I expect most of the times)
      ;;         (capitalize "abc")      -> "Abc" (good)
      ;;         (capitalize "ABC")      -> "Abc" (good)
      (t (capitalize-region p1 p2)
         ;; Capitalize -> UPPER
         (put this-command 'next-state "UPPER")))))

(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2015-12-17"
  (interactive)
  (if (null line-spacing)
      (setq line-spacing 0.2) ; add 0.5 height between lines
    (setq line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-frame (selected-frame)))

(defun my/upcase ()     (interactive) (xah-cycle-letter-case 4) (forward-word))
(defun my/downcase ()   (interactive) (xah-cycle-letter-case 16) (forward-word))
(defun my/capitalize () (interactive) (xah-cycle-letter-case 64) (forward-word))


;; remap M-u to `my/upcase'
(global-set-key [remap upcase-word] 'my/upcase)
;; remap M-l to `my/downcase'
(global-set-key [remap downcase-word] 'my/downcase)
;; remap M-c to `my/capitalize'
(global-set-key [remap capitalize-word] 'my/capitalize)

(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(bind-key "C-c h h c" (defhydra hydra-change-case (:color blue
                                                   :hint nil)
                        "
_c_apitalize     _C_ camel→snake→kebab      _U_PCASE        _d_owncase        _<SPC>_ →Cap→UP→down→
"
                        ("c"     my/capitalize)
                        ("U"     my/upcase)
                        ("u"     my/upcase)
                        ("d"     my/downcase)
                        ("C"     my-string-inflection-cycle-auto :color red)
                        ("<SPC>" xah-cycle-letter-case :color red)
                        ("q"     nil "cancel" :color blue)))

(global-set-key (kbd "C-<f9>") 'hydra-move/body)

;; hydra for movement keys
(defhydra hydra-move
  (:body-pre (next-line)
   :hint nil)
  "
_f_: -> char        _F_: -> word         _n_: -> line       _a_: beginning-of-line
_b_: <- char        _B_: <- word         _p_: <- line       _e_: end-of-line
_m_: set mark       _v_: scroll down     _l_: recenter      _'_: avy       _`_: avy-word
_j_: goto mark      _V_: scroll up       _w_: ace-window    _._: -> buffer _,_: <- buffer
_s_: -> sentence    _PA_: -> paragraph    _g_: -> page       _>_: end-of-buffer
_S_: <- sentence    _PB_: <- paragraph    _G_: <- page       _<_: beginning-of-buffer
 "
  ("n" forward-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("a" mwim-beginning-of-code-or-line-or-comment)
  ("e" mwim-end-of-code-or-line)
  ("v" scroll-up)
  ("V" scroll-down)
  ("F" forward-word)
  ("B" backward-word)
  ("l" recenter-top-bottom)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("g" forward-page)
  ("G" backward-page)
  ("s" forward-sentence)
  ("S" backward-sentence)
  ("PA" forward-paragraph)
  ("PB" backward-paragraph)
  ("'" avy-goto-char-timer :color blue)
  ("`" avy-goto-word-1 :color blue)
  ("w" ace-window)
  ("m" org-mark-ring-push)
  ("j" org-mark-ring-goto)
  ("." next-buffer)
  ("," previous-buffer)
  ("q" nil :color blue))

;;;https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)
;;;https://with-emacs.com/posts/tips/quit-current-context/ ;;end

;;;Sort-paragraphs
;;https://emacs.stackexchange.com/a/24363
(defun my-sort-paragraphs (reverse beg end)
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse
                 (function
                  (lambda ()
                    (while (and (not (eobp)) (looking-at paragraph-separate))
                      (forward-line 1))))
                 'forward-paragraph nil nil
                 (lambda (a b)
                   (< (string-to-number (progn (goto-char (car a))
                                               (current-word)))
                      (string-to-number (progn (goto-char (car b))
                                               (current-word)))))))))
;;;Sort-paragraphs

(defun copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer
      (erase-buffer))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position)
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))

(defun occur-mode-clean-buffer ()
  "Removes all commentary from the *Occur* buffer, leaving the
unadorned lines."
  (interactive)
  (if (get-buffer "*Occur*")
      (save-excursion
        (set-buffer (get-buffer "*Occur*"))
        (fundamental-mode)
        (goto-char (point-min))
        (read-only-mode 0)
        (set-text-properties (point-min) (point-max) nil)
        (if (looking-at (rx bol (one-or-more digit)
                            (or " lines matching \""
                                " matches for \"")))
            (kill-line 1))
        (while (re-search-forward (rx bol
                                      (zero-or-more blank)
                                      (one-or-more digit)
                                      ":")
                                  (point-max)
                                  t)
          (replace-match "")
          (forward-line 1)))

    (message "There is no buffer named \"*Occur*\".")))

(define-key occur-mode-map (kbd "C-c C-K") 'occur-mode-clean-buffer)

;;; My repeat commands
;; (eval-when-compile (require 'cl))
(defun def-rep-command (alist)
  "Return a lambda that calls the first function of ALIST.
It sets the transient map to all functions of ALIST."
  (let ((keymap (make-sparse-keymap))
        (func (cdar alist)))
    (mapc (lambda (x)
            (when x
              (define-key keymap (car x) (cdr x))))
          alist)
    (lambda (arg)
      (interactive "p")
      (when func
        (funcall func arg))
      (set-transient-map keymap t))))

;; Enlarge and shrink window horizontall commands
(global-set-key (kbd "C-x ^")
                (def-rep-command
                  '(("^" . enlarge-window)
                    ("6" . shrink-window))))

(global-set-key (kbd "C-x {")
                (def-rep-command
                  '(("{" . enlarge-window-horizontally)
                    ("}" . shrink-window-horizontally))))


;;; My repeat commands

;; hl-todos

(setq hl-todo-keyword-faces
      '(("TODO"   . "#cc9393")
        ("FIXME"  . "#FF0000")
        ("DONT"   . "#5f7f5f")
        ("NOTES"  . "#d0bf8f")
        ("HACK"   . "#d0bf8f")
        ("TEMP"   . "#d0bf8f")
        ("XXX+"   . "#cc9393")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))

;;Ispell
(setq ispell-program-name "aspell"
      ispell-choices-win-default-height 4)
(when sys/macp
  (setq ispell-extra-args
        '("--sug-mode=fast" "--lang=en_US" "--camel-case"
          "--run-together" "--run-together-limit=16"))
  )

;; NOTES: Aspell is installed by nix
;;aspell in ubuntu 18.04 is not updated 0.68.0, so removed camelcase
(when sys/linuxp
  (setq ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--run-together" "--run-together-limit=16")))

;; set specific browser to open links
;; (setq browse-url-browser-function 'browse-url-firefox)

;; (setq browse-url-browser-function 'eww-browse-url) ; emacs browser

;; (setq browse-url-browser-function 'browse-url-chromium)

(when sys/macp
  (setq browse-url-browser-function 'browse-url-chrome
        browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))

(when sys/linuxp
  (setq browse-url-browser-function 'browse-url-firefox
        browse-url-firefox-program "firefox"))
;; NOTES: Leaving this code here for reference.
;; code from - http://ergoemacs.org/emacs/emacs_set_default_browser.html
;; use browser depending on url
;; ( setq
;; browse-url-browser-function
;; '(
;;   ("wikipedia\\.org" . browse-url-firefox)
;;   ("github" . browse-url-chromium)
;;   ("thefreedictionary\\.com" . eww-browse-url)
;;   ("." . browse-url-default-browser)
;;   )
;; )

;;;my super key bindings
(global-set-key (kbd "s-b") 'ivy-switch-buffer)
(global-set-key (kbd "s-O") 'ns-open-file-using-panel)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-0") 'delete-window)
;; (global-set-key (kbd "M-/") 'company-dabbrev)
(global-set-key (kbd "M-i") 'tab-to-tab-stop)
(global-set-key (kbd "C-M-?") 'complete-symbol)
(global-set-key (kbd "M-.") 'xref-find-definitions)

;;;

;; https://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
;;(defun my-font-lock-comment-annotations ()
;;  "Highlight a bunch of well known comment annotations.
;;
;;This functions should be added to the hooks of major modes for programming."
;;  (font-lock-add-keywords
;;   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
;;          1 font-lock-warning-face t))))
;;
;;(add-hook 'prog-mode-hook 'my-font-lock-comment-annotations)

(defun my/quit ()
  "Quit in current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
  (interactive)
  (cond ((active-minibuffer-window)
         (if (minibufferp)
             (minibuffer-keyboard-quit)
           (abort-recursive-edit)))
        (t
         (unless (or defining-kbd-macro
                     executing-kbd-macro)
           (keyboard-quit))))
  (message this-command))
(global-set-key [remap keyboard-quit] #'my/quit)

;; to quit lsp-ui-doc-view
(when (fboundp 'my/quit)
  (define-advice lsp-ui-doc--make-request (:around (foo))
    (unless (eq this-command 'my/quit)
      (funcall foo))))


;;;;;; Use this if you need it
  ;;;; Define f5 as an alias for C-x r
;; (global-set-key (kbd "<f5>") (lookup-key global-map (kbd "C-x r")))

(global-set-key (kbd "C-x v R") 'vc-refresh-state)


;;Aliases
                                        ; minor modes
;;(defalias 'wsm 'whitespace-mode)
;;(defalias 'gwsm 'global-whitespace-mode)
;;(defalias 'vlm 'visual-line-mode)
;;(defalias 'ln 'global-display-line-numbers-mode)
(defalias 'flm font-lock-mode)

(when sys/macp
  (setq-default counsel-search-engine 'google))

(when sys/macp
  (defun my/html2org-clipboard ()
    "Convert clipboard contents from HTML to Org and then paste (yank)."
    (interactive)
    (kill-new (shell-command-to-string "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t org"))
    (yank)))

(defun ztree-dired-diff-toggle ()
  (interactive)

  (unless (= (length (window-list)) 2)
    (error "invalid number of visible buffers (expected 2)"))

  (let (dir-A dir-B)
    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
        (setq dir-A buffer-file-name)
      (setq dir-A default-directory))

    (other-window 1)

    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
        (setq dir-B buffer-file-name)
      (setq dir-B default-directory))

    (other-window 1)

    (unless (and (file-directory-p dir-A)
                 (file-directory-p dir-B))
      (error "not all buffers directories"))

    (delete-other-windows)
    (ztree-diff dir-A dir-B)))

(defun my/counsel-company ()
  (interactive)
  (let ((my-company-complete-newstr nil)
        (candidates nil)
        (no-len nil))
    (unwind-protect
        (progn
          (company-mode 1)
          (unless company-candidates
            (company-complete))
          (let ((len (cond (company-prefix
                            (length company-prefix)))))
            (if (not len)
                (setq no-len t)
              (setq ivy-completion-beg (- (point) len))
              (setq ivy-completion-end (point))
              ;; (ivy-posframe-mode 1)
              (setq bname (current-buffer))
              (ivy-read "company cand: " company-candidates
                        :initial-input (if (ignore-errors (string-prefix-p company-prefix company-common))
                                           (regexp-quote company-common)
                                         (regexp-quote company-prefix))
                        :action #'(lambda (str)
                                    (setq my-company-complete-newstr str)

                                    ;; this is where you remove the parts that should not be inserted
                                    ;; you will need to adapt this for the languages you use.
                                    ;; example: remove everything after "-"
                                    (setq my-company-complete-newstr (replace-regexp-in-string
                                                                      " - .+$"
                                                                      "" my-company-complete-newstr))

                                    (ivy-completion-in-region-action my-company-complete-newstr))
                        :unwind #'(lambda ()
                                    (company-abort)
                                    ;; (ivy-posframe-mode 0)
                                    )
                        :caller 'my-counsel-company)))))))

;; https://superuser.com/a/132844
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;; FIXME: issue from https://www.reddit.com/r/emacs/comments/flb0vc/wrongtypeargument_stringp_require_info_with_emacs/
;; FIXME: https://emacs.stackexchange.com/a/5565
(defun load-history-filename-element (file-regexp)
  "Get the first elt of `load-history' whose car matches FILE-REGEXP.
        Return nil if there isn't one."
  (let* ((loads load-history)
         (load-elt (and loads (car loads))))
    (save-match-data
      (while (and loads
                  (or (null (car load-elt))
                      (not (and (stringp (car load-elt)) ; new condition
                                (string-match file-regexp (car load-elt))))))
        (setq loads (cdr loads)
              load-elt (and loads (car loads)))))
    load-elt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert file name at point                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.emacswiki.org/emacs/InsertFileName
(defun my/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

(bind-keys*
 ("M-m i i" . my/insert-file-name))


(defun my/org-get-outline-path()
  "put the current outline path into kill ring"
  (interactive)
  (unless (org-before-first-heading-p)
    (kill-new
     (concat
      (mapconcat #'identity (org-get-outline-path) " --> ")
      " --> "
      (substring-no-properties (org-get-heading t t))))))

;; https://emacs.stackexchange.com/questions/47200/copy-paste-text-among-split-window-buffers
(defun my-copy-to-next-window (b e)
  "Copy text in the region to next window."
  (interactive "r")
  (pcase (window-list)
    (`(,w0 ,w1)
     (with-selected-window w1
       (insert-buffer-substring (window-buffer w0) b e)))
    (t (user-error "Only works with 2 windows"))))

;; https://emacs.stackexchange.com/questions/3743/how-to-move-region-to-other-window
(defun my-move-region-to-other-window (start end)
  "Move selected text to other window"
  (interactive "r")
  (if (use-region-p)
      (let ((count (count-words-region start end)))
        (save-excursion
          (kill-region start end)
          (other-window 1)
          (goto-char (point-max))
          (yank)
          (newline))
        (other-window -1)
        (message "Moved %s words" count))
    (message "No region selected")))

;; https://stackoverflow.com/a/32353255
;; https://stackoverflow.com/questions/49042522/orgmode-region-to-table
(defun org-convert-csv-table-with-quotes-to-org (beg end)
  "convert csv to org-table considering  a,\"12,12\",b --> a | 12,12 |b"
  (interactive (list (point) (mark)))
  (replace-match "\\(^\\)\\|\\(\".*?\"\\)\\|," (quote (replace-eval-replacement
                                                       replace-quote (cond ((equal "^" (match-string 1)) "|")
                                                                           ((equal "," (match-string 0)) "|")
                                                                           ((match-string 2))) ))  nil  beg end))


;; https://www.emacswiki.org/emacs/RecentFiles#h5o-21
;; https://www.reddit.com/r/emacs/comments/8u8slx/dired_quickly_jumping_to_previously_visited/
(eval-after-load "recentf"
  '(progn
     (defun recentf-track-opened-file ()
       "Insert the name of the dired or file just opened or written into the recent list."
       (let ((buff-name (or buffer-file-name (and (derived-mode-p 'dired-mode) default-directory))))
         (and buff-name
              (recentf-add-file buff-name)))
       ;; Must return nil because it is run from `write-file-functions'.
       nil)

     (defun recentf-track-closed-file ()
       "Update the recent list when a file or dired buffer is killed.
That is, remove a non kept file from the recent list."
       (let ((buff-name (or buffer-file-name (and (derived-mode-p 'dired-mode) default-directory))))
         (and buff-name
              (recentf-remove-if-non-kept buff-name))))

     (add-hook 'dired-after-readin-hook 'recentf-track-opened-file)))

;; http://pragmaticemacs.com/emacs/open-a-recent-directory-in-dired-revisited/
(defun my/ivy-dired-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs
         (delete-dups
          (mapcar (lambda (file)
                    (if (file-directory-p file) file (file-name-directory file)))
                  recentf-list))))

    (let ((dir (ivy-read "Directory: "
                         recent-dirs
                         :re-builder #'ivy--regex
                         :sort nil
                         :initial-input nil)))
      (dired dir))))
(global-set-key (kbd "C-x C-d") 'my/ivy-dired-recent-dirs)

;; https://github.com/jorgenschaefer/circe/wiki/Configuration#safer-password-management
;; using this function in restclient variables like this
;; :app_secret := (my-fetch-password :user "login" :host "machine")
;; to refresh authinfo changes run `M-x auth-source-forget-all-cached'
(defun my-fetch-password (&rest params)
  "fetch password from .authinfo file"
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))


;;https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))

(pretty-hydra-define my-hydra (:title (pretty-hydra-title "my custom functions" 'faicon "product-hunt" :v-adjust -0.1)
                               :foreign-keys run :color amaranth :quit-key "q")
  ("📁 File"
   (("df" my/delete-current-buffer-file "Delete current file")
    ("d2u" my/dos2unix "dos2unix")
    ("mbf" my/move-buffer-file "move file")
    ("rcb" my/rename-current-buffer-file "rename buffer and file")
    ("oc" my/open-config "open config")
    ("ifn" my/insert-file-name "insert file name")
    )
   "© Copy"
   (("cfp" my/put-current-path-to-clipboard "copy file name with path")
    ("cfn" my/put-current-filename-to-clipboard "copy file name")
    ("cml" copy-lines-matching-re "copy lines regex")
    ("cob" my-copy-to-next-window "copy text to other window")
    ("mob" my-move-region-to-other-window "move text other window")
    )
   "🧪 Misc."
   (("dlk" diff-last-two-kills "diff last to kills")
    ("ls" xah-toggle-line-spacing "toggle line spacing")
    ("id" my/xah-insert-date-time "insert date")
    ("jl" my/join-line "join lines")
    ("flk" flush-kill-lines "flush-lines and save kills")
    ("sp" my-sort-paragraphs "Sort-paragraphs")
    ("obc" occur-mode-clean-buffer "clean occur buffer")
    ("up" my/package-upgrade-all "Upgrade all packages"))
   "␣ Open"
   (("oo" xah-open-in-external-app "Open in external app")
    ("ov" xah-open-in-vscode "open in vscode")
    ("ot" xah-open-in-terminal "open in terminal")
    )
   )
  )

(pretty-hydra-define my-org-hydra (:title (pretty-hydra-title "Org-Mode" 'sucicon "nf-custom-orgmode" :face 'nerd-icons-green :height 1.1 :v-adjust 0.0)
                                   :foreign-keys run :color amaranth :quit-key "q")
  ("basic navigation"
   (("i" org-cycle)
    ("I" org-shifttab)
    ("h" org-up-element)
    ("l" org-down-element)
    ("j" org-forward-element)
    ("k" org-backward-element))
   "navigating links"
   (("n" org-next-link)
    ("p" org-previous-link)
    ("o" org-open-at-point))
   "navigation blocks"
   (("N" org-next-block)
    ("P" org-previous-block))
   "results"
   (("r" org-babel-remove-result-one-or-many))
   "updates"
   (("." org-ctrl-c-ctrl-c)
    ("*" org-ctrl-c-star)
    ("-" org-ctrl-c-minus))
   "change todo state"
   (("H" org-shiftleft)
    ("L" org-shiftright)
    ("J" org-shiftdown)
    ("K" org-shiftup)
    ("t" org-todo))))

(defhydra hydra-flycheck
  (:pre (flycheck-list-errors)
   :post (quit-windows-on "*Flycheck errors*")
   :hint nil)
  "Errors"
  ("f" flycheck-error-list-set-filter "Filter")
  ("j" flycheck-next-error "Next")
  ("k" flycheck-previous-error "Previous")
  ("gg" flycheck-first-error "First")
  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q" nil))

(provide 'init-my-cust-fun)
