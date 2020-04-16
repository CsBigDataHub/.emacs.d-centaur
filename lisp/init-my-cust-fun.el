;;; -*- lexical-binding: t; -*-
;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun my/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

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

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun my/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun mybrowse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun my/dos2unix (buffer)
  "Convert BUFFER from DOS file format to UNIX."
  (interactive "*b")
  (shell-command (format "dos2unix %s" (file-truename buffer))))


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

(defun my/upcase-backward-word (arg)
  (interactive "p")
  (upcase-word (- arg)))

(defun my/downcase-backward-word (arg)
  (interactive "p")
  (downcase-word (- arg)))

(defun my/capitalize-backward-word (arg)
  (interactive "p")
  (capitalize-word (- arg)))

;;(global-set-key (kbd "C-M-u")	 'upcase-backward-word)
;;(global-set-key (kbd "C-M-l")	 'downcase-backward-word)
;;(global-set-key (kbd "M-c")	 'capitalize-backward-word)

(defun my/kill-word-at-point ()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~],.、。" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))

;;(global-set-key (kbd "M-d")  'kill-word-at-point)

(defun my/backward-kill-word-or-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

;;(global-set-key (kbd "C-w")  'backward-kill-word-or-region)

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
              (if (eq system-type 'gnu/linux)
                  (call-process "import" nil nil nil filename))
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
                  (call-process "import" nil nil nil filename))
                                        ; insert into file if correctly taken
              (if (file-exists-p filename)
                  (insert (concat "![" filename "](" filename ")")))
              (markdown-toggle-inline-images)
              )))

(defun my/xah-search-current-word ()
  "Call `isearch' on current word or text selection."
  ;;“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
  ;;URL `http://ergoemacs.org/emacs/modernization_isearch.html'
  ;;Version 2015-04-09"
  (interactive)
  (let ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))
    (setq mark-active nil)
    (when (< $p1 (point))
      (goto-char $p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

;;(global-set-key (kbd "<f8>") 'my/xah-search-current-word)

(require 'ido)
(defun my/xah-insert-date-time ()
  "Insert current date time.
  Insert date in this format: yyyy-mm-dd.
  When called with `universal-argument', prompt for a format to use.
  If there's text selection, delete it first.

  URL `http://ergoemacs.org/emacs/elisp_insert-date-time.html'
  version 2018-07-03"
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
                  "8 → 12-April-2018"
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
       (format-time-string "%d-%B-%Y")
       ;; "12-April-2018"
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



(defun my/move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun my/move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

;;(global-set-key (kbd "<C-S-down>") 'my/move-line-down)
;;(global-set-key (kbd "<C-S-up>") 'my/move-line-up)

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
(define-key ctl-x-map "n" #'my/narrow-or-widen-dwim)


(defun my/clean-buffer-formatting ()
  "Indent and clean up the buffer"
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(global-set-key "\C-cn" 'my/clean-buffer-formatting)

;; by default,
;; highlight trailing whitespace
;; and show form-feed chars as horizontal rules

(defun my/general-formatting-hooks ()
  (setq show-trailing-whitespace 't)
  (turn-on 'form-feed))

(defun fixup-json ()
  "Re-indent json buffers with broken literal strings. Needs jsonpp installed (available using homebrew)"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "sed -e ':a' -e 'N' -e '$!ba' -e 's/\\n/ /g'|jsonpp"  nil t))

;;(defun my/text-formatting-hooks ()
;;  (turn-on 'auto-fill)) ; turn on automatic hard line wraps
;;
;;(add-hook 'text-mode-hook
;;          'my/text-formatting-hooks)


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
      :else closest-ahead))))

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

(defhydra my/hydra-of-windows (:color red
                               :hint nil)
  "
 ^Move^           ^Size^      ^Change^                    ^Split^           ^Text^
 ^^^^^^^^^^^------------------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _u_: winner-undo _o_: rotate  _v_: vertical     _+_: zoom in
 _h_ ^+^ _l_   _H_ ^+^ _L_   _r_: winner-redo              _s_: horizontal   _-_: zoom out
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _c_: close                    _z_: zoom         _q_: quit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" shrink-window-horizontally)
  ("K" shrink-window)
  ("J" enlarge-window)
  ("L" enlarge-window-horizontally)
  ("v" my/split-right-and-move)
  ("s" my/split-below-and-move)
  ("c" delete-window)
  ("f" my/toggle-frame-fullscreen-non-native :color blue)
  ("o" my/rotate-windows)
  ("z" delete-other-windows)
  ("u" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("r" winner-redo)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :color blue))

(bind-keys*
 ("C-c h w" . my/hydra-of-windows/body))

(defhydra my/hydra-bookmarks (:color blue
                              :hint nil)
  "
 _s_: set  _l_: list  _b_: save   _j_: jump   _d_: delete   _q_: quit
  "
  ("s" bookmark-set)
  ("l" list-bookmarks)
  ("b" bookmark-save)
  ("j" bookmark-jump)
  ("d" bookmark-delete)
  ("q" nil :color blue))

(bind-keys*
 ("C-c h B" . my/hydra-bookmarks/body))

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

(defun my/remove-mark ()
  "Deactivate the region"
  (interactive)
  (if (region-active-p)
      (deactivate-mark)))

(bind-keys*
 ("M-m E" . my/remove-mark))


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

(bind-keys*
 ("M-m g A SPC" . my/align-whitespace)
 ("M-m g A &"   . my/align-ampersand)
 ("M-m g A ,"   . my/align-comma)
 ("M-m g A \""  . my/align-quote-space)
 ("M-m g A      ." . my/align-dot)
 ("M-m g A ="   . my/align-equals)
 ("M-m g A :"   . my/align-colon)
 ("M-m g A A"   . align-regexp))

(defun my/sk/insert-date (prefix)
  "Insert the current date. With prefix-argument, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%A, %d %B %Y")
                 ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
    (insert (format-time-string format))))

(bind-keys*
 ("M-m g D" . my/sk/insert-date))


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


(defun my/copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))

(bind-keys*
 ("M-m g y" . my/copy-current-file-path))

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


(defun my/open-line-above (args)
  "Insert a new line above the current one or open a new line above for editing"
  (interactive "P")
  (if (equal args '(4))
      (save-excursion
        (unless (bolp)
          (beginning-of-line))
        (newline)
        (indent-according-to-mode))
    (unless (bolp)
      (beginning-of-line))
    (newline)
    (forward-line -1)
    (indent-according-to-mode)
    (modalka-mode 0)))

;;(bind-keys*
;; ("M-o o" . my/open-line-above))


(defun my/join-line ()
  "Join the current line with the next line"
  (interactive)
  (next-line)
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
  (next-line 1)
  (my/smarter-move-beginning-of-line 1))

(bind-keys*
 ("M-m i l" . my/select-inside-line)
 ("M-m a l" . my/select-around-line))

(defun my/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun my/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (my/move-text-internal arg))
(defun my/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (my/move-text-internal (- arg)))

(bind-keys*
 ("M-m [ e" . my/move-text-up)
 ("M-m ] e" . my/move-text-down))

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
    (replace-string current-word snakified nil beg end)))

(bind-keys*
 ("M-m g _" . my/snakeify-current-word))

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

;;Hydra for Dired
(defhydra my/hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew                       _m_ark             _(_ details          _i_nsert-subdir      _w_dired
_P_eep             _n_ filter                   _@_ mark regex     _<_ subtree-cycle    _>_ subtree-toggle
_C_opy             _O_ view other               _U_nmark all       _)_ omit-mode                             C-x C-q : edit
_D_elete           _o_pen other                 _u_nmark           _l_ redisplay        _w_ kill-subdir      C-c C-c : commit
_R_ename-or-move   _M_ chmod                    _t_oggle           _g_ revert buf       _e_ ediff            C-c ESC : abort
_Y_ rel symlink    _G_ chgrp                    _E_xtension mark   _s_ort
_S_ymlink          _fc_ copy-file-name          _F_ind marked      _._ toggle hydra
_r_sync            _fp_ copy-file-name-path     _I_ Git Info       ^ ^                  _?_ summary
_z_ compress-file  _A_ find regexp              _<return>_ Dired-find-file
_Z_ compress       _Q_ repl regexp              ^        ^

T - tag prefix
"
 ;;("\\" dired-do-ispell)
  ("n" dired-narrow)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ;;("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ;;("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  (">" dired-subtree-toggle)
  ("<" dired-subtree-cycle)
  ("e" ora-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("I" dired-git-info-mode)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("P" peep-dired)
  ("@" dired-mark-files-regexp)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-rsync)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("fc" dired-copy-filename-as-kill)
  ("fp" dired-copy-file-path-as-kill)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("<return>" dired-find-file)
  ("q" nil)
  ("." nil :color blue))

;;(define-key dired-mode-map "." 'my/hydra-dired/body)

;; Avy hydra
(defhydra my/hydra-avy (:exit t :hint nil)
  "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
  ("c" avy-goto-char-timer)
  ("C" avy-goto-char)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))


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

(defhydra my/hydra-markdown-mode (:hint nil)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

Lists             C-c C-x    _m_: insert item

Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference

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
  ("R" markdown-insert-reference-link-dwim :color blue)
  )


;;(define-key markdown-mode-map (kbd "<f9>") 'my/hydra-markdown-mode/body)

(defhydra my/hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra my/hydra-projectile (:color teal
                               :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   my/hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))


(bind-keys*
 ("C-c h P" . my/hydra-projectile))

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

(defhydra my/hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                              :color pink
                              :hint nil
                              :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))      ;; ok

(bind-keys*
 ("C-c h r" . my/hydra-rectangle/body))

(defhydra my/hydra-macro (:hint nil :color pink :pre
                          (when defining-kbd-macro
                            (kmacro-end-macro 1)))
  "
         ^Create-Cycle^                          ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────╯
            ^[_i_] cycle-ring-previous^          [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
             ^^↑^^                               [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 [_j_] start ←   → [_l_] end                     [_o_] edit       [_a_] add       [_x_] register  [_V_] view
             ^^↓^^                               [_r_] region     [_f_] format    [_B_] defun
            ^[_k_] cycle-ring-next^              [_m_] step
            ^^   ^^                              [_s_] swap                                       [_q_] quit
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
 ("C-c h M" . my/hydra-of-macros/body))

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
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
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
  ("o"   link-hint-open-link :exit t)
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
  ("fr" org-agenda-filter-by-tag-refine)
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

(defun my/insert-unicode (unicode-name)
  "Same as C-x 8 enter UNICODE-NAME."
  (insert-char (gethash unicode-name (ucs-names))))

(global-set-key
 (kbd "C-c h 9")
 (defhydra hydra-unicode (:hint nil)
   "
        Unicode  _e_ €  _s_ ZERO WIDTH SPACE
                 _f_ ♀  _o_ °   _m_ µ
                 _r_ ♂  _a_ →
        "
   ("e" (my/insert-unicode "EURO SIGN"))
   ("r" (my/insert-unicode "MALE SIGN"))
   ("f" (my/insert-unicode "FEMALE SIGN"))
   ("s" (my/insert-unicode "ZERO WIDTH SPACE"))
   ("o" (my/insert-unicode "DEGREE SIGN"))
   ("a" (my/insert-unicode "RIGHTWARDS ARROW"))
   ("m" (my/insert-unicode "MICRO SIGN"))))

(defhydra hydra-window ()
  "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←           _v_ertical      _b_uffer		_q_ X←
_j_ ↓           _x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑           _z_ undo        _a_ce 1		_e_ X↑
_l_ →           _Z_ reset       _s_wap		_r_ X→
_F_ollow		_D_lt Other     _S_ave		max_i_mize
_SPC_ cancel	_o_nly this     _d_elete
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" ivy-switch-buffer)
  ("f" counsel-find-files)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   )
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   )
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("o" delete-other-windows)
  ("i" ace-maximize-window)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo))
   )
  ("Z" winner-redo)
  ("SPC" nil)
  )

(bind-keys*
 ("C-c h w" . hydra-window/body))

(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(bind-keys*
 ("C-c h l l" . hydra-lsp/body))

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



(defun my/align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

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

(defun my/magit-copy-remte-url-to-kill-ring ()
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

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark (&optional arg)
  "Paste things to mark, or to the prompt in shell-mode."
  (unless (eq arg 1)
    (if (string= "shell-mode" major-mode)
        (comint-next-prompt 25535)
      (goto-char (mark)))
    (yank)))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c w") (quote copy-word))

(defun copy-backward-word ()
  "copy word before point - rocky @ stackexchange"
  (interactive "")
  (save-excursion
    (let ((end (point))
          (beg (get-point 'backward-word 1)))
      (copy-region-as-kill beg end))))

(global-set-key (kbd "C-c l") (quote copy-line))

(defun copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (copy-thing 'backward-paragraph 'forward-paragraph arg)
  (paste-to-mark arg)
  )

(global-set-key (kbd "C-c P")(quote copy-paragraph))


(defhydra my/nav-mode (:foreign-keys run)
  "
_a_ beginning-of-line  _e_     View-scroll-half-page-forward    _j_ next-line          _<_ beginning-of-buffer
_l_ forward-char       _u_     View-scroll-half-page-backward   _p_ previous-line      _>_ end-of-buffer
_h_ backward-char      _SPC_   scroll-up-command                _k_ previous-line      _._ end-of-buffer
_n_ next-line          _S-SPC_ scroll-down-command              _d_ kill-buffer
 "
  ("C-h" hl-line-mode)
  ("t" toggle-truncate-lines)
  ("a" beginning-of-line)
  ("l" forward-char)
  ("h" backward-char)
  ("n" next-line)
  ("j" next-line)
  ("p" previous-line)
  ("k" previous-line)
  ("e" View-scroll-half-page-forward)
  ("u" View-scroll-half-page-backward)
  ("SPC" scroll-up-command)
  ("S-SPC" scroll-down-command)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("." end-of-buffer)
  ("C-'" nil)
  ("d" (when (y-or-n-p "Kill buffer?")
         (kill-this-buffer))
   :exit t)
  ("/" isearch-forward-regexp :exit t)
  ("?" isearch-backward-regexp :exit t)
  ("i" nil :exit t)
  ("q" nil :exit t))

(bind-keys*
 ("C-c h n n" . my/nav-mode/body))

(defhydra my/indent-tools-hydra (:color red :hint nil)
  "
 ^Indent^         | ^Navigation^        | ^Actions^
------------------+---------------------+-----------
 _._ indent       | _j_ v               | _K_ kill
 _,_ de-indent    | _k_ ʌ               | _i_ imenu
 _l_ end of level | _n_ next sibling    | _C_ Copy…
 _E_ end of fn    | _p_ previous sibling| _c_ comment
 _P_ paragraph    | _u_ up parent       | _U_ uncomment (paragraph)
 _SPC_ space      | _d_ down child      | _f_ fold
 ___ undo         | _e_ end of tree     | _q_ quit
"

  ("." indent-tools-indent)
  ("," indent-tools-demote)
  ("E" indent-tools-indent-end-of-defun)
  ("c" indent-tools-comment)
  ("U" indent-tools-uncomment)
  ("P" indent-tools-indent-paragraph)
  ("l" indent-tools-indent-end-of-level)
  ("K" indent-tools-kill-tree)
  ("C" indent-tools-copy-hydra/body :color blue)
  ("s" indent-tools-select)
  ("e" indent-tools-goto-end-of-tree)
  ("u" indent-tools-goto-parent)
  ("d" indent-tools-goto-child)
  ("S" indent-tools-select-end-of-tree)
  ("n" indent-tools-goto-next-sibling)
  ("p" indent-tools-goto-previous-sibling)
  ("i" counsel-imenu)
  ("j" forward-line)
  ("k" previous-line)
  ("SPC" indent-tools-indent-space)
  ("_" undo-tree-undo)
  ("L" recenter-top-bottom)
  ("f" yafolding-toggle-element)
  ("q" nil))

(bind-keys*
 ("C-c h n n" . my/nav-mode/body))

(defhydra my/hydra-coolmoves-text-motions (:color amaranth :hint nil :foreign-keys nil)
  "
    ^
	^Motions^
	-------------------------
	_l_: line ↓      _w_: word →
	_L_: line ↑      _W_: word ←
	_p_: par  ↓      _c_: char →
	_P_: par  ↑      _C_: char ←
	_s_: sentence →  _x_: sexp →
	_S_: sentence ←  _X_: sexp ←

    "

  ("<escape>" nil)
  ("u" nil)

  ("l" cool-moves/line-forward)
  ("L" cool-moves/line-backward)

  ("p" cool-moves/paragraph-forward)
  ("P" cool-moves/paragraph-backward)

  ("w" cool-moves/word-forward)
  ("W" cool-moves/word-backwards)

  ("c" cool-moves/character-forward)
  ("C" cool-moves/character-backward)

  ("s" cool-moves/sentence-forward)
  ("S" cool-moves/sentence-backward)

  ("x" cool-moves/sexp-forward)
  ("X" cool-moves/sexp-backward))

(bind-keys*
 ("C-c h n c" . my/hydra-coolmoves-text-motions/body))

(defhydra hydra-ediff (:color blue :hint nil)
  "
ediff
^Buffers           Files                 VC                   Ediff regions
--------------------------------------------------------------------------------
_b_uffers           _f_iles (_=_)        _r_evisions            _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                             _w_ordwise
                  _c_urrent file

_q_ quit
"
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise)
  ("q" nil))

(global-set-key (kbd "C-c h d") 'hydra-ediff/body)

(defhydra my/hydra-highlight-symbol (:color blue)
  ("h" highlight-symbol-at-point)
  ("n" highlight-symbol-next)
  ("p" highlight-symbol-prev)
  ("q" highlight-symbol-query-replace)
  ("r" highlight-symbol-remove-all))

(global-set-key (kbd "C-c h h s") 'my/hydra-highlight-symbol/body)


(defhydra hydra-hide-show (:idle 1.0)
  "
Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_oggle    _n_ext line
_d_ hide block    _a_ show block              _p_revious line
_l_ hide level

_q_ cancel
"
  ("s" hs-show-all)
  ("h" hs-hide-all)
  ("a" hs-show-block)
  ("d" hs-hide-block)
  ("t" hs-toggle-hiding)
  ("l" hs-hide-level)
  ("n" forward-line)
  ("p" (forward-line -1))
  ("q" nil :color blue)
  )

(global-set-key (kbd "C-c h @") 'hydra-hide-show/body)

(global-set-key (kbd "C-c h t")
                (defhydra hydra-transpose (:color red)
                  "Transpose"
                  ("c" transpose-chars "characters")
                  ("w" transpose-words "words")
                  ("o" org-transpose-words "Org mode words")
                  ("l" transpose-lines "lines")
                  ("s" transpose-sentences "sentences")
                  ("e" org-transpose-elements "Org mode elements")
                  ("p" transpose-paragraphs "paragraphs")
                  ("t" org-table-transpose-table-at-point "Org mode table")
                  ("q" nil "cancel" :color blue)))

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
   :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
   :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))


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
      (kill-buffer buf))))

(global-set-key (kbd "C-x c w") 'formatted-copy)

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

(defun duplicate-line ()
  (interactive)
  (let ((col (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column col)))

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

(defun my/upcase ()     (interactive) (xah-cycle-letter-case 4))
(defun my/downcase ()   (interactive) (xah-cycle-letter-case 16))
(defun my/capitalize () (interactive) (xah-cycle-letter-case 64))

(bind-key "C-c h h c" (defhydra hydra-change-case (:color blue
                                                   :hint nil)
                        "
_c_apitalize     _C_ camel→snake→kebab      _U_PCASE        _d_owncase        _<SPC>_ →Cap→UP→down→
"
                        ("c"     my/capitalize)
                        ("U"     my/upcase)
                        ("u"     my/upcase)
                        ("d"     my/downcase)
                        ("C"     string-inflection-all-cycle :color red)
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
  ("n" next-line)
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
        (toggle-read-only 0)
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
(eval-when-compile (require 'cl))
(defun def-rep-command (alist)
  "Return a lambda that calls the first function of ALIST.
It sets the transient map to all functions of ALIST."
  (lexical-let ((keymap (make-sparse-keymap))
                (func (cdar alist)))
    (mapc (lambda (x)
            (define-key keymap (car x) (cdr x)))
          alist)
    (lambda (arg)
      (interactive "p")
      (funcall func arg)
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

;;; Dired copy or move to split window suggestions
;;https://emacs.stackexchange.com/questions/5603/how-to-quickly-copy-move-file-in-emacs-dired
(setq dired-dwim-target t)

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
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--camel-case" "--run-together" "--run-together-limit=16"))

;;Ispell for camel-case
;; Spell Check
(use-package wucuo
  :config
  (setq ispell-extra-args "--run-together"))

;;;my super key bindings
(global-set-key (kbd "s-b") 'ivy-switch-buffer)
(global-set-key (kbd "s-O") 'ns-open-file-using-panel)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-0") 'delete-window)
;;;

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


;;; Code to replace exec-path-from-shell
;; Need to create file in $HOME/.emacs.d/.local/env
;; use this command to create the file  `printenv > $HOME/.emacs.d/.local/env'

(defconst my-local-dir (concat user-emacs-directory ".local/"))

(defconst my-env-file (concat my-local-dir "env"))

(defun my-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (not (file-readable-p file))
      (unless noerror
        (signal 'file-error (list "Couldn't read envvar file" file)))
    (let (envvars environment)
      (with-temp-buffer
        (save-excursion
          (insert "\n")
          (insert-file-contents file))
        (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
          (push (match-string 1) envvars)
          (push (buffer-substring
                 (match-beginning 1)
                 (1- (or (save-excursion
                           (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                             (line-beginning-position)))
                         (point-max))))
                environment)))
      (when environment
        (setq process-environment
              (append (nreverse environment) process-environment)
              exec-path
              (if (member "PATH" envvars)
                  (append (split-string (getenv "PATH") path-separator t)
                          (list exec-directory))
                exec-path)
              shell-file-name
              (if (member "SHELL" envvars)
                  (or (getenv "SHELL") shell-file-name)
                shell-file-name))
        envvars))))

(when (and (or (display-graphic-p)
               (daemonp))
           (file-exists-p my-env-file))
  (my-load-envvars-file my-env-file))

(defun my-load-envvars-now ()
  (interactive)
  (my-load-envvars-file my-env-file))
;;; Code to replace exec-path-from-shell

(provide 'init-my-cust-fun)
