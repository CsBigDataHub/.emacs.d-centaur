;; init-reader.el --- Initialize readers.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Vincent Zhang

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
;; PDF/EPUB/RSS readers.
;;

;;; Code:

(require 'init-const)

;; PDF reader
(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
    :hook ((pdf-view-mode . pdf-view-midnight-minor-mode)
           (pdf-view-mode . pdf-isearch-minor-mode))
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
           ("C-s" . isearch-forward))
    :init
    (setq pdf-view-use-scaling t
          pdf-view-use-imagemagick nil
          pdf-annot-activate-created-annotations t)

    ;; Set dark theme
    (defun my-pdf-view-set-midnight-colors ()
      "Set pdf-view midnight colors."
      (setq pdf-view-midnight-colors
            `(,(face-foreground 'default) . ,(face-background 'solaire-default-face nil t))))
    (my-pdf-view-set-midnight-colors)

    (defun my-pdf-view-set-dark-theme ()
      "Set pdf-view midnight theme as color theme."
      (my-pdf-view-set-midnight-colors)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq major-mode 'pdf-view-mode)
            (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))
    (add-hook 'after-load-theme-hook #'my-pdf-view-set-dark-theme)
    :config
    (with-no-warnings
      ;; Build pdfinfo if needed, locking until it's complete
      (defun my-pdf-tools-install ()
        (unless (file-executable-p pdf-info-epdfinfo-program)
          (let ((wconf (current-window-configuration)))
            (pdf-tools-install t)
            (message "Building epdfinfo. Please wait for a moment...")
            (while compilation-in-progress
              ;; Block until `pdf-tools-install' is done
              (sleep-for 1))
            (when (file-executable-p pdf-info-epdfinfo-program)
              (set-window-configuration wconf)))))
      (advice-add #'pdf-view-decrypt-document :before #'my-pdf-tools-install)

      ;; Highlight matches
      (defun my-pdf-isearch-hl-matches (current matches &optional occur-hack-p)
        "Highlighting edges CURRENT and MATCHES."
        (cl-destructuring-bind (fg1 bg1 fg2 bg2)
          (pdf-isearch-current-colors)
          (let* ((width (car (pdf-view-image-size)))
                 (page (pdf-view-current-page))
                 (window (selected-window))
                 (buffer (current-buffer))
                 (tick (cl-incf pdf-isearch--hl-matches-tick))
                 (pdf-info-asynchronous
                  (lambda (status data)
                    (when (and (null status)
                               (eq tick pdf-isearch--hl-matches-tick)
                               (buffer-live-p buffer)
                               (window-live-p window)
                               (eq (window-buffer window)
                                   buffer))
                      (with-selected-window window
                        (when (and (derived-mode-p 'pdf-view-mode)
                                   (or isearch-mode
                                       occur-hack-p)
                                   (eq page (pdf-view-current-page)))
                          (pdf-view-display-image
                           (pdf-view-create-image data :width width))))))))
            (pdf-info-renderpage-text-regions
             page width t nil
             `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                            current))
             `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                            (apply 'append
                                   (remove current matches))))))))
      (advice-add #'pdf-isearch-hl-matches :override #'my-pdf-isearch-hl-matches)

      ;; Show annotation
      (defun my-pdf-annot-show-annotation (a &optional highlight-p window)
        "Make annotation A visible."
        (save-selected-window
          (when window (select-window window 'norecord))
          (pdf-util-assert-pdf-window)
          (let ((page (pdf-annot-get a 'page))
                (size (pdf-view-image-size)))
            (unless (= page (pdf-view-current-page))
              (pdf-view-goto-page page))
            (let ((edges (pdf-annot-get-display-edges a)))
              (when highlight-p
                (pdf-view-display-image
                 (pdf-view-create-image
                     (pdf-cache-renderpage-highlight
                      page (car size)
                      `("white" "steel blue" 0.35 ,@edges))
                   :map (pdf-view-apply-hotspot-functions
                         window page size)
                   :width (car size))))
              (pdf-util-scroll-to-edges
               (pdf-util-scale-relative-to-pixel (car edges)))))))
      (advice-add #'pdf-annot-show-annotation :override #'my-pdf-annot-show-annotation))

    ;; Recover last viewed position
    (use-package saveplace-pdf-view
      :commands (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
      :init
      (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
      (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))))

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (centaur-read-mode)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (with-no-warnings
    ;; HACK: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

  ;; Fix encoding issue on Windows
  (when sys/win32p
    (setq process-coding-system-alist
          (cons `(,nov-unzip-program . (gbk . gbk))
                process-coding-system-alist))))

;; Atom/RSS reader
(when emacs/>=25.2p
  (use-package elfeed
    :pretty-hydra
    ((:title (pretty-hydra-title "Elfeed" 'faicon "rss-square" :face 'all-the-icons-orange :height 1.1 :v-adjust -0.05)
      :color amaranth :quit-key "q")
     ("Search"
      (("C" elfeed-db-compact "compact db")
       ("g" elfeed-search-update--force "refresh")
       ("G" elfeed-search-fetch "update")
       ("y" elfeed-search-yank "copy URL")
       ("+" elfeed-search-tag-all "tag all")
       ("-" elfeed-search-untag-all "untag all"))
      "Filter"
      (("ss" elfeed-search-live-filter "live filter")
       ("st" my/elfeed-search-tag-filter "tag filter")
       ("c" elfeed-search-clear-filter "clear filter")
       ("S" elfeed-search-set-filter "set filter")
       ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
       ("a" (elfeed-search-set-filter "@6-months-ago") "all")
       ("t" (elfeed-search-set-filter "@1-day-ago") "today"))
      "Article"
      (("b" elfeed-search-browse-url "browse")
       ("B" my/elfeed-search-browse-background-url "browse-at-background")
       ("n" next-line "next")
       ("p" previous-line "previous")
       ("8" elfeed-toggle-star "star article")
       ("u" elfeed-search-tag-all-unread "mark unread")
       ("r" elfeed-search-untag-all-unread "mark read")
       ("RET" elfeed-search-show-entry "show"))))
    :bind (("C-x w w" . elfeed)
           :map elfeed-search-mode-map
           ("?" . elfeed-hydra/body)
           :map elfeed-show-mode-map
           ("o" . ace-link)
           ("q" . delete-window))
    :hook (elfeed-show-mode . centaur-read-mode)
    :init (setq url-queue-timeout 30
                elfeed-db-directory (locate-user-emacs-file ".elfeed")
                elfeed-show-entry-switch #'pop-to-buffer
                elfeed-show-entry-delete #'delete-window
                elfeed-feeds '(("https://planet.emacslife.com/atom.xml" planet emacslife)
                               ("http://www.masteringemacs.org/feed/" mastering)
                               ("https://oremacs.com/atom.xml" oremacs)
                               ("https://pinecast.com/feed/emacscast" emacscast)
                               ("https://www.reddit.com/r/orgmode.rss" reddit-orgmode)
                               ("https://www.reddit.com/r/planetemacs.rss" reddit-planet-emacs)
                               ("https://www.reddit.com/r/emacs.rss" reddit)
                               ("https://itsfoss.com/feed" itsfoss)
                               ("https://fossbytes.com/feed" fossbytes)
                               ("https://www.omgubuntu.co.uk/feed" omgubuntu)
                               ("https://trstringer.com/feed" trstringer)
                               ))
    :config
    ;; Ignore db directory in recentf
    (push elfeed-db-directory recentf-exclude)
    (setq elfeed-show-unique-buffers t)
    (defalias 'elfeed-toggle-star
      (elfeed-expose #'elfeed-search-toggle-all 'star))

    (eval-after-load 'elfeed-search
      '(define-key elfeed-search-mode-map (kbd "8") 'elfeed-toggle-star))
    ;; face for starred articles
    (defface elfeed-search-starred-title-face
      '((t :foreground "#7C00FC000000"))
      "Marks a starred Elfeed entry.")
    (push '(star elfeed-search-starred-title-face) elfeed-search-face-alist)

    (defun my/elfeed-search-tag-filter ()
      "Filter `elfeed' by tags using completion.

Arbitrary input is also possible, but you may need to exit the
minibuffer with `exit-minibuffer' (I bind it to C-j in
`minibuffer-local-completion-map')."
      (interactive)
      (unwind-protect
          (elfeed-search-clear-filter)
        ;; NOTE for the `crm-separator' to work with just a space, you
        ;; need to make SPC self-insert in the minibuffer (the default is
        ;; to behave like tab-completion).
        (let* ((crm-separator " ")
               (elfeed-search-filter-active :live)
               (db-tags (elfeed-db-get-all-tags))
               (plus-tags (delete-dups
                           (mapcar (lambda (x)
                                     (concat "+" (format "%s" x)))
                                   db-tags)))
               (minus-tags (delete-dups
                            (mapcar (lambda (x)
                                      (concat "-" (format "%s" x)))
                                    db-tags)))
               (all-tags (append plus-tags minus-tags))
               (tags (completing-read-multiple
                      "Apply tag: "
                      all-tags nil t))
               (input (cons elfeed-search-filter tags)))
          (setq elfeed-search-filter
                ;; How to unlist properly (remove parentheses)?  Keeping
                ;; this inelegant form until I find that…
                (substring (format "%s" input) 1 -1)))
        (elfeed-search-update :force)))

    (defun  my/elfeed-search-browse-background-url ()
      "Open current ` elfeed ' entry (or region entries) in browser without losing focus.
        http://xenodium.com/open-emacs-elfeed-links-in-background/index.html
        For Linux - In firefox's about:config page set preference:
        'browser.tabs.loadDivertedInBackground' to 'true'.
      "
      (interactive)
      (let ((entries (elfeed-search-selected)))
        (mapc (lambda (entry)
                (when sys/macp
                  (start-process (concat  "open " (elfeed-entry-link entry))
                                 nil  "open"  "--background" (elfeed-entry-link entry)))
                (when sys/linuxp ;; https://stackoverflow.com/a/2276679
                  (start-process (concat "firefox" (elfeed-entry-link entry))
                                 nil "firefox" "--new-tab" (elfeed-entry-link entry)))
                (elfeed-untag entry 'unread)
                (elfeed-search-update-entry entry))
              entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
          (forward-line))))
    ;; Use xwidget if possible
    (with-no-warnings
      (defun my-elfeed-show-visit (&optional use-generic-p)
        "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
        (interactive "P")
        (let ((link (elfeed-entry-link elfeed-show-entry)))
          (when link
            (message "Sent to browser: %s" link)
            (cond
             ((featurep 'xwidget-internal)
              (centaur-webkit-browse-url link))
             (use-generic-p
              (browse-url-generic link))
             (t (browse-url link))))))
      (advice-add #'elfeed-show-visit :override #'my-elfeed-show-visit)

      (defun my-elfeed-search-browse-url (&optional use-generic-p)
        "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
        (interactive "P")
        (let ((entries (elfeed-search-selected)))
          (cl-loop for entry in entries
                   do (elfeed-untag entry 'unread)
                   when (elfeed-entry-link entry)
                   do (cond
                       ((featurep 'xwidget-internal)
                        (centaur-webkit-browse-url it t))
                       (use-generic-p
                        (browse-url-generic it))
                       (t (browse-url it))))
          (mapc #'elfeed-search-update-entry entries)
          (unless (or elfeed-search-remain-on-entry (use-region-p))
            (forward-line))))
      (advice-add #'elfeed-search-browse-url :override #'my-elfeed-search-browse-url))
    )
  )

;; Another Atom/RSS reader
(use-package newsticker
  :ensure nil
  :bind ("C-x W" . newsticker-show-news)
  :hook (newsticker-treeview-item-mode . centaur-read-mode)
  :init (setq newsticker-url-list
              '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
                ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
                ("Oremacs" "https://oremacs.com/atom.xml")
                ("EmacsCast" "https://pinecast.com/feed/emacscast")
                ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss"))))

(provide 'init-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reader.el ends here
