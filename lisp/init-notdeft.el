;;init-notdeft.el --- Initialize notdeft configurations.	-*- lexical-binding: t -*-

;; (add-to-list 'load-path "~/.emacs.d/lisp/notdeft/")

(use-package notdeft
  :straight (notdeft
             :type git
             :host github
             :repo "hasu/notdeft"
             :pre-build (("make")
                         ("bash" "-c" "cd xapian && make"))
             :files ("*.el" "xapian" "extras/*-hydra.el")))

(require 'notdeft-autoloads)

(when sys/macp
  (setq notdeft-directories '("/Users/ckoneru/Documents/my-org-notes/")))
;; "$HOME/GitRepos/my-projects/Mac-pref-Backup/org-file-notes/"

(setq notdeft-extension "org")
(setq notdeft-secondary-extensions '("md" "txt"))
(setq notdeft-allow-org-property-drawers t)

(setq notdeft-xapian-program (expand-file-name "straight/build/notdeft/xapian/notdeft-xapian" user-emacs-directory))
;; (setq notdeft-xapian-program-compile-command-format "g++ -o %s %s -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs`")

(defun run-local-variables-mode-hooks ()
  "Run hooks for `major-mode' with locals set.
Like `run-mode-hooks', but run later, with any buffer and
directory local variables set."
  (run-hooks (intern (concat (symbol-name major-mode)
                             "-local-variables-hook"))))
(add-hook 'hack-local-variables-hook 'run-local-variables-mode-hooks)

;; A variable determining whether to enable minor mode.
(defcustom notdeft-note-mode-auto-enable nil
  "Whether to enable NotDeft Note minor mode for a buffer."
  :type 'boolean
  :safe 'booleanp)
(make-variable-buffer-local 'notdeft-note-mode-auto-enable)

;; Define a hook for conditionally enabling the NotDeft minor mode.
(defun default-notdeft-hook ()
  "Conditionally enable `notdeft-note-mode'.
Enable when the buffer local variable
`notdeft-note-mode-auto-enable' is set to a non-nil value."
  (when notdeft-note-mode-auto-enable
    (notdeft-note-mode 1)))

;; Have Org mode files respect the flag. A hook like this should be
;; set for all NotDeft note file types, and no others.
(add-hook 'org-mode-local-variables-hook 'default-notdeft-hook)

(defun my-notdeft-add-directory-local-variables ()
  "Add `notdeft-note-mode-auto-enable' flag.
Add it for all `notdeft-directories'."
  (interactive)
  (dolist (dir notdeft-directories)
    (make-directory dir t)
    (let ((default-directory dir))
      (add-dir-local-variable nil 'notdeft-note-mode-auto-enable t))))

;; Org mode "deft:" and "notdeft:" link support.
(eval-after-load 'org
  (lambda ()
    (let ((ver (ignore-errors
                 (car (version-to-list org-version)))))
      (require (if (and ver (< ver 9))
                   'notdeft-org8
                 'notdeft-org9)))))

;; Add global bindings for NotDeft. To do that, bind a custom keymap
;; that inherits from NotDeft's, one that we can use to override and
;; add to the predefined set of bindings.
(require 'notdeft-global)
(defvar my-notdeft-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(a) (d) (l) (v)]
      #'my-notdeft-add-directory-local-variables)
    (define-key map [(l)] #'notdeft-org-link-existing-note) ;; l for link
    (define-key map [(n)] #'notdeft-org-link-new-file) ;; n for new
    (define-key map [(s)] #'org-store-link) ;; s for store
    (define-key map [(S)] #'notdeft-org-store-deft-link) ;; s for store
    (define-key map [(*)] #'notdeft-org-open-heading-as-query)
    (set-keymap-parent map 'notdeft-global-map)
    map)
  "Custom keymap for accessing NotDeft functionality.

\\{my-notdeft-global-map}")
(fset 'my-notdeft-global-map my-notdeft-global-map)
(global-set-key (kbd "C-c d n") 'my-notdeft-global-map)

;; Add Org-specific bindings that are also usable in a NotDeft buffer.
(add-hook 'notdeft-load-hook
          (lambda ()
            (define-key notdeft-mode-map (kbd "C-c S")
              #'notdeft-org-store-deft-link)))

(require 'hydra nil t)
(when (featurep 'hydra)
  (autoload 'notdeft-global-hydra/body "notdeft-global-hydra" nil t)
  (autoload 'notdeft-mode-hydra/body "notdeft-mode-hydra")
  ;; Augment `notdeft-mode' bindings with a hydra.
  (autoload 'notdeft-mode-hydra/body "notdeft-mode-hydra" nil t)
  (add-hook 'notdeft-load-hook
            (lambda ()
              (define-key notdeft-mode-map (kbd "C-c d n h")
                #'notdeft-mode-hydra/body)))

  ;; Augment the global NotDeft keymap with a hydra also.
  (autoload 'notdeft-global-hydra/body "notdeft-global-hydra" nil t)
  (define-key my-notdeft-global-map [(h)] 'notdeft-global-hydra/body))

;; (require 'org-web-tools)
;;
;; (defun my-org-web-tools--html-to-org-with-pandoc (html &optional selector)
;;   "Return string of HTML converted to Org with Pandoc.
;; When SELECTOR is non-nil, the HTML is filtered using
;; `esxml-query' SELECTOR and re-rendered to HTML with
;; `org-web-tools--dom-to-html', which see."
;;   (when selector
;;     (setq html (->> (with-temp-buffer
;;                       (insert html)
;;                       (libxml-parse-html-region 1 (point-max)))
;;                     (esxml-query selector)
;;                     ;; MAYBE: Should probably use `shr-dom-print' instead.
;;                     (org-web-tools--dom-to-html))))
;;   (with-temp-buffer
;;     (insert html)
;;     (unless (zerop (call-process-region (point-min) (point-max) "pandoc"
;;                                         t t nil
;;                                         (org-web-tools--pandoc-no-wrap-option)
;;                                         "-f" "html+smart" "-t" "org+smart" "--standalone"))
;;       ;; TODO: Add error output, see org-protocol-capture-html
;;       (error "Pandoc failed"))
;;     (org-web-tools--clean-pandoc-output)
;;     (buffer-string)))
;;
;; (defun my-org-web-tools--url-as-readable-org (&optional url)
;;   "Return string containing Org entry of URL's web page content.
;; Content is processed with `eww-readable' and Pandoc.  Entry will
;; be a top-level heading, with article contents below a
;; second-level \"Article\" heading, and a timestamp in the
;; first-level entry for writing comments."
;;   ;; By taking an optional URL, and getting it from the clipboard if
;;   ;; none is given, this becomes suitable for use in an org-capture
;;   ;; template, like:
;;
;;   ;; ("wr" "Capture Web site with eww-readable" entry
;;   ;;  (file "~/org/articles.org")
;;   ;;  "%(org-web-tools--url-as-readable-org)")
;;   (-let* ((url (or url (org-web-tools--get-first-url)))
;;           (html (org-web-tools--get-url url))
;;           (html (org-web-tools--sanitize-html html))
;;           ((title . readable) (org-web-tools--eww-readable html))
;;           (title (org-web-tools--cleanup-title (or title "")))
;;           (converted (my-org-web-tools--html-to-org-with-pandoc readable))
;;           (link (org-make-link-string url title))
;;           (timestamp (format-time-string (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]"))))
;;     (with-temp-buffer
;;       (org-mode)
;;       ;; Insert article text
;;       (insert converted)
;;       ;; Demote in-article headings
;;       ;; MAYBE: Use `org-paste-subtree' instead of demoting headings ourselves.
;;       (org-web-tools--demote-headings-below 2)
;;       ;; Insert headings at top
;;       (goto-char (point-min))
;;       (insert
;;        "#+TITLE: " title "\n\n"
;;        "* " link " :website:" "\n\n"
;;        timestamp "\n\n"
;;        "** Article" "\n\n")
;;       (buffer-string))))
;;
;; (defun my-notdeft-import-web-page (url &optional ask-dir)
;;   "Import the web page at URL into NotDeft.
;; Query for the target directory if ASK-DIR is non-nil.
;; Interactively, query for a URL, and set ASK-DIR if a prefix
;; argument is given. Choose a file name based on any document
;; <title>, or generate some unique name."
;;   (interactive "sPage URL: \nP")
;;   (let* ((s (my-org-web-tools--url-as-readable-org url))
;;          (html (org-web-tools--get-url url))
;;          (title (org-web-tools--html-title html)
;;                 ))
;;     (notdeft-create-file
;;      (and ask-dir 'ask)
;;      (and title `(title, title))
;;      "org" s)))

;; override to using VLF instead of find-file
;; (defun notdeft-find-file (file)
;;   "Edit NotDeft note FILE.
;; Enable NotDeft note mode for the buffer for editing the file.
;; Called interactively, query for the FILE using the minibuffer."
;;   (interactive "FFind NotDeft file: ")
;;   (prog1 (vlf file)
;;     (notdeft-note-mode 1)))

(provide 'init-notdeft)
