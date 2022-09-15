;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2022 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
;; Directory configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode)
         ( "." . my/hydra-dired/body))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
;;; Dired copy or move to split window suggestions
  ;;https://emacs.stackexchange.com/questions/5603/how-to-quickly-copy-move-file-in-emacs-dired
  (setq dired-dwim-target t)

  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
            (and (not sys/macp) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; Quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind (:map dired-mode-map
             ("S" . hydra-dired-quick-sort/body))))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
           (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync)))

  ;; Colorful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package all-the-icons-dired
    :diminish
    :hook (dired-mode . (lambda ()
                          (when (icon-displayable-p)
                            (all-the-icons-dired-mode))))
    :init (setq all-the-icons-dired-monochrome nil)
    :config
    (with-no-warnings
      (defun my-all-the-icons-dired--icon (file)
        "Return the icon for FILE."
        (if (file-directory-p file)
            (all-the-icons-icon-for-dir file
                                        :height 0.9
                                        :face 'all-the-icons-dired-dir-face
                                        :v-adjust all-the-icons-dired-v-adjust)
          (apply 'all-the-icons-icon-for-file file
                 (append
                  '(:height 0.9)
                  `(:v-adjust ,all-the-icons-dired-v-adjust)
                  (when all-the-icons-dired-monochrome
                    `(:face ,(face-at-point)))))))
      (advice-add #'all-the-icons-dired--icon :override #'my-all-the-icons-dired--icon)))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand t
    :config
    (let ((cmd (cond (sys/mac-x-p "open")
                     (sys/linux-x-p "xdg-open")
                     (sys/win32p "start")
                     (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-garbage-files-regexp
          "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'")

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

;; my-personal

(use-package dired-narrow
  :config
  (bind-key "C-c C-n" #'dired-narrow)
  (bind-key "C-c C-f" #'dired-narrow-fuzzy)
  (bind-key "C-x C-N" #'dired-narrow-regexp)
  )

;;(use-package dired-subtree
;;  :after dired
;;  :config
;;  (bind-key "t" #'dired-subtree-toggle dired-mode-map)
;;  (bind-key "T" #'dired-subtree-cycle dired-mode-map))


(use-package peep-dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (define-key dired-mode-map (kbd "P") 'peep-dired)
  )

(use-package dired-single
  :config
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  )

(use-package dired-collapse
  :init
  (add-hook 'dired-mode-hook 'dired-collapse-mode))

(use-package image-dired+
  :config
  (setq auto-image-file-mode t)
  (eval-after-load 'image-dired+ '(image-diredx-async-mode 1)))

(use-package dired-filter
  ;;:init
  ;;(add-hook 'dired-mode-hook 'dired-filter-group-mode)
  :config
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Directories" (directory))
           ("PDF"
            (extension . "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("Org"
            (extension . "org"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar"))
           ("Multimedia"
            (extension "ogg" "flv" "mpg" "avi" "mp4" "mp3"))
           )))
  )

;; Display the recursive size of directories in Dired
(use-package dired-du
  :after dired
  :config
  ;; human readable size format
  (setq dired-du-size-format t))

(use-package wdired
  :after dired
  :config
  (setq wdired-create-parent-directories t)
  ;; Make permission bits editable
  (setq wdired-allow-to-change-permissions t))

(use-package dired-hacks-utils
  :hook (dired-mode . dired-utils-format-information-line-mode))

(use-package dired-ranger
  :after dired
  :init
  (bind-keys :map dired-mode-map
             :prefix "c"
             :prefix-map dired-ranger-map
             :prefix-docstring "Map for ranger operations."
             ("c" . dired-ranger-copy)
             ("p" . dired-ranger-paste)
             ("m" . dired-ranger-move))

  (bind-keys :map dired-mode-map
    ("'" . dired-ranger-bookmark)
    ("`" . dired-ranger-bookmark-visit)))

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

(defun dired-dwim-target-directory ()
  ;; Try to guess which target directory the user may want.
  ;; If there is a dired buffer displayed in one of the next windows,
  ;; use its current subdir, else use current subdir of this dired buffer.
  (let ((this-dir (and (eq major-mode 'dired-mode)
                       (dired-current-directory))))
    ;; non-dired buffer may want to profit from this function, e.g. vm-uudecode
    (if dired-dwim-target
        (let* ((other-win (get-window-with-predicate
                           (lambda (window)
                             (with-current-buffer (window-buffer window)
                               (eq major-mode 'dired-mode)))
                           nil
                           'visible)) ; <========================
               (other-dir (and other-win
                               (with-current-buffer (window-buffer other-win)
                                 (and (eq major-mode 'dired-mode)
                                      (dired-current-directory))))))
          (or other-dir this-dir))
      this-dir)))

;;;###autoload - http://oremacs.com/2016/02/24/dired-rsync/
(defun ora-dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

(define-key dired-mode-map "Y" 'ora-dired-rsync)

;;http://xenodium.com/drill-down-emacs-dired-with-dired-subtree/
(use-package dired-subtree
  :after dired
  :config
  (bind-key ">" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<" #'dired-subtree-cycle dired-mode-map))
;; my-personal


(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
