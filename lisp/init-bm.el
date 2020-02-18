(use-package bm
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)


  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  ;;(setq bm-repository-file "~/.emacs.d/var/bm-repository");; handled by no-littering package

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


  ;; :bind (("H-b n" . bm-next)
  ;;        ("H-b p" . bm-previous)
  ;;        ("H-b b" . bm-toggle))


  (bind-key "C-c h b"
            (defhydra hydra-bm (:color pink
                                :hint nil
                                :body-pre (when (not (use-region-p))
                                            (push-mark)))
              "
Bookmark  _n_ext (_N_ in lifo order)            _b_ toggle bookmark        _l_ bm list                                            _s_ toggle persistence
          _p_revious (_P_ in lifo order)        _a_nnotate               _x_/_X_ remove all bm from current/all buffer(s)
          show _A_nnotation                     _M_ toggle bookmark       _L_ bm list all
"
              ("b"   bm-toggle)
              ("M"   bm-toggle :color blue)
              ("a"   bm-bookmark-annotate :color blue)
              ("A"   bm-bookmark-show-annotation)
              ("n"   bm-common-next)
              ("N"   bm-lifo-next)
              ("l"   bm-show)
              ("L"   bm-show-all)
              ("p"   bm-common-previous)
              ("P"   bm-lifo-previous)
              ("s"   bm-toggle-buffer-persistence)
              ("x"   bm-remove-all-current-buffer :color blue)
              ("X"   bm-remove-all-all-buffers :color blue)
              ("r"   pop-to-mark-command :color blue)
              ("RET" nil "cancel" :color blue)
              ("q"   nil "cancel" :color blue)))
  )

(provide 'init-bm)
