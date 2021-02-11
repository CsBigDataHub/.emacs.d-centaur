;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((magit-status-mode . ((eval . (remove-hook 'magit-refs-sections-hook 'magit-insert-tags))
                       (eval . (magit-disable-section-inserter 'magit-insert-tags-header))
                       (magit-todos-exclude-globs . ("*.elc" "elpa/**" "straight/**" "*.md" "*.org")))))
