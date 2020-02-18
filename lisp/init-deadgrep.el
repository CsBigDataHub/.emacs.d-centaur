(use-package deadgrep
  :bind (("C-c d g" . deadgrep))
  :config
  (setq-default deadgrep--search-type 'regexp) ;Default is 'string

  (defun my/deadgrep--jump-to-and-execute (re)
    "Execute the button that matches RE and push it."
    (goto-char (point-min))
    (re-search-forward re)
    (backward-char 3)
    (push-button))

  (defun my/deadgrep-change-search-term ()
    "Change the search term."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Search term: .*change$"))

  (defun my/deadgrep-change-search-type-to-string ()
    "Change the search type to 'string'."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Search type: .*string"))

  (defun my/deadgrep-change-search-type-to-words ()
    "Change the search type to 'words'."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Search type: .*words"))

  (defun my/deadgrep-change-search-type-to-regexp ()
    "Change the search type to 'regexp'."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Search type: .*regexp"))

  (defun my/deadgrep-change-case-to-smart ()
    "Change the case sensitivity to 'smart'."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Case: .*smart"))

  (defun my/deadgrep-change-case-to-sensitive ()
    "Change the case sensitivity to 'sensitive'."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Case: .*sensitive"))

  (defun my/deadgrep-change-case-to-ignore ()
    "Change the case sensitivity to 'ignore'."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Case: .*ignore"))

  (defun my/deadgrep-change-context-to-none ()
    "Don't show ny context around the search results."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Context: .*none"))

  (defun my/deadgrep-change-context-to-before ()
    "Set 'before' context for the search results."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Context: .*before"))

  (defun my/deadgrep-change-context-to-after ()
    "Set 'after' context for the search results."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Context: .*after"))

  (defun my/deadgrep-change-directory ()
    "Change the root directory for searches."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Directory: .*$"))

  (defun my/deadgrep-search-all-files ()
    "Change file search scope to 'all'."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Files: .*all"))

  (defun my/deadgrep-search-files-by-type ()
    "Search only in the specified file types."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Files: .*type"))

  (defun my/deadgrep-search-files-by-glob ()
    "Search in files names that match the specified glob."
    (interactive)
    (my/deadgrep--jump-to-and-execute "^Files: .*glob"))

  (bind-keys
   :map deadgrep-mode-map
   ("s" . my/deadgrep-change-search-term)
   ("ts" . my/deadgrep-change-search-type-to-string)
   ("tw" . my/deadgrep-change-search-type-to-words)
   ("tr" . my/deadgrep-change-search-type-to-regexp)
   ("cs" . my/deadgrep-change-case-to-smart)
   ("cc" . my/deadgrep-change-case-to-sensitive)
   ("ci" . my/deadgrep-change-case-to-ignore)
   ("xn" . my/deadgrep-change-context-to-none)
   ("xb" . my/deadgrep-change-context-to-before)
   ("xa" . my/deadgrep-change-context-to-after)
   ("d" . my/deadgrep-change-directory)
   ("fa" . my/deadgrep-search-all-files)
   ("ft" . my/deadgrep-search-files-by-type)
   ("fg" . my/deadgrep-search-files-by-glob))
  )

(provide 'init-deadgrep)
