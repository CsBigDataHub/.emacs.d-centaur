;; init-engine.el --- Initialize engine configurations.	-*- lexical-binding: t -*-
(use-package engine-mode
  :config
  (which-key-add-key-based-replacements
    "C-c /" "engine-mode")
  ;; enable engine-mode globally
  (engine-mode t)

  ;; https://github.com/hrs/engine-mode#changing-your-default-browser
  ;; (setq engine/browser-function 'eww-browse-url)

  (defengine amazon
    "https://www.amazon.com/s/ref=nb_sb_noss?field-keywords=%s"
    :keybinding "a")

  (defengine dictionary
    "http://www.dictionary.com/browse/%s"
    :keybinding "d")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine google-translate
    "https://translate.google.com/#auto/en/%s"
    :keybinding "t")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")
  )

(provide 'init-engine)
