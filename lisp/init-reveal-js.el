;;init-reveal-js.el --- Initialize ox-reveal configurations.	-*- lexical-binding: t -*-

(use-package ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize)

(provide 'init-reveal-js)
