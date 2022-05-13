;;; init-lsp-hydra.el --- Hydra for LSP modes -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Chetan Koneru

;; Author: Chetan Koneru

;;; Commentary: https://github.com/jerrypnz/.emacs.d/blob/master/lisp/jp-lsp-hydra.el

;;; Code:


(require 'init-const)
(require 'init-custom)
(require 'init-funcs)
(require 'major-mode-hydra)

(defun with-mode-icon (mode str &optional height nospace face)
  (let* ((v-adjust (if (eq major-mode 'emacs-lisp-mode) 0.0 0.05))
         (args     `(:height ,(or height 1) :v-adjust ,v-adjust))
         (_         (when face
                      (lax-plist-put args :face face)))
         (icon     (apply #'all-the-icons-icon-for-mode mode args))
         (icon     (if (symbolp icon)
                       (apply #'all-the-icons-octicon "file-text" args)
                     icon)))
    (s-concat icon (if nospace "" " ") str)))

(defun my-lsp-hydra--title ()
  (with-mode-icon major-mode
                  (propertize (s-concat "LSP Commands (" (format-mode-line mode-name) ")")
                              'face '(:weight bold :height 1.1))
                  1.1))

(pcase centaur-lsp
  ('eglot
   (major-mode-hydra-define+ (rust-mode go-mode python-mode java-mode scala-mode terraform-mode yaml-mode csharp-mode)
     (:foreign-keys run :color amaranth :quit-key "q" :title (my-lsp-hydra--title))
     ("Quick Action"
      (("d" eldoc-print-current-symbol-info "describe symbol")
       ("a" eglot-code-actions "code action")
       ("m" counsel-imenu "imenu")
       ("i" eldoc-doc-buffer "info at point")
       ("f" eglot-format "format"))
      "Find & Goto"
      (("gr" xref-find-references "references")
       ("gd" xref-find-definitions "definitions")
       ;; ("gf" lsp-ivy-workspace-symbol "workspace symbol")
       )
      "Errors"
      (("ee" flymake-show-diagnostics-buffer "list errors")
       ("er" flymake-goto-next-error "next error")
       ("ew" flymake-goto-prev-error "prev error")
       ("es" flymake-start "start checks")
       ("eS" flymake-stop-all-syntax-checks "stop checks"))
      "Refactor"
      (("rs" eglot-rename "rename"))
      "Connection"
      (("cc" eglot "start")
       ("cr" eglot-reconnect "restart")
       ("cd" eglot-events-buffer "describe session")
       ("ce" eglot-stderr-buffer "lsp error buffer")
       ("xx" eglot-shutdown "shutdown")
       ("XX" eglot-shutdown-all "shutdown all"))
      ))
   )
  ('lsp-mode
   (major-mode-hydra-define+ (rust-mode go-mode python-mode java-mode scala-mode terraform-mode yaml-mode csharp-mode)
     (:foreign-keys run :color amaranth :quit-key "q" :title (my-lsp-hydra--title))
     ("Quick Action"
      (("d" lsp-describe-thing-at-point "describe symbol")
       ("a" lsp-execute-code-action "code action")
       ("m" lsp-ui-imenu "imenu")
       ("i" lsp-info-under-point "info at point")
       ("f" lsp-format-buffer "format"))
      "Find & Goto"
      (("gr" lsp-ui-peek-find-references "references")
       ("gd" lsp-ui-peek-find-definitions "definitions")
       ("gt" lsp-goto-type-definition "type defintion")
       ("gf" lsp-ivy-workspace-symbol "workspace symbol"))
      "Errors"
      (("ee" hydra-flycheck/body)
       ("el" lsp-ui-flycheck-list))
      "Refactor"
      (("rs" lsp-rename))
      "Connection"
      (("cc" lsp "start")
       ("cr" lsp-restart-workspace "restart")
       ("cd" lsp-describe-session "describe session")
       ("cq" lsp-shutdown-workspace "shutdown"))
      "Toggles"
      (("ol" lsp-lens-mode "toggle lens" :toggle t :exit nil)
       ("od" lsp-ui-doc-mode "toggle hover doc" :toggle t :exit nil)
       ("os" lsp-ui-sideline-mode "toggle sideline" :toggle t :exit nil))))


   (major-mode-hydra-define+ java-mode
     (:foreign-keys run :color amaranth :quit-key "q" :title (my-lsp-hydra--title))
     ("Quick Action"
      (("b" lsp-java-build-project "build")
       ("t" dap-java-run-test-class "test")
       ("T" dap-java-debug-test-class "debug test")
       ("O" lsp-java-organize-imports "organize imports")
       ("U" lsp-java-update-project-configuration "update project config"))
      "Generate"
      (("gs" lsp-generate-to-string "generate toString")
       ("ge" lsp-java-generate-equals-and-hash-code "generate equals/hashCode")
       ("go" lsp-java-generate-overrides "generate overrides")
       ("gg" lsp-java-generate-getters-and-setters "generate getters/setters"))
      "Refactor"
      (("re" lsp-java-extract-to-constant "extract constant")
       ("rm" lsp-java-extract-method "extract method")
       ("ri" lsp-java-add-import  "add import")
       ("ru" lsp-java-add-unimplemented-methods "add unimplemented methods")
       ("rp" lsp-java-create-parameter "introduce parameter")
       ("rf" lsp-java-create-field "introduce field")
       ("rl" lsp-java-create-local "introduce local variable"))))

   (defun my-lsp-metals-build-restart ()
     (interactive)
     (lsp-send-execute-command "build-restart" ()))

   (major-mode-hydra-define+ scala-mode nil
     ("Connection"
      (("ci" lsp-metals-build-import "import build")
       ("cg" my-lsp-metals-build-restart "restart build"))))
   )
  )

(major-mode-hydra-define+ python-mode
  (:foreign-keys run :color amaranth :quit-key "q" :title (my-lsp-hydra--title))
  ("Quick Action"
   (("B" blacken-buffer "black format")))
  )

(major-mode-hydra-define+ go-mode
  (:foreign-keys run :color amaranth :quit-key "q" :title (my-lsp-hydra--title))
  ("Quick Action"
   (("Ia" go-import-add "add")
    ("Ir" go-remove-unused-imports "cleanup")))
  )
(provide 'init-lsp-hydra)

;;; init-lsp-hydra.el ends here
