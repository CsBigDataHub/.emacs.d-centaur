;; init-gradle.el --- Initialize gradle configurations.	-*- lexical-binding: t -*-

(use-package gradle-mode
  :config
  (setq gradle-gradlew-executable "./gradlew"
        gradle-use-gradlew t)
  (gradle-mode))

(use-package flycheck-gradle
  :commands (flycheck-gradle-setup)
  :init
  (mapc
   (lambda (x)
     (add-hook x #'flycheck-gradle-setup))
   '(java-mode-hook kotlin-mode-hook)))

(provide 'init-gradle)
