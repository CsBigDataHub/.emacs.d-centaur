;;init-pass.el --- Initialize password-store configurations.	-*- lexical-binding: t -*-

(require 'epa-file)
(epa-file-enable)

(use-package password-generator)

(use-package pass)

(use-package password-store)


(provide 'init-pass)
