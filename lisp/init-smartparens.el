;;init-smartparens.el --- Initialize smartparens configurations.	-*- lexical-binding: t -*-

(use-package smartparens
  :demand t
  :bind* (("M-m m j" . sp-down-sexp)
          ("M-m m k" . sp-backward-up-sexp)
          ("M-m m h" . sp-backward-down-sexp)
          ("M-m m l" . sp-up-sexp)
          ("M-m m f" . sp-forward-sexp)
          ("M-m m b" . sp-backward-sexp)
          ("M-m m a" . sp-beginning-of-sexp)
          ("M-m m e" . sp-end-of-sexp)
          ("M-m m n" . sp-next-sexp)
          ("M-m m p" . sp-previous-sexp)
          ("M-m m >" . sp-forward-barf-sexp)
          ("M-m m <" . sp-backward-barf-sexp)
          ("M-m m )" . sp-forward-slurp-sexp)
          ("M-m m (" . sp-backward-slurp-sexp)
          ("M-m m x" . sp-transpose-sexp)
          ("M-m m d" . sp-kill-sexp)
          ("M-m m y" . sp-copy-sexp)
          ("M-m m u" . sp-unwrap-sexp)
          ("M-m m U" . sp-backward-unwrap-sexp)
          ("M-m m C" . sp-convolute-sexp)
          ("M-m m r" . sp-raise-sexp)
          ("M-m m s" . sp-split-sexp)
          ("M-m m S" . sp-splice-sexp)
          ("M-m m F" . sp-splice-sexp-killing-forward)
          ("M-m m B" . sp-splice-sexp-killing-backward)
          ("M-m m A" . sp-splice-sexp-killing-around))
  :diminish smartparens-mode
  :diminish smartparens-strict-mode
  :config

  ;; Disable in org- mode as `smartparens' is heavy in `org-self-insert-command'.
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  ;; Disable for csharp-mode due to extra closing parenthesis entered.
  ;; https://www.reddit.com/r/emacs/comments/mskdsk/help_resolve_electric_pair_issue_in_c_mode/
  (add-to-list 'sp-ignore-modes-list 'csharp-mode)
  (add-to-list 'sp-ignore-modes-list 'c-mode)

  ;;(require 'smartparens-config)
  (smartparens-global-mode)
  ;;(smartparens-global-strict-mode)
  (show-smartparens-global-mode)
  (which-key-add-key-based-replacements
    "M-m m" "move prefix"))

;;https://github.com/Fuco1/smartparens/issues/286
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "`" nil
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp--org-skip-markup ms mb me)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(provide 'init-smartparens)
