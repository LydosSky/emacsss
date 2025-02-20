;;; module-formatter.el --- Apheleia code formatting configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up Apheleia for asynchronous code formatting using external formatters.
;; It integrates seamlessly with your existing Emacs setup.

;;; Code:

(use-package apheleia
  :ensure t
  :defer t
  :commands (apheleia-format-buffer apheleia-mode)
  :hook ((prog-mode . apheleia-mode))
  :init
  ;; Enable Apheleia globally (optional)
  ;; (apheleia-global-mode +1)
  )

(provide 'module-formatter)
;;; module-formatter.el ends here
