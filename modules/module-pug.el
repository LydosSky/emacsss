;;; module-pug.el --- Pug mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This module sets up pug-mode for editing Pug templates in Emacs.
;;; Code:

(use-package pug-mode
  :ensure t
  :defer t
  :mode "\\.pug\\'"
  :config
  ;; Optional customizations for pug-mode
  ;; Set indentation level to 2 spaces
  (setq pug-tab-width 2))


(provide 'module-pug)
;;; module-pug.el ends here
