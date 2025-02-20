;;; module-git-gutter.el --- diff-hl configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up git-gutter and git-gutter-fringe for version control diff highlighting,
;; customized to use slimmer pipes instead of thick blocks, and ensures
;; compatibility with TTY Emacs by using the margin.

;;; Code:

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :ensure t
  :config
  (setq git-gutter:update-interval 0.02))
(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


(provide 'module-git-gutter)
;;; module-git-gutter.el ends here
