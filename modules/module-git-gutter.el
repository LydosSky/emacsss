;;; module-git-gutter.el --- diff-hl configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up git-gutter and git-gutter-fringe for version control diff highlighting,
;; customized to use slimmer pipes instead of thick blocks, and ensures
;; compatibility with TTY Emacs by using the margin.

;;; Code:

(use-package git-gutter
  :ensure t
  :hook
  ((prog-mode . git-gutter-mode)
   (text-mode . git-gutter-mode))
  :config
  ;; Adjust update interval (in seconds)
  (setq git-gutter:update-interval 0.2)
  (unless (display-graphic-p)
    (setq git-gutter:modified-sign"▎")
    (setq git-gutter:added-sign   "▎")
    (setq git-gutter:deleted-sign "▎")
    
    ;; Let git-gutter display in the margin
    (setq git-gutter:visibility-indicator nil)
    (setq git-gutter:window-width 1)
    ;; Adjust where the indicators appear
    (setq git-gutter:side 'left)))

(when (display-graphic-p)
  (use-package git-gutter-fringe
    :ensure t
    :demand t
    :after git-gutter
    :config
    ;; Use slim vertical bars for indicators
    (setq-default fringes-outside-margins t)
    
    (when (display-graphic-p)
      ;; Adjust fringe width if necessary
      ;; Define custom fringe bitmaps
      (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
      (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
      (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
      ))
  )


(provide 'module-git-gutter)
;;; module-git-gutter.el ends here
