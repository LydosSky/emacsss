;;; module-diff-hl.el --- diff-hl configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up diff-hl for version control diff highlighting,
;; customized to use slimmer pipes instead of thick blocks, and ensures
;; compatibility with TTY Emacs by using the margin.

;;; Code:

;; Ensure use-package is available
(require 'use-package)

(use-package diff-hl
  :ensure t
  :defer t
  :hook
  ((prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Customize diff-hl indicators
  (setq diff-hl-margin-symbols-alist
        '((insert . "│")  ;; Slimmer pipes for inserts
          (delete . "│")  ;; Slimmer pipes for deletes
          (change . "│"))) ;; Slimmer pipes for changes

  ;; Use margin instead of fringe in TTY
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)
    (setq diff-hl-margin-side 'right))

  ;; Update fringe bitmaps to use slimmer pipes in GUI
  (when (display-graphic-p)
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'diff-hl-bmp-insert [0 0 0 0 0 0 24] nil nil 'top)
      (define-fringe-bitmap 'diff-hl-bmp-delete [0 0 0 0 0 0 24] nil nil 'top)
      (define-fringe-bitmap 'diff-hl-bmp-change [0 0 0 0 0 0 24] nil nil 'top))
    (setq diff-hl-fringe-bmp-function
          (lambda (type _pos)
            (pcase type
              (`insert 'diff-hl-bmp-insert)
              (`delete 'diff-hl-bmp-delete)
              (`change 'diff-hl-bmp-change)))))
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode)
  )


(provide 'module-diff-hl)
;;; module-diff-hl.el ends here
