;;; module-ui.el --- Doom Themes configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up doom-themes, providing a variety of aesthetically pleasing color themes.

;;; Code:
;; (use-package doom-themes
;;   :ensure t
;;   :defer t
;;   :init
;;   (load-theme 'doom-one-light t)
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   )

(use-package spaceline
  :ensure t
  :defer t
  :init
  (setq powerline-default-separator nil)
  (setq spaceline-minor-modes-p nil)
  (setq spaceline-buffer-encoding-p nil)
  (setq spaceline-hud-p nil)
  (setq spaceline-buffer-size-p nil)
  (setq spaceline-buffer-position-p nil)
  )

(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :height 130
                    :weight 'regular)


(use-package inhibit-mouse
  :ensure t
  :defer t
  :config
  (inhibit-mouse-mode))

;; (use-package base16-theme
;;   :defer t
;;   :init
;;   (setq base16-theme-256-color-source 'colors)
;;   (load-theme 'base16-one-light t)
;;   )

(use-package sublime-themes
  :defer t
  :init
  (load-theme 'spolsky t)
  )

(defun spaceline-init-hook()
  "Hook to enable modeline"
  (setq left-fringe-width 4)
  (setq right-fringe-width 4)
  (spaceline-spacemacs-theme))

(add-hook 'after-init-hook 'spaceline-init-hook)
(setq default-frame-alist '((undecorated . t)
                            (drag-internal-border . 1)))
(provide 'module-ui)
;;; module-ui.el ends here

