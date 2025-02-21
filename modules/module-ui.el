;;; module-ui.el --- Doom Themes configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up doom-themes, providing a variety of aesthetically pleasing color themes.

;;; Code:
(use-package doom-themes
  :ensure t
  :defer t
  :init
  (load-theme 'doom-one-light t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  )

(use-package spaceline
  :ensure t
  :defer t
  :init
  (setq powerline-default-separator nil)
  )

(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :height 130
                    :weight 'regular)

(defun my/spaceline-setup()
  "Spaceline Custom configuration"
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-encoding-off)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-position-off))
(add-hook 'after-init-hook 'my/spaceline-setup)

(provide 'module-ui)
;;; module-ui.el ends here
