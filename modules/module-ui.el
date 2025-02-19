;;; module-ui.el --- Doom Themes configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up doom-themes, providing a variety of aesthetically pleasing color themes.

;;; Code:

(use-package doom-themes
  :ensure t
  :defer t
  :init
  ;; Load the theme at startup
  (load-theme 'doom-one-light t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; Optional: Enable custom neotree theme (requires all-the-icons)
  ;; (doom-themes-neotree-config)
  ;; Optional: Corrects (and improves) treemacs colors
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  )

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :init
  ;; Set the height of the modeline
  (setq doom-modeline-height 15)
  ;; Display buffer encoding
  (setq doom-modeline-buffer-encoding t)
  ;; Show time in modeline (optional)
  ;; (setq display-time-default-load-average nil)
  ;; (display-time-mode 1)
  ;; Enable icons (requires all-the-icons)
  (setq doom-modeline-icon nil)
  ;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)
  ;; Whether display the colorful icon for major mode.
  (setq doom-modeline-major-mode-color-icon t)
  )


(provide 'module-ui)
;;; module-ui.el ends here
