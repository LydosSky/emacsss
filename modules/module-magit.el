
;;; module-magit.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;;; Code:


(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-blame magit-log)
  :bind ("C-x g" . magit-status)
  :init
  ;; Settings before Magit loads
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  )

(provide 'module-magit)
;;; module-magit.el ends here
