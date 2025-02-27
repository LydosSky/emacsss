;;; pre-ealy-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;;; Code:


;; Start the timer at the very beginning of init.el
(setq minimal-emacs-var-dir (expand-file-name "packages/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)
(setq custom-file null-device)
(setq-default cursor-type '(bar . 3))
(delete-selection-mode)
(global-set-key (kbd "C-x C-b") (interactive))
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(provide 'pre-ealy-init)
;;; pre-ealy-init.el ends here
