;;; pre-ealy-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;;; Code:


;; Start the timer at the very beginning of init.el
(setq minimal-emacs-var-dir (expand-file-name "packages/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)
(setq custom-file null-device)
(setq-default cursor-type '(bar . 3))
(set-face-attribute 'default nil
                     :family "Roboto Mono"
                     :height 130
                     :weight 'regular)
(provide 'pre-ealy-init)
;;; pre-ealy-init.el ends here

