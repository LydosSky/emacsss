;;; pre-ealy-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;;; Code:
;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq minimal-emacs-var-dir (expand-file-name "packages/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)
(setq custom-file null-device)

(provide 'pre-ealy-init)
;;; pre-ealy-init.el ends here

