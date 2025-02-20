;;; module-env.el --- Environment configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This module ensures that Emacs inherits environment variables
;; from the user's shell, such as PATH, MANPATH, etc.

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :defer nil
  :if (memq window-system '(mac ns x))
  :init
  ;; List of environment variables to copy from the shell
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SSH_AUTH_SOCK" "GOPATH" "PYTHONPATH"))
  ;; Initialize exec-path-from-shell
  (exec-path-from-shell-initialize)
  ;; Optional: Silence warnings about remote hosts
  (setq exec-path-from-shell-arguments '("-l"))
  )

(provide 'module-env)
;;; module-env.el ends here
