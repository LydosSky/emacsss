;;; post-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-


;;; Commentary:
;;; Code:
;; Start the timer at the very beginning of init.el

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))
(add-hook 'kill-emacs-hook #'recentf-cleanup)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

(add-to-list 'load-path (expand-file-name "modules" minimal-emacs-user-directory))
(add-to-list 'load-path minimal-emacs-user-directory)

(require 'modules-config)

(defun load-modules (enabled-modules directory)
  "Load all modules in ENABLED-MODULES list from DIRECTORY."
  (when (file-directory-p directory)
    (dolist (module-file (directory-files directory t "\\`module-.*\\.el\\'"))
      (let* ((module-filename (file-name-sans-extension (file-name-nondirectory module-file)))  ; e.g., "module-company"
             (module-name (intern (string-remove-prefix "module-" module-filename))))  ; e.g., 'company
        (when (member module-name enabled-modules)
          (message "Loading module: %s" module-filename)
          (require (intern module-filename)))))))


(load-modules list-modules (expand-file-name "modules" minimal-emacs-user-directory))

(provide 'post-init)
;;; post-init.el ends here
