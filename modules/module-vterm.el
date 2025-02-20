;;; module-vterm.el --- vterm configuration module -*- lexical-binding: t; -*-

;;; Commentary:

;; This module sets up vterm, a fast terminal emulator inside Emacs,
;; providing a full-featured terminal experience.

;;; Code:

;; Install and configure vterm
(use-package vterm
  :ensure t
  :defer t
  :commands (vterm vterm-other-window)
  :config
  ;; Optional: Set the shell to use in vterm
  ;; (setq vterm-shell "/bin/bash")
  ;; Adjust the max scrollback buffer size
  (setq vterm-max-scrollback 10000)
  ;; Disable confirmation on exit
  (setq vterm-kill-buffer-on-exit t))

;; Custom toggle function for single vterm buffer
(defun my/vterm-toggle ()
  "Toggle a persistent vterm buffer."
  (interactive)
  (let ((vterm-buffer "*vterm*"))
    (if (get-buffer vterm-buffer)
        (if (equal (current-buffer) (get-buffer vterm-buffer))
            ;; If we're in the vterm buffer, bury it
            (progn
              (bury-buffer)
              (delete-window))
          ;; If the vterm buffer exists but is not visible, switch to it
          (switch-to-buffer vterm-buffer))
      ;; If the vterm buffer doesn't exist, create it
      (vterm vterm-buffer))))

(defun my/vterm-display (buffer &optional alist)
  "Display the vterm buffer at the bottom of the frame."
  (let ((window (or (get-buffer-window buffer)
                    (display-buffer-in-side-window buffer '((side . bottom)
                                                            (window-height . 0.3))))))
    (select-window window)
    window))

(setq display-buffer-alist
      '(("^\\*vterm\\*$" my/vterm-display)))


;; Bind the custom toggle function to a key
(global-set-key (kbd "C-c t") 'my/vterm-toggle)

(provide 'module-vterm)
;;; module-vterm.el ends here
