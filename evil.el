
;; ================== Evil package =========================

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-minibuffer t)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  :config
  (setq evil-want-C-i-jump nil)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal

                          )(evil-mode 1))

(use-package undo-fu)

(setq evil-want-keybinding nil)
(use-package evil-collection
  :init

  :after evil
  :config
  (evil-collection-init))
(use-package evil-matchit :ensure t :config (global-evil-matchit-mode 1))
(use-package evil-surround :ensure t :config (global-evil-surround-mode 1))


(defun dw/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))
;; Disable arrow keys in normal and visual modes
(define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
(define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
(define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
(define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
(define-key evil-insert-state-map (kbd "<backspace>") 'dw/dont-arrow-me-bro)
(evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
(evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
(evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
(evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)
(setq evil-want-fine-undo t)
(define-key evil-normal-state-map (kbd "TAB") 'tab-to-tab-stop)


(setq evil-magit-state 'normal)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil)
;; (dolist (key '("ESC"))
;;   (global-unset-key key))
(use-package evil-god-state :ensure t
  :config (setq god-mode-alist '((nil . "C-") ("o" . "M-") ("O" . "C-M-"))))
(use-package diminish :ensure t :after evil-god-state)
(evil-define-key 'normal global-map "gl"  'evil-execute-in-god-state )
(add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
(add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
(evil-define-key 'god global-map [escape] 'evil-god-state-bail)

;;=========== End of evil mode ================
