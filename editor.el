
;; ============== Editor connfiguration ===============
(cd "g:/projects/")

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")
(setq find-program ( getenv  "find-program" ))
(setq markdown-program ( getenv  "markdown-program" ))

(setq inhibit-startup-message t)
(set-language-environment "UTF-8")

(set-default-coding-systems 'utf-8)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(electric-pair-mode t)
(auto-fill-mode 1)
(abbrev-mode 1)
(subword-mode 1)
(electric-layout-mode t)
(show-paren-mode 1)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

(setq compile-command "")
(global-set-key (kbd "<C-tab>") 'up-list)
(global-set-key (kbd "<backtab>") 'backward-up-list)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(tab-bar-mode t)
(setq confirm-kill-emacs 'yes-or-no-p)
;; maximize sccreen and windowSet frame transparency and maximize windows by default.
(add-to-list 'default-frame-alist '(alpha . (85 . 90)))
(set-frame-parameter (selected-frame) 'alpha '(85 . 90))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; minimize/maximize code folds
(add-hook 'prog-mode-hook #'(lambda () (hs-minor-mode t)))
;; Set up the visible bell
(setq visible-bell t)
(setq sgml-quick-keys 'close)
(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

(column-number-mode)
;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)
;;Basic Customization
(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)
(display-time-mode t)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-u") 'universal-argument)
(setq read-quoted-char-radix 10)
(setq initial-scratch-message nil)
;; ============ Editor ======================
