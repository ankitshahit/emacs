;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 100)

;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))


(setq dw/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))
(setq dw/is-guix-system (and (eq system-type 'gnu/linux)

                             (require 'f)
                             (string-equal (f-read "/etc/issue")
                                           "\nThis is the GNU system.  Welcome.\n")))
;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)
;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Recipe is always a list
;; Install via Guix if length == 1 or :guix t is present

(defvar dw/guix-emacs-packages '()
  "Contains a list of all Emacs package names that must be
installed via Guix.")

;; Examples:
;; - (org-roam :straight t)
;; - (git-gutter :straight git-gutter-fringe)

(defun dw/filter-straight-recipe (recipe)
  (let* ((plist (cdr recipe))
         (name (plist-get plist :straight)))
    (cons (if (and name (not (equal name t)))
              name
            (car recipe))
          (plist-put plist :straight nil))))

(setup-define :pkg
  (lambda (&rest recipe)
    (if (and dw/is-guix-system
             (or (eq (length recipe) 1)
                 (plist-get (cdr recipe) :guix)))
        `(add-to-list 'dw/guix-emacs-packages
                      ,(or (plist-get recipe :guix)
                           (concat "emacs-" (symbol-name (car recipe)))))
      `(straight-use-package ',(dw/filter-straight-recipe recipe))))
  :documentation "Install RECIPE via Guix or straight.el"
  :shorthand #'cadr)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(setup (:pkg no-littering)
  (require 'no-littering))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (delete-word (- arg))))

(setup (:pkg vertico)
  ;; :straight '(vertico :host github
  ;;                     :repo "minad/vertico"
  ;;                     :branch "main")
  (vertico-mode)
  (:with-map vertico-map
    (:bind "C-j" vertico-next
           "C-k" vertico-previous
           "C-f" vertico-exit))
  (:with-map minibuffer-local-map
    (:bind "M-h" dw/minibuffer-backward-kill))
  (:option vertico-cycle t)
  (custom-set-faces '(vertico-current ((t (:background "#3a3f5a"))))))

(setup (:pkg ws-butler)
  (:hook-into text-mode prog-mode))

(setup (:pkg evil-nerd-commenter)
  (:global "M-/" evilnc-comment-or-uncomment-lines))

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)


;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(cd "g:/projects/")
;; Loading tree-sitter package
(require 'tree-sitter-langs)
(require 'tree-sitter)

;; Activate tree-sitter globally (minor mode registered on every buffer)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'js-mode-hook #'tree-sitter-mode)



(setq inhibit-startup-message t)
(set-language-environment "UTF-8")

(set-default-coding-systems 'utf-8)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(electric-pair-mode t)
(show-paren-mode 1)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
;; maximize sccreen and windowSet frame transparency and maximize windows by default.
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
 (set-frame-parameter (selected-frame) 'alpha '(93 . 95))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; minimize/maximize code folds
(add-hook 'prog-mode-hook #'(lambda () (hs-minor-mode t)))
;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-u") 'universal-argument)
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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

(setup (:require paren)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(require `org)
;; (setq org-clock-sound "C:\Users\ankit\Downloads\despair-metal-trailer-109943.mp3")

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))
(setq inhibit-compacting-font-caches t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(setq doom-modeline-major-mode-icon t)
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))
;; ================= LSP MODE ===================
;;
;;
;;
(use-package js
  :ensure nil
  ;; :mode ("\\.jsx?\\'" . js-jsx-mode)
  :config
  (setq js-indent-level 2)
  (add-hook 'flycheck-mode-hook
            #'(lambda ()
                (let* ((root (locate-dominating-file
                              (or (buffer-file-name) default-directory)
                              "node_modules"))
                       (eslint
                        (and root
                             (expand-file-name "node_modules/.bin/eslint"
                                               root))))
                  (when (and eslint (file-executable-p eslint))
                    (setq-local flycheck-javascript-eslint-executable eslint))))))
(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))
(use-package lsp-mode
  :hook ((
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; mspyls
          web-mode
          ) . lsp)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-diagnostic-package :none)             ; disable flycheck-lsp for most modes
  (add-hook 'web-mode-hook #'lsp-flycheck-enable) ; enable flycheck-lsp for web-mode locally
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-javascript-display-enum-member-value-hints t)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-javascript-format-insert-space-after-constructor t)

  (setq lsp-javascript-suggest-complete-function-calls t)
  (setq lsp-javascript-format-insert-space-after-opening-and-before-closing-empty-braces t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-enable-folding t)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-completion-at-point t)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0)
  (setq lsp-prefer-capf t) ; prefer lsp's company-capf over company-lsp
  (define-key evil-normal-state-map (kbd "g d") 'lsp-goto-implementation)
  (define-key evil-normal-state-map (kbd "g t") 'lsp-goto-type-definition))
(advice-add 'json-parse-buffer :around
              (lambda (orig &rest rest)
                (save-excursion
                  (while (re-search-forward "\\\\u0000" nil t)
                    (replace-match "")))
                (apply orig rest)))
  ;; (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))

(use-package company
  :hook (prog-mode . company-mode)
  :bind ("TAB" . company-complete)
  :config
 (setq company-require-match nil
        company-show-numbers t
        company-selection-wrap-around t)

  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2
 (global-company-mode 1)
(use-package company-restclient
  :defer t
  :after company)

(use-package company-web
  :defer t
  :after (web-mode company))
(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.1))
(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("components\/.*\.js\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\/.*\.js\'" . rjsx-mode)))

(use-package typescript-mode :ensure t)

(add-hook 'js-mode-hook #'lsp)

(with-eval-after-load 'js
  (setq js-indent-level 2)
  (define-key js-mode-map (kbd "M-.") nil))






;; ================== End LSP MODE ===============
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-minibuffer t)

  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
;; Set Emacs state modes
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))
    (defun dw/dont-arrow-me-bro ()
      (interactive)
      (message "Arrow keys are bad, you know?"))
  ;; Disable arrow keys in normal and visual modes
    (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq use-dialog-box nil)
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(setup (:require paren)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setup (:pkg corfu :host github :repo "minad/corfu")
  (:with-map corfu-map
    (:bind "C-j" corfu-next
           "C-k" corfu-previous
           "TAB" corfu-insert
           "C-f" corfu-insert))
  (:option corfu-cycle t)
  (global-corfu-mode))

(setup (:pkg orderless)
  (require 'orderless)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(setup (:pkg consult)
  (require 'consult)
  (:global "C-s" consult-line
           "C-M-l" consult-imenu
           "C-M-j" persp-switch-to-buffer*)

  (:with-map minibuffer-local-map
    (:bind "C-r" consult-history))

  (defun dw/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))

  (:option consult-project-root-function #'dw/get-project-root
           completion-in-region-function #'consult-completion-in-region))

(setup (:pkg consult-dir :straight t)
  (:global "C-x C-d" consult-dir)
  (:with-map vertico-map
    (:bind "C-x C-d" consult-dir
           "C-x C-j" consult-dir-jump-file))
  (:option consult-dir-project-list-function nil))

;; ;; ;; Thanks Karthik!
(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell."
  (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                          (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                 :narrow ?e
                                                 :category file
                                                 :face consult-file
                                                 :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
        (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))


(setup (:pkg marginalia)
  (:option marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil))
  (marginalia-mode))

(setup (:pkg embark)
 (:also-load embark-consult)
 (:global "C-S-a" embark-act)
 (:with-map minibuffer-local-map
  (:bind "C-d" embark-act))

 ;; Show Embark actions via which-key
 (setq embark-action-indicator
       (lambda (map)
         (which-key--show-keymap "Embark" map nil nil 'no-paging)
         #'which-key--hide-popup-ignore-command)
       embark-become-indicator embark-action-indicator))

(setup winner
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

(setup (:pkg visual-fill-column)
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (:hook-into org-mode))

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(defun dw/popper-window-height (window)
  (let (buffer-mode (with-current-buffer (window-buffer window)
                      major-mode))
    (pcase buffer-mode
      ('exwm-mode 40)
      (_ 15))))

(setup (:pkg popper
             :host github
             :repo "karthink/popper"
             :build (:not autoloads))
  (:global "C-M-'" popper-toggle-latest
           "M-'" popper-cycle
           "C-M-\"" popper-toggle-type)
  (:option popper-window-height 12
           ;; (popper-window-height
           ;; (lambda (window)
           ;;   (let ((buffer-mode (with-current-buffer (window-buffer window)
           ;;                        major-mode)))
           ;;     (message "BUFFER MODE: %s" buffer-mode)
           ;;     (pcase buffer-mode
           ;;       ('exwm-mode 40)
           ;;       ('helpful-mode 20)
           ;;       ('eshell-mode (progn (message "eshell!") 10))
           ;;       (_ 15)))))
           popper-reference-buffers '("^\\*eshell\\*"
                                      "^vterm"
                                      help-mode
                                      helpful-mode
                                      compilation-mode))
  (require 'popper) ;; Needed because I disabled autoloads
  (popper-mode 1))


(setup (:pkg all-the-icons-dired))
(setup (:pkg dired-single))
(setup (:pkg dired-ranger))
(setup (:pkg dired-collapse))

;; (setup dired
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (unless (or dw/is-termux
                          (s-equals? "/gnu/store/" (expand-file-name default-directory)))
                (all-the-icons-dired-mode 1))
              (hl-line-mode 1)))

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste)

;;(setup (:pkg dired-rainbow))
;;   (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
;;   (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
;;   (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
;;   (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
;;   (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
;;   (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
;;   (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
;;   (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
;;   (dired-rainbow-define log "#c17d11" ("log"))
;;   (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
;;   (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
;;   (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
;;   (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;;   (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;;   (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
;;   (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
;;   (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
;;   (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
;;   (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
;;   (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")

(defun dw/dired-link (path)
  (lexical-let ((target path))
    (lambda () (interactive) (message "Path: %s" target) (dired target))))

;; (dw/leader-key-def
;;   "d"   '(:ignore t :which-key "dired")
;;   "dd"  '(dired :which-key "Here")
;;   "dh"  `(,(dw/dired-link "~") :which-key "Home")
;;   "dn"  `(,(dw/dired-link "~/Notes") :which-key "Notes")
;;   "do"  `(,(dw/dired-link "~/Downloads") :which-key "Downloads")
;;   "dp"  `(,(dw/dired-link "~/Pictures") :which-key "Pictures")
;;   "dv"  `(,(dw/dired-link "~/Videos") :which-key "Videos")
;;   "d."  `(,(dw/dired-link "~/.dotfiles") :which-key "dotfiles")
;;   "de"  `(,(dw/dired-link "~/.emacs.d") :which-key ".emacs.d"))

(setup (:pkg magit)
  (:also-load magit-todos)
  (:global "C-M-;" magit-status)
  (:option magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setup (:pkg magit-todos))

(setup (:pkg git-link)
  (setq git-link-open-in-browser t)
)

(setup (:pkg yaml-mode)
  (:file-match "\\.ya?ml\\'"))

(setup (:pkg web-mode)
  (:file-match "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'")
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(setup (:pkg impatient-mode :straight t))
(setup (:pkg skewer-mode :straight t))

(setup (:pkg markdown-mode)
  (setq markdown-command "marked")
  (:file-match "\\.md\\'")
  (:when-loaded
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))))


(setup (:pkg rainbow-delimiters)
  (:hook-into prog-mode))

(setup (:pkg rainbow-mode)
  (:hook-into org-mode
              emacs-lisp-mode
              web-mode
              typescript-mode
              js2-mode))

(setup (:pkg ledger-mode)
  (:file-match "\\.lgr\\'")
  (:bind "TAB" completion-at-point)
  (:option
   ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                    ("bal this quarter" "%(binary) -f %(ledger-file) --period \"this quarter\" bal")
                    ("bal last quarter" "%(binary) -f %(ledger-file) --period \"last quarter\" bal")
                    ("reg" "%(binary) -f %(ledger-file) reg")
                    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                    ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(setup (:pkg hledger-mode :straight t)
  (:bind "TAB" completion-at-point))

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun dw/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun dw/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun dw/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'dw/map-line-to-status-char status-lines)))))

(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
      (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun dw/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (dw/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " ॐ " 'face `(:foreground "white"))
     (propertize (dw/get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "white"))
        (propertize package-version 'face `(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))

  (add-hook 'eshell-banner-load-hook
            (lambda ()
               (setq eshell-banner-message
                     (concat "\n" (propertize " " 'display (create-image "~/.dotfiles/.emacs.d/images/flux_banner.png" 'png nil :scale 0.2 :align-to "center")) "\n\n"))))

(defun dw/eshell-configure ()
  ;; Make sure magit is loaded
  (require 'magit)

  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

  (setup (:pkg xterm-color))

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'dw/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))

(setup eshell
  (add-hook 'eshell-first-time-mode-hook #'dw/eshell-configure)
  (setq eshell-directory-name "~/.dotfiles/.emacs.d/eshell/"
        eshell-aliases-file (expand-file-name "~/.dotfiles/.emacs.d/eshell/alias")))

;; (setup (:pkg eshell-z)
;;   (:disabled) ;; Using consult-dir for this now
;;   (add-hook 'eshell-mode-hook (lambda () (require 'eshell-z)))
;;   (add-hook 'eshell-z-change-dir-hook (lambda () (eshell/pushd (eshell/pwd);;))))

(setup (:pkg exec-path-from-shell)
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim" "rush")))

(setup (:pkg eshell-syntax-highlighting)
    (eshell-syntax-highlighting-global-mode +1))

(defun dw/esh-autosuggest-setup ()
  (require 'company)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

(setup (:pkg esh-autosuggest)
  (require 'esh-autosuggest)
  (setq esh-autosuggest-delay 0.5)
  (:hook dw/esh-autosuggest-setup)
  (:hook-into eshell-mode))


;; Binding will be set by desktop config
;; (setup (:pkg app-launcher))

;; (setup (:pkg avy)
;;   (dw/leader-key-def
;;     "j"   '(:ignore t :which-key "jump")
;;     "jj"  '(avy-goto-char :which-key "jump to char")
;;     "jw"  '(avy-goto-word-0 :which-key "jump to word")
;;     "jl"  '(avy-goto-line :which-key "jump to line")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hydra evil-collection evil which-key use-package rainbow-delimiters ivy-rich helpful general doom-themes doom-modeline counsel command-log-mode all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
