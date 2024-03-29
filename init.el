;;; -*- lexical-binding: t; -*-
;;;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 100)
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(setq garbage-collection-messages t)
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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
;;(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(load-file "~/.emacs.d/setup.el")
(require 'setup)
;; Profile emacs startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org-contrib" . "https://elpa.nongnu.org/nongnu/")
                         ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; Recipe is always a list
;; Install via Guix if length == 1 or :guix t is present

(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
(setq auto-save-file-name-transforms
`((".*" "~/.emacs.d/backup"  t)))
(setq delete-by-moving-to-trash t)
;; Initialize package sources
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
(use-package gcmh
  :config
  (gcmh-mode 1)
  )

(use-package perfect-margin)
(setq perfect-margin-visible-width 128)
(perfect-margin-mode 1)
(load-file "~/.emacs.d/editor.el")
(load-file "~/.emacs.d/evil.el")
(use-package dashboard :ensure t
  :config (dashboard-open)
)
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

(setup (:require paren)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

;; (require `org)
;; (setq org-clock-sound "C:\Users\ankit\Downloads\despair-metal-trailer-109943.mp3")

;; (use-package command-log-mode)
;; ============ Styling ================
(use-package golden-ratio  :commands (golden-ratio))
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
;; (use-package telephone-line :ensure t :config (telephone-line-mode 1))
(use-package doom-modeline
  :ensure t  :hook (after-init . doom-modeline-mode)

  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-project-detection 'auto)
(setq doom-modeline-project-detection 'ffip)
(setq inhibit-compacting-font-caches t)
(use-package doom-themes
  :init (load-theme 'doom-dracula t))
(setup (:require paren)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

;; ======== Styling complete ==================
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

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
    :keymaps '(normal )
    :prefix "SPC"
    )
  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tgr"  '(golden-ratio-mode  :which-key "toggles golden ratio mode")
    "es" '(eshell :which-key "eshell")
    "gf" '(grep-find :which-key "search for text across files")
    "acf" '(company-capf :which-key "autocomplete company capf")
    "kb" '(only-current-buffer :which-key "kill all buffers except current one")
    "ji" '(org-jira-create-issue :which-key "create jira issue")
    "mm" '(magit :which-key "magit")
    "mc" '(magit-clone :which-key "magit clone")
    "se" '(setenv :which-key "set env")
    "hl" '(tree-sitter-hl-mode :which-key "tree sitter highlight mode")
    "cc" '(compile :which-key "Compile")
    "cr" '(recompile :which-key "recompile")
    "cb" '(eval-buffer :which-key "Eval buffer")
    "aa" '(org-agenda-list :which-key "Agenda list")
    "gl" '(git-link :which-key "git link")
    "gh" '(git-link-homepage :which-key "git homepage")
    "gr" '(golden-ratio :which-key "golden ratio for windows")
    "tff" '(toggle-frame-fullscreen :which-key "toggle frame fullscreen")
    "fd" '(find-name-dired :which-key "find name dired")
    "l" '(:keymap lsp-command-map :package lsp-mode :which-key "lsp mode")
    ))


;; ================= LSP MODE ===================
;;
;;
(load-file "~/.emacs.d/dap.el")
(use-package js
  :ensure nil
  :mode ("\\.js?\\'" . js-mode)
  :config
  (require 'dap-node)
  (dap-node-setup)
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
(require `company)
;; (setq lsp-keymap-prefix "SPC l")

(use-package lsp-mode
  :init (add-to-list 'company-backends 'company-capf)
  :hook ((
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          python-mode     ; mspyls
          web-mode
          js2-mode
          ) . lsp)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-diagnostic-package :flycheck)             ; disable flycheck-lsp for most modes
  (setq warning-suppress-types '((lsp-mode)))
  (setq lsp-enable-symbol-highlighting t)
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
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
  (setq read-process-output-max (* 1024 2048)) ;; 1mb
  (setq lsp-idle-delay 0)
  (setq lsp-prefer-capf t) ; prefer lsp's company-capf over company-lsp
  (setq lsp-language-id-configuration '((java-mode . "java")
                                        (python-mode . "python")
                                        (gfm-view-mode . "markdown")
                                        (rust-mode . "rust")
                                        (css-mode . "css")
                                        (xml-mode . "xml")
                                        (c-mode . "c")
                                        (c++-mode . "cpp")
                                        (objc-mode . "objective-c")
                                        (web-mode . "html")
                                        (html-mode . "html")
                                        (sgml-mode . "html")
                                        (mhtml-mode . "html")
                                        (go-mode . "go")
                                        (haskell-mode . "haskell")
                                        (php-mode . "php")
                                        (json-mode . "json")
                                        (javascript . "javascript")
                                        (typescript-mode . "typescript")))
  )
(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t))

(use-package terraform-mode
  :config (add-to-list 'company-backends 'company-terraform)
  :mode (
         ( "\\.tf\\'" . terraform-mode )
         ( "\\.tfvars\\'" . terraform-mode )
         )
  :ensure t
  )

(use-package tree-sitter
  :after lsp-mode
  :ensure t
  :config
  (tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs
  :after tree-sitter
  :ensure t
  :commands (tree-sitter-hl-mode)
  )

(defun my-lsp-hook ()
  "Activate tree-sitter-hl-mode when lsp-mode is activated."
  (when lsp-mode (tree-sitter-hl-mode 1)))
(add-hook 'lsp-mode-hook #'my-lsp-hook)

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
(use-package company-terraform :after company :ensure t :commands (company-mode))
(use-package company-restclient
  :defer t
  :after company)

(use-package company-web
  :defer t
  :after (web-mode company))

;;========================== flycheck ==================
(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.0))


(use-package flycheck-inline :ensure t :after flycheck-mode )

;; (setq flycheck-inline-display-function (lambda (msg pos errors)
;;  (flycheck-inline-display-phantom msg))
;;  flycheck-inline-clear-function #'flycheck-inline-clear-phantoms)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
;;========================== end flycheck ==================
(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("components\/.*\.js\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("components\/.*\.jsx\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\/.*\.js\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\/.*\.jsx\'" . rjsx-mode))
)


(use-package js-mode
  :ensure nil
  :mode (("\\.js?\\'" . js-mode)
         ("\\.jsx?\\'" . js-mode))
  :config
  (require 'dap-node)
  (dap-node-setup)
  (setq javascript-indent-level 2)
  (setq js-indent-level 2)
)

(with-eval-after-load 'js
  (setq js-indent-level 2)
  (define-key js-mode-map (kbd "M-.") nil))
(add-hook 'js-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(advice-add 'json-parse-buffer :around
            (lambda (orig &rest rest)
              (save-excursion
                (while (re-search-forward "\\\\u0000" nil t)
                  (replace-match "")))
              (apply orig rest)))


(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js-mode-hook #'prettier-js-mode)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  )

;; ================== End LSP MODE ===============

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
                                        ; (message "Buffer is set to read-only because it is large.  Undo also disabled.")
    ))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
(add-hook 'so-long-hook 'my-so-long-hook)
(defun my-so-long-hook ()
  "Used in `so-long-hook'."
  ;; Disable the old `idle-highlight' (pre-`idle-highlight-mode')
  (when (bound-and-true-p idle-highlight-timer)
    (cancel-timer idle-highlight-timer)
    (setq idle-highlight-timer nil)))
(global-so-long-mode 1)

(load-file "~/.emacs.d/org.el")
(load-file "~/.emacs.d/ledger.el")

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

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

(setup winner
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

(setup (:pkg visual-fill-column)
  (setq visual-fill-column-width 95
        visual-fill-column-center-text t)
  (:hook-into org-mode)
  (:hook-into text-mode)
  (:hook-into lisp-mode)
  (:hook-into hs-minor-mode)
  (:hook-into js-mode)
  (:hook-into js2-mode)
  (:hook-into js-jsx-mode)
  (:hook-into python-mode)
  (:hook-into terraform-mode)

  )

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
           popper-reference-buffers '("^\\*eshell\\*"
                                      "^vterm"
                                      shell-mode
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

(defun dw/dired-link (path)
  (lexical-let ((target path))
               (lambda () (interactive) (message "Path: %s" target) (dired target))))


(setup (:pkg magit)
  (:also-load magit-todos)
  (:global "C-M-;" magit-status)
  (:option magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(setup (:pkg magit-todos))

(setup (:pkg git-link)
  (setq git-link-open-in-browser t)
  )
(eval-after-load 'git-link
  '(progn (add-to-list 'git-link-remote-alist '("fd-main" git-link-codecommit))))
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
              js2-mode
              js-mode
))


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
  (setq esh-autosuggest-delay 0.3)
  (:hook dw/esh-autosuggest-setup)
  (:hook-into eshell-mode))
;; ========================= csv mode  ===============
(use-package csv-mode :mode ("\\.csv?\\'" . csv-mode))

;; You should skip load-path when installed from MELPA
(use-package awscli-capf
  :commands (awscli-capf-add)
  :hook (eshell-mode . awscli-capf-add)
:hook (shell-mode . awscli-capf-add)
:hook (yaml-mode . awscli-capf-add)
)
;; ========================= firefox ===============
;; (use-package edit-server
;;   :ensure t
;;   :commands edit-server-start
;;   :init (if after-init-time
;;               (edit-server-start)
;;             (add-hook 'after-init-hook
;;                       #'(lambda() (edit-server-start))))
;;   :config (setq edit-server-new-frame-alist
;;                 '((name . "Edit with Emacs FRAME")
;;                   (top . 200)
;;                   (left . 200)
;;                   (width . 80)
;;                   (height . 25)
;;                   (minibuffer . t)
;;                   (menu-bar-lines . t)
;;                   (window-system . x))))

;; (setup :straight '(edit-server-htmlize :host github))
;;  (when (and (daemonp) (require 'edit-server nil :noerror))
;;    (edit-server-start))

;; (when (require 'edit-server nil :noerror)
;;   (setq edit-server-new-frame nil)
;;   (edit-server-start))
;;   (add-hook 'edit-server-start-hook 'markdown-mode)

;; ========================= end firefox ===============

(defun only-current-buffer ()
  (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
;;=======================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hydra evil-collection evil which-key use-package
  rainbow-delimiters ivy-rich helpful general doom-themes
  doom-modeline counsel command-log-mode all-the-icons))

 '(warning-suppress-types '(( lsp-mode )))
 '(warning-suppress-types '((lsp-mode) (lsp-mode)))
'(warning-suppress-log-types '((use-package) (lsp-mode) (lsp-mode)))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 )
