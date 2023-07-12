;;; -*- lexical-binding: t; -*-
;; ==================legder MODE ===============

(setup (:pkg ledger-mode)
  (:file-match "\\.lgr\\'")
  (:file-match "\\.dat\\'")
  (:file-match "\\.ledger\\'")

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
(delete 'company-dabbrev company-backends)
(use-package flycheck-ledger :after ledger-mode)
(eval-after-load 'flycheck '(require 'flycheck-ledger))
(global-flycheck-mode t)

;; ================== End ledger MODE ===============
