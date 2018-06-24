(require 'cider-mode)
(require 'clj-refactor)
(require 'clojure-mode-extra-font-locking)

(add-hook 'clojure-mode-hook
          (lambda ()
            (yas-minor-mode    1)
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "C-c M-r")))

(dolist (hook '(clojure-mode-hook cider-repl-mode-hook))
  (add-hook hook #'paredit-mode)
  (add-hook hook #'subword-mode)
  (add-hook hook #'aggressive-indent-mode)
  (add-hook hook #'rainbow-delimiters-mode))

(setq cider-repl-wrap-history               t
      cider-save-file-on-load               t
      cider-show-error-buffer               nil
      cider-font-lock-dynamically           '(macro core function var)
      cider-auto-select-error-buffer        t
      cider-repl-use-pretty-printing        t
      cider-repl-pop-to-buffer-on-connect   'display-only
      cider-repl-history-display-duplicates nil)

(define-key cider-mode-map      (kbd "C-c C-v C-e") #'cider-visit-error-buffer)
(define-key cider-repl-mode-map (kbd "RET")         #'cider-repl-newline-and-indent)
(define-key cider-repl-mode-map (kbd "C-<return>")  #'cider-repl-return)