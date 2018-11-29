(dolist (hook '(clojure-mode-hook cider-repl-mode-hook))
  (add-hook hook #'paredit-mode)
  (add-hook hook #'subword-mode)
  (add-hook hook #'rainbow-delimiters-mode))

(add-hook 'cider-mode-hook #'eldoc-mode)

(setq cider-repl-wrap-history               t
      cider-save-file-on-load               t
      cider-show-error-buffer               nil
      cider-prompt-for-symbol               nil
      cider-font-lock-dynamically           '(macro core function var)
      cider-auto-select-error-buffer        t
      cider-repl-use-pretty-printing        t
      cider-repl-pop-to-buffer-on-connect   'display-only
      cider-repl-history-display-duplicates nil)