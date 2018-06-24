(show-paren-mode     1)
(save-place-mode     1)
(global-hl-line-mode 1)

(setq-default indent-tabs-mode nil)

(setq create-lockfiles                    nil
      auto-save-default                   nil
      make-backup-files                   nil
      mouse-yank-at-point                 t
      electric-indent-mode                nil
      select-enable-primary               t
      select-enable-clipboard             t
      save-interprogram-paste-before-kill t)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(global-set-key (kbd "M-/") #'hippie-expand)