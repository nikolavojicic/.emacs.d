(load-theme 'zenburn t)

(menu-bar-mode          -1)
(tool-bar-mode          -1)
(scroll-bar-mode        -1)
(blink-cursor-mode       0)
(global-visual-line-mode 1)

(setq ring-bell-function             'ignore
      scroll-conservatively          101
      inhibit-startup-message        t
      initial-scratch-message        ""
      mouse-wheel-scroll-amount      '(1)
      mouse-wheel-progressive-speed  nil)

(set-face-attribute
 'default nil
 :height 140
 :family "Consolas")

(set-frame-parameter nil 'fullscreen 'fullboth)

(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1
       font-lock-warning-face t)))))