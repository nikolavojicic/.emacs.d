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

(set-face-attribute 'default nil :height 140)
(set-frame-parameter nil 'fullscreen 'fullboth)