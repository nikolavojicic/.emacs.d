(require 'flx-ido)
(require 'recentf)
(require 'uniquify)
(require 'ido-vertical-mode)

(smex-initialize)
(global-company-mode)

(ido-mode            1)
(recentf-mode        1)
(flx-ido-mode        1)
(ido-everywhere      1)
(ido-vertical-mode   1)
(ido-ubiquitous-mode 1)

(setq apropos-do-all             t
      ido-use-faces              nil
      recentf-max-menu-items     40
      ido-use-virtual-buffers    t
      ido-enable-flex-matching   t
      ido-vertical-define-keys   'C-n-and-C-p-only
      ido-use-filename-at-point  nil
      uniquify-buffer-name-style 'forward)

(global-set-key (kbd "M-x")     #'smex)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(fset 'yes-or-no-p 'y-or-n-p)