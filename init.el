(require 'package)


(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
      ;;("melpa"        . "https://melpa.org/packages/")
      ;;("marmalade"    . "https://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))


(package-initialize)


(unless package-archive-contents
  (package-refresh-contents))


(dolist (package '(avy
                   smex
                   cider
                   magit
                   company
                   flx-ido
                   paredit
                   flycheck
                   clojure-mode
                   expand-region
                   zenburn-theme
                   ido-vertical-mode
                   flycheck-clj-kondo
                   idle-highlight-mode
                   ido-completing-read+))
  (unless (package-installed-p package)
    (package-install package)))


(unless (file-exists-p "~/.emacs.d/generated")
  (make-directory "~/.emacs.d/generated"))


(setq backup-directory-alist       nil
      auto-save-list-file-prefix   nil
      custom-file                  "~/.emacs.d/generated/custom.el"
      smex-save-file               "~/.emacs.d/generated/smex-items"
      save-place-file              "~/.emacs.d/generated/places"
      recentf-save-file            "~/.emacs.d/generated/recentf"
      ido-save-directory-list-file "~/.emacs.d/generated/ido.last")


(add-to-list 'load-path "~/.emacs.d/generated")

(when (eq system-type 'windows-nt)
  (setq default-directory "C:/")
  (add-to-list 'exec-path "C:/Program Files/7-Zip"))


;;        _
;;  _   _(_)
;; | | | | |
;; | |_| | |
;;  \__,_|_|


(load-theme 'zenburn t)


(menu-bar-mode          -1)
(tool-bar-mode          -1)
(scroll-bar-mode        -1)
(blink-cursor-mode       0)
(global-visual-line-mode 1)


(setq ring-bell-function              'ignore
      scroll-conservatively           101
      inhibit-startup-message         t
      initial-scratch-message         ""
      mouse-wheel-scroll-amount       '(1)
      mouse-wheel-progressive-speed   nil
      eldoc-echo-area-use-multiline-p nil)


(set-face-attribute
 'default nil
 :height  110
 :family  "Monaco")


(set-frame-parameter nil 'fullscreen 'fullboth)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\(FIXME\\|TODO\\)" 1
       font-lock-warning-face t)))))


;;           _ _ _   _
;;   ___  __| (_) |_(_)_ __   __ _
;;  / _ \/ _` | | __| | '_ \ / _` |
;; |  __/ (_| | | |_| | | | | (_| |
;;  \___|\__,_|_|\__|_|_| |_|\__, |
;;                           |___/


(set-language-environment "UTF-8")


(show-paren-mode 1)
(save-place-mode 1)


(setq-default indent-tabs-mode nil)


(setq create-lockfiles                    nil
      auto-save-default                   nil
      make-backup-files                   nil
      mouse-yank-at-point                 t
      org-log-into-drawer                 'LOGBOOK
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
(global-set-key (kbd "C-=") #'er/expand-region)


(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly))


;;                     _             _   _
;;  _ __   __ ___   __(_) __ _  __ _| |_(_) ___  _ __
;; | '_ \ / _` \ \ / /| |/ _` |/ _` | __| |/ _ \| '_ \
;; | | | | (_| |\ V / | | (_| | (_| | |_| | (_) | | | |
;; |_| |_|\__,_| \_/  |_|\__, |\__,_|\__|_|\___/|_| |_|
;;                       |___/


(require 'flx-ido)
(require 'recentf)
(require 'uniquify)
(require 'ido-vertical-mode)
(require 'ido-completing-read+)


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
(global-set-key (kbd "C-'")     #'avy-goto-char-timer)


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a") #'org-agenda))


(fset 'yes-or-no-p 'y-or-n-p)


;;  _ _
;; | (_)___ _ __
;; | | / __| '_ \
;; | | \__ \ |_) |
;; |_|_|___/ .__/
;;         |_|


(add-hook 'cider-mode-hook      #'eldoc-mode)
(add-hook 'prog-mode-hook       #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'prog-mode-hook       #'subword-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook    #'flycheck-mode)
(add-hook 'prog-mode-hook       #'idle-highlight-mode)
(add-hook 'cider-repl-mode-hook #'idle-highlight-mode)


(setq cider-use-overlays                    t
      cider-repl-wrap-history               t
      cider-save-file-on-load               t
      cider-prompt-for-symbol               nil
      cider-font-lock-dynamically           '(macro core function var)
      cider-auto-select-error-buffer        nil
      cider-repl-display-help-banner        nil
      cider-repl-use-pretty-printing        t
      cider-repl-pop-to-buffer-on-connect   'display-only
      cider-auto-select-test-report-buffer  nil
      cider-repl-history-display-duplicates nil)


(require 'clojure-mode)
(require 'flycheck-clj-kondo)


(define-clojure-indent
  (defroutes  'defun)
  (GET        2)
  (POST       2)
  (PUT        2)
  (DELETE     2)
  (HEAD       2)
  (ANY        2)
  (OPTIONS    2)
  (PATCH      2)
  (rfn        2)
  (let-routes 1)
  (context    2))
