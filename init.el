(require 'package)


(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
      ;;("melpa"        . "https://melpa.org/packages/")
      ;;("marmalade"    . "https://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))


(package-initialize)


(unless package-archive-contents
  (package-refresh-contents))


(dolist (package '(smex
                   cider
                   magit
                   company
                   ob-http
                   flx-ido
                   paredit
                   flycheck
                   clojure-mode
                   expand-region
                   plantuml-mode
                   zenburn-theme
                   ido-vertical-mode
                   flycheck-clj-kondo
                   ido-completing-read+))
  (unless (package-installed-p package)
    (package-install package)))


(setq backup-directory-alist     nil
      auto-save-list-file-prefix nil
      custom-file                "~/.emacs.d/custom.el")


(when (eq system-type 'windows-nt)
  (setq default-directory "C:/")
  (add-to-list 'exec-path "C:/Program Files/7-Zip"))


;; ui ======
;;        _
;;  _   _(_)
;; | | | | |
;; | |_| | |
;;  \__,_|_|
;; =========


(defun my/default-theme-overrides ()
  (set-cursor-color "red")
  (set-background-color "lightgray")
  (set-face-background 'fringe "lightgray")
  (set-face-foreground 'vertical-border "darkgray")
  (set-face-foreground 'line-number-current-line "red")
  (with-eval-after-load 'org
    (set-face-background 'org-block "gray90")
    (set-face-attribute 'org-meta-line nil
                        :background "gray75"
                        :foreground "black"
                        :slant 'italic))
  (with-eval-after-load 'magit
    (set-face-background 'magit-section-highlight "gray90")
    (set-face-background 'magit-diff-context-highlight "gray90")))


(global-set-key
 [f12]
 (lambda ()
   (interactive)
   (if (eq (car custom-enabled-themes) 'zenburn)
       (progn (disable-theme 'zenburn)
              (my/default-theme-overrides))
     (progn (load-theme 'zenburn t)
            (with-eval-after-load 'org
              (set-face-attribute 'org-meta-line nil
                                  :background nil
                                  :foreground nil
                                  :slant 'italic))))))


(menu-bar-mode          -1)
(tool-bar-mode          -1)
(scroll-bar-mode        -1)
(blink-cursor-mode       0)
(global-hi-lock-mode     1)
(global-visual-line-mode 1)


(setq ring-bell-function                 'ignore
      scroll-conservatively              101
      inhibit-startup-message            t
      initial-scratch-message            ""
      mouse-wheel-scroll-amount          '(1)
      mouse-wheel-progressive-speed      nil
      eldoc-echo-area-use-multiline-p    nil
      magit-section-visibility-indicator nil)


(set-face-attribute
 'default nil
 :height  120
 :family  "iosevka ss07")


(my/default-theme-overrides)
(set-frame-parameter nil 'fullscreen 'fullboth)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\(FIXME\\|TODO\\)" 1
       font-lock-warning-face t)))))


;; editing ========================
;;           _ _ _   _
;;   ___  __| (_) |_(_)_ __   __ _
;;  / _ \/ _` | | __| | '_ \ / _` |
;; |  __/ (_| | | |_| | | | | (_| |
;;  \___|\__,_|_|\__|_|_| |_|\__, |
;;                           |___/
;; ================================


(set-language-environment "UTF-8")


(show-paren-mode 1)
(save-place-mode 1)


(setq-default indent-tabs-mode nil)


(setq create-lockfiles                    nil
      auto-save-default                   nil
      make-backup-files                   nil
      mouse-yank-at-point                 t
      electric-indent-mode                nil
      select-enable-primary               t
      select-enable-clipboard             t
      flycheck-display-errors-function    nil
      save-interprogram-paste-before-kill t
      flycheck-check-syntax-automatically '(save idle-change mode-enabled))


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


(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "<f6>")
    (lambda ()
      (interactive)
      (if (get-buffer-window flycheck-error-list-buffer)
          (quit-windows-on flycheck-error-list-buffer)
        (list-flycheck-errors)))))


;; navigation =========================================
;;                     _             _   _
;;  _ __   __ ___   __(_) __ _  __ _| |_(_) ___  _ __
;; | '_ \ / _` \ \ / /| |/ _` |/ _` | __| |/ _ \| '_ \
;; | | | | (_| |\ V / | | (_| | (_| | |_| | (_) | | | |
;; |_| |_|\__,_| \_/  |_|\__, |\__,_|\__|_|\___/|_| |_|
;;                       |___/
;; ====================================================


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


(setq apropos-do-all                         t
      ido-use-faces                          nil
      recenter-positions                     '(top middle bottom)
      recentf-max-menu-items                 40
      ido-use-virtual-buffers                t
      ido-enable-flex-matching               t
      ido-vertical-define-keys               'C-n-and-C-p-only
      ido-use-filename-at-point              nil
      uniquify-buffer-name-style             'forward
      ido-auto-merge-work-directories-length -1)


(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil))


(global-set-key (kbd "M-x")     #'smex)
(global-set-key (kbd "C-x C-b") #'ibuffer)


(global-set-key (kbd "<f2> u") 'windmove-up)
(global-set-key (kbd "<f2> d") 'windmove-down)
(global-set-key (kbd "<f2> l") 'windmove-left)
(global-set-key (kbd "<f2> r") 'windmove-right)
(global-set-key (kbd "<f2> s") 'window-swap-states)


(fset 'yes-or-no-p 'y-or-n-p)


;; lisp ==========
;;  _ _
;; | (_)___ _ __
;; | | / __| '_ \
;; | | \__ \ |_) |
;; |_|_|___/ .__/
;;         |_|
;; ===============


(add-hook 'cider-mode-hook      #'eldoc-mode)
(add-hook 'prog-mode-hook       #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'prog-mode-hook       #'subword-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook    #'flycheck-mode)


(setq cider-use-overlays                    t
      cider-repl-wrap-history               t
      cider-save-file-on-load               t
      cider-prompt-for-symbol               nil
      cider-font-lock-dynamically           '(macro core function var)
      cider-use-fringe-indicators           nil
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


;; org =============
;;   ___  _ __ __ _
;;  / _ \| '__/ _` |
;; | (_) | | | (_| |
;;  \___/|_|  \__, |
;;            |___/
;; =================


(setq org-log-done                   'note
      plantuml-jar-path              (getenv "plantuml")
      org-log-reschedule             'note
      org-log-into-drawer            'LOGBOOK
      org-plantuml-jar-path          (getenv "plantuml")
      org-src-fontify-natively       t
      org-hide-emphasis-markers      t
      org-confirm-babel-evaluate     nil
      plantuml-default-exec-mode     'jar
      org-enforce-todo-dependencies  t
      org-startup-with-inline-images t)


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a") #'org-agenda)
  (define-key org-mode-map (kbd "C-c c") #'org-capture)
  (define-key org-mode-map (kbd "C-c l") #'org-store-link)
  (define-key org-mode-map (kbd "<f5>" ) (lambda ()
                                           (interactive)
                                           (kill-new (org-id-get-create))))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)
     (js         . t)
     (java       . t)
     (http       . t)
     (shell      . t)
     (plantuml   . t)
     (emacs-lisp . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-capture-templates
        '(("t" "Tasks" entry (file+headline "~/.emacs.d/org/tasks.org" "Tasks")
           "* TODO %?\n  %iSCHEDULED: %U\n  %a")))
  (setq org-babel-js-function-wrapper
        "process.stdout.write(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true }))"))
