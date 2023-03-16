(require 'package)


(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-pinned-packages
      '((eros                 . "melpa"       )
        (sicp                 . "melpa"       )
        (smex                 . "melpa-stable")
        (cider                . "melpa-stable")
        (magit                . "melpa-stable")
        (company              . "melpa-stable")
        (ob-http              . "melpa-stable")
        (flx-ido              . "melpa-stable")
        (paredit              . "melpa-stable")
        (flycheck             . "melpa-stable")
        (clojure-mode         . "melpa-stable")
        (expand-region        . "melpa-stable")
        (plantuml-mode        . "melpa-stable")
        (zenburn-theme        . "melpa-stable")
        (ido-vertical-mode    . "melpa-stable")
        (flycheck-clj-kondo   . "melpa-stable")
        (idle-highlight-mode  . "melpa"       )
        (ido-completing-read+ . "melpa-stable")))


(package-initialize)


(unless package-archive-contents
  (package-refresh-contents))


(dolist (package (mapcar #'car package-pinned-packages))
  (unless (package-installed-p package)
    (package-install package)))


(setq backup-directory-alist     nil
      auto-save-list-file-prefix nil
      custom-file                "~/.emacs.d/custom.el")


(when (eq system-type 'windows-nt)
  (unless buffer-file-name
    (setq default-directory "C:/"))
  (add-to-list 'exec-path "C:/Program Files/7-Zip")
  (setq find-program "\"C:/Program Files/Git/usr/bin/find.exe\""))


;; ui ======
;;        _
;;  _   _(_)
;; | | | | |
;; | |_| | |
;;  \__,_|_|
;; =========


(when-let (theme (car custom-enabled-themes))
  (disable-theme theme))


(load-theme 'concrete t)


(global-set-key
 [f12]
 (lambda ()
   (interactive)
   (if (eq (car custom-enabled-themes) 'zenburn)
       (progn (disable-theme 'zenburn)
              (load-theme 'concrete t))
     (progn (disable-theme 'concrete)
            (load-theme 'zenburn t)))))


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


(global-set-key
 [f2]
 (lambda ()
   (interactive)
   (save-excursion
     (delete-trailing-whitespace)
     (untabify      (point-min) (point-max) nil)
     (indent-region (point-min) (point-max) nil)
     (save-buffer))))


(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "M-[") #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") #'paredit-wrap-curly))


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
  (setq magit-module-sections-nested nil)
  (define-key magit-mode-map (kbd "C-<tab>") nil))


(global-set-key (kbd "M-x")     #'smex)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-<tab>") #'other-window)


(fset 'yes-or-no-p 'y-or-n-p)


;; lisp ==========
;;  _ _
;; | (_)___ _ __
;; | | / __| '_ \
;; | | \__ \ |_) |
;; |_|_|___/ .__/
;;         |_|
;; ===============


(eros-mode 1)


(global-set-key (kbd "C-c C-c") #'eval-defun)
(global-set-key (kbd "C-c C-e") #'eval-last-sexp)
(global-set-key (kbd "C-c C-p") #'pp-eval-last-sexp)


(global-set-key
 (kbd "C-c M-w")
 (lambda ()
   (interactive)
   (save-excursion
     (kill-ring-save
      (progn (beginning-of-defun) (point))
      (progn (end-of-defun)       (point))))))


(define-key emacs-lisp-mode-map
  (kbd "C-c C-k")
  (lambda ()
    (interactive)
    (load-file buffer-file-name)))


(define-key emacs-lisp-mode-map
  (kbd "C-c C-u")
  (lambda ()
    (interactive)
    (thread-last (or (thing-at-point 'symbol) "")
      (read-from-minibuffer "Undefine symbol: ")
      (intern) (fmakunbound))))


(add-hook 'cider-mode-hook      #'eldoc-mode)
(add-hook 'prog-mode-hook       #'subword-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook    #'flycheck-mode)
(add-hook 'org-mode-hook        #'idle-highlight-mode)
(add-hook 'prog-mode-hook       #'idle-highlight-mode)
(add-hook 'cider-repl-mode-hook #'idle-highlight-mode)


(add-hook 'clojure-mode-hook         #'paredit-mode)
(add-hook 'cider-mode-hook           #'paredit-mode)
(add-hook 'cider-repl-mode-hook      #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook      #'paredit-mode)
(add-hook 'scheme-mode-hook          #'paredit-mode)
(add-hook 'inferior-scheme-mode-hook #'paredit-mode)


(setq cider-use-overlays                    t
      cider-enrich-classpath                t
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
      org-agenda-files               (list (expand-file-name "org/mobile" (getenv "dropbox")))
      plantuml-jar-path              (getenv "plantuml")
      org-log-reschedule             'note
      org-log-into-drawer            'LOGBOOK
      org-plantuml-jar-path          (getenv "plantuml")
      org-src-fontify-natively       t
      org-hide-emphasis-markers      t
      org-confirm-babel-evaluate     nil
      plantuml-default-exec-mode     'jar
      org-agenda-dim-blocked-tasks   t
      org-enforce-todo-dependencies  t
      org-startup-with-inline-images t)


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a") #'org-agenda)
  (define-key org-mode-map (kbd "C-c c") #'org-capture)
  (define-key org-mode-map (kbd "C-c l") #'org-store-link)
  (define-key org-mode-map (kbd "<f5>" ) (lambda ()
                                           (interactive)
                                           (kill-new (org-id-get-create))))
  (add-to-list 'org-export-backends 'beamer)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)
     (js         . t)
     (dot        . t)
     (java       . t)
     (http       . t)
     (shell      . t)
     (fortran    . t)
     (plantuml   . t)
     (emacs-lisp . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-capture-templates
        `(("t" "Tasks" entry
           (file+headline ,(expand-file-name "org/mobile/tasks.org" (getenv "dropbox")) "Tasks")
           "* TODO %?\n  %iSCHEDULED: %U\n  %a")))
  (setq org-babel-js-function-wrapper
        "process.stdout.write(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true }))"))


;; audio =====================
;;                  _ _
;;   __ _ _   _  __| (_) ___
;;  / _` | | | |/ _` | |/ _ \
;; | (_| | |_| | (_| | | (_) |
;;  \__,_|\__,_|\__,_|_|\___/
;; ===========================


(defun xspf-playlists-recursively ()
  (interactive)
  (let ((root (read-directory-name "Enter root dir for XSPF playlists: ")))
    (when (y-or-n-p (format "Confirm %s? " root))
      (require 'xml)
      (thread-last (directory-files-recursively root "." t)
        (seq-filter (lambda (fname) (file-directory-p fname)))
        (cons root)
        (mapc
         (lambda (dir)
           (when-let (tracks (seq-filter
                              (lambda (fname)
                                (when-let (ext (file-name-extension fname))
                                  (member (downcase ext)
                                          '("aac" "ac3" "aif" "amr" "ape" "au"
                                            "flac" "m4a" "m4b" "m4p" "mka"
                                            "mp3" "mp4" "ogg" "opus" "ra" "rm"
                                            "sd2" "tta" "wav" "wma"))))
                              (directory-files dir)))
             (let ((out (expand-file-name "playlist.xspf" dir)))
               (delete-file out)
               (with-temp-file out
                 (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                 (newline)
                 (insert "<playlist>")
                 (newline)
                 (insert "  ")
                 (insert "<trackList>")
                 (newline)
                 (mapcar (lambda (track)
                           (insert "    ")
                           (insert (format "<track><location>%s</location></track>"
                                           (xml-escape-string track)))
                           (newline))
                         tracks)
                 (insert "  ")
                 (insert "</trackList>")
                 (newline)
                 (insert "</playlist>"))))))))))
