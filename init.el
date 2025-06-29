(require 'package)


(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-pinned-packages
      '((avy                      . "melpa-stable")
        (eros                     . "melpa"       )
        (sicp                     . "melpa"       )
        (cider                    . "melpa-stable")
        (magit                    . "melpa-stable")
        (company                  . "melpa-stable")
        (gnuplot                  . "melpa-stable")
        (jq-mode                  . "melpa-stable")
        (ob-http                  . "melpa-stable")
        (paredit                  . "melpa-stable")
        (flycheck                 . "melpa-stable")
        (web-mode                 . "melpa-stable")
        (clojure-mode             . "melpa-stable")
        (expand-region            . "melpa-stable")
        (ob-typescript            . "melpa"       )
        (plantuml-mode            . "melpa-stable")
        (zenburn-theme            . "melpa-stable")
        (dired-subtree            . "melpa"       )
        (typescript-mode          . "melpa-stable")
        (multiple-cursors         . "melpa-stable")
        (flycheck-clj-kondo       . "melpa-stable")
        (highlight-function-calls . "melpa-stable")))

(package-initialize)


(unless package-archive-contents
  (package-refresh-contents))


(dolist (package (mapcar #'car package-pinned-packages))
  (unless (package-installed-p package)
    (package-install package)))


(setq backup-directory-alist     nil
      auto-save-list-file-prefix nil
      find-function-C-source-directory "~/.emacs.d/src"
      custom-file                      "~/.emacs.d/custom.el")


(when (eq system-type 'windows-nt)
  (unless buffer-file-name
    (let* ((dir (getenv "USERPROFILE"))
           (dir (string-replace "\\" "/" dir)))
      (setq default-directory (concat dir "/"))))
  (add-to-list 'exec-path "C:/Program Files/7-Zip")
  (setq find-program "\"C:/Program Files/Git/usr/bin/find.exe\""))


;; ui ======
;;        _
;;  _   _(_)
;; | | | | |
;; | |_| | |
;;  \__,_|_|
;; =========


(defun disable-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


(add-hook 'prog-mode-hook    #'remove-dos-eol)
(add-hook 'special-mode-hook #'remove-dos-eol)


(advice-add 'load-theme :before (lambda (&rest args) (disable-themes)))
(load-theme 'concrete t)


(global-set-key
 [f12]
 (lambda ()
   (interactive)
   (let* ((theme (car custom-enabled-themes))
          (box   '(:line-width -1 :style released-button)))
     (cond ((eq theme 'concrete)
            (load-theme 'zenburn t)
            (custom-theme-set-faces
             'zenburn
             '(default                       ((t (:background "#383838"))))
             '(fringe                        ((t (:background "#383838"))))
             '(vertical-border               ((t (:foreground "#656555"))))
             '(highlight                     ((t (:background "#2B2B2B"))))
             '(font-lock-doc-face            ((t (:foreground "#9FC59F" :slant italic))))
             '(font-lock-builtin-face        ((t (:foreground "#BFEBBF"))))
             '(org-hide                      ((t (:foreground "#383838"))))
             '(org-block                     ((t (:background "#494949"))))
             '(org-meta-line                 ((t (:background "#3F3F3F" :foreground "#7F9F7F"))))
             '(clojure-keyword-face          ((t (:foreground "#bfebbf"))))
             '(dired-subtree-depth-1-face    ((t (:background "inherit"))))
             '(dired-subtree-depth-2-face    ((t (:background "inherit"))))
             '(dired-subtree-depth-3-face    ((t (:background "inherit"))))
             '(dired-subtree-depth-4-face    ((t (:background "inherit"))))
             '(dired-subtree-depth-5-face    ((t (:background "inherit"))))
             '(dired-subtree-depth-6-face    ((t (:background "inherit"))))
             '(cider-debug-code-overlay-face (()))
             '(cider-error-overlay-face      ((t (:foreground "#D0BF8F"))))
             '(highlight-function-calls-face ((t (:foreground "#93E0E3"))))
             '(web-mode-json-key-face        ((t (:foreground "#BFEBBF"))))
             '(web-mode-json-context-face    ((t (:foreground "#BFEBBF"))))))
           ((eq theme 'zenburn)
            (load-theme 'manoj-dark t)
            (custom-theme-set-faces
             'manoj-dark
             '(cursor                           ((t (:background "red"))))
             '(mc/cursor-face                   ((t (:background "blue"))))
             '(mc/region-face                   ((t (:foreground "red"))))
             '(fringe                           ((t (:background "inherit"))))
             '(vertical-border                  ((t (:foreground "red"))))
             '(mode-line                        ((t (:foreground "red" :box ,box))))
             '(highlight                        ((t (:background "blue"))))
             '(completions-common-part          ((t (:foreground "red"))))
             '(line-number-current-line         ((t (:foreground "red"))))
             '(font-lock-type-face              ((t (:foreground "#019afe"))))
             '(font-lock-builtin-face           ((t (:foreground "#fee301"))))
             '(font-lock-function-name-face     ((t (:foreground "#01fe65"))))
             '(font-lock-variable-name-face     ((t (:foreground "#fe9801"))))
             '(font-lock-keyword-face           ((t (:foreground "#fe019a"))))
             '(font-lock-comment-face           ((t (:foreground "gray70"))))
             '(font-lock-comment-delimiter-face ((t (:foreground "gray70"))))
             '(org-block                        ((t (:background "gray10"))))
             '(org-meta-line                    ((t (:background "gray20" :foreground "chocolate1"))))
             '(org-hide                         ((t (:foreground "black"))))
             '(clojure-keyword-face             ((t (:foreground "#fee301"))))
             '(cider-repl-stderr-face           ((t (:foreground "red"))))
             '(cider-error-overlay-face         ((t (:background "inherit" :foreground "red"))))
             '(cider-debug-code-overlay-face    (()))
             '(cider-result-overlay-face        (()))
             '(eros-result-overlay-face         (()))
             '(dired-subtree-depth-1-face       ((t (:background "inherit"))))
             '(dired-subtree-depth-2-face       ((t (:background "inherit"))))
             '(dired-subtree-depth-3-face       ((t (:background "inherit"))))
             '(dired-subtree-depth-4-face       ((t (:background "inherit"))))
             '(dired-subtree-depth-5-face       ((t (:background "inherit"))))
             '(dired-subtree-depth-6-face       ((t (:background "inherit"))))
             '(isearch                          ((t (:background "blue" :foreground "red"))))
             '(lazy-highlight                   ((t (:foreground "red"))))
             '(highlight-function-calls-face    ((t (:foreground "#01fe65"))))
             '(web-mode-json-key-face           ((t (:foreground "LightSlateBlue"))))
             '(web-mode-json-context-face       ((t (:foreground "LightSlateBlue"))))
             '(font-lock-preprocessor-face      ((t (:foreground "CornFlowerBlue"))))
             '(company-tooltip                  ((t (:background "black"))))
             '(company-tooltip-common           ((t (:foreground "red"))))
             '(company-tooltip-selection        ((t (:background "blue")))))
            (set-face-attribute 'completions-common-part nil :height 1.0 :weight 'bold)
            (set-face-attribute 'header-line             nil :height 1.0)
            (set-face-attribute 'mode-line-buffer-id     nil :height 1.0 :background "inherit")
            (set-face-attribute 'mode-line-highlight     nil :height 1.0)
            (set-face-attribute 'mode-line-inactive      nil :height 1.0 :background "inherit"))
           ("default theme"
            (load-theme 'concrete t))))
   (set-face-attribute 'font-lock-keyword-face  nil :weight 'normal)
   (set-face-attribute 'font-lock-warning-face  nil :weight 'normal)
   (set-face-attribute 'mode-line-buffer-id     nil :weight 'normal :slant 'italic)
   (set-face-attribute 'font-lock-string-face   nil :weight 'normal :foreground nil)
   (set-face-attribute 'font-lock-constant-face nil :weight 'normal :foreground nil)))


(global-set-key
 (kbd "C-x _")
 (lambda ()
   (interactive)
   (let ((delta (- (window-size (selected-window) t)
                   (let ((longest-line 0))
                     (save-excursion
                       (goto-char (point-min))
                       (while (not (eobp))
                         (let ((line-length 0))
                           (while (and (not (eolp)) (not (eobp)))
                             (unless (invisible-p (point))
                               (setq line-length (1+ line-length)))
                             (forward-char 1))
                           (setq longest-line (max longest-line line-length)))
                         (forward-line 1)))
                     longest-line)
                   7)))
     (when (not (= delta 0))
       (enlarge-window-horizontally (- delta))))))


(menu-bar-mode          -1)
(tool-bar-mode          -1)
(scroll-bar-mode        -1)
(blink-cursor-mode       0)
(global-hi-lock-mode     1)
(global-visual-line-mode 1)


(setq ring-bell-function                 'ignore
      scroll-conservatively              101
      inhibit-startup-message            t
      initial-scratch-message            nil
      display-time-24hr-format           t
      display-time-day-and-date          t
      mouse-wheel-scroll-amount          '(1)
      custom--inhibit-theme-enable       nil
      mouse-wheel-progressive-speed      nil
      eldoc-echo-area-use-multiline-p    nil
      display-time-default-load-average  nil
      magit-section-visibility-indicator nil)


(progn ;; mode must run after setting format
  (setq display-time-format "%d.%m.%Y. %H:%M (%A)")
  (display-time-mode 1))


(set-face-attribute
 'default nil
 :height  110
 :family  "IBM Plex Mono Medium")


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
(global-auto-revert-mode 1)


(setq-default indent-tabs-mode nil)
(put 'downcase-region 'disabled nil)


(setq create-lockfiles                    nil
      auto-save-default                   nil
      dired-dwim-target                   t
      make-backup-files                   nil
      mouse-yank-at-point                 t
      electric-indent-mode                nil
      select-enable-primary               t
      select-enable-clipboard             t
      delete-by-moving-to-trash           t
      flycheck-display-errors-function    nil
      global-auto-revert-non-file-buffers t
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


(global-set-key (kbd "M-/")   #'hippie-expand)
(global-set-key (kbd "C-=")   #'er/expand-region)
(global-set-key (kbd "C-c s") #'sort-lines)


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
  (define-key paredit-mode-map (kbd "M-?") nil) ;; undefine
  (define-key paredit-mode-map (kbd "M-[") #'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-{") #'paredit-wrap-curly))


(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "<f6>")
              (lambda ()
                (interactive)
                (if (get-buffer-window flycheck-error-list-buffer)
                    (quit-windows-on   flycheck-error-list-buffer)
                  (list-flycheck-errors)))))


;; navigation =========================================
;;                     _             _   _
;;  _ __   __ ___   __(_) __ _  __ _| |_(_) ___  _ __
;; | '_ \ / _` \ \ / /| |/ _` |/ _` | __| |/ _ \| '_ \
;; | | | | (_| |\ V / | | (_| | (_| | |_| | (_) | | | |
;; |_| |_|\__,_| \_/  |_|\__, |\__,_|\__|_|\___/|_| |_|
;;                       |___/
;; ====================================================


(require 'uniquify)
(require 'multiple-cursors)


(repeat-mode)
(global-company-mode)
(fido-vertical-mode  1)
(put 'narrow-to-region 'disabled nil)


(setq apropos-do-all             t
      recenter-positions         '(top middle bottom)
      dired-listing-switches     "-alFh"
      uniquify-buffer-name-style 'forward)


(add-hook 'dired-mode-hook                   #'dired-hide-details-mode)
(add-hook 'dired-mode-hook                   #'dired-omit-mode)
(add-hook 'dired-mode-hook                   #'subword-mode)
(add-hook 'minibuffer-setup-hook             #'subword-mode)
(add-hook 'dired-subtree-after-insert-hook   #'dired-omit-mode)
(add-hook 'dired-subtree-after-collapse-hook #'dired-omit-mode)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))


(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-<tab>"  ) #'dired-subtree-cycle)
  (define-key dired-mode-map (kbd "<tab>"    ) #'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "<backtab>") #'dired-subtree-remove)
  (define-key dired-mode-map (kbd "%f"       ) #'find-name-dired)
  (define-key dired-mode-map (kbd "%F"       ) #'find-grep-dired))


(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil))


(with-eval-after-load 'doc-view
  (let* ((pdf-program "mutool"))
    (setq doc-view-resolution 300
          doc-view-ghostscript-program pdf-program)))


(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-'")     #'avy-goto-char-timer)
(global-set-key (kbd "C-c C-'") #'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-;") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-=") #'mc/mark-all-like-this-in-defun)
(global-set-key (kbd "C-x C-=") #'mc/mark-all-like-this)


(define-key mc/keymap (kbd "C-'")  #'mc/mark-next-like-this)
(define-key mc/keymap (kbd "C-;")  #'mc/mark-previous-like-this)
(define-key mc/keymap (kbd "C-\"") #'mc/unmark-next-like-this)
(define-key mc/keymap (kbd "C-:")  #'mc/unmark-previous-like-this)
(define-key mc/keymap (kbd "C-`")  #'mc/cycle-forward)


(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)


;; lisp ==========
;;  _ _
;; | (_)___ _ __
;; | | / __| '_ \
;; | | \__ \ |_) |
;; |_|_|___/ .__/
;;         |_|
;; ===============


(eros-mode 1)


;; eval elisp everywhere
(global-set-key (kbd "C-c C-c") #'eval-defun)
(global-set-key (kbd "C-c C-e") #'eval-last-sexp)
(global-set-key (kbd "C-c C-p") #'pp-eval-last-sexp)


(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-e") #'eval-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c C-p") #'pp-eval-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c C-k")
              (lambda ()
                (interactive)
                (load-file buffer-file-name)))
  (define-key emacs-lisp-mode-map (kbd "C-c C-u")
              (lambda ()
                (interactive)
                (thread-last
                  (or (thing-at-point 'symbol) "")
                  (read-from-minibuffer "Undefine symbol: ")
                  (intern) (makunbound) (fmakunbound)))))


;; ========== inline edebug result ==========
(advice-add #'edebug-compute-previous-result :around
            (lambda (_ &rest args)
              (let ((previous-value (nth 0 args)))
                (when edebug-unwrap-results
                  (setq previous-value (edebug-unwrap* previous-value)))
                (setq edebug-previous-result (edebug-safe-prin1-to-string previous-value)))))


(advice-add #'edebug-previous-result :around
            (lambda (_ &rest args)
              (eros--make-result-overlay edebug-previous-result
                :where    (point)
                :duration eros-eval-result-duration)))
;; ==========================================


(global-set-key
 (kbd "C-c M-w")
 (lambda ()
   (interactive)
   (save-excursion
     (kill-ring-save
      (progn (beginning-of-defun) (point))
      (progn (end-of-defun)       (point))))))


(with-eval-after-load 'cider-repl
  (define-key cider-repl-mode-map (kbd "<return>") #'cider-repl-closing-return))


(with-eval-after-load 'cider
  (define-key cider-mode-map (kbd "C-c C-j")
              (lambda ()
                (interactive)
                (cider-jump-to-compilation-error)
                (recenter nil t)))
  (define-key cider-mode-map (kbd "<f7>")
              (lambda ()
                (interactive)
                (if (get-buffer-window cider-error-buffer)
                    (quit-windows-on   cider-error-buffer)
                  (cider-popup-buffer-display cider-error-buffer))))
  (cider-enable-cider-completion-style)
  ;; TODO https://github.com/clojure-emacs/cider/issues/3775
  (setq cider-injected-middleware-version "0.52.1"))


(add-hook 'cider-mode-hook      #'eldoc-mode)
(add-hook 'prog-mode-hook       #'subword-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook    #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-function-calls-mode)


(add-hook 'clojure-mode-hook         #'paredit-mode)
(add-hook 'cider-mode-hook           #'paredit-mode)
(add-hook 'cider-repl-mode-hook      #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook      #'paredit-mode)
(add-hook 'scheme-mode-hook          #'paredit-mode)
(add-hook 'inferior-scheme-mode-hook #'paredit-mode)
(add-hook 'js-mode-hook              #'paredit-mode)
(add-hook 'web-mode-hook             #'paredit-mode)
(add-hook 'typescript-mode-hook      #'paredit-mode)


(with-eval-after-load 'paredit
  (add-to-list 'paredit-space-for-delimiter-predicates
               (lambda (endp delimiter)
                 (or
                  (bound-and-true-p js-mode)
                  (bound-and-true-p web-mode)
                  (bound-and-true-p typescript-mode)))))


(setq cider-use-overlays                     t
      cider-enrich-classpath                 nil ;; set to t when needed
      inferior-lisp-program                  "clisp"
      cider-repl-wrap-history                t
      cider-save-file-on-load                t
      cider-show-error-buffer                nil
      cider-prompt-for-symbol                nil
      cider-auto-jump-to-error               nil
      cider-font-lock-dynamically            '(macro core function var)
      cider-use-fringe-indicators            nil
      cider-auto-select-error-buffer         nil
      cider-repl-display-help-banner         nil
      cider-repl-use-pretty-printing         t
      cider-repl-pop-to-buffer-on-connect    'display-only
      cider-auto-select-test-report-buffer   nil
      cider-repl-history-display-duplicates  nil
      cider-repl-display-output-before-window-boundaries t)


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


;; web ================
;;               _
;; __      _____| |__
;; \ \ /\ / / _ \ '_ \
;;  \ V  V /  __/ |_) |
;;   \_/\_/ \___|_.__/
;; ====================


(require 'web-mode)
(define-derived-mode vue-mode web-mode "Vue")


(setq auto-mode-alist
      (append '(("\\.html\\'" . web-mode)
                ("\\.xml\\'"  . web-mode)
                ("\\.json\\'" . web-mode)
                ("\\.css\\'"  . web-mode)
                ("\\.scss\\'" . web-mode)
                ("\\.vue\\'"  . vue-mode)
                ("\\.js\\'"   . js-mode)
                ("\\.ts\\'"   . typescript-mode))
              auto-mode-alist))


(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset    2
                  web-mode-code-indent-offset      2
                  web-mode-css-indent-offset       2
                  web-mode-enable-auto-indentation nil)))


;; lsp =========
;;  _
;; | |___ _ __
;; | / __| '_ \
;; | \__ \ |_) |
;; |_|___/ .__/
;;       |_|
;; =============


(require 'eglot)


(add-to-list 'eglot-server-programs
             '((js-mode typescript-mode) .
               ("typescript-language-server" "--stdio")))


(defun vue-eglot-init-options ()
  (let ((tsdk (thread-last
                (shell-command-to-string "npm root -g")
                (string-trim-right)
                (expand-file-name "typescript/lib"))))
    `(:typescript (:tsdk ,tsdk)
                  :vue              (:hybridMode  :json-false)
                  :languageFeatures (:completion  (:defaultTagNameCase "both"
                                                                       :defaultAttrNameCase "kebabCase"
                                                                       :getDocumentNameCasesRequest nil
                                                                       :getDocumentSelectionRequest nil)
                                                  :diagnostics (:getDocumentVersionRequest nil))
                  :documentFeatures (:documentFormatting (:defaultPrintWidth 100
                                                                             :getDocumentPrintWidthRequest nil)
                                                         :documentSymbol t
                                                         :documentColor  t))))


(add-to-list 'eglot-server-programs
             `((vue-mode) .
               ("vue-language-server" "--stdio"
                :initializationOptions
                ,(vue-eglot-init-options))))


(add-hook 'js-mode-hook         'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'vue-mode-hook        'eglot-ensure)
(add-hook 'js-mode-hook         (lambda () (setq js-indent-level         2)))
(add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))


(define-key eglot-mode-map (kbd "C-c M-r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c M-h") #'eldoc)


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
      org-latex-caption-above        nil
      org-src-fontify-natively       t
      org-hide-emphasis-markers      t
      org-confirm-babel-evaluate     nil
      plantuml-default-exec-mode     'jar
      org-agenda-dim-blocked-tasks   t
      org-enforce-todo-dependencies  t
      org-startup-with-inline-images t
      org-latex-src-block-backend    'minted)


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-'"  ) nil)
  (define-key org-mode-map (kbd "C-c a") #'org-agenda)
  (define-key org-mode-map (kbd "C-c c") #'org-capture)
  (define-key org-mode-map (kbd "C-c l") #'org-store-link)
  (define-key org-mode-map (kbd "<f5>" ) (lambda ()
                                           (interactive)
                                           (kill-new (org-id-get-create))))
  (add-to-list 'org-latex-packages-alist '("" "minted" nil))
  (add-to-list 'org-export-backends 'beamer)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)
     (jq         . t)
     (js         . t)
     (dot        . t)
     (sql        . t)
     (java       . t)
     (http       . t)
     (shell      . t)
     (python     . t)
     (clojure    . t)
     (fortran    . t)
     (gnuplot    . t)
     (plantuml   . t)
     (emacs-lisp . t)
     (typescript . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-babel-python-command
        "py"
        org-capture-templates
        `(("t" "Tasks" entry
           (file+headline ,(expand-file-name "org/mobile/tasks.org" (getenv "dropbox")) "Tasks")
           "* TODO %?\n  %iSCHEDULED: %U\n  %a"))
        org-babel-js-function-wrapper
        "process.stdout.write(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true }))"
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-latex-minted-options
        '(("breaklines" "true")
          ("breakanywhere" "true"))
        org-babel-command:typescript
        "tsc --lib es2015,dom"))

(defun org-remove-all-result-blocks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#+begin_src " nil t)
      (org-babel-remove-result nil t))))


;; audio =====================
;;                  _ _
;;   __ _ _   _  __| (_) ___
;;  / _` | | | |/ _` | |/ _ \
;; | (_| | |_| | (_| | | (_) |
;;  \__,_|\__,_|\__,_|_|\___/
;; ===========================


(defun xspf-playlists-recursively ()
  (interactive)
  (let* ((root (read-directory-name "Enter root dir for XSPF playlists: ")))
    (when (y-or-n-p (format "Confirm %s? " root))
      (require 'xml)
      (thread-last
        (directory-files-recursively root "." t)
        (seq-filter #'file-directory-p)
        (cons root)
        (mapcar
         (lambda (dir)
           (when-let (tracks (seq-filter
                              (lambda (fname)
                                (when-let (ext (file-name-extension fname))
                                  (member (downcase ext)
                                          '("aac" "ac3" "aif" "amr" "ape" "au"
                                            "flac" "m4a" "m4b" "m4p" "mka"
                                            "mp3" "mp4" "ogg" "opus" "ra" "rm"
                                            "sd2" "tta" "wav" "wma"))))
                              (directory-files dir t)))
             (let* ((out (expand-file-name "playlist.xspf" dir)))
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
                           (insert (format "<track><location>file:///%s</location></track>"
                                           (xml-escape-string track)))
                           (newline))
                         tracks)
                 (insert "  ")
                 (insert "</trackList>")
                 (newline)
                 (insert "</playlist>")))
             t)))
        (remq nil)
        (length)
        (message "%d playlists created.")))))
