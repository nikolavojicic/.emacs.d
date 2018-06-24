(require 'package)

(setq package-archives
      '(("gnu"       . "https://elpa.gnu.org/packages/")
        ("melpa"     . "https://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(smex
                   cider
                   magit
                   company
                   flx-ido
                   paredit
                   which-key
                   projectile
                   clj-refactor
                   clojure-mode
                   zenburn-theme
                   ido-ubiquitous
                   aggressive-indent
                   ido-vertical-mode
                   rainbow-delimiters
                   clojure-mode-extra-font-locking))
  (unless (package-installed-p package)
    (package-install package)))

;; remove ido compile warnings
(defvar predicate            nil)
(defvar ido-cur-item         nil)
(defvar ido-cur-list         nil)
(defvar ido-default-item     nil)
(defvar inherit-input-method nil)

(unless (file-exists-p "~/.emacs.d/generated")
  (make-directory "~/.emacs.d/generated"))

(setq backup-directory-alist         nil
      auto-save-list-file-prefix     nil
      custom-file                    "~/.emacs.d/generated/custom.el"
      smex-save-file                 "~/.emacs.d/generated/smex-items"
      save-place-file                "~/.emacs.d/generated/places"
      recentf-save-file              "~/.emacs.d/generated/recentf"
      projectile-cache-file          "~/.emacs.d/generated/projectile.cache"
      ido-save-directory-list-file   "~/.emacs.d/generated/ido.last"
      projectile-known-projects-file "~/.emacs.d/generated/projectile-bookmarks.eld")

(add-to-list 'load-path "~/.emacs.d/generated")
(add-to-list 'load-path "~/.emacs.d/customizations")

(seq-do #'load '("ui.el" "custom.el" "editing.el"
                 "clojure.el" "navigation.el"))