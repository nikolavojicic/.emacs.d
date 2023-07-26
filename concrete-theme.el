(deftheme concrete "The concrete GRAY color theme")


(custom-theme-set-faces
 'concrete
 '(cursor ((t (:background "red"))))
 '(default ((t (:background "lightgray"))))
 '(fringe ((t (:background "lightgray"))))
 '(vertical-border ((t (:foreground "darkgray"))))
 '(mode-line-buffer-id ((t (:foreground "blue"))))
 '(line-number-current-line ((t (:foreground "red"))))
 '(font-lock-doc-face ((t (:foreground "firebrick" :slant italic))))
 '(font-lock-comment-face ((t (:foreground "firebrick" :weight bold))))
 '(magit-section-highlight ((t (:background "gray90"))))
 '(magit-diff-context-highlight ((t (:background "gray90"))))
 '(org-block ((t (:background "gray90"))))
 '(org-meta-line ((t (:background "gray75" :foreground "black"))))
 '(font-lock-type-face ((t (:foreground "darkgreen" :weight bold))))
 '(clojure-keyword-face ((t (:foreground "darkgreen"))))
 '(cider-repl-stderr-face ((t (:foreground "firebrick" :weight bold))))
 '(cider-error-overlay-face ((t (:foreground "firebrick" :weight bold))))
 '(dired-subtree-depth-1-face ((t (:background "inherit"))))
 '(dired-subtree-depth-2-face ((t (:background "inherit"))))
 '(dired-subtree-depth-3-face ((t (:background "inherit"))))
 '(dired-subtree-depth-4-face ((t (:background "inherit"))))
 '(dired-subtree-depth-5-face ((t (:background "inherit"))))
 '(dired-subtree-depth-6-face ((t (:background "inherit")))))


(provide-theme 'concrete)
