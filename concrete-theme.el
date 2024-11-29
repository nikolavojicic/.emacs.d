(deftheme concrete "The concrete GRAY color theme")


(custom-theme-set-faces
 'concrete
 '(cursor                        ((t (:background "red"))))
 '(mc/cursor-face                ((t (:background "red" :foreground "gray40"))))
 '(default                       ((t (:background "gray83"))))
 '(fringe                        ((t (:background "gray83"))))
 '(vertical-border               ((t (:foreground "gray65"))))
 '(mode-line-buffer-id           ((t (:foreground "blue"))))
 '(line-number-current-line      ((t (:foreground "red"))))
 '(font-lock-doc-face            ((t (:foreground "firebrick" :slant italic))))
 '(font-lock-comment-face        ((t (:foreground "gray40"))))
 '(show-paren-match              ((t (:background "gray40" :foreground "red"))))
 '(magit-section-highlight       ((t (:background "gray90"))))
 '(magit-diff-context-highlight  ((t (:background "gray90"))))
 '(org-block                     ((t (:background "gray90"))))
 '(org-meta-line                 ((t (:background "gray75" :foreground "black"))))
 '(org-hide                      ((t (:foreground "lightgray"))))
 '(org-level-1                   ((t (:foreground "purple"))))
 '(org-level-3                   ((t (:foreground "blue"))))
 '(org-level-4                   ((t (:foreground "darkgreen"))))
 '(org-level-5                   ((t (:foreground "firebrick"))))
 '(font-lock-builtin-face        ((t (:foreground "darkgreen"))))
 '(font-lock-type-face           ((t (:foreground "darkcyan"))))
 '(clojure-keyword-face          ((t (:foreground "darkgreen"))))
 '(cider-repl-stderr-face        ((t (:foreground "firebrick"))))
 '(cider-error-overlay-face      ((t (:foreground "firebrick"))))
 '(cider-debug-code-overlay-face (()))
 '(cider-result-overlay-face     (()))
 '(eros-result-overlay-face      (()))
 '(dired-subtree-depth-1-face    ((t (:background "inherit"))))
 '(dired-subtree-depth-2-face    ((t (:background "inherit"))))
 '(dired-subtree-depth-3-face    ((t (:background "inherit"))))
 '(dired-subtree-depth-4-face    ((t (:background "inherit"))))
 '(dired-subtree-depth-5-face    ((t (:background "inherit"))))
 '(dired-subtree-depth-6-face    ((t (:background "inherit")))))


(provide-theme 'concrete)
