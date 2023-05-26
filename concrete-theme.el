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
 '(font-lock-comment-face ((t (:foreground "firebrick" :weight bold :slant italic))))
 '(magit-section-highlight ((t (:background "gray90"))))
 '(magit-diff-context-highlight ((t (:background "gray90"))))
 '(org-block ((t (:background "gray90"))))
 '(org-meta-line ((t (:background "gray75" :foreground "black" :slant italic))))
 '(cider-repl-stderr-face ((t (:foreground "firebrick" :weight bold)))))


(provide-theme 'concrete)
