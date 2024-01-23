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