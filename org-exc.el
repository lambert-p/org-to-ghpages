(save-excursion
  ;; map over all task entries
  (let ((dev-file (expand-file-name
                   "blog.org"
                   (file-name-directory (buffer-file-name))))
        (posts-dir (expand-file-name
                    "~/code/lambertington.github.io/_posts/"
                    (file-name-directory (buffer-file-name))))
        (yaml-front-matter '(("layout" . "default"))))
    ;; go through both the tasks and the bugs
    (mapc
     (lambda (top-level)
       (find-file dev-file)
       (goto-char (point-min))
       (outline-next-visible-heading 1)
       (org-map-tree
        (lambda ()
          (let* ((props (org-entry-properties))
                 (todo (cdr (assoc "TODO" props)))
                 (time (cdr (assoc "TIMESTAMP_IA" props))))
            ;; each task with a state and timestamp can be exported as a jekyll blog post
            (when (and todo time)
              (message "time=%s" time)
              (let* ((heading (org-get-heading))
                     (title (replace-regexp-in-string
                             "[:=\(\)\?]" ""
                             (replace-regexp-in-string
                              "[ \t]" "-" heading)))
                     (str-time (and (string-match "\\([[:digit:]\-]+\\) " time)
                                    (match-string 1 time)))
                     (to-file (format "%s-%s.html" str-time title))
                     (org-buffer (current-buffer))
                     (yaml-front-matter (cons (cons "title" heading) yaml-front-matter))
                     html)
                (org-narrow-to-subtree)
                (setq html (org-export-as-html nil nil nil 'string t nil))
                (set-buffer org-buffer) (widen)
                (with-temp-file (expand-file-name to-file posts-dir)
                  (when yaml-front-matter
                    (insert "---\n")
                    (mapc (lambda (pair) (insert (format "%s: %s\n" (car pair) (cdr pair))))
                          yaml-front-matter)
                    (insert "---\n\n"))
                  (insert html))
                (get-buffer org-buffer)))))))
     '(1 2))))
                    
                       
                                    
