(mapc
 (lambda (file)
   (let ((full-file (expand-file-name
                     (concat file ".org")
                     (file-name-directory (buffer-file-name))))
         (yaml-front-matter `(("layout" . "default")
                              ("title" . ,file)))
         html)
     ;; go to the top level tasks heading
     (find-file full-file)
     (setq html (org-export-as-html nil nil nil 'string t nil))
     (with-temp-file (concat file ".html")
       (when yaml-front-matter
         (insert "---\n")
         (mapc (lambda (pair) (insert (format "%s: %s\n" (car pair) (cdr pair))))
               yaml-front-matter)
         (insert "---\n"))
       (insert html)))) '("index" "publish" "dev-updates"))
