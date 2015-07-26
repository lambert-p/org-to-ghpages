(mapc
 (lambda (file)
   (let ((full-file (expand-file-name
                     (concat file ".org")
                     (file-name-directory (buffer-file-name))))
         (yaml-front-matter `(("layout" . "post")
                              ("title" . ,file)))
         html)
     ;; go to the top level tasks heading
     (find-file full-file)
     ;;(setq html (org-html-export-as-html nil nil nil 'string nil))
     (setq md (org-md-export-as-markdown nil t nil))
     (with-temp-file (concat file ".md")
       (when yaml-front-matter
         (insert "---\n")
         (mapc (lambda (pair) (insert (format "%s: %s\n" (car pair) (cdr pair))))
               yaml-front-matter)
         (insert "---\n"))
       (insert html)))) '("blog"))
