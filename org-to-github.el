;;; org-to-github.el
;;; Author: Paul Lambert <lambertington@gmail.com>

(require 'ox)

(defvar *org-github-yaml-front-matter* t)

(defun orgh:normalize-string (str)
  (downcase (replace-regexp-in-string " " "-" str)))

(defvar *org-github-pygments-langs*
  (mapcar #'orgh:normalize-string
          '("actionscript" "ada" "antlr" "applescript" "assembly" "asymptote" "awk"
            "befunge" "boo" "brainfuck" "c, c++" "c#" "clojure" "coffeescript"
            "coldfusion" "common lisp" "coq" "cryptol" "cython" "d" "dart" "delphi"
            "dylan" "erlang" "factor" "fancy" "fortran" "f#" "gap" "gherkin" "gl shaders"
            "groovy" "haskell" "idl" "io" "java" "javascript" "lasso" "llvm" "logtalk"
            "lua" "matlab" "minid" "modelica" "modula-2" "mupad" "nemerle" "nimrod"
            "objective-c" "objective-j" "octave" "ocaml" "php" "perl" "povray"
            "postscript" "powershell" "prolog" "python" "rebol" "red" "redcode"
            "ruby" "rust" "s" "s-plus" "r" "scala" "scheme" "scilab" "smalltalk"
            "snobol" "tcl" "vala" "verilog" "vhdl" "visual basic.net" "visual foxpro"
            "xquery" "zephir" "cheetah" "django" "jinja" "erb" "genshi" "jsp" "myghty"
            "mako" "smarty" "tea" "apache" "bash" "bbcode" "cmake" "css" "debian" "diff"
            "dtd" "gettext" "gnuplot" "groff" "html" "http" "ini" "irc" "lighttpd"
            "makefile" "moinmoin" "mysql" "nginx" "pov-ray" "ragel" "redcode" "rest"
            "robot" "rpm" "sql" "trac" "mysql" "sqlite" "squid" "tex" "tcsh" "vimscript"
            "windows" "xml" "xslt" "yaml")))

(org-export-define-backend 'github-pages
  '(
    (bold . org-github-bold)
    (fixed-width . org-github-fixed-width)
    (headline . org-github-headline)
    (italic . org-github-italic)
    (link . org-github-link)
    (paragraph . org-github-paragraph)
    (section . org-github-section)
    (src-block . org-github-src-block)
    (template . org-github-template)))

(defun org-github-template (contents info)
  "Accepts the final transcoded string and a plist of export options,
returns the final string with YAML frontmatter prepended"
  (message "in org-github-template")
  (let (;; (title (plist-get info :title))
        ;; (date (car (plist-get info :date)))
        ;; (tags (car (plist-get info :categories)))
        ;; (permalink (orgh:normalize-string (plist-get info :title)))
        (frontmatter
         "---
layout: post
title: %s
date: %s 
comments: true
categories: %s
permalink: %s
---
"))
    (if *org-github-yaml-front-matter*
        (concat (format frontmatter yaml-title yaml-date yaml-tags yaml-permalink) contents)
      contents)))

(defun get-lang (lang)
  (and lang
       (let ((lang (orgh:normalize-string lang)))
         (cond ((string= lang "emacs-lisp") "common-lisp")
               ((not (member lang *org-github-pygments-langs*)) nil)
               (t lang)))))

(defun org-github-src-block (src-block contents info)
  "Transcode a #+BEGIN_SRC block from Org to Github Pages style"
  (let* ((lang (get-lang (org-element-property :language src-block)))
         (value (org-element-property :value src-block))
         ;; (name (org-element-property :name src-block))
         (header (if lang
                     (concat "{% highlight " lang " %}\n")
                   "```\n"))
         (footer (if lang "{% endhighlight %}" "```\n")))
    (concat
     header
     value
     footer
     contents)))

(defun repeat (x n)
  (let (acc)
    (dotimes (_ n acc)
      (push x acc))))

(defun org-github-headline (headline contents info)
  "Parse our headline from the given data, and prepend the necessary number of #s"
  (let ((value (org-element-property :raw-value headline))
        (level (org-element-property :level headline)))
    (concat (apply 'concat (repeat "#" level))
            " "
            value
            "\n"
            contents)))

(defun org-github-link (link contents info)
  (let ((path (org-element-property :raw-link link)))
    (format "[%s](%s)" contents path)))

(defun org-github-paragraph (paragraph contents info)
  contents)

(defun org-github-section (section contents info)
  contents)

(defun org-github-italic (text contents info)
  (format "*%s*" contents))

(defun org-github-bold (text contents info)
  (format "**%s**" contents))

(defun is-empty (s)
  (string= s ""))

(defun drop-while (f list)
  (cond ((null list) nil)
        ((funcall f (car list)) (drop-while f (cdr list)))
        (t list)))

(defun take-while (f list)
  (cond ((null list) nil)
        ((funcall f (car list)) (cons (car list)
                                      (take-while f (cdr list))))
        (t nil)))

(defun complement (f)
  (lexical-let ((f f))
    (lambda (&rest args)
      (not (apply f args)))))

(defun string-join (xs y)
  (mapconcat #'identity xs y))

(defun trim-empty-lines (s)
  (let ((lines (split-string s "\n")))
    (string-join
     (reverse (drop-while #'is-empty
                          (reverse (drop-while #'is-empty lines)))) "\n")))

(defun org-github-fixed-width (fixed-width contents info)
  (concat "```\n"
          (trim-empty-lines (org-element-property :value fixed-width))
          "\n```\n"))


(defun org-github-export-as-github
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (save-excursion 
    ;; find our first TODO state
    (while (null (org-entry-get (point) "TODO" nil t))
      (outline-up-heading 1 t))

    (setq yaml-date (format-time-string "%Y-%m-%d" (org-get-scheduled-time (point) nil)))
    (setq yaml-tags (mapconcat 'identity (org-get-tags-at) " "))
    (setq yaml-title (org-get-heading t t))
    (setq yaml-permalink (orgh:normalize-string yaml-title))
    (setq org-export-output-file-name (concat yaml-date "-" yaml-permalink))
      
    (let* ((extension ".md")
           (subtreep
            (save-restriction
              (org-narrow-to-subtree)
              (buffer-string)))
           ;; (file (concat (yaml-date "-" yaml-permalink extension subtreep))))
           (file (org-export-output-file-name extension subtreep)))
      (if async
          (org-export-async-start
              (lambda (output)
                (with-current-buffer (get-buffer-create "*Org Github Pages Export*")
                  (erase-buffer)
                  (insert output)
                  (goto-char (point-min))
                  (org-export-add-to-stack (current-buffer) 'github-pages)))
            `(org-export-as 'github-pages ,subtreep ,visible-only ,body-only ',ext-plist))
        (let ((outbuf (org-export-to-buffer 'github-pages
                          "*Org Github Pages Export*"
                        nil subtreep visible-only body-only ext-plist)))
          (with-current-buffer outbuf (set-auto-mode t))
          (when org-export-show-temporary-export-buffer
            (switch-to-buffer-other-window outbuf)))))))

(defun org-github-publish-to-github-pages (plist filename pub-dir)
  (org-publish-org-to 'github-pages filename ".md" plist pub-dir))

(defun make-org-publish-project-alist
    (name blog-root github-pages-root)
  (let ((github-posts (concat (file-name-as-directory github-pages-root)
                              "_posts")))
    `(("posts"
       :base-directory ,blog-root
       :base-extension "org"
       :publishing-directory ,github-posts
       :publishing-function org-github-publish-to-github-pages)
      (,name :components ("posts")))))



