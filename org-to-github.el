;;; org-to-github.el - GitHub Flavored Markdown for use with Jekyll

;; Author: Paul Lambert <lambertington@gmail.com>
;; Copyright (C) 2015 Paul Lambert
;; Please consult the included license.txt


;;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)


;;;; User config variables

(defgroup org-export-github nil
  "Options for exporting org-mode files to Github Pages Markdown"
  :tag "Org GitHub Flavored Markdown"
  :group `org-export
  :version "24.5.1")

(defcustom org-github-post-dir (expand-file-name "~/code/lambertington.github.io/_posts")
  "directory to save posts"
  :group 'org-export-github
  :type 'directory)

(defcustom org-github-include-yaml-front-matter t
  "automatically generate YAML front matter?"
  :group 'org-export-github
  :type 'boolean)

(defcustom org-github-layout "post"
  "define each top level as a post by default"
  :group 'org-export-github
  :type 'string)

(defcustom org-github-categories ""
  "categories should be space-separated"
  :group 'org-export-github
  :type 'string)

(defcustom org-github-comments t
  "include disqus comments by default"
  :group 'org-export-github
  :type 'boolean)

(defcustom org-github-use-src-plugin t
  "if true, uses pygments-style code blocking"
  :group 'org-export-github-use-src-plugin
  :type 'boolean)

(defun org-github-normalize-string (str)
  "Makes strings HTML-friendly for use in URLs"
  (downcase (replace-regexp-in-string " " "-" str)))

(defvar *org-github-pygments-langs*
  (mapcar #'org-github-normalize-string
          '("actionscript" "ada" "antlr" "applescript" "assembly" "asymptote" "awk"
            "befunge" "boo" "brainfuck" "c" "c++" "c#" "clojure" "coffeescript"
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

;;; Define back-end

(org-export-define-derived-backend 'github-pages 'md
  :export-block '("MD" "GITHUB")
  :menu-entry
  '(?g "Export to GitHub Flavored Markdown"
       ((?G "To temporary buffer"
            (lambda (a s v b) (org-github-export-as-gfm a s v)))
        (?g "To file" (lambda (a s v b) (org-github-export-to-gfm a s v)))))
  :translate-alist
  '((src-block . org-github-src-block)
    (template . org-github-template)
    (italic . org-github-italic)
    (headline . org-github-headline)))

(defun org-github-template (contents info)
  "Accepts the final transcoded string and a plist of export options,
returns the final string with YAML frontmatter prepended"
  (let ((frontmatter
         "---
layout: post
title: %s
date: %s
comments: true
categories: %s
permalink: %s
---\n"))
    (if org-github-include-yaml-front-matter
        (concat (format frontmatter yaml-title yaml-date yaml-tags yaml-permalink) contents)
      contents)))

(defun org-github-get-pygments-lang (lang)
  (and lang
       (let ((lang (org-github-normalize-string lang)))
         (cond ((string= lang "emacs-lisp") "common-lisp")
               ((string= lang "shell-script") "bash")
               ((not (member lang *org-github-pygments-langs*)) nil)
               (t lang)))))

(defun org-github-src-block (src-block contents info)
  "Transcode a #+BEGIN_SRC block from Org to Github Pages style"
  (let* ((lang (org-github-get-pygments-lang (org-element-property :language src-block)))
         (value (org-element-property :value src-block))
         ;; (name (org-element-property :name src-block))
         (header (if lang
                     (concat "{% highlight " lang " %}\n")
                   "```\n"))
         (footer (if lang "{% endhighlight %}" "```\n")))
    (concat header value footer contents)))

(defun org-github-italic (italic contents info)
  "Transcode ITALIC object into Github Flavored Markdown format.
CONTENTS is the text within italic markup. 
INFO is a plist used as a communication channel."
  (format "*%s*" contents))

(defun org-github-headline (headline contents info)
  "Transcode HEADLINE element into GitHub Flavored Markdown.
Please consult ./lisp/org/ox-md.el.gz for additional documentation."
  (concat "#" (org-md-headline headline contents info)))

;;; Interactive functions

;;;###autoload
(defun org-github-export-as-gfm
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a GitHub Flavored Markdown buffer."
  
  (interactive)
  (save-excursion
    ;; find our first TODO state
    (while (null (org-entry-get (point) "TODO" nil t))
      (outline-up-heading 1 t))

    (setq yaml-date (format-time-string "%Y-%m-%d" (org-get-scheduled-time (point) nil)))
    (setq yaml-tags (mapconcat 'identity (org-get-tags-at) " "))
    (setq yaml-title (org-get-heading t t))
    (setq yaml-permalink (org-github-normalize-string yaml-title))
    (setq org-export-output-file-name (concat yaml-date "-" yaml-permalink))

    (let* ((extension ".md")
           (subtreep
            (save-restriction
              (org-narrow-to-subtree)
              (buffer-string)))
           ;; (file (org-export-output-file-name extension subtreep)))
           )
      (if async
          (org-export-async-start
              (lambda (output)
                (with-current-buffer (get-buffer-create "*Org Github Pages Export*")
                  (erase-buffer)
                  (insert output)
                  (goto-char (point-min))
                  (org-export-add-to-stack (current-buffer) 'github-pages)))
            `(org-export-as 'github-pages ,subtreep ,visible-only ,body-only ',ext-plist))
        (let ((outbuf (org-export-to-buffer 'github-pages "*Org Github Pages Export*"
                        nil subtreep visible-only body-only ext-plist)))
          (with-current-buffer outbuf (set-auto-mode t)))))))

;;;###autoload
(defun org-github-export-to-gfm
    (&optional async subtreep visible-only body-only ext-plist)
  "Export to file"
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'github-pages outfile async subtreep visible-only body-only ext-plist)))

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

(provide 'org-to-github)
