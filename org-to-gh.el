(require 'ox)

(defgroup org-export-ghpages nil
  "Options for exporting org-mode files to Github Pages Markdown"
  :tag "Org Github Pages"
  :group `org-export)

(defcustom org-ghpages-post-dir (expand-file-name "~/code/lambertington.github.io/_posts")
  "directory to save posts"
  :group 'org-export-ghpages
  :type 'directory)

(defcustom org-ghpages-include-yaml-front-matter t
  "automatically generate YAML front matter?"
  :group 'org-export-ghpages
  :type 'boolean)

(defcustom org-ghpages-layout "post"
  "define each top level as a post by default"
  :group 'org-export-ghpages
  :type 'string)

(defcustom org-ghpages-categories ""
  "categories should be space-separated"
  :group 'org-export-ghpages
  :type 'string)

(defcustom org-ghpages-comments t
  "include disqus comments by default"
  :group 'org-export-ghpages
  :type 'boolean)

(defcustom org-ghpages-use-src-plugin t
  "if true, uses pygments-style code blocking"
  :group 'org-export-ghpages-use-src-plugin
  :type 'boolean)

(defvar *org-ghpages-pygments-langs*
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

(org-export-define-derived-backend 'ghpages 'md
    :translate-alist
    '((src-block . org-ghpages-src-block)
      (template . org-ghpages-template)
      (inner-template . org-ghpages-inner-template))
    :options-alist
    '((:ghpages-layout "GHPAGES_LAYOUT" nil org-ghpages-layout)
      (:ghpages-categories "GHPAGES_CATEGORIES" nil org-ghpages-categories)
      (:ghpages-comments "GHPAGES_COMMENTS" nil org-ghpages-comments)))

(defun get-lang (lang)
  (and lang
       (let ((lang (orgh:normalize-string lang)))
         (cond ((string= lang "emacs-lisp") "common-lisp")
               ((not (member lang *org-ghpages-pygments-langs*)) nil)
               (t lang)))))


(defun org-ghpages-src-block (src-block contents info)
  "Transcode a #+BEGIN_SRC block from Org to Github Pages style"
  (if org-ghpages-use-src-plugin
      (let* ((lang (get-lang (org-element-property :language src-block)))
             (value (org-element-property :value src-block))
             (header (if lang
                         (concat "{% highlight " lang " %}\n")
                       "```\n"))
             (footer (if lang "{% endhighlight %}" "```\n")))
        (concat
         header
         value
         footer
         contents))
    (org-export-with-backend 'html src-block contents info)))

(defun org-ghpages-template (contents info)
  "return the complete document after markdown conversion"
  (if org-ghpages-include-yaml-front-matter
      (concat (org-ghpages-yaml-front-matter info)
              contents)
    contents))

(defun org-ghpages-inner-template (contents info)
  "return the body of the document after markdown conversion"
  (org-md-export-as-markdown contents))

(defun org-ghpages-normalize-string (str)
  (downcase (replace-regexp-in-string " " "-" str)))

(defun org-ghpages-export ()
  "export this current subtree to our blog as a GitHub Pages compliant markdown post
with YAML front-matter"
  (interactive)
  (save-excursion
    ;; find our first TODO state
    (while (null (org-entry-get (point) "TODO" nil t))
      (outline-up-heading 1 t))
    (let* ((date (format-time-string "%Y-%m-%d" (org-get-scheduled-time (point) nil)))
           (tags (org-get-tags-at))
           (title (org-get-heading t t))
           (permalink (org-ghpages-normalize-string title)))
      (message "date is: %S" date)
      (message "tags are: %S" tags)
      (message "title is: %s" title)
      (message "permalink is: %S" permalink))))
    
  

