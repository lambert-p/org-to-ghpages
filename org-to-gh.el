(require 'ox)
(require 'ox-md)

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

(defun org-ghpages-normalize-string (str)
  (downcase (replace-regexp-in-string " " "-" str)))

(defvar *org-ghpages-pygments-langs*
  (mapcar #'org-ghpages-normalize-string
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
       (let ((lang (org-ghpages-normalize-string lang)))
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
  (message "%S" (org-ghpages-yaml-front-matter info))
  (if org-ghpages-include-yaml-front-matter
      (concat (org-ghpages-yaml-front-matter info)
              contents)
    contents))

(defun org-ghpages-inner-template (contents info)
  "return the body of the document after markdown conversion"
  (org-md-export-as-markdown contents))

;;; YAML Front Matter
(defun org-ghpages-get-option (info property-name &optional default)
  (let ((property (org-export-data (plist-get info property-name) info)))
    (format "%s" (or property default ""))))

(defun org-ghpages-yaml-front-matter (info)
  (let ((title
         (org-ghpages-get-option info :title))
        (date
         (org-ghpages-get-option info :date))
        (layout
         (org-ghpages-get-option info :ghpages-layout org-ghpages-layout))
        (categories
         (org-ghpages-get-option info :ghpages-categories org-ghpages-categories)))
    (org-ghpages-yaml-front-matter-helper title date categories title)))

(defun org-ghpages-yaml-front-matter-helper (title date tags permalink)
  (concat
   "---"
   "\nlayout: post"
   "\ntitle: \"" title
   "\"\ndate: " date
   "\ncomments: true"
   "\ntags: " tags
   "\npermalink: \"" permalink
   "\"\n---\n"))

(defun org-ghpages-prepare-input-buffer (header content reference-buffer)
  "Insert content and clean it up a bit"
  (insert header content)
  (goto-char (point-min))
  (org-mode)
  (outline-next-heading)
  )

(defun org-ghpages-export-subtree-as-md
    (&optional async visible-only body-only ext-plist)
  "export current subtree to a HTML buffer adding the YAML front-matter"
  (interactive))

(defun org-ghpages-export-as-md
    (&optional async subtreep visible-only body-only ext-plist)
  "export current buffer to a HTML buffer adding the YAML front-matter"
  (interactive)
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org GitHub Pages MD Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (funcall org-html-display-buffer-mode);AHH WTF
              (org-export-add-to-stack (current-buffer) 'ghpages)))
        (org-export-as 'ghpages ,subtreep ,visible-only, body-only ',ext-plist))
    (let ((outbuf (org-export-to-buffer
                      'ghpages "*Org GitHubPages MD Export*"
                    nil subtreep visible-only body-only ext-plist)))
      (with-current-buffer outbuf (set-auto-mode t))
      (if org-export-show-temporary-export-buffer
          (switch-to-buffer-other-window outbuf)
        outbuf))))

(defun org-ghpages-export ()
  "export this current subtree to our blog as a GitHub Pages compliant markdown post
with YAML front-matter"
  (interactive)
  (save-excursion
    ;; find our first TODO state
    (while (null (org-entry-get (point) "TODO" nil t))
      (outline-up-heading 1 t))
    (let* ((date (format-time-string "%Y-%m-%d" (org-get-scheduled-time (point) nil)))
           (tags (mapconcat 'identity (org-get-tags-at) " "))
           (title (org-get-heading t t))
           (permalink (org-ghpages-normalize-string title))
           (subtree-content
            (save-restriction
              (org-narrow-to-subtree)
              (buffer-string)))
           (reference-buffer (current-buffer)))
      (setq yaml (org-ghpages-yaml-front-matter-helper title date tags permalink))
      ;; (message "%S" yaml)
      ;; (message "%S" subtree-content)
      (with-temp-buffer
        (org-ghpages-prepare-input-buffer yaml subtree-content reference-buffer)
        (org-ghpages-export-as-md nil t nil nil)
        (with-current-buffer "*Org GitHub Pages to md Export*"
          (goto-char (point-min))))
      )
    )
  )

