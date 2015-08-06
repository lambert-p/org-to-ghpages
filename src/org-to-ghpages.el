;;; org-to-ghpages.el - GitHub Flavored Markdown Export for use with GitHub Pages/Jekyll

;; Copyright (C) 2015 Paul Lambert

;; Author: Paul Lambert <lambertington at gmail dot com>
;; Keywords: github, markdown, org
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a GitHub Flavored Markdown backend for the
;; Org exporter, and is built on top of the `ox-md` backend.
;;
;; By default, this library can be invoked by executing C-c C-e g
;; from within org-mode. Its default output is designed for
;; GitHub Pages blogs, built upon Jekyll.
;;
;; Through configuration, it can also be used for publishing standard
;; GitHub Flavored Markdown, useful for GitHub Pages (such as generating
;; README.md's for a GitHub's project.)
;;
;; For more complete documentation, including usage, please refer to
;; either the included README.md or the project's repository, located
;; at https://github.com/lambertington/org-to-ghpages/

;;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-md)


;;;; User config variables

(defgroup org-export-ghpages nil
  "Options for exporting org-mode files to Github Pages Markdown"
  :tag "Org GitHub Flavored Markdown"
  :group 'org-export
  :version "24.5.1")

(defcustom org-ghpages-post-dir (expand-file-name "~/Documents")
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

(defcustom org-ghpages-comments t
  "include disqus comments by default"
  :group 'org-export-ghpages
  :type 'boolean)

(defcustom org-ghpages-use-src-plugin t
  "if true, uses pygments-style code blocking"
  :group 'org-export-ghpages
  :type 'boolean)

(defcustom org-ghpages-auto-mark-as-done t
  "if true, automatically changes TODO state to DONE state upon exporting"
  :group 'org-export-ghpages
  :type 'boolean)


;;; Helper functions

(defun org-ghpages-normalize-string (str)
  "Makes strings HTML-friendly for use in URLs"
  (downcase (replace-regexp-in-string " " "-" str)))

(defun org-ghpages-get-file-name ()
  "Returns our post's file name, created by using our YAML front matter
if it exists; else we default to README.md"
  (if org-ghpages-include-yaml-front-matter
      (concat org-ghpages-post-dir "/" yaml-date "-" yaml-permalink ".md")
    (concat org-ghpages-post-dir "/" yaml-permalink ".md")))

(defvar *org-ghpages-pygments-langs*
  (mapcar #'org-ghpages-normalize-string
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
            (lambda (a s v b) (org-ghpages-export-as-gfm a s v)))
        (?g "To file" (lambda (a s v b) (org-ghpages-export-to-gfm a s v)))))
  :translate-alist
  '((src-block . org-ghpages-src-block)
    (template . org-ghpages-template)
    (strike-through . org-ghpages-strike-through)
    (italic . org-ghpages-italic)
    (headline . org-ghpages-headline)))

;;; Transcode 

(defun org-ghpages-template (contents info)
  "Return complete document string after conversion."
  (let ((yaml-comments (if org-ghpages-comments "true" "false"))
        (frontmatter
         "---
layout: %s
title: %s
date: %s
comments: %s
categories: %s
permalink: %s
---\n\n"))
    (if org-ghpages-include-yaml-front-matter
        (concat (format frontmatter org-ghpages-layout yaml-title yaml-date yaml-comments yaml-tags yaml-permalink) contents)
      (concat "# " yaml-title "\n\n" contents))))

(defun org-ghpages-get-pygments-lang (lang)
  "Determine whether our SRC-BLOCK data is in a language supported
by Pygments coloring. Otherwise revert to default coloring behavior."
  (and lang
       (let ((lang (org-ghpages-normalize-string lang)))
         (cond ((string= lang "emacs-lisp") "common-lisp")
               ((string= lang "shell-script") "bash")
               ((not (member lang *org-ghpages-pygments-langs*)) nil)
               (t lang)))))

(defun org-ghpages-src-block (src-block contents info)
  "Transcode a #+BEGIN_SRC block from Org to Github Pages style
Please consult ./lisp/org/ox-md.el.gz for additional documentation."
  (let* ((lang (org-ghpages-get-pygments-lang (org-element-property :language src-block)))
         (value (org-element-property :value src-block))
         (header (if lang
                     (if org-ghpages-use-src-plugin
                         (concat "{% highlight " lang " %}\n")
                       (concat "```" lang "\n"))
                   "```\n"))
         (footer (if lang
                     (if org-ghpages-use-src-plugin
                         "{% endhighlight %}\n"
                       "```\n")
                   "```\n")))
    (concat header value footer contents)))

(defun org-ghpages-italic (italic contents info)
  "Transcode ITALIC object into Github Flavored Markdown format.
Please consult ./lisp/org/ox-md.el.gz for additional documentation."
  (format "*%s*" contents))

(defun org-ghpages-headline (headline contents info)
  "Transcode HEADLINE element into GitHub Flavored Markdown.
Please consult ./lisp/org/ox-md.el.gz for additional documentation."
  (concat "#" (org-md-headline headline contents info)))

(defun org-ghpages-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH into GitHub Flavored Markdown.
Please consult ./lisp/org/ox-md.el.gz for additional documentation."
  (format "~~%s~~" contents))

;;; Interactive functions

;;;###autoload
(defun org-ghpages-export-as-gfm
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a GitHub Flavored Markdown buffer."
  
  (interactive)
  (save-excursion
    ;; find our first TODO state

    (while (null (org-entry-get (point) "TODO" nil t))
      (outline-up-heading 1 t))
    (if org-ghpages-auto-mark-as-done
        (org-todo 'done))

    ;; extract our YAML for creating the frontmatter
    (setq yaml-date (format-time-string "%Y-%m-%d" (org-get-scheduled-time (point) nil)))
    (setq yaml-tags (mapconcat 'identity (org-get-tags-at) " "))
    (setq yaml-title (org-get-heading t t))
    (setq yaml-permalink (org-ghpages-normalize-string yaml-title))

    (let* ((extension ".md")
           (subtreep
            (save-restriction
              (org-narrow-to-subtree)
              (buffer-string))))
      (let ((outbuf (org-export-to-buffer 'github-pages "*Org Github Pages Export*"
                      nil subtreep visible-only body-only ext-plist)))
        (with-current-buffer outbuf (set-auto-mode t))))))
        ;; (with-current-buffer outbuf (set-mode markdown-mode))))))

;;;###autoload
(defun org-ghpages-export-to-gfm
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to file in GitHub Flavored Markdown format"

  (interactive)
  (save-excursion
    ;; find our first TODO state

    (while (null (org-entry-get (point) "TODO" nil t))
      (outline-up-heading 1 t))

    (if org-ghpages-auto-mark-as-done
        (org-todo 'done))

    ;; extract our YAML for creating the frontmatter
    (setq yaml-date (format-time-string "%Y-%m-%d" (org-get-scheduled-time (point) nil)))
    (setq yaml-tags (mapconcat 'identity (org-get-tags-at) " "))
    (setq yaml-title (org-get-heading t t))
    (setq yaml-permalink (org-ghpages-normalize-string yaml-title))

    (let* ((extension ".md")
           (subtreep
            (save-restriction
              (org-narrow-to-subtree)
              (buffer-string))))
      (let ((org-export-coding-system org-html-coding-system))
        (org-export-to-file 'github-pages (org-ghpages-get-file-name) async subtreep
                            visible-only body-only ext-plist)))))


;;;; End code

(provide 'org-to-ghpages)
