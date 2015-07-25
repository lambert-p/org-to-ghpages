;;; org-to-github.el
;;; Author: Paul Lambert <lambertington@gmail.com>

(require 'ox)

(defvar *org-github-yaml-front-matter* t)

(defun orgh:normalize-lang (str)
  (downcase (replace-regexp-in-string " " "-" str)))

(defvar *org-github-pygments-langs*
  (mapcar #'orgh:normalize-lang
          '("SML" "ActionScript" "Ada" "ANTLR" "AppleScript"
            "Assembly" "Asymptote" "Awk" "Befunge" "Boo"
            "BrainFuck" "C" "C++" "C#" "Clojure" "CoffeeScript"
            "ColdFusion" "Common Lisp" "Coq" "Cython" "D"
            "Dart" "Delphi" "Dylan" "Erlang" "Factor" "Fancy"
            "Fortran" "F#" "Gherkin" "GL shaders" "Groovy"
            "Haskell" "IDL" "Io" "Java" "JavaScript" "LLVM"
            "Logtalk" "Lua" "Matlab" "MiniD" "Modelica"
            "Modula-2" "MuPad" "Nemerle" "Nimrod" "Objective-C"
            "Objective-J" "Octave" "OCaml" "PHP" "Perl" "PovRay"
            "PostScript" "PowerShell" "Prolog" "Python" "Rebol"
            "Redcode" "Ruby" "Rust" "S" "S-Plus" "R" "Scala"
            "Scheme" "Scilab" "Smalltalk" "SNOBOL" "Tcl"
            "Vala" "Verilog" "VHDL" "Visual Basic.NET"
            "Visual FoxPro" "XQuery")))

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
  (let ((title (plist-get info :title))
        (date (car (plist-get info :date)))
        (time "")
        (frontmatter
         "---
layout: post
title: %s
date: %s %s
comments: true
categories:
permalink:
---
"))
    (if *org-github-yaml-front-matter*
        (concat (format frontmatter title date time) contents)
      contents)))

(defun get-lang (lang)
  (and lang
       (let ((lang (orgh:normalize-lang lang)))
         (cond ((string= lang "emacs-lisp") "common-lisp")
               ((not (member lang *org-github-pygments-langs*)) nil)
               (t lang)))))

(defun org-github-src-block (src-block contents info)
  "Transcode a #+BEGIN_SRC block from Org to Github Pages style"
  (let* ((lang (get-lang (org-element-property :language src-block)))
         (value (org-element-property :value src-block))
         (name (org-element-property :name src-block))
         (header
          ;; backtick code blocks support lang or lang and name, but not name alone
          (cond ((and lang name)
                 (concat "```" lang " " name "\n"))
                (lang
                 (concat "```" lang "\n"))
                (t "{% codeblock %}\n")))
         (footer (if lang "```\n" "{% endcodeblock %}")))
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
  (if async
      (org-export-async-start
          (lambda (output)
            (with-current-buffer (get-buffer-create "*Org Github Pages Export*")
              (erase-buffer)
              (insert output)
              (goto-char (point-min))
              (org-export-add-to-stack (current-buffer) 'github-pages)))


