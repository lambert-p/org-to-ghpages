# org-to-ghpages README

This is a small emacs script for exporting org-mode TODO list subtrees to GitHub Flavored Markdown for usage on a Jekyll-powered blog or GitHub project. 

## A Visual Guide

Let's say we have all of our blog posts as `TODO` items in an org file: 
![img](https://raw.githubusercontent.com/lambertington/org-to-ghpages/master/images/emacs1.png)

This has practical value, as we can get automatic reminders (using org-agenda) about writing and quickly refactor drafts:
![img](https://raw.githubusercontent.com/lambertington/org-to-ghpages/master/images/emacs2.png)

Once we're done, we export to GitHub Flavored Markdown for posting to our Jekyll blog! We can either use `M-x org-to-ghposts-as-gfm` or use the exporting menu by hitting `C-c C-e g G` to put it in a temp buffer&#x2026;
![img](https://raw.githubusercontent.com/lambertington/org-to-ghpages/master/images/emacs3.png)

(An astute viewer will notice that the YAML front matter has been automatically filled out, with the publishing date being the `SCHEDULED` date, the categories being taken from org-mode tags, and the permalink being the title of the post in a Web-friendly way.)

Or we can just hit `C-c C-e g g` (alternatively, `M-x org-ghpages-export-to-gfm`) to export it directly to our blog (assuming we've set up our default directory properly):
![img](https://raw.githubusercontent.com/lambertington/org-to-ghpages/master/images/emacs4.png)

And bam, that's all. From here you can run your blog locally to make sure everything's peachy, or just `git push` off to victory!

Easy peasy.

## Quick Start

Place `src/org-to-ghpages.el` somewhere in your Emacs' load path, and then add the following lines to your init.el:

```common-lisp
(require 'org-to-ghpages)
(setq org-ghpages-post-dir "~/location/of/jekyll/blog/_posts/")
```

(To see a literate example of how I use this, please refer to [my init file](https://github.com/lambertington/dotfiles/blob/master/emacs.d/lambert-config.org#external-scripts).)

By default, this library can be invoked by executing `C-c C-e g` from within org-mode on a `TODO` list item. Its default output is designed for GitHub Pages blogs, built upon Jekyll. Namely, it outputs a GitHub Flavored Markdown file to your specified `org-ghpages-post-dir` with smart guesses for YAML front matter data, with the file name of `YYYY-MM-DD-title-of-post.md`. 

## Custom Options

`org-ghpages-post-dir`: The directory to export posts to. By default, they will be output to your `~/Documents` directory.

```common-lisp
(defcustom org-ghpages-post-dir (expand-file-name "~/Documents")
  "directory to save posts"
  :group 'org-export-ghpages
  :type 'directory)
```

---

`org-ghpages-export-to-jekyll`: Automatically sets options for exporting to Jekyll, such as including YAML frontmatter and Pygments-style source code coloring. It is set to true by default. 

If you plan on exporting just to straight GitHub Flavored Markdown, such as for generating README files or project documentation, set this variable to `nil` (e.g., `M-x [RET] set-variable [RET] org-ghpages-export-to-jekyll [RET] nil [RET]`), or specify it in your init file.

```common-lisp
(defcustom org-ghpages-export-to-jekyll t
  "By default, export will be configured for use with 
Jekyll and the gh-pages gem. In particular, include
YAML front matter by default and use Pygments style 
highlighting"
  :group 'org-export-ghpages
  :type 'boolean)

```

---

`org-ghpages-include-yaml-front-matter`: A boolean to determine whether you want to include YAML front matter. By default, values will include:

```yaml
layout: post
title: The TODO Text
date: date specified by the `Scheduled` agenda date for this TODO item
comments: true
categories: tags taken from org-mode
permalink: the-todo-text
```

This is set to true by default.

```common-lisp
(defvar org-ghpages-include-yaml-front-matter t
  "Automatically generate YAML front matter? Set variable
to `nil' if not exporting to Jekyll (e.g., generating 
project notes or a README")

```

---

`org-ghpages-layout`: What the `layout` value in your YAML front matter should be. By default, it is set to "post".

```common-lisp
(defcustom org-ghpages-layout "post"
  "Define each top level as a post by default. Used when
generating YAML front matter."
  :group 'org-export-ghpages
  :type 'string)

```

---

`org-ghpages-comments`: A boolean to indicate whether you want to include Disqus comments. This choice is reflected in the YAML front matter.

```common-lisp
(defvar org-ghpages-comments t
  "Include Disqus comments by default. Used when 
generating YAML front matter.")

```

---

`org-ghpages-use-src-plugin`: A boolean to indicate whether you want source blocks surrounded with Pygments-style tags. 
-   If set to `t` (which is the default), your source blocks will be wrapped with `{% highlight lang %} / {% endhighlight %}` tags, where the value of `lang` is taken from your `#+BEGIN_SRC` declarations.
-   If set to `nil`, it will wrap source blocks with the triple backquotes tags, including `lang` if it is available (again, taken from your `#+BEGIN_SRC` blocks).

```common-lisp
(defcustom org-ghpages-use-src-plugin t
  "If true, uses pygments-style code blocking. If not 
exporting to Pygments, e.g. generating project notes 
or a README, set value to `nil'.")

```

---

`org-ghpages-auto-mark-as-done`: A boolean to indicate whether exporting (either to temp buffer or directly to GitHub Flavored Markdown file should change status from TODO to DONE. By default, set to true.

```common-lisp
(defcustom org-ghpages-auto-mark-as-done t
  "If true, automatically changes TODO state to DONE state upon exporting")

```

## Sensible defaults

Aside from having to set up your default export path (which is used when you use `C-c C-e g g`, or manually invoke `org-ghpages-export-to-gfm`), all of the default settings will work for exporting Jekyll/gh-pages blog posts. 

If you're trying to just export to GitHub Flavored Markdown for use in GitHub Pages (such as creating READMEs for projects), please use:

```common-lisp
(setq org-ghpages-export-to-jekyll nil)
```

## Examples

To see a practical example of how this is used, please look at `project.org` which is in this repository. I generated both the `README.md` and `examples.md` files aginst this file.

## Requirements

This is built on top of the `ox-md` backend, which requires `org-mode` of at least version 8. I have only tested this with GNU Emacs 24.5.1, but it should work fine with versions 23+.

## Help!

-   If you're getting nasty Table of Contents HTML output with your files, make sure you have `#+OPTIONS: toc:nil` towards the top of your org file.
-   If ~~strikethrough~~ isn't showing up on Jekyll, make sure you edit your \_config.yml to include

```yaml
markdown: redcarpet
redcarpet:
  extensions: ["strikethrough"]
```

-   Other issues? Contact me [via Twitter](https://twitter.com/lambertington), or create an issue on this repo.

## License

Copyright (C) 2015 Paul Lambert

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
