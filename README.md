# org-to-ghpages

This is a small emacs script for exporting org-mode TODO list subtrees to GitHub Flavored Markdown for usage on a Jekyll-powered blog or GitHub project. 

![emacs1](/emacs1.png?raw=true)

## Usage

Place `org-to-ghpages.el` somewhere in your Emacs' load path, and then add the following lines to your init.el:

```common-lisp
(require 'org-to-ghpages)
(setq org-ghpages-post-dir "~/location/of/jekyll/blog/_posts/")
```

(To see a literate example of how *I* use this, please refer to [my init file](https://github.com/lambertington/dotfiles/blob/master/emacs.d/lambert-config.org#external-scripts).)

By default, this library can be invoked by executing `C-c C-e g` from within org-mode on a `TODO` list item. Its default output is designed for GitHub Pages blogs, built upon Jekyll. Namely, it outputs a GitHub Flavored Markdown file to your specified `org-ghpages-post-dir` with smart guesses for YAML front matter data, with the file name of `YYYY-MM-DD-title-of-post.md`. 

### Custom Options

`org-ghpages-post-dir`: The directory to export posts to. By default, they will be output to your `~/Documents` directory.

```common-lisp
(defcustom org-ghpages-post-dir (expand-file-name "~/Documents")
  "directory to save posts"
  :group 'org-export-ghpages
  :type 'directory)
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

If you want to export just a GitHub Flavored Markdown post, i.e. a README, set this value to nil.

`M-x set-variable [RET] org-ghpages-include-yaml-front-matter [RET] nil [RET]`

```common-lisp
(defcustom org-ghpages-include-yaml-front-matter t
  "automatically generate YAML front matter?"
  :group 'org-export-ghpages
  :type 'boolean)
```

---

`org-ghpages-layout`: What the `layout` value in your YAML front matter should be. By default, it is set to "post".

```common-lisp
(defcustom org-ghpages-layout "post"
  "define each top level as a post by default"
  :group 'org-export-ghpages
  :type 'string)
```

---

`org-ghpages-comments`: A boolean to indicate whether you want to include Disqus comments. This choice is reflected in the YAML front matter.

```common-lisp
(defcustom org-ghpages-comments t
  "include disqus comments by default"
  :group 'org-export-ghpages
  :type 'boolean)
```

---

`org-ghpages-use-src-plugin`: A boolean to indicate whether you want source blocks surrounded with Pygments-style tags. 
-   If set to `t` (which is the default), your source blocks will be wrapped with `{% highlight lang %} / {% endhighlight %}` tags, where the value of `lang` is taken from your `#+BEGIN_SRC` declarations.
-   If set to `nil`, it will wrap source blocks with the triple backquotes tags, including `lang` if it is available (again, taken from your `#+BEGIN_SRC` blocks.

```common-lisp
(defcustom org-ghpages-use-src-plugin t
  "if true, uses pygments-style code blocking"
  :group 'org-export-ghpages
  :type 'boolean)
```

### Sensible defaults

Aside from having to set up your default export path (which is used when you use `C-c C-e g g`, or manually invoke `org-ghpages-export-to-gfm`), all of the default settings will work for exporting Jekyll/gh-pages blog posts. 

If you're trying to just export to GitHub Flavored Markdown for use in GitHub Pages (such as creating READMEs for projects), please use:

```common-lisp
(setq org-ghpages-include-yaml-front-matter nil)
(setq org-ghpages-use-src-block-plugin nil)
```

## Examples

To see how this is used, please look at `project.org` which is in this repository. I generated both the `README.md` and `examples.md` files aginst this file.

## Requirements

This is built on top of the `ox-md` backend, which requires `org-mode` of at least version 8. I have only tested this with GNU Emacs 24.5.1, but it should work fine with versions 23+.

## Help!

-   If you're getting nasty Table of Contents HTML output with your files, make sure you have `#+OPTIONS: toc:nil` towards the top of your org file.
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
