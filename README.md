# org-to-ghpages README

This is a small emacs script for exporting org-mode TODO list subtrees to GitHub Flavored Markdown for usage on a Jekyll-powered blog or GitHub project. 

## Quick Start

Place `src/org-to-ghpages.el` somewhere in your Emacs' load path, and then add the following lines to your init.el:

```common-lisp
(require 'org-to-ghpages)
(setq org-ghpages-post-dir "~/location/of/jekyll/blog/_posts/")
```

(To see a literate example of how I use this, please refer to [my init file](https://github.com/lambertington/dotfiles/blob/master/emacs.d/lambert-config.org#external-scripts).)

By default, this library can be invoked by executing `C-c C-e g` from within org-mode on a `TODO` list item. 

This extension supports both Jekyll posts using the `gh-pages` gem and `Pygments` extension, and exporting plain GitHub Flavored Markdown for creating project documentation.

When exporting as Jekyll, the file will automatically be named `YYYY-MM-DD-title-of-post.md` and placed within your specified post directory. It will also include automatically generated YAML front matter.

When exporting as GitHub Flavored Markdown, the file will be named `title-of-post.md` and use standard triple-backtick source highlighting.

## A Visual Guide

Let's say we have all of our blog posts as `TODO` items in an org file: 
![img](https://raw.githubusercontent.com/lambertington/org-to-ghpages/master/images/emacs1.png)

This has practical value, as we can get automatic reminders (using org-agenda) about writing and quickly refactor drafts:
![img](https://raw.githubusercontent.com/lambertington/org-to-ghpages/master/images/emacs2.png)

Once we're done, we export to GitHub Flavored Markdown for posting to our Jekyll blog! 

Access the org-mode export menu (by default, `C-c C-e`) and selecting the *Export to GitHub Flavored Markdown* section by hitting `g`. We can put this in a temp buffer by now hitting `J` (or, alternatively, directly call `M-x org-ghpages-export-as-jekyll`):
![img](https://raw.githubusercontent.com/lambertington/org-to-ghpages/master/images/emacs3.png)

(An astute viewer will notice that the YAML front matter has been automatically filled out, with the publishing date being the `SCHEDULED` date, the categories being taken from org-mode tags, and the permalink being the title of the post in a Web-friendly way.)

Or we can just hit `C-c C-e g j` (alternatively, `M-x org-ghpages-export-to-jekyll`) to export it directly to our blog (assuming we've set up our default directory properly):
![img](https://raw.githubusercontent.com/lambertington/org-to-ghpages/master/images/emacs4.png)

And bam, that's all. From here you can run your blog locally to make sure everything's peachy, or just `git push` off to victory!

Easy peasy.

## Sensible defaults

By default, the only variable that needs to be set is `org-ghpages-post-dir` &#x2013; everything else will be automatically handled for you. 

When exporting as Jekyll, YAML frontmatter will be prepended to the actual post data, and include `title`, `layout`, `date`, `comments`, `categories`, and `permalink` attributes. The `date` data is acquired from your org-agenda scheduled date. Further, Pygments-style source hightlighting will be generated, using the lang specified from your org-mode `#+BEGIN_SRC` blocks. 

When exporting as GitHub Flavored Markdown, merely the post itself will be exported, using the appropriate triple-backtick source hightlighting scheme. Again, the lang will be specified from your org-mode `#+BEGIN_SRC` blocks.

Essentially, everything should work fine out of the box.

## Custom Options

`org-ghpages-post-dir`: The directory to export posts to. By default, they will be output to your `~/Documents` directory.

```common-lisp
(defcustom org-ghpages-post-dir (expand-file-name "~/Documents")
  "directory to save posts"
  :group 'org-export-ghpages
  :type 'directory)
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
(defcustom org-ghpages-comments t
  "Include Disqus comments by default. Used when 
generating YAML front matter."
  :group 'org-export-ghpages
  :type 'boolean)
```

---

`org-ghpages-auto-mark-as-done`: A boolean to indicate whether exporting (either to temp buffer or directly to GitHub Flavored Markdown file should change status from TODO to DONE. By default, set to true.

```common-lisp
(defcustom org-ghpages-auto-mark-as-done t
  "If true, automatically changes TODO state to DONE state upon exporting"
  :group 'org-export-ghpages
  :type 'boolean)
```

## Examples

To see a practical example of how this is used, please look at `project.org` which is in this repository. I generated both the `README.md` and `Examples.md` files aginst this file.

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
