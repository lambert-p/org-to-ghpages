This is a small emacs script for exporting org-mode files to github flavored markdown for usage on a Jekyll-powered blog or GitHub project . To get started, place org-to-github.el somewhere in your load path, and update your emacs.d to reference it. 

Literate programming is **awesome**! 

## Usage

Place `org-to-github.el` somewhere in your emacs' init file's load path, and then add the following lines to your init.el:

```common-lisp
(require 'org-to-ghpages)
(setq org-ghpages-post-dir "~/location/of/jekyll/blog/_posts/")
```

(To see a literate example of how *I* use this, please refer to [my init file](https://github.com/lambertington/dotfiles/blob/master/emacs.d/lambert-config.org#external-scripts).)

Make sure you use `#+OPTIONS: toc:nil` to turn off exporting the ugly table of contents stuff.

### Custom Options

There's a bunch of vars to set. Umm. I need to implement their functionality first, and then I'll have better documentation.
