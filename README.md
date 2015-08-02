

Literate programming is **awesome**! 

## Usage

Place `org-to-github.el` somewhere in your emacs' init file's load path, and then add the following lines to your init.el:

```common-lisp
(load-file "~/code/dotfiles/emacs.d/lisp/org-to-github.el")
(setq org-publish-project-alist
      (make-org-publish-project-alist
       "blog"
       "~/org/"
       "~/code/lambertington.github.io"))
(org-publish-project "blog" t)                             
```

Make sure you use `#+OPTIONS: toc:nil` to turn off exporting the ugly table of contents stuff.

### Custom Options

There's a bunch of vars to set. Umm. I need to implement their functionality first, and then I'll have better documentation.
