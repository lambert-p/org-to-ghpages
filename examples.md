# Examples

The following are example outputs for this export mode. You can check the `Examples` subheading in `project.org` and the equivalent `examples.md` for the corresponding GitHub Flavored Markdown export. This file was generated merely by setting `org-github-use-src-plugin` and `org-github-include-yaml-front-matter` both to `nil` before exporting (due to GitHub not supporting Pygments and not
needing frontmatter!)

---

The usual **bolding** and *italicizing* works as well, as well as ~~strikethrough~~!

---

Headings:

## Headline 2

### Headline 3

#### Headline 4

---

Inline code: `sudo apt-get update && sudo apt-get upgrade`

Tagged code-block (with org-github-use-src-plugin set to nil):

```common-lisp
(defgroup org-export-github nil
  "Options for exporting org-mode files to Github Pages Markdown"
  :tag "Org GitHub Flavored Markdown"
  :group 'org-export
  :version "24.5.1")
```

---

For blockquotes:

> "Never trust anything you read on the internet &#x2013; especially if it
> is on GitHub, HackerNews, or Reddit." - Abraham Lincoln

---

Lists:

My favorite lists are (in this order):

1.  Ones that are succinct
2.  Ones that have some practical value
    -   Like showing off functionality
    -   Or, ideally, providing somewhat useful documentation
3.  Ones that eventually end
    -   The end is nigh!
    -   **Important value:** has some importance to somebody
