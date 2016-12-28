# org-seek.el

Searching Org-mode with search tools.

# Install #

- You can install this package through MELPA.

# Config #

## use-package ##

``` emacs-lisp
(use-package org-seek
  :ensure t
  :commands (org-seek-string org-seek-regexp org-seek-headlines)
  )
```

# Usage #

## Use Cases ##

If you organize your knowledge wiki with Org-mode, and store them in a
directory, you may want to search like this:

- search through all Org files for a specific thing with `[M-x org-seek-string]`.
- search all Org files under current path by specify `[C-u]`.
- search with regular expression with `[M-x org-seek-regexp]`.
- search only org-mode headlines for quick search (because most user define a
  short description with headline) with `[M-x org-seek-headlines]`.

