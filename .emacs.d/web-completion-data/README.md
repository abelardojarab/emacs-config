Shared web completion data for Emacs ac-html and company-web
============================================================

### Contribute:

Data source are in yaml files (sadly yaml-mode not so comfortable for editing yaml files)
and located in `src/` directory. Edit them, but not `html-stuff/` files, they are converted from yaml.

You should have nodejs installed in your system to make convertor work.

```bash
# Install dependencies for convertor
(cd convertor/ && npm install)
# Convert
make
```

Yaml have next structure:

```yaml
# Define tags, just ley-value - tag name and documentation
tags:
  html: The <html> tag tells the browser that this is an HTML documen...
  div:

# Define attributes, array of KV - attribute names and properties "t" "v" "d":
#  where "t" is array or string - html tags;
#        "v" array of values or KV - value and short documentation;
#        "d" documentation for this attribute, should be used only once per tag attribute.
attributes:
  - src:
      t: a
      d: "The href attribute specifies the link's destination"
  - dir:
      t: global
      v:
        auto:
        ltr: Right-to-left text direction
        rtr: Left-to-right text direction
      d: |
        Text direction of the element's content.
  - charset:
      t: [meta, script]
      d:

  # next attribute only define possible values for div.class
  - class:
      t: div
      v: [container, "container-info"]
```
