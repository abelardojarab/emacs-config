- [README](#readme)
  - [Introduce](#introduce)
  - [Install](#install)
  - [Configure](#configure)
  - [Usage](#usage)

# README<a id="org9117e32"></a>

## Introduce<a id="org55bd972"></a>

ebib-handy is a ebib tool, which can let ebib become a cite chooser.
![img](./snapshots/ebib-handy.gif)

## Install<a id="orgb15aaa0"></a>

1.  Config melpa: <http://melpa.org/#/getting-started>
2.  M-x package-install RET ebib-handy RET

## Configure<a id="orge8af8b7"></a>

    (require 'ebib-handy)
    (ebib-handy-enable)

    (setq ebib-extra-fields
          '((BibTeX "keywords" "abstract" "timestamp"
                    "file"  "url" "crossref" "annote" "doi")
            (biblatex "keywords" "abstract" "timestamp"
                      "file"  "url" "crossref" "annote" "doi")))

## Usage<a id="org5cf7f8d"></a>

    (global-set-key "\C-c b" 'ebib-handy)

You can open "example/thesis.org" then type 'C-c b'.
