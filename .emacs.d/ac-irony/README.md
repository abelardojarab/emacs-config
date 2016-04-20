# [Auto-complete][ac-ref] support for [irony-mode][irony-mode-ref]

This package provides an [auto-complete][ac-ref] source for
[irony-mode][irony-mode-ref]. It provides semantic completion for C/C++ and
Objective-C languages.


## Status

**This package is not yet ready for prime-time, this documentation is therefore
a work of fiction.**


## Screenshots

TODO: non-misleading screenshot.

## Dependencies

This package depends on:

* [auto-complete][ac-ref]
* [irony-mode][irony-mode-ref]
* [yasnippet][yasnippet-ref] (optional, used when available by `irony-mode`)

Please refer to the documentation of these packages for the basic configuration
needed to get started.


## Installation

The recommended way to install `ac-irony` and its dependencies is to use a
package manager.

* Using [MELPA](http://melpa.milkbox.net/)

        M-x package-install RET ac-irony RET

* Using [el-get](https://github.com/dimitri/el-get)

        M-x el-get-install RET ac-irony RET


## Configuration

~~~el
(defun my-ac-irony-setup ()
  ;; be cautious, if yas is not enabled before (auto-complete-mode 1), overlays
  ;; *may* persist after an expansion.
  (yas-minor-mode 1)
  (auto-complete-mode 1)

  (add-to-list 'ac-sources 'ac-source-irony)
  (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))

(add-hook 'irony-mode-hook 'my-ac-irony-setup)
~~~


[irony-mode-ref]: https://github.com/Sarcasm/irony-mode          "Irony Mode"
[ac-ref]:         https://github.com/auto-complete/auto-complete "Auto Complete"
[yasnippet-ref]:  https://github.com/capitaomorte/yasnippet      "YASnippet"
