# Sway-mode



## Introduction

A sway specific major mode for emacs that includes:

- Syntax highlighting
- Integrations with forc, forc-lisp and forc-fmt

## Installation

Clone this repository locally and then add these to your init.el file
``` emacs-lisp
(add-to-list 'load-path "/path/to/sway-mode/")
(autoload 'sway-mode "sway-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sw\\'" . sway-mode))
```


## Feature Guide


### Key bindings:
| Key binding | Function | description |
|-------------|----------|-------------|
| C-c c       | sway-mode-fmt     | an emacs function to format the present sway file         |
| C-c a       | sway-mode-fm-custom | an emacs function to format a specific sway file providing the forc toml path        |
| C-c t       | sway-mode-test | an emacs function to activate forc test        |

