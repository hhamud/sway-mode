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

if the above fails then just load in the manually and activate the mode within any `.sw` file


## Feature Guide


### Key bindings:
| Key binding | Function | description |
|-------------|----------|-------------|
| C-c C-c       | sway-mode-fmt     | an emacs function to format the present sway file         |
| C-c C-a       | sway-mode-fm-custom | an emacs function to format a specific sway file providing the forc toml path        |
| C-c C-t       | sway-mode-test | an emacs function to activate forc test        |
| C-c C-b       | sway-mode-build | an emacs function to activate forc build        |
| C-c C-d       | sway-mode-deploy | an emacs function to activate forc deploy        |


### LSP mode:

Integration with forc-lsp requires that you have lsp-mode package installed and that you run lsp-mode with the major mode by running the command . ```M-x lsp-mode```
