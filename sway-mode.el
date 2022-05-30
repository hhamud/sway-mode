;;; sway-mode.el --- major mode for sway  -*- lexical-binding: t; -*-

;; Version: 0.0.1
;; Author: Hamza Hamud
;; Url: https://github.com/hhamud/sway-mode
;; Keywords: languages
;; Package-Requires: ((emacs "25.1")(lsp-mode "6.0"))

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; commentary:

;; This package implements a major-mode for editing Sway source code.


;;; code:

(eval-when-compile
  (require 'rx))

(defun sway-mode-fmt-custom (path)
  "Formats sway files within the supplied PATH."
  (interactive "spath to toml file:")
  (shell-command "forc fmt --path %s" path))

(defun sway-mode-fmt ()
  "Formats a single sway file."
  (interactive)
  (let ((default-directory (expand-file-name "../")))
    (shell-command "forc fmt")))

(defun sway-mode-test ()
  "Run forc test."
  (interactive)
  (let ((default-directory (expand-file-name "../")))
    (shell-command "forc test")))

(defun sway-mode-build ()
  "Build a forc project."
  (interactive)
  (let ((default-directory (expand-file-name "../")))
    (shell-command "forc build")))

(defun sway-mode-deploy ()
  "Build a forc project."
  (interactive)
  (let ((default-directory (expand-file-name "../")))
    (shell-command "forc deploy")))

(defvar sway-function-call-highlights "\\(\\(?:\\w\\|\\s_\\)+\\)\\(<.+>\\)?\s*("
  "Regex for general Sway function calls.")

(defvar sway-function-call-type-highlights "\\b\\([A-Za-z][A-Za-z0-9_]*\\|_[A-Za-z0-9_]+\\)\\(::\\)\\(<.*>\s*\\)\("
  "Regex for Sway function calls with type.")

(defvar sway-declarations-without-name '("contract" "script" "predicate")
  "Sway specific declarations.")

(defvar sway-type-level-declaration  "\\b\\(abi\\|library\\)\\s-"
  "Sway specific type level declaration.")

(setq sway-highlights
      `(
        (,(regexp-opt sway-declarations-without-name 'symbols) . 'font-lock-keyword-face)
        (, sway-type-level-declaration . 'font-lock-keyword-face)
        (,sway-function-call-highlights . (1 'font-lock-function-name-face))
        (,sway-function-call-type-highlights . (1 'font-lock-function-name-face))
        ))



;;; KeyMap
(defvar sway-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c c") 'sway-mode-fmt)
    (define-key keymap (kbd "C-c a") 'sway-mode-fmt-custom)
    (define-key keymap (kbd "C-c t") 'sway-mode-test)
    (define-key keymap (kbd "C-c b") 'sway-mode-build)
    (define-key keymap (kbd "C-c d") 'sway-mode-deploy)
    keymap)
  "Keymap for `sway-mode'.")


;;;###autoload
(define-derived-mode sway-mode rust-mode
  "Sway"
  (font-lock-add-keywords nil sway-highlights)
  (use-local-map sway-mode-map))


;;;###autoload
(when (featurep 'lsp-mode)
  (add-to-list 'lsp-language-id-configuration
               '(sway-mode . "sway"))
  (lsp-register-client
   (make-lsp-client :major-modes '(sway-mode)
                    :server-id 'forc-lsp
                    :new-connection (lsp-stdio-connection "forc-lsp"))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sw\\'" . sway-mode))

;; add to feature list
(provide 'sway-mode)
;;; sway-mode.el ends here
