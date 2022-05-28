;;; sway-mode.el --- major mode for sway  -*- lexical-binding: t; -*-
;;; commentary:
;;; code:
(eval-when-compile
  (require 'rx))


(setq sway-highlights
      '(
        ("abi" . font-lock-keyword-face)
        ("contract" . font-lock-keyword-face)
        ("library" . font-lock-keyword-face)
        ("storage" . font-lock-keyword-face)
        ))

;;;###autoload
(define-derived-mode sway-mode rust-mode
  "Sway"
 (font-lock-add-keywords nil sway-highlights))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sw" . sway-mode))


(provide 'sway-mode)
;;; sway-mode.el ends here
