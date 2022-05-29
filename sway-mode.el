;;; sway-mode.el --- major mode for sway  -*- lexical-binding: t; -*-
;;; commentary:
;;; code:
(eval-when-compile
  (require 'rx))


(defvar function-call-highlights "\\(\\(?:\\w\\|\\s_\\)+\\)\\(<.+>\\)?\s*("
  "Regex for general Sway function calls.")

(defvar function-call-type-highlights "\\b\\([A-Za-z][A-Za-z0-9_]*\\|_[A-Za-z0-9_]+\\)\\(::\\)\\(<.*>\s*\\)\("
  "Regex for Sway function calls with type.")

(defvar top-level-declaration "\\b\\(library\\)\\s+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
  "Regex for Sway top level declarations with name.")

(defvar declarations-without-name '("contract" "script" "predicate")
  "Sway specific declarations.")


(setq sway-highlights
      `(
        (,(regexp-opt declarations-without-name 'word) . 'font-lock-keyword-face)
        (,top-level-declaration . 'font-lock-keyword-face)
        (,function-call-highlights . (1 'font-lock-function-name-face))
        (,function-call-type-highlights . (1 'font-lock-function-name-face))
        ))


;;;###autoload
(define-derived-mode sway-mode rust-mode
  "Sway"
 (font-lock-add-keywords nil sway-highlights))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sw" . sway-mode))

;; add to feature list
(provide 'sway-mode)
;;; sway-mode.el ends here
