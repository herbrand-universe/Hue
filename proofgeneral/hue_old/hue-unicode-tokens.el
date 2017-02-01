
(require 'cl)				; for-loop

(eval-when (compile)
  (require 'unicode-tokens)	    ; it's loaded dynamically at runtime
  (require 'proof-unicode-tokens))  ; that file loads us at runtime

;;
;; Customization
;;

(defgroup hue-tokens nil
  "Variables which configure Hue tokens for Unicode Tokens mode."
  :group 'hue
  :prefix "hue-")

(defun hue-set-and-restart-tokens (sym val)
  "Function to restart Unicode Tokens when a token value is adjusted."
  (set-default sym val)
  (when (featurep 'hue-unicode-tokens) ; not during loading
    (hue-init-shortcut-alists)
    (if (featurep 'unicode-tokens)
	(unicode-tokens-initialise))))


;;
;; Symbols
;;

(defconst hue-token-format "\\<%s>")

(defconst hue-token-variant-format-regexp
  "\\\\<\\(%s\\)[0-9]*>") ; unofficial interpretation of usual syntax



(defcustom hue-token-symbol-map '(("alpha" "α") )
  "hue-token-symbol-map"
  :type 'unicode-tokens-token-symbol-map
  :group 'hue
  :set 'hue-set-and-restart-tokens
  :tag "Hue Unicode Token Mapping")



;;
;; Shortcuts
;;

(defcustom hue-symbol-shortcuts
  '(
    ("\\pi" . "Π")
    ("\\lam" . "λ"))
  "Shortcut key sequence table for symbol tokens input.
See `unicode-tokens-shortcut-alist'."
    :type 'unicode-tokens-shortcut-alist
    :set 'hue-set-and-restart-tokens
    :group 'hue
    :tag "Hue symbol shortcuts")

(defcustom hue-shortcut-alist nil
  "Shortcut key sequence table for token input.
See `unicode-tokens-shortcut-alist'."
  :type 'unicode-tokens-shortcut-alist
  :set 'hue-set-and-restart-tokens
  :group 'hue
  :tag "Hue Unicode Input Shortcuts")

(defun hue-init-shortcut-alists ()
  "Set defaults for `hue-shortcut-alist' and `hue-shortcut-replacement-alist'."
  (custom-set-default 'hue-shortcut-alist hue-symbol-shortcuts))

(hue-init-shortcut-alists)




(provide 'hue-unicode-tokens)

;;; hue-unicode-tokens.el ends here
