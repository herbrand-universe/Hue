(require 'proof)
(require 'hue-syntax)

(defpgdefault menu-entries
  '(
    ["Use Three Panes" proof-three-window-toggle
      :style    toggle
      :active   (not proof-multiple-frames-enable)
      :selected proof-three-window-enable
      :help     "Use three panes"]
    ["Unicode tokens" (proof-unicode-tokens-toggle (if (boundp 'unicode-tokens-mode) (if unicode-tokens-mode 0 1) 1)) ]

    ["Weak-check mode" hue-proof-weak-mode-toggle
     :style    toggle
     :selected hue-proof-weak-mode
     :help     "Toggles Hue check mode."]
))

(provide 'hue-abbrev)
