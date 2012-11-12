(require 'proof)
(require 'hue-syntax)

(defpgdefault  menu-entries
  '(
    ["Use Three Panes" proof-three-window-toggle
      :style toggle
      :active (not proof-multiple-frames-enable)
      :selected proof-three-window-enable
      :help "Use three panes"]
    ""
    ["Index Menu" proof-imenu-toggle
      :active (stringp (locate-library "imenu"))
      :style toggle
      :selected proof-imenu-enable
      :help "Generate an index menu of definitions, display which function in modeline"]

    ["Unicode tokens" (proof-unicode-tokens-toggle (if (boundp 'unicode-tokens-mode) (if unicode-tokens-mode 0 1) 1)) ]

    ["Speedbar" speedbar
      :active (stringp (locate-library "speedbar"))
      :style toggle
      :selected (and (boundp 'speedbar-frame) speedbar-frame)
      :help "Speedbar navigation window"]
))



(provide 'hue-abbrev)
