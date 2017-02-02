(defvar hue-bytac-keywords '(
  "exact"
  "assumption"
  "smt"
  "by"
  "reflexivity"
  "done"
))

(defvar hue-dangerous-keywords '(
  "admit"
))

(defvar hue-global-keywords '(
  "define"
  "assume"
  "proof"
  "qed"
  "var"
))

(defvar hue-internal-keywords '(
  "pragma_undo"
  "pragma_restart"
  "pragma"
))

(defvar hue-prog-keywords '(
  "forall"
  "exists"
  "fun"
  "let"
  "in"
  "var"
  "is"
))

(defvar hue-tactic-keywords '(
  "intro"
  "assumption"
  "apply"
  "exact"
))

(defvar hue-tactical-keywords '(
  "try"
  "first"
  "last"
  "do"
  "strict"
  "expect"
))

(provide 'hue-keywords)
