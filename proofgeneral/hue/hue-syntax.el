;; --------------------------------------------------------------------
;; Copyright (c) - 2012--2016 - IMDEA Software Institute
;; Copyright (c) - 2012--2016 - Inria
;;
;; Distributed under the terms of the GPL-v3 license
;; --------------------------------------------------------------------

(require 'proof-syntax)
(require 'hue-keywords)
(require 'hue-hooks)

(defconst hue-id "[A-Za-z_]+")

(defconst hue-terminal-string    ";")
(defconst hue-command-end-regexp ";")

(defconst hue-keywords-proof-goal '("proof" "equiv"))
(defconst hue-keywords-proof-save '("save" "qed"))

(defconst hue-non-undoables-regexp "^pragma\\b")

(defvar hue-other-symbols "\\(\\.\\.\\|\\[\\]\\)")

(defvar hue-operator-char-1 "=\\|<\\|>\\|~")
(defvar hue-operator-char-2 "\\+\\|\\-")
(defvar hue-operator-char-3 "\\*\\|/\\|%")
(defvar hue-operator-char-4 "!\\|\\$\\|&\\|\\?\\|@\\|\\^\\|:\\||\\|#")

(defvar hue-operator-char-1234
  (concat "\\(" hue-operator-char-1
          "\\|" hue-operator-char-2
		  "\\|" hue-operator-char-3
		  "\\|" hue-operator-char-4
          "\\)"))

(defconst hue-proof-save-regexp
  (concat "^\\(" (proof-ids-to-regexp hue-keywords-proof-save) "\\)\\b"))

(defconst hue-goal-command-regexp
  (concat "^\\(?:local\\s-+\\)?\\(?:" (proof-ids-to-regexp hue-keywords-proof-goal) "\\)"
          "\\s-+\\(?:nosmt\\s-+\\)?\\(\\sw+\\)"))

(defun hue-save-command-p (span str)
  "Decide whether argument is a [save|qed] command or not"
  (let ((txt (or (span-property span 'cmd) "")))
       (proof-string-match-safe hue-proof-save-regexp txt)))

(defun hue-goal-command-p (span)
  "Is SPAN a goal start?"
  (let ((txt (or (span-property span 'cmd) "")))
       (proof-string-match-safe hue-goal-command-regexp txt)))

(defun hue-init-output-syntax-table ()
  "Set appropriate values for syntax table for Hue output."
  (modify-syntax-entry ?,   " ")
  (modify-syntax-entry ?\'  "_")
  (modify-syntax-entry ?_   "_")

  ;; For comments
  (modify-syntax-entry ?\* ". 23") 

  ;; For blocks
  (modify-syntax-entry ?\( "()1")
  (modify-syntax-entry ?\) ")(4")
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))

;; ----- regular expressions

(defvar hue-error-regexp "^\\[error-[0-9]+-[0-9]+\\]\\|^anomaly"
  "A regexp indicating that the Hue process has identified an error.")

(defvar hue-shell-proof-completed-regexp "No more goals"
  "*Regular expression indicating that the proof has been completed.")

(defconst hue-any-command-regexp
  ;; allow terminator to be considered as command start:
  ;; FIXME: really needs change in generic function to take account of this,
  ;; since the end character of a command is not the start
  (concat "\\(\\s(\\|\\s)\\|\\sw\\|\\s \\|\\s_\\)*="  ;match assignments
          "\\|;;\\|;\\|" 
          (proof-ids-to-regexp hue-global-keywords))
  "Regexp matching any Hue command start or the terminator character.")

(defconst hue-keywords-indent-open
  (append '("local") hue-keywords-proof-goal))

(defconst hue-keywords-indent-close
  (append hue-keywords-proof-save
          ))

(defconst hue-keywords-indent-enclose
  (append 
          
          
          hue-keywords-proof-goal
          hue-keywords-proof-save))

; Regular expression for indentation
(defconst hue-indent-any-regexp
  (proof-regexp-alt hue-any-command-regexp "\\s(" "\\s)"))
    
(defconst hue-indent-enclose-regexp
  (proof-regexp-alt (proof-ids-to-regexp hue-keywords-indent-enclose) "\\s)"))

(defconst hue-indent-open-regexp
  (proof-regexp-alt (proof-ids-to-regexp hue-keywords-indent-open) "\\s("))

(defconst hue-indent-close-regexp
  (proof-regexp-alt (proof-ids-to-regexp hue-keywords-indent-close) "\\s)"))

(defface hue-tactics-closing-face
  (proof-face-specs
   (:foreground "red")
   (:foreground "red")
   ())
  "Face for names of closing tactics in proof scripts."
  :group 'proof-faces)

(defface hue-tactics-dangerous-face
  (proof-face-specs
   (:background "red")
   (:background "red")
   ())
  "Face for names of dangerous tactics in proof scripts."
  :group 'proof-faces)

(defface hue-tactics-tacticals-face
  (proof-face-specs
   (:foreground "dark green")
   (:foreground "dark green")
   ())
  "Face for names of tacticals in proof scripts."
  :group 'proof-faces)

(defconst hue-tactics-closing-face   'hue-tactics-closing-face)
(defconst hue-tactics-dangerous-face 'hue-tactics-dangerous-face)
(defconst hue-tactics-tacticals-face 'hue-tactics-tacticals-face)

(defvar hue-font-lock-keywords
  (list
    (cons (proof-ids-to-regexp hue-global-keywords)    'font-lock-keyword-face)
    (cons (proof-ids-to-regexp hue-tactic-keywords)    'proof-tactics-name-face)
    (cons (proof-ids-to-regexp hue-tactical-keywords)  'hue-tactics-tacticals-face)
    (cons (proof-ids-to-regexp hue-bytac-keywords)     'hue-tactics-closing-face)
    (cons (proof-ids-to-regexp hue-dangerous-keywords) 'hue-tactics-dangerous-face)
    (cons (proof-ids-to-regexp hue-prog-keywords)      'font-lock-keyword-face)
    (cons (concat hue-operator-char-1234 "+")          'font-lock-type-face)
    (cons hue-other-symbols                            'font-lock-type-face)))

(defun hue-init-syntax-table ()
  "Set appropriate values for syntax table in current buffer."
  (modify-syntax-entry ?\$ ".")
  (modify-syntax-entry ?\/ ".")
  (modify-syntax-entry ?\\ ".")
  (modify-syntax-entry ?+  ".")
  (modify-syntax-entry ?-  ".")
  (modify-syntax-entry ?=  ".")
  (modify-syntax-entry ?%  ".")
  (modify-syntax-entry ?<  ".")
  (modify-syntax-entry ?>  ".")
  (modify-syntax-entry ?\& ".")
  (modify-syntax-entry ?_  "_")
  (modify-syntax-entry ?\' "_")
  (modify-syntax-entry ?\| ".")
  (modify-syntax-entry ?\. ".")
  (modify-syntax-entry ?\* ". 23n")
  (modify-syntax-entry ?\( "()1")
  (modify-syntax-entry ?\) ")(4"))

(provide 'hue-syntax)
