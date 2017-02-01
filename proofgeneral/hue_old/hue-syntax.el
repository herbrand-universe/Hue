(require 'proof-syntax)
(require 'hue-keywords)

;; Check me
(defconst hue-keywords-proof-goal 
  '("proof"))

(defconst hue-keywords-proof-save
  '("qed"))

(defconst hue-keywords-major
  '(  
      "var"
      "def"
))


(defconst hue-keywords-index
  '( "var" "def" "proof"))

;; End check me


(defconst hue-id "[A-Za-z_][A-Za-z0-9]+")
(defconst hue-terminal-string ";")

;; For imenu
(defconst  hue-keywords-imenu
  (append 
	  hue-keywords-index))

(defconst  hue-entity-regexp
  (concat "\\(" (proof-ids-to-regexp hue-keywords-imenu) "\\)"))

(defconst  hue-named-regexp
  (concat "\\s *\\(" hue-id "\\)\\s *"))

(defconst  hue-named-entity-regexp
  (concat hue-entity-regexp
    "\\ *"
    hue-named-regexp
    "\\ *is"))


(defconst  hue-generic-expression
  (mapcar  (lambda (kw)
    (list 
      (capitalize kw)
      (concat "\\ *" kw "\\ +\\([A-Za-z0-9_]+\\)\\ *is")
      1))
    hue-keywords-imenu))



(defun hue-init-output-syntax-table ()
  "Set appropriate values for syntax table for Hue output."
  (modify-syntax-entry ?`   " ")
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

;
;; ----- regular expressions

(defvar hue-error-regexp "^\\[ERROR\\]"
  "A regexp indicating that the Hue process has identified an error.")


(defvar hue-shell-proof-completed-regexp "QED"
  "*Regular expression indicating that the proof has been completed.")


(defvar hue-font-lock-keywords
  (list
    (cons (proof-ids-to-regexp hue-program-keywords) 
        'font-lock-keyword-face)
			; 'proof-declaration-name-face)
    (cons (proof-ids-to-regexp hue-tactics-keywords) 
          'font-lock-type-face)
    (cons (proof-ids-to-regexp hue-tactics2-keywords) 
          'font-lock-type-face)


    (cons (proof-ids-to-regexp hue-common-keywords)  
        'font-lock-keyword-face)))
          ;'font-lock-type-face)))

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

