(require 'proof)
(require 'hue-syntax)
(require 'hue-abbrev)

;(defcustom unicode-token-enable t)


(defcustom hue-prog-name "hueopt"
  "*Name of program to run Hue."
  :type 'file
  :group 'hue)

(defcustom hue-web-page
  "http://www...."
  "URL of web page for Hue."
  :type 'string
  :group 'hue)





;; ======== Configuration of generic modes ========
;;

(defun hue-config ()
  "Configure Proof General scripting for Hue."
  (hue-init-output-syntax-table)
  
;  (setq  proof-terminal-string                 hue-terminal-string)
  (setq  proof-script-command-end-regexp       ";")
  (setq  proof-script-comment-start            "(*"
         proof-script-comment-end              "*)")
  

  ;; For func-menu and finding goal...save regions
  (setq  ;proof-goal-command-p                  'hue-goal-command-p
         proof-goal-command-regexp             "proof"
         proof-save-command-regexp             "qed"
         proof-goal-with-hole-regexp           hue-named-entity-regexp
         proof-goal-with-hole-result           1
         proof-save-with-hole-regexp           nil
         proof-script-imenu-generic-expression hue-generic-expression
)
  
  (setq  proof-goal-command                    "proof %s is"
         proof-save-command                    "qed") 
  
  (setq  proof-prog-name                       hue-prog-name
         proof-assistant-home-page             hue-web-page)

  ; Options
  (setq  proof-three-window-enable             t)

  (hue-init-syntax-table)
  ;; we can cope with nested comments
  (set (make-local-variable 'comment-quote-nested) nil)

  (setq  proof-script-font-lock-keywords
         hue-font-lock-keywords))


(defun hue-shell-config ()
  "Configure Proof General shell for Hue."
  (hue-init-output-syntax-table)
  (setq  proof-shell-auto-terminate-commands     nil)
  (setq  proof-shell-annotated-prompt-regexp     "^\\(Huetop#\\|#\\)")
  (setq  proof-shell-error-regexp                hue-error-regexp)
  (setq  proof-shell-truncate-before-error       nil)
  (setq  proof-shell-quit-cmd                    ":quit")
  (setq  proof-shell-start-goals-regexp          "^Right")
  (setq  proof-shell-end-goals-regexp nil)  ; up to next prompt
  (setq  proof-shell-font-lock-keywords 
         hue-font-lock-keywords))


;;
;; ======== Defining the derived modes ========
;;
;; The derived modes set the variables, then call the
;; <mode>-config-done function to complete configuration.

(define-derived-mode hue-mode proof-mode
  "Hue script" nil
  (hue-config)
  (proof-config-done))

(define-derived-mode hue-shell-mode proof-shell-mode
  "Hue shell" nil
  (hue-shell-config)
  (proof-shell-config-done))

(define-derived-mode hue-response-mode proof-response-mode
  "Hue response" nil
  (hue-init-output-syntax-table)
  (setq  proof-response-font-lock-keywords 
         hue-font-lock-keywords)
  (proof-response-config-done))

(define-derived-mode hue-goals-mode proof-goals-mode
  "Hue goals" nil
  (hue-init-output-syntax-table)
  (setq  proof-goals-font-lock-keywords 
         hue-font-lock-keywords)
  (proof-goals-config-done))

(provide 'hue)
