;; --------------------------------------------------------------------
;; Copyright (c) - 2012--2016 - IMDEA Software Institute
;; Copyright (c) - 2012--2016 - Inria
;;
;; Distributed under the terms of the GPL-v3 license
;; --------------------------------------------------------------------

(require 'proof)
(require 'pg-custom)
(require 'hue-syntax)
(require 'hue-hooks)
(require 'hue-abbrev)


(setq proof-general-debug 1)
(setq unicode-tokens-mode 1)


(add-to-list 'hs-special-modes-alist
  '(hue-mode "{" "}" "/[*/]" nil nil))

;; --------------------------------------------------------------------
(defun hue-load-path-safep (path)
  (and
   (listp path)
   (every (lambda (entry) (stringp entry)) path)))

;; --------------------------------------------------------------------
(defcustom hue-prog-name "huetop"
  "*Name of program to run Hue."
  :type  'file
  :group 'hue)

(defcustom hue-load-path nil
  "Non-standard Hue library load path.
This list specifies the include path for Hue. The elements of
this list are strings."
  :type  '(repeat (string :tag "simple directory (-I)"))
  :safe  'hue-load-path-safep
  :group 'hue)

(defcustom hue-web-page
  "https://github.com/herbrand-universe/Hue"
  "URL of web page for Hue."
  :type  'string
  :group 'hue-config)

;; --------------------------------------------------------------------
;;(defun hue-build-prog-args ()
;;  (delete "-emacs" hue-prog-args)
;;  (push "-emacs" hue-prog-args))

;;(hue-build-prog-args)

;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; Generic mode

(defun hue-config ()
  "Configure Proof General scripting for Hue."
  (hue-init-output-syntax-table)
  
  (setq  proof-script-command-start-regexp     hue-command-start-regexp)

  (setq  proof-script-comment-start            "(*"
         proof-script-comment-end              "*)")
  
  ;; For undo
  (setq  proof-find-and-forget-fn              'hue-find-and-forget
         proof-completed-proof-behaviour       nil
         proof-kill-goal-command               "kill"
         proof-non-undoables-regexp            hue-non-undoables-regexp
         proof-shell-restart-cmd               "pragma restart. ")

  (set (make-local-variable 'comment-quote-nested) nil)

  ;; For func-menu and finding goal...save regions
  (setq  proof-save-command-regexp             hue-proof-save-regexp
         proof-really-save-command-p           'hue-save-command-p
         proof-save-with-hole-regexp           nil
         proof-goal-command-p                  'hue-goal-command-p
         proof-goal-with-hole-regexp           hue-goal-command-regexp
         proof-goal-with-hole-result           1)

  (setq  proof-goal-command                    "proof %s: "
         proof-save-command                    "qed")
  
  (setq  proof-prog-name                       hue-prog-name
         proof-assistant-home-page             hue-web-page)

  ; Options
  (setq  proof-three-window-enable             t
         proof-three-window-mode-policy        (quote hybrid)
         proof-auto-multiple-files             t)

  ; Setting indents 
  (set   (make-local-variable 'indent-tabs-mode) nil)
  (setq  proof-indent-enclose-offset   (- proof-indent)
         proof-indent-open-offset     0
         proof-indent-close-offset    0
         proof-indent-any-regexp      hue-indent-any-regexp
         proof-indent-enclose-regexp  hue-indent-enclose-regexp
         proof-indent-open-regexp     hue-indent-open-regexp
         proof-indent-close-regexp    hue-indent-close-regexp)

  ; Silent/verbose mode for batch processing
  (setq proof-shell-start-silent-cmd "pragma silent. "
        proof-shell-stop-silent-cmd  "pragma verbose. ")

  ; Ask for the current goal
  (setq proof-showproof-command "pragma noop. ")

  (hue-init-syntax-table)
  ;; we can cope with nested comments
  (set (make-local-variable 'comment-quote-nested) nil)

  (setq  proof-script-font-lock-keywords
         hue-font-lock-keywords))

(defun hue-shell-config ()
  "Configure Proof General shell for Hue."
  (hue-init-output-syntax-table)
  (setq  proof-shell-auto-terminate-commands    nil)
  (setq  proof-shell-strip-crs-from-input       t)
  (setq  proof-shell-annotated-prompt-regexp    "Huetop\\[[0-9]+\\]#")
  (setq  proof-shell-clear-goals-regexp         hue-shell-proof-completed-regexp)
  (setq  proof-shell-proof-completed-regexp     hue-shell-proof-completed-regexp)
  (setq  proof-shell-error-regexp               "\\[ERROR\\]")
  (setq  proof-shell-truncate-before-error      nil)
  (setq  proof-shell-start-goals-regexp         "^Right")
  (setq  proof-shell-end-goals-regexp           nil)  ; up to next prompt
  (setq  proof-shell-font-lock-keywords         hue-font-lock-keywords))

;; --------------------------------------------------------------------
;; Derived modes

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

(defun hue-get-last-error-location () 
  "Remove [error] in the error message and extract the position
  and length of the error "
  (proof-with-current-buffer-if-exists proof-response-buffer

     (goto-char (point-max))
     (when (re-search-backward "\\[error-\\([0-9]+\\)-\\([0-9]+\\)\\]" nil t)
        (let* ((inhibit-read-only t)
               (pos1 (string-to-number (match-string 1)))
               (pos2 (string-to-number (match-string 2)))
               (len (- pos2 pos1)))

              (delete-region (match-beginning 0) (match-end 0))
              (list pos1 len)))))

(defun hue-advance-until-command ()
   (while (proof-looking-at "\\s-") (forward-char 1)))

(defun hue-highlight-error ()
  "Use 'hue-get-last-error-location' to know the position
  of the error and then highlight in the script buffer"
  (proof-with-current-buffer-if-exists proof-script-buffer
    (let ((mtch (hue-get-last-error-location)))
        (when mtch
          (let ((pos (car mtch))
                  (lgth (cadr mtch)))
          (if (eq (proof-unprocessed-begin) (point-min))
                (goto-char (proof-unprocessed-begin))
                (goto-char (+ (proof-unprocessed-begin) 1)))
            (hue-advance-until-command)
             (goto-char (+ (point) pos))
             (span-make-self-removing-span
               (point) (+ (point) lgth)
               'face 'proof-script-highlight-error-face))))))

(defun hue-highlight-error-hook ()
  (hue-highlight-error))

(defun hue-redisplay-hook ()
  (hue-redisplay))

;;(add-hook 'proof-shell-handle-error-or-interrupt-hook
;;          'hue-highlight-error-hook t)

;; --------------------------------------------------------------------
;; Key bindings


;; --------------------------------------------------------------------
;; 3-window pane layout hack
(add-hook
  'proof-activate-scripting-hook
  '(lambda () (when proof-three-window-enable (proof-layout-windows))))

;; --------------------------------------------------------------------
(provide 'hue)
