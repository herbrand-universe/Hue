;; --------------------------------------------------------------------
;; Copyright (c) - 2012--2016 - IMDEA Software Institute
;; Copyright (c) - 2012--2016 - Inria
;;
;; Distributed under the terms of the GPL-v3 license
;; --------------------------------------------------------------------

(require 'span)
(require 'proof)

(defvar hue-last-but-one-statenum 0)
(defvar hue-proof-weak-mode nil)

;; Proof mode
(defun hue-proof-weak-mode-toggle ()
  "Toggle EasyCrypt check mode."
  (interactive)
  (cond
     (hue-proof-weak-mode
        (proof-shell-invisible-cmd-get-result "pragma Proofs:check"))
     (t (proof-shell-invisible-cmd-get-result "pragma Proofs:weak")))
  (if
      (eq 'error proof-shell-last-output-kind)
      (message "Failed to set proof mode")))

;; Function for set or get the information in the span
(defsubst hue-get-span-statenum (span)
  "Return the state number of the SPAN."
  (span-property span 'statenum))

(defsubst hue-set-span-statenum (span val)
  "Set the state number of the SPAN to VAL."
  (span-set-property span 'statenum val))

(defsubst proof-last-locked-span ()
  (with-current-buffer proof-script-buffer
  (span-at (- (proof-unprocessed-begin) 1) 'type)))

(defun hue-last-prompt-info (s)
  "Extract the information from prompt."
  (let ((lastprompt (or s (error "no prompt"))))
     (when (string-match "\\[\\([0-9]+\\)|\\(\\sw+\\)\\]" lastprompt)
           (list (string-to-number (match-string 1 lastprompt))
                 (if (equal (match-string 2 lastprompt) "weakcheck") t nil)))))

(defun hue-last-prompt-info-safe ()
  "Take from `proof-shell-last-prompt' the last information in the prompt."
  (hue-last-prompt-info proof-shell-last-prompt))

(defun hue-set-state-infos ()
  "Set information necessary for backtracking."
  (if proof-shell-last-prompt
     ;; infos = prompt infos of the very last prompt
     ;; sp    = last locked span, which we want to fill with prompt infos
     (let ((sp    (if proof-script-buffer (proof-last-locked-span)))
           (infos (or (hue-last-prompt-info-safe) '(0 nil))))

       (unless (or (not sp) (hue-get-span-statenum sp))
         (hue-set-span-statenum sp hue-last-but-one-statenum))
       (setq hue-last-but-one-statenum (car infos))
       (setq hue-proof-weak-mode (car (cdr infos)))
     )))

(add-hook 'proof-state-change-hook 'hue-set-state-infos)

(defun hue-find-and-forget (span)
  (let ((span-staten (hue-get-span-statenum span)))
       (list (format "pragma_undo %s;" (int-to-string span-staten)))))

(provide 'hue-hooks)
