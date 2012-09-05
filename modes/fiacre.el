(defvar fiacre-mode-hook nil)

(defvar fiacre-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Fiacre major mode")

(defconst fiacre-font-lock-keywords-1
  (list
   '("\\<\\(component\\|process\\)\\>" . font-lock-function-name-face)
;   '("\\<\\(and\\|append\\|case\\|closed\\|const\\|dequeue\\|do\\|else\\|elsif\\|empty\\|end\\|enqueue\\|first\\|foreach\\|from\\|full\\|if\\|in\\|init\\|is\\|loop\\|not\\|null\\|of\\|or\\|out\\|par\\|priority\\|queue\\|read\\|record\\|select\\|states\\|then\\|to\\||unless\\|var\\|wait\\|where\\|while\\|write\\)\\>" . font-lock-builtin-face)
   '("\\<\\(and\\|any\\|append\\|array\\|bool\\|case\\|channel\\|component\\|const\\|dequeue\\|do\\|else\\|elsif\\|empty\\|end\\|enqueue\\|false\\|first\\|foreach\\|from\\|full\\|if\\|in\\|init\\|int\\|is\\|loop\\|nat\\|none\\|not\\|null\\|of\\|on\\|or\\|out\\|par\\|port\\|priority\\|process\\|queue\\|read\\|record\\|select\\|states\\|then\\|to\\|true\\|type\\|union\\|unless\\|var\\|wait\\|where\\|while\\|write\\)\\>" . font-lock-builtin-face)
   '("\\(\\[\\|\\]\\|(\\|)\\)" . font-lock-comment-delimiter-face)
;   '("" . font-lock-comment-face )
   '("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)
;   '("" . font-lock-doc-face)
;   '("" . font-lock-function-name-face)
;   '("" . font-lock-keyword-face)
;   '("" . font-lock-negation-char-face)
;   '("" . font-lock-preprocessor-face)
;   '("" . font-lock-reference-face)
;   '("" . font-lock-string-face)
   '("\\<\\(array\\|bool\\|int\\|nat\\|port\\|channel\\|union\\|type\\|any\\|none\\)\\>". font-lock-type-face)
;   '("" . font-lock-variable-name-face)
;   '("" . font-lock-warning-face)

   '("\\('\\w*\\)" . font-lock-variable-name-face))
  "Minimal highlighting expression for Fiacre mode")

(defvar fiacre-font-lock-keywords fiacre-font-lock-keywords-1
  "Default highlighting expressions for Fiacre mode")


(defun fiacre-indent-line ()
  "Indent current line as Fiacre code"
  (interactive)
  (beginning-of-line)
  (if (looking-at "^[ \t]*\\(component\\|process\\)")
      (indent-line-to 0)
    (if (looking-at "^[ \t]*\\(case\\|states\\|init\\|from\\|var\\|port\\|par\\|init\\)")
	(indent-line-to default-tab-width)
      (let ((not-indented t) cur-indent)
	(if (looking-at "^[ \t]*end")
	    (progn
	      (save-excursion
		(forward-line -1)
		(setq cur-indent (- (current-indentation) default-tab-width)))
	      (if (< cur-indent 0)
		  (setq cur-indent 0)))
	  (save-excursion 
	    (while not-indented
	      (forward-line -1)
	      (if (looking-at "^[ \t]*end") ; Check for rule 3
		  (progn
		    (setq cur-indent (current-indentation))
		    (setq not-indented nil))
					; Check for rule 4
		(if (looking-at "^[ \t]*\\(component\\|process\\|case\\|states\\|init\\|from\\|var\\|port\\|par\\|init\\|select\\|if\\)")
		    (progn
		      (setq cur-indent (+ (current-indentation) default-tab-width))
		      (setq not-indented nil))
		  (if (bobp) ; Check for rule 5
		      (setq not-indented nil)))))))
	(if cur-indent
	    (indent-line-to cur-indent)
	  (indent-line-to 0)))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar fiacre-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for fiacre-mode")

(defun fiacre-mode ()
  "Major mode for editing Fiacre Language files"
  (interactive)
  (kill-all-local-variables)
;  (set-syntax-table fiacre-mode-syntax-table)
;  (use-local-map fiacre-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(fiacre-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'fiacre-indent-line)  
  (setq major-mode 'fiacre-mode)
  (setq mode-name "Fiacre")
  (run-hooks 'fiacre-mode-hook))

(provide 'fiacre-mode)



