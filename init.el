(require 'cl)

;###################################
;Package startup
;###################################
(require 'package)
;;Add Marmalade as package archive source
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-eshell
                      clojure-mode 
                      clojure-test-mode 
                      rainbow-delimiters 
                      auto-complete
                      undo-tree
                      nrepl
                      ac-nrepl
                      nzenburn-theme
                      paredit)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))



;###################################
;Windows mode! =)
;###################################
(winner-mode 1)
(global-set-key (kbd "C-x <left>")  'windmove-left) ; move to left windnow
(global-set-key (kbd "C-x <right>") 'windmove-right) ; move to right window
(global-set-key (kbd "C-x <up>")    'windmove-up)     ; move to upper window
(global-set-key (kbd "C-x <down>")  'windmove-down)    ; move to downer window
;(global-set-key (kbd "M-g") 'goto-line)

;###################################
;Autocomplete configs
;###################################
(require 'auto-complete-config)
(ac-config-default)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (or (minibufferp (current-buffer))
                                    (not (numberp
                                          (compare-strings "*eshell*" 0 7
                                                           (buffer-name
                                                            (current-buffer)) 0 7)))))
                         (auto-complete-mode 1))))


;###################################
;Paredit configurations
;###################################
(add-hook 'clojure-mode-hook 'paredit-mode)


;###################################
;Visual forecasts
;###################################
(load-file "~/.emacs.d/elpa/nzenburn-theme-20130506/nzenburn-theme.el")
(set-face-font 'default "Inconsolata 10")


;###################################
;Nrepl configurations
;###################################
(require 'nrepl)
(add-hook 'nrepl-mode-hook 'paredit-mode)

;;Popup the error stacktrace
(setq nrepl-popup-stacktraces t)

(setq nrepl-popup-stacktraces-in-repl t)

;###################################
;Nrepl ac configurations
;###################################

;;hook paredit to nrepl
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;;tab triggering autocomplete
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;;popup documentation
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Maps swaps [ for ( and vice versa                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (keyboard-translate ?\( ?\[)
;; (keyboard-translate ?\[ ?\()
;; (keyboard-translate ?\) ?\])
;; (keyboard-translate ?\] ?\))

;###################################
;General configs
;###################################
;; newline also indents
(global-set-key "\r" 'newline-and-indent)

;; hide menus
(menu-bar-mode 1)
(tool-bar-mode 0)

;; Keep session
(desktop-save-mode 1)

;; Inhibit startup window, very annoying
(setq inhibit-startup-message 1)

;; Make copy and paste to work with other programs
(setq x-select-enable-clipboard 1)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; put something different in the scratch buffer
(setq initial-scratch-message
      "; Welcome my master!")

;; Automatically reload files after they've been modified
(global-auto-revert-mode 1)

(setq standard-indent 2)

;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; Remember the position where we closed a file
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t) ;; activate it for all buffers
(require 'saveplace) ;; get the package

;; Ignore case when looking for a file
(setq read-file-name-completion-ignore-case t)

;; comment and uncomment region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;;bind goto line to M G
(global-set-key "\M-g" 'goto-line)


;; Highlight search object
(setq search-highlight           t)

;; Highlight query object
(setq query-replace-highlight    t)

