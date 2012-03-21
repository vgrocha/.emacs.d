(defvar *emacs-load-start* (current-time))

(defvar *my-default-lib* "~/.emacs.d")
(add-to-list 'load-path *my-default-lib*)


(require 'cl)



;###################################
;### Package manager - marmalade ###
;###################################
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;refreshing contents
(when (not package-archive-contents)
 (package-refresh-contents))

;initializing packages!
(package-initialize)

;basic packages for clojure =)
;; (defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings 
;;                       clojure-mode clojure-test-mode 
;;                       rainbow-delimiters 
;;                       ac-slime 
;;                       markdown-mode))

;; (dolist (p my-packages)
;;  (when (not (package-installed-p p))
;;    (package-install p)))



;#####################################################
;; Styling. Check if not in terminal to set the nice colors and fonts
;#####################################################
(unless (string= 'nil window-system)
  (progn
    (set-face-font 'default "Inconsolata 12")
    (require 'color-theme)
    (color-theme-initialize)
    (load-file (concat *my-default-lib* "/color-theme-twilight.el"))
    (load-file (concat *my-default-lib* "/zenburn-el/zenburn.el"))
    ;; ;; theme for darker enviroments

    ;; (color-theme-twilight)
    (color-theme-zenburn)
    ;; theme for lighted enviroments
    ;; (color-theme-greiner)
    ;; greiner is cool in a lighted enviroment.w9b
    ))


;###################################
;Windows mode! =)
;###################################
(winner-mode 1)
(global-set-key (kbd "C-x <left>")  'windmove-left) ; move to left windnow
(global-set-key (kbd "C-x <right>") 'windmove-right) ; move to right window
(global-set-key (kbd "C-x <up>")    'windmove-up)     ; move to upper window
(global-set-key (kbd "C-x <down>")  'windmove-down)    ; move to downer window


;###################################
;Some small configurations
;###################################

;; automatically sets the global mode for all buffers
(global-linum-mode t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Maps swaps [ for ( and vice versa                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))


