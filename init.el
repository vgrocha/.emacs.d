(defvar *emacs-load-start* (current-time))

(defvar *my-default-lib* "~/.emacs.d")
(add-to-list 'load-path *my-default-lib*)

(require 'cl)

(setq tab-width 3)

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
 ;;                       markdown-mode
 ;;                       auto-complete
 ;;                       undo-tree))


 ;; (dolist (p my-packages)
 ;;  (when (not (package-installed-p p))
 ;;    (package-install p)))

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(defun override-slime-repl-bindings-with-paredit ()
            (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
          (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;#####################################################
;; Styling. Check if not in terminal to set the nice colors and fonts
;#####################################################
(unless (string= 'nil window-system)
  (progn
    (set-face-font 'default "Inconsolata 10")
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
(global-set-key (kbd "M-g") 'goto-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat *my-default-lib* "/elpa/auto-complete-1.4"))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat *my-default-lib* "/elpa/auto-complete-1.4/ac-dict"))
(ac-config-default)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (or (minibufferp (current-buffer))
                                    (not (numberp
                                          (compare-strings "*eshell*" 0 7
                                                           (buffer-name
                                                            (current-buffer)) 0 7)))))
                         (auto-complete-mode 1))))

;; tab in insert mode calls autocomplete
(ac-set-trigger-key "TAB")

(real-global-auto-complete-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://jandmworks.com/lisp.html#SBCL quirks
(add-to-list 'load-path (concat *my-default-lib* "/slime"))
(autoload 'slime-mode "slime" nil)

(eval-after-load "slime"
  '(progn
;     (slime-setup '(slime-repl))
     (setq inferior-lisp-program "sbcl")
     (set-language-environment "UTF-8")
     (setq slime-net-coding-system 'utf-8-unix)
     (setq common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/")
     (global-set-key (kbd "C-c s") 'slime-selector)
     ;; autocomplete with slime's documentation
     (add-to-list 'load-path (concat *my-default-lib* "/elpa/ac-slime"))

     (require 'ac-slime)
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))


(dolist (hook '(lisp-mode-hook
                clojure-mode-hook))
  (add-hook hook (lambda () (slime-mode t))))

(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


(defun scratch-lisp-file ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
   the current buffer."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    (insert ";;;; " file "\n")
    (insert "\n(defpackage :" package "\n  (:use :cl))\n\n")
    (insert "(in-package :" package ")\n\n")))


;###################################
;;Modes
;###################################
; FIACRE
;;;(autoload <function called> <filename> <doc> <interactive>)
;; (add-to-list 'load-path (expand-file-name "~/elisp/modes/fiacre"))
;; (autoload 'fiacre-mode "fiacre" "Fiacre Mode." t)
(load-file (concat *my-default-lib* "/modes/fiacre.el"))
;;;##autoload
(add-to-list 'auto-mode-alist '("\\.fcr$" . fiacre-mode))

;AMPL
(load-file (concat *my-default-lib* "/modes/ampl-mode.el"))
;;;##autoload
(add-to-list 'auto-mode-alist '("\\(.mod\\|.dat\\|.ampl\\)'" . ampl-mode))


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

;;bind goto line to M G
(global-set-key "\M-g" 'goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Maps swaps [ for ( and vice versa                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))

;; Highlight search object
(setq search-highlight           t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; Highlight query object
(setq query-replace-highlight    t)


;;disable word wrapping in html mode
(defun my-html-mode-hook ()
  (auto-fill-mode -1))

(add-hook 'html-mode-hook 'my-html-mode-hook)


;;haskell identation
(custom-set-variables
     '(haskell-mode-hook '(turn-on-haskell-indentation)))