;(defvar *emacs-load-start* (current-time))

;; used by rxvt-like terminals; with this env, emacs in terminal
;; will correctly know how to use default colors
(setenv "COLORFGBG" "default;default;0")

;; basic stuff
(setq inhibit-splash-screen t)
(set-background-color "black")
(set-foreground-color "gray")
(set-cursor-color "gray")
(set-face-background 'modeline "black")
(set-face-foreground 'modeline "gray")
(set-face-foreground 'font-lock-comment-face "#FF7B11")
(set-face-foreground 'font-lock-constant-face "#92AFCE")
(set-face-foreground 'font-lock-builtin-face "#FE8592")
(set-face-foreground 'font-lock-function-name-face "#B3FF79")
(set-face-foreground 'font-lock-variable-name-face "#95E275")
(set-face-foreground 'font-lock-keyword-face "#8EFFC7")
(set-face-foreground 'font-lock-string-face "#60FFA6")
(set-face-foreground 'font-lock-doc-face "#FFD86A")
(set-face-foreground 'font-lock-type-face "#EDFF5F")
 
;; bold fonts slows things a shit
(set-face-bold-p 'bold nil)
;; disable italics
(set-face-italic-p 'italic nil)

;; in case 'emacsclient -c' was called
(add-to-list 'default-frame-alist '(foreground-color . "gray"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(cursor-color . "gray"))

(set-default-font "Monospace-10")
(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(setq bidi-display-reordering nil)
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
	  jit-lock-defer-contextually t
	  jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

;; speeds up things considerably
(setq font-lock-maximum-decoration
	  '((c-mode . 2) (c++-mode . 2) (t . 1)))

(setq-default c-basic-offset 4
			  c-default-style "linux"
			  tab-width 4
			  shift-width 4
			  indent-tabs-mode t
			  ;indicate-empty-lines t
			  ;show-trailing-whitespace t
			  )

(when window-system
  (tool-bar-mode -1)
  (tooltip-mode  -1)
  (set-scroll-bar-mode 'right))

(menu-bar-mode -1)
(blink-cursor-mode -1)

;; text width
(setq-default fill-collumn 72)
;; apply fill-collumn width in text-mode
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; no backup and autosave
(setq backup-inhibited t
      auto-save-default nil
	  ;; scrolling
	  scroll-step 1
	  scroll-conservatively 2000)

;; allow buffer erasing
(put 'erase-buffer 'disabled nil)

;; no vc
(setq vc-handled-backends nil)

;; tramp
(setq password-cache-expiry nil)

;; viper
;(setq viper-mode t)
;(setq viper-inhibit-startup-message t)
;(setq viper-expert-level '3)
;(setq viper-auto-indent t)
;(setq viper-fast-keyseq-timeout 50)
;(set 'viper-no-multiple-ESC t)
;(defun viper-translate-all-ESC-keysequences () t)
;(global-set-key [(control w)] 'other-window)
;(require 'viper)
;(load "~/.emacs.d/vimlike.el")

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/modes")

(add-to-list 'load-path "~/.emacs.d/evil/lib")
(add-to-list 'load-path "~/.emacs.d/evil")
(defvar evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

(evil-ex-define-cmd "bdp" 'kill-buffer)

;; load tags and evil will adjust 'g]' for etags-select menu selection
(require 'etags-select)

;; fullscreen support
(if window-system
	(defun fullscreen ()
	  (interactive)
	  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
							 '(2 "_NET_WM_STATE_FULLSCREEN" 0))))
;; dired addon
(require 'dired-details)
(dired-details-install)

;; on 'a' do not ask me about creating new buffer
(put 'dired-find-alternate-file 'disabled nil)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

(autoload 'clojure-mode "clojure-mode.el"
  "Major mode for editing cloure files" t)

(add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))

;(defun my-clojure-eval-defun ()
;  (lisp-eval-defun)
;  (process-send-string "*inferior-lisp*" "(use :reload *ns*)"))

(add-hook 'clojure-mode-hook
  (lambda ()
	;(require 'rainbow-delimiters)
	(global-set-key (kbd "C-c C-c") 'lisp-eval-defun)
	(setq inferior-lisp-program "/usr/local/bin/lein repl") ))

(autoload 'jam-mode "jam-mode.el"
  "Major more for editing jam files" t)

(add-to-list 'auto-mode-alist '("\\.jam" . jam-mode))
(add-to-list 'auto-mode-alist '("Jamfile" . jam-mode))
(add-to-list 'auto-mode-alist '("Jamrules" . jam-mode))

;; stolent from: http://www.credmp.org/?p=27
;(require 'url)
;(defun google-it (term)
;  "Search term in google."
;  (interactive "sSearch for: ")
;  (browse-url (concat "http://www.google.com/search?q="
;                      (url-hexify-string (encode-coding-string term 'utf-8)) )))

;(autoload 'babel "babel"
;   "Use a web translation service to translate the message MSG." t)
;(autoload 'babel-region "babel"
;   "Use a web translation service to translate the current region." t)
;(autoload 'babel-as-string "babel"
;   "Use a web translation service to translate MSG, returning a string." t)
;(autoload 'babel-buffer "babel"
;   "Use a web translation service to translate the current buffer." t)

;; org mode
(setq org-log-done 'time)

;; hide modeline
(defun toggle-mode-line ()
  "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
		(if (equal mode-line-format nil)
			(default-value 'mode-line-format)) )
  (redraw-display))

;;; buffer management
(defun next-user-buffer ()
  "Switch to the next user buffer. User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
	(while (and (string-match "^*" (buffer-name))
				(< i 50))
	  (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer. User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
	(while (and (string-match "^*" (buffer-name))
				(< i 50))
	  (setq i (1+ i)) (previous-buffer) )))

(defun paste-and-indent ()
  "Paste text and do indent on it."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun cpp-highlight-if-0/1 ()
  "Modify the face of text in between #if 0 ... #endif."
  (interactive)
  (setq cpp-known-face '(foreground-color . "chocolate")
		cpp-unknown-face 'default
		cpp-face-type 'dark
		cpp-known-writable 't
		cpp-unknown-writable 't)

  (setq cpp-edit-list
		'((#("0" 0 1 (fontified nil))
		   (foreground-color . "chocolate")
		   nil
		   both nil)))
  (cpp-highlight-buffer t))

(defun warning-keywords ()
  "Highlight warning keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                                ("\\<\\(TODO\\):" 1 font-lock-warning-face t))))

(add-hook 'c-mode-common-hook 'warning-keywords)
(add-hook 'c-mode-common-hook
  (lambda ()
	(cpp-highlight-if-0/1)
	(add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)))

(global-set-key (kbd "<S-prior>") 'previous-user-buffer)
(global-set-key (kbd "<S-next>") 'next-user-buffer)
(global-set-key [f11] 'ibuffer)
(global-set-key [f12] 'eshell)

;; force TAB on Shift-Tab
(global-set-key (kbd "<backtab>") (lambda () (interactive) (insert-char 9 1)))

;; slime
(add-to-list 'load-path "~/.emacs.d/slime/")
(setq inferior-lisp-program "~/programs/sbcl/run-sbcl.sh")
;(require 'slime)
;(slime-setup)

;(message ".emacs loaded in %ds"
;		 (destructuring-bind (hi lo ms) (current-time)
;		   (-
;			 (+ hi lo)
;			 (+ (first *emacs-load-start*)
;				(second *emacs-load-start*) ))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(font-lock-maximum-decoration 1)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
