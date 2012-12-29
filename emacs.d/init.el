;(defvar *emacs-load-start* (current-time))

;; basic stuff
(setq inhibit-splash-screen t)
(set-background-color "black")
(set-foreground-color "gray")
(set-cursor-color "green")
(set-face-background 'modeline "black")
(set-face-foreground 'modeline "gray")

;; bold fonts slows things a shit
(set-face-bold-p 'bold nil)
;; disable italics
(set-face-italic-p 'italic nil)

;; in case 'emacsclient -c' was called
(add-to-list 'default-frame-alist '(foreground-color . "gray"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(cursor-color . "green"))

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

(tool-bar-mode -1)
(tooltip-mode  -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(set-scroll-bar-mode 'right)

;; text width
(setq-default fill-collumn 72)
;; apply fill-collumn width in text-mode
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; no backup and autosave
(setq backup-inhibited t
      auto-save-default nil)

;; scrolling
(setq scroll-step 1
	  scroll-conservatively 2000)

;; allow buffer erasing
(put 'erase-buffer 'disabled nil)

;; no vc
(setq vc-handled-backends nil)

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

;; load tags and evil will adjust 'g]' for etags-select menu selection
(require 'etags-select)

;; fullscreen support
(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
						 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;; dired addon
(require 'dired-details)
(dired-details-install)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

(autoload 'clojure-mode "clojure-mode.el"
  "Major mode for editing cloure files" t)

;; clojure stuff
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

;; company
;(add-to-list 'load-path "~/.emacs.d/company-mode")
;
;;; eclim
;(add-to-list 'load-path "~/.emacs.d/emacs-eclim")
;(add-to-list 'load-path "~/.emacs.d/emacs-eclim/vendor")
;
;(setq eclim-eclipse-dirs '("~/programs/eclipse")
;      eclim-executable "~/programs/eclipse/eclim")
;
;(require 'eclim)
;
;(add-hook 'eclim-mode-hook
;  (lambda ()
;	(global-set-key (kbd "C-n") 'eclim-complete)))
;
;(global-eclim-mode)
;
;(require 'company)
;(require 'company-emacs-eclim)
;(company-emacs-eclim-setup)
;(global-company-mode t)

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

;; newsticker
;(require 'htmlr)
;(require 'newsticker)
;

;(setq newsticker-display-interval    15.3
;	  newsticker-keep-obsolete-items nil
;	  newsticker-scroll-smoothly     nil
;	  newsticker-use-full-width      nil
;	  newsticker-html-renderer       'htmlr
;	  newsticker-sort-method         'sort-by-time)
;
;(setq newsticker-url-list-defaults
;	  '(("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600)
;		("Slashdot" "http://slashdot.org/slashdot.rss" nil 3600)
;		("Hacker News" "http://news.ycombinator.com/rss" nil 3600)
;		("Reddit Programming" "http://www.reddit.com/r/programming/.rss" nil 3600)
;		("Reddit Scheme" "http://www.reddit.com/r/scheme/.rss" nil 3600)
;		("Reddit Lisp" "http://www.reddit.com/r/lisp/.rss" nil 3600)
;		("Freshmeat" "http://freecode.com/?format=atom" nil 3600)
;		("Planet Clojure" "http://planet.clojure.in/atom.xml" nil 3600)
;		("Planet Gnome" "http://planet.gnome.org/atom.xml" nil 3600)
;		("Hackful Europe" "http://hackful.com/frontpage.rss" nil 3600)
;		("Clojure Google Group" "http://groups.google.com/group/clojure/feed/rss_v2_0_msgs.xml" nil 3600)))
;
;; org mode
(setq org-log-done 'time)

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
  (setq cpp-known-face '(foreground-color . "chocolate"))
  (setq cpp-unknown-face 'default)
  (setq cpp-face-type 'dark)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
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

;; slime
;(add-to-list 'load-path "~/.emacs.d/slime/")
;(setq inferior-lisp-program "/opt/ecl/bin/ecl")
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
