;(defvar *emacs-load-start* (current-time))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; used by rxvt-like terminals; with this env, emacs in terminal
;; will correctly know how to use default colors
(setenv "COLORFGBG" "default;default;0")

;; package archives
(setq package-archives
	  '(("gnu" . "http://elpa.gnu.org/packages/")
		("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
		("marmalade" . "http://marmalade-repo.org/packages/")))

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; basic stuff
(setq inhibit-splash-screen t)
(set-background-color "black")
(set-foreground-color "gray")
(set-cursor-color "gray")

;; modeline stuff was renamed
(defvar modeline 'modeline)
(if (and (>= emacs-major-version 24)
         (>= emacs-minor-version 3))
  (setq modeline 'mode-line))

(set-face-background modeline "black")
(set-face-foreground modeline "gray")
(set-face-foreground 'minibuffer-prompt "green")
(set-face-background 'modeline-inactive "gray10")
(set-face-foreground 'font-lock-comment-face "#FF7B11")
(set-face-foreground 'font-lock-constant-face "#92AFCE")
(set-face-foreground 'font-lock-builtin-face "#FE8592")
(set-face-foreground 'font-lock-function-name-face "#B3FF79")
(set-face-foreground 'font-lock-variable-name-face "#95E275")
(set-face-foreground 'font-lock-keyword-face "#8EFFC7")
(set-face-foreground 'font-lock-string-face "#60FFA6")
(set-face-foreground 'font-lock-doc-face "#FFD86A")
(set-face-foreground 'font-lock-type-face "#EDFF5F")

;; invisible vertical border
; (set-face-background 'vertical-border "black")
; (set-face-foreground 'vertical-border (face-background 'vertical-border))

;; vertical border in terminal
(when (not window-system)
  (set-face-inverse-video-p 'vertical-border nil)
  (set-face-background 'vertical-border (face-background 'default))
  (set-display-table-slot standard-display-table
						  'vertical-border
						  (make-glyph-code ?│)))

;; to keep dark clients
(setq frame-background-mode 'dark)

;; Disable bold/italic fonts slows things a shit. It is done last, so other mode
;; does not set it in the mean time.
(mapc (lambda (face)
        (set-face-attribute face nil :weight 'normal :underline nil))
      (face-list))

(set-face-bold-p 'bold nil)
(set-face-bold-p 'italic nil)

(set-default-font "inconsolata-13")
;(add-to-list 'default-frame-alist '(font . "inconsolata-12"))
(setq bidi-display-reordering nil)

;; speeds up things considerably
(setq font-lock-maximum-decoration
	  '((c-mode . 2) (c++-mode . 2) (java-mode . 2) (t . 1)))

(setq-default c-basic-offset 4
              c-default-style "linux"
              tab-width 4
              shift-width 4
              indent-tabs-mode t
              ;indicate-empty-lines t
              ;show-trailing-whitespace t
              )

;; 24.4 addon
(setq blink-matching-paren 'jump)

;; indent case label by c-indent-level
(c-set-offset 'case-label '+)

(if window-system
  (progn
    (tool-bar-mode -1)
    (tooltip-mode  -1)
    (set-scroll-bar-mode 'right)

    (defun fullscreen ()
      (interactive)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))))
  (progn
    ;; fix copy/paste from terminal
    (when (getenv "DISPLAY")
	  (defun selection-type-to-xsel-arg (type)
		(cond
		 ((eq type 'PRIMARY) "--primary")
		 ((eq type 'CLIPBOARD) "--clipboard")
		 (t
		  (error "Got unknown selection type: %s" type))))
 
      (defun x-set-selection (type data)
        (message "Copied to %s selection" type)
        (let ((sel-type (selection-type-to-xsel-arg type)))
          (with-temp-buffer
            (insert text)
            (call-process-region (point-min) (point-max) "xsel" nil 0 nil sel-type "--input"))))
	  
	  (defun x-get-selection (type data)
		(message "Paste from %s selection" type)
		(let ((sel-type (selection-type-to-xsel-arg type)))
		  (insert (shell-command-to-string (format "xsel %s --output" sel-type)))))
	  ;; make sure we have this or evil pasting mechanism will try all types, causing
	  ;; it to paste content 3 times
	  (setq x-select-request-type 'STRING))))

(menu-bar-mode -1)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when (fboundp 'global-eldoc-mode) (global-eldoc-mode -1))
;; disable blinking in urxvt
(setq visible-cursor nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; text width
(setq-default fill-collumn 72)
;; apply fill-collumn width in text-mode
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; no backup and autosave
(setq backup-inhibited t
      auto-save-default nil
	  ;; backups
	  ;backup-by-copying t      ; don't clobber symlinks
	  ;backup-directory-alist '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
	  ;delete-old-versions t
	  ;kept-new-versions 6
	  ;kept-old-versions 2
	  ;version-control t       ; use versioned backups
      ;; scrolling
      scroll-step 1
      scroll-conservatively 2000)

;; allow buffer erasing
(put 'erase-buffer 'disabled nil)

;; disable xml validation
(setq rng-nxml-auto-validate-flag nil)

;; vc
(setq vc-handled-backends '(SVN Git))

;; tramp
(setq password-cache-expiry nil)

;; do not use vc on tramp files
(setq vc-ignore-dir-regexp
	  (format "\\(%s\\)\\|\\(%s\\)"
			  vc-ignore-dir-regexp
			  tramp-file-name-regexp))

;; ido fuzzy mode
(setq ido-enable-flex-matching t)

;; ibuffer
(setq ibuffer-show-empty-filter-groups nil)

(defun ibuffer-close-all-filter-groups ()
  "Close displaying status of all filter groups"
  (interactive)
  (setq ibuffer-hidden-filter-groups
		(mapcar #'car (ibuffer-current-filter-groups-with-position)))
  (ibuffer-update nil t))

;; Unique buffers. Can't use lazy load, since it overrides core functions like
;; rename-buffer or create-file-buffer
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; evil
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/modes")
(add-to-list 'load-path "~/.emacs.d/evil/lib")
(add-to-list 'load-path "~/.emacs.d/evil")

(defvar evil-want-C-u-scroll t)
(setq evil-search-module 'evil-search)
(require 'evil)
(evil-mode 1)
(setq-default evil-symbol-word-search t)
(evil-ex-define-cmd "bdp" 'kill-buffer)
(evil-ex-define-cmd "winsa[ve]" 'window-configuration-to-register)
(evil-ex-define-cmd "winlo[ad]" 'jump-to-register)
(evil-ex-define-cmd "ag[enda]" 'org-agenda)
(evil-ex-define-cmd "ca[pture]" 'org-capture)
(evil-ex-define-cmd "ta[propos]" 'tags-apropos)
(evil-ex-define-cmd "mks[ession]" 'desktop-save)
(evil-ex-define-cmd "so[urce]" 'desktop-change-dir)
(evil-ex-define-cmd "bi[do]" 'ido-switch-buffer)
(evil-ex-define-cmd "bf[ile]" 'ido-find-file)
(evil-ex-define-cmd "bmarks" 'bookmark-bmenu-list)
(evil-ex-define-cmd "bm[ark]" 'bookmark-set)

(autoload 'ack "ack" "Runs ack.el." t)

;; A small adjustment so we autoload etags-select.el only when this
;; sequence was pressed. Evil already predefines 'g]' to be set, but only
;; after etags-select.el was loaded...
(define-key evil-motion-state-map "g]" 'etags-select-find-tag-at-point)
(autoload 'etags-select-find-tag-at-point "etags-select" "Runs etags-select." t)

(add-to-list 'evil-emacs-state-modes 'etags-stack-mode)
(autoload 'etags-stack-show "etags-stack" "Runs etags-stack-show." t)
(evil-ex-define-cmd "tags" 'etags-stack-show)

;; gnu global support

(eval-after-load 'ggtags
  '(progn
     (evil-make-overriding-map ggtags-mode-map 'normal)
     ;; force update evil keymaps after ggtags-mode loaded
	 (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps)))

(defun evil-jump-to-tag-advice (old-fn &rest args)
  "Wrapper aroung evil-jump-to-tag with gtags support."
  (if (bound-and-true-p gtags-mode)
	  (gtags-find-tag-from-here)
	(apply old-fn args)))
(advice-add 'evil-jump-to-tag :around #'evil-jump-to-tag-advice)

(defun evil-pop-tag-stack ()
  (interactive)
  (if (bound-and-true-p gtags-mode)
	  (gtags-pop-stack)
	(pop-tag-mark)))
(define-key evil-normal-state-map "\C-t" 'evil-pop-tag-stack)

(add-to-list 'auto-mode-alist '("\\.claws-mail/tmp/tmpmsg\\.0x" . mail-mode))
;; on 'a' do not ask me about creating new buffer
(put 'dired-find-alternate-file 'disabled nil)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

(autoload 'restclient "restclient.el"
  "Major mode for contacting REST services" t)

(autoload 'clojure-mode "clojure-mode.el"
  "Major mode for editing cloure files" t)

(add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))

(autoload 'newlisp-mode "newlisp-mode.el"
  "Major mode for editing newLisp files" t)

(autoload 'cmake-mode "cmake-mode.el"
  "Major mode for editing CMake files" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake" . cmake-mode))

(autoload 'groovy-mode "groovy-mode.el"
  "Major mode for editing files groovy files" t)
(add-to-list 'auto-mode-alist '("\\.groovy" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle" . groovy-mode))

;(defun my-clojure-eval-defun ()
;  (lisp-eval-defun)
;  (process-send-string "*inferior-lisp*" "(use :reload *ns*)"))
;
;(add-hook 'clojure-mode-hook
;  (lambda ()
;    ;(require 'rainbow-delimiters)
;    (global-set-key (kbd "C-c C-c") 'lisp-eval-defun)
;    (global-set-key (kbd "C-c C-k")
;                    (lambda ()
;                      (interactive)
;                      (save-excursion
;                        (mark-whole-buffer)
;                        (lisp-eval-region (region-beginning) (region-end)))))
;    (global-set-key (kbd "C-c C-d")
;                    (lambda (symbol)
;                      (interactive
;                       (list
;                        (let* ((sym (thing-at-point 'symbol))
;                               (sym (if sym (substring-no-properties sym)))
;                               (prompt "Describe")
;                               (prompt (if sym
;                                           (format "%s (default %s): " prompt sym)
;                                         (concat prompt ": "))))
;                          (read-string prompt nil nil sym))))
;                      (process-send-string "*inferior-lisp*"
;                                           (format "(clojure.repl/doc %s)\n" symbol))))
;    (setq inferior-lisp-program "lein repl")))

(autoload 'jam-mode "jam-mode.el"
  "Major more for editing jam files" t)

(add-to-list 'auto-mode-alist '("\\.jam" . jam-mode))
(add-to-list 'auto-mode-alist '("Jamfile" . jam-mode))
(add-to-list 'auto-mode-alist '("Jamrules" . jam-mode))

;; google selection
(defun lookup-selection ()
  "Lookup word under cursor or selection in given browser engine."
  (interactive)
  (let* ((engine "https://www.google.com/search?q=")
         (word   (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'symbol)))
         (word   (replace-regexp-in-string " " "+" word)))
    (browse-url (concat engine word))))

(define-key evil-normal-state-map "\\s" 'lookup-selection)
(define-key evil-visual-state-map "\\s" 'lookup-selection)

;; hide modeline
(defun toggle-mode-line ()
  "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)))
  (redraw-display))

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

(add-hook 'c-mode-common-hook
  (lambda ()
    (cpp-highlight-if-0/1)
    (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)))

(add-hook 'prog-mode-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("\\<\\(NOTE\\|FIXME\\|TODO\\|BUG\\|HACK\\|REFACTOR\\):" 1 font-lock-warning-face t)))))

(global-set-key (kbd "<S-prior>") 'previous-buffer)
(global-set-key (kbd "<S-next>") 'next-buffer)
(global-set-key [f9]  'ibuffer)
(global-set-key [f11] 'neotree-toggle)
(autoload 'neotree-toggle "neotree" "Runs neotree browser." t)
(global-set-key [f12] 'eshell)

;; force TAB on Shift-Tab
(global-set-key (kbd "<backtab>") (lambda () (interactive) (insert-char 9 1)))

(add-hook 'lisp-mode-hook
  (lambda ()
    (setq inferior-lisp-program "~/programs/sbcl/run-sbcl.sh")))

;; org mode
(setq org-directory "~/cloud/org"
	  org-default-notes-file (format "%s/notes.org" org-directory)
	  org-agenda-files (list (format "%s/TODO.org" org-directory))
	  org-mobile-files (mapcar
						(lambda (x)
						  (concat org-directory "/" x))
						'("TODO.org" "auto.org" "notes.org" "movies.org"))
	  org-mobile-inbox-for-pull (format "%s/notes.org" org-directory)
	  org-mobile-directory (format "%s/mobileorg" org-directory)
	  org-refile-targets '(("TODO.org" :maxlevel . 1))
	  ;; in clock summary, only put total hours
	  org-time-clocksum-format "%d:%02d"
	  ;; don't show done tasks in agenda
	  org-agenda-skip-scheduled-if-done t
	  ;; include commands from emacs calendar, also allow showing holidays in agenda view
	  org-agenda-include-diary t)

;; org appointments; with this, org-mode scheduled items will be notified 10 minutes
;; earlier. To make it work, make sure to run ':ag' or 'org-agenda' so the entries are
;; populated inside 'appt-time-msg-list'.
;;
;; To see current entries, print 'appt-time-msg-list'.
(add-hook 'org-finalize-agenda-hook
  (lambda ()
	(setq appt-message-warning-time 10 ;; warn 10 min in advance
		  appt-display-diary nil       ;; do not display diary when (appt-activate) is called
		  appt-display-mode-line t     ;; show in the modeline
		  appt-display-format 'window  ;; use our func
		  calendar-mark-diary-entries-flag t) ;; mark diary entries in calendar
	(org-agenda-to-appt)
	(appt-activate 1)))                ;; active appt (appointment notification)

 ;; update appt each time agenda opened
;(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; org html export style
(setq org-export-html-style-include-scripts nil
	  org-export-html-style-include-default nil)

(setq org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://thomasf.github.io/solarized-css/solarized-dark.min.css\" />")
 
(add-hook 'org-mode-hook
  (lambda ()
	(add-to-list 'org-modules 'org-habit)
	(require 'org-habit) ;; without this, org-habit will not work for me
    (setq org-log-done 'time
          org-icalendar-include-todo t
          org-icalendar-use-scheduled '(todo-due event-if-todo event-if-not-todo)
          org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo)
          org-icalendar-timezone "Europe/Sarajevo"
          org-icalendar-include-bbdb-anniversaries t
          org-icalendar-store-UID t)))

;; for display-time-world; uses default values plus some new
(setq display-time-world-list
	  '(("UTC" "UTC")
		("America/Los_Angeles" "Seattle")
		("America/New_York" "New York")
		("Australia/Melbourne" "Melbourne")
		("Australia/Darwin" "Darwin")
		("Europe/London" "London")
		("Europe/Paris" "Paris")
		("Asia/Calcutta" "Bangalore")
		("Asia/Tokyo" "Tokyo")))

;; for browse-url
(setq browse-url-browser-function 'browse-url-chromium
      ;browse-url-browser-function 'browse-url-generic
      ;browse-url-generic-program "firefox"
      )

;; something I can quickly call from eshell
(defun E (&rest args)
  "Invoke `find-file' on the file. \"E +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(setenv "EDITOR" "E")

(defun insert-today-date ()
  "Insert date in form 'abbr-week-name, day-in-month."
  (interactive)
  (insert (format-time-string "%a, %d")))

(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

(defun eww-more-readable ()
  "Make eww more pleasent to use. Assumes run after eww is loaded."
  (interactive)
  (setq eww-header-line-format nil)
  (toggle-mode-line)
  (set-window-margins (get-buffer-window) 20 20)
  (eww-reload 'local))

;; maven error support

;(add-to-list 'compilation-error-regexp-alist
;             'maven)
;
;(add-to-list
; 'compilation-error-regexp-alist-alist
; '(maven
;   "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].\*" 1 2 3))

;;;; load the rest
;;(load-file "~/.emacs.d/lisp/rss-setup.el")
(load-file "~/.emacs.d/lisp/elfeed-setup.el")
(load-file "~/.emacs.d/lisp/notmuch-setup.el")
(load-file "~/.emacs.d/lisp/org-setup.el")

(autoload 'clojure-enable-monroe "~/programs/projects/monroe/monroe.el" "Starts monroe." t)
(add-hook 'clojure-mode-hook 'clojure-enable-monroe)

;(message ".emacs loaded in %ds"
;        (destructuring-bind (hi lo ms) (current-time)
;          (-
;            (+ hi lo)
;            (+ (first *emacs-load-start*)
;               (second *emacs-load-start*)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filter-groups
   (quote
	(("my-groups"
	  ("erc"
	   (used-mode . erc-mode))
	  ("adpa"
	   (filename . "accrue\\/adpa"))
	  ("accrue"
	   (filename . "accrue"))
	  ("quieromipedido"
	   (filename . "quieromipedido"))
	  ("kwelia"
	   (filename . "kwelia"))
	  ("ra-scraper"
	   (filename . "ra-scraper"))
	  ("heroku-buildpack-openresty"
	   (filename . "heroku-buildpack-openresty"))
	  ("lore"
	   (filename . "lore"))
	  ("nxd2"
	   (filename . "nxd2"))
	  ("nxd"
	   (filename . "nxd"))
	  ("quandl-wikipedia"
	   (filename . "quandl-wikipedia"))
	  ("mails"
	   (used-mode . notmuch-message-mode))
	  ("stoys"
	   (filename . "stoys"))
	  ("duna"
	   (filename . "duna"))
	  ("z2o"
	   (filename . "z2o"))
	  ("pgnotify-demo"
	   (filename . "pgnotify-demo"))
	  ("flyerbee.com"
	   (filename . "flyerbee.com"))
	  ("dextrz.com"
	   (filename . "dextrz.com"))
	  ("nuvomeds"
	   (filename . "nuvomeds"))
	  ("grs"
	   (filename . "grs"))
	  ("ip-ranger"
	   (filename . "ip-ranger"))
	  ("joss"
	   (filename . "joss"))
	  ("gtrends-scraper"
	   (filename . "gtrends-scraper"))
	  ("mw"
	   (filename . "mw"))
	  ("rssget"
	   (filename . "rssget"))
	  ("pav-census-api"
	   (filename . "pav-census-api"))
	  ("pav-nlp"
	   (filename . "pav-nlp"))
	  ("pav-conf"
	   (filename . "pav-conf"))
	  ("pav-api"
	   (filename . "pav-api"))
	  ("uino-scraper"
	   (filename . "uino-scraper"))
	  ("pav-user-api"
	   (filename . "pav-user-api"))
	  ("pav-nlp-playground"
	   (filename . "pav-nlp-playground"))
	  ("pav-profile-timeline-worker"
	   (filename . "pav-profile-timeline-worker"))
	  ("fwd"
	   (filename . "fwd"))
	  ("pav-congress-api-bootstrapper"
	   (filename . "pav-congress-api-bootstrapper"))
	  ("OpenGrok"
	   (filename . "OpenGrok"))
	  ("onyx-starter"
	   (filename . "onyx-starter"))
	  ("onyx-examples"
	   (filename . "onyx-examples"))
	  ("wick"
	   (filename . "wick"))
	  ("drivewatch"
	   (filename . "drivewatch"))
	  ("booster"
	   (filename . "booster"))
	  ("b2c"
	   (filename . "blogger2cryogen"))
	  ("cryogen-html"
	   (filename . "cryogen-html"))
	  ("acidwords"
	   (filename . "acidwords"))
	  ("lein-vaadin"
	   (filename . "lein-vaadin-template"))
	  ("elan"
	   (filename . "elan"))
	  ("if-else"
	   (filename . "if-else"))
	  ("omp"
	   (filename . "omp"))
	  ("ex"
	   (filename . "emailxtractor"))
	  ("ophion"
	   (filename . "ophion"))
	  ("yaims"
	   (filename . "yaims"))
	  ("aries"
	   (filename . "aries"))
	  ("org"
	   (used-mode . org-mode))
	  ("novate"
	   (filename . "novate"))))))
 '(ibuffer-saved-filters
   (quote
	(("gnus"
	  ((or
		(mode . message-mode)
		(mode . mail-mode)
		(mode . gnus-group-mode)
		(mode . gnus-summary-mode)
		(mode . gnus-article-mode))))
	 ("programming"
	  ((or
		(mode . emacs-lisp-mode)
		(mode . cperl-mode)
		(mode . c-mode)
		(mode . java-mode)
		(mode . idl-mode)
		(mode . lisp-mode)))))))
 '(package-selected-packages
   (quote
	(log4j-mode vlf yaml-mode request-deferred php-mode magit lua-mode ledger-mode jabber f dash-functional)))
 '(safe-local-variable-values (quote ((create-lockfiles))))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date ((t (:inherit org-agenda-structure))) t))
