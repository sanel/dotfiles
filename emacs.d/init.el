;(defvar *emacs-load-start* (current-time))

;; used by rxvt-like terminals; with this env, emacs in terminal
;; will correctly know how to use default colors
(setenv "COLORFGBG" "default;default;0")

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

;; to keep dark clients
(setq frame-background-mode 'dark)

; disable bold/italic fonts slows things a shit
(mapc (lambda (face)
        (set-face-attribute face nil :weight 'normal :underline nil))
      (face-list))

(set-face-bold-p 'bold nil)
(set-face-bold-p 'italic nil)

(set-default-font "Monospace-10")
;(add-to-list 'default-frame-alist '(font . "inconsolata-12"))
(setq bidi-display-reordering nil)

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
      (defadvice x-set-selection (around my-x-set-selection (type data))
        (message "Copied to %s selection" type)
        (let ((sel-type
               (cond
                ((eq type 'PRIMARY) "--primary")
                ((eq type 'CLIPBOARD) "--clipboard")
                (t
                 (error "Got unknown selection type: %s" type)))))
          (with-temp-buffer
            (insert text)
            (call-process-region (point-min) (point-max) "xsel" nil 0 nil sel-type "--input"))))

      (defadvice x-get-selection-value (before my-x-get-selection-value)
        (message "Paste from PRIMARY selection")
        (insert (shell-command-to-string "xsel --primary --output")))

      (defadvice x-get-clipboard (before my-x-get-clipboard)
        (message "Paste from CLIPBOARD selection")
        (insert (shell-command-to-string "xsel --clipboard --output")))

      (ad-activate 'x-set-selection)
      (ad-activate 'x-get-selection-value)
      (ad-activate 'x-get-clipboard))))

(menu-bar-mode -1)
(blink-cursor-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; evil
(add-to-list 'load-path "~/.emacs.d")
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

(autoload 'ack "ack")

;; A small adjustment so we autoload etags-select.el only when this
;; sequence was pressed. Evil already predefines 'g]' to be set, but only
;; after etags-select.el was loaded...
(define-key evil-motion-state-map "g]" 'etags-select-find-tag-at-point)
(autoload 'etags-select-find-tag-at-point "etags-select")

(add-to-list 'auto-mode-alist '("\\.claws-mail/tmp/tmpmsg\\.0x" . mail-mode))
;; on 'a' do not ask me about creating new buffer
(put 'dired-find-alternate-file 'disabled nil)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

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

(add-hook 'clojure-mode-hook
  (lambda ()
    ;(require 'rainbow-delimiters)
    (global-set-key (kbd "C-c C-c") 'lisp-eval-defun)
    (global-set-key (kbd "C-c C-k")
                    (lambda ()
                      (interactive)
                      (save-excursion
                        (mark-whole-buffer)
                        (lisp-eval-region (region-beginning) (region-end)))))
    (global-set-key (kbd "C-c C-d")
                    (lambda (symbol)
                      (interactive
                       (list
                        (let* ((sym (thing-at-point 'symbol))
                               (sym (if sym (substring-no-properties sym)))
                               (prompt "Describe")
                               (prompt (if sym
                                           (format "%s (default %s): " prompt sym)
                                         (concat prompt ": "))))
                          (read-string prompt nil nil sym))))
                      (process-send-string "*inferior-lisp*"
                                           (format "(clojure.repl/doc %s)\n" symbol))))
    (setq inferior-lisp-program "lein repl")))

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
(autoload 'neotree-toggle "neotree")
(global-set-key [f12] 'eshell)

;; force TAB on Shift-Tab
(global-set-key (kbd "<backtab>") (lambda () (interactive) (insert-char 9 1)))

(add-hook 'lisp-mode-hook
  (lambda ()
    (setq inferior-lisp-program "~/programs/sbcl/run-sbcl.sh")))

;; org mode
(setq org-default-notes-file "~/cloud/org/notes.org")
(setq org-agenda-files '("~/cloud/org/TODO.org"))
(setq org-directory "~/cloud/org")
(setq org-mobile-files '("~/cloud/org/movies.org" "~/cloud/org/TODO.org" "~/cloud/org/notes.org"))
(setq org-mobile-inbox-for-pull "~/cloud/org/notes.org")
(setq org-mobile-directory "~/cloud/org/mobileorg")

(add-hook 'org-mode-hook
  (lambda ()
    (add-to-list 'org-modules 'org-habit)
    (setq org-log-done 'time
          org-icalendar-include-todo t
          org-icalendar-use-scheduled '(todo-due event-if-todo event-if-not-todo)
          org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo)
          org-icalendar-timezone "Europe/Sarajevo"
          org-icalendar-include-bbdb-anniversaries t
          org-icalendar-store-UID t)))

;; something I can quickly call from eshell
(defun E (&rest args)
  "Invoke `find-file' on the file. \"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(setenv "EDITOR" "E")

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
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(newsticker-treeview-old-face ((t (:inherit nil))) t)
 '(org-agenda-date ((t (:inherit org-agenda-structure))) t))
