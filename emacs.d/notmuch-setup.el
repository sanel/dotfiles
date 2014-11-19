(require 'notmuch)
(require 'notmuch-address)

(define-key notmuch-show-mode-map "d"
  (lambda ()
	"toggle deleted tag for message"
	(interactive)
	(if (member "deleted" (notmuch-show-get-tags))
		(notmuch-show-tag (list "-deleted"))
	  (notmuch-show-tag (list "+deleted")))))

(define-key notmuch-search-mode-map "d"
  (lambda ()
	"toggle deleted tag for message"
	(interactive)
	(if (member "deleted" (notmuch-search-get-tags))
		(notmuch-search-tag (list "-deleted"))
	  (notmuch-search-tag (list "+deleted")))))

(defun notmuch-get-date (date) 
  "Converts a date for notmuch processing" 
  (substring (shell-command-to-string (concat "date --date=\"" date "\" +%s")) 0 -1))

(defun notmuch-today ()
  "Shows today's mail"
  (interactive)
  (notmuch-search
   (concat
	(notmuch-get-date "today 0") ".." (notmuch-get-date "now"))))

(defun notmuch-week ()
  "Shows this week's mail"
  (interactive)
  (notmuch-search
   (concat
	(notmuch-get-date "last monday") ".." (notmuch-get-date "now"))))

(defun notmuch-unread ()
  "Shows unread mail"
  (interactive)
  (notmuch-search "tag:unread"))

(evil-define-command evil-nms (&optional query)
  (interactive "<a>")
  (notmuch-search query))

(define-key notmuch-show-mode-map "j" 'next-line)
(define-key notmuch-show-mode-map "k" 'previous-line)
(define-key notmuch-search-mode-map "j" 'next-line)
(define-key notmuch-search-mode-map "k" 'previous-line)
(define-key notmuch-show-mode-map ":" 'evil-ex)
(define-key notmuch-search-mode-map ":" 'evil-ex)

(evil-ex-define-cmd "nmu" 'notmuch-unread)
(evil-ex-define-cmd "nmt" 'notmuch-today)
(evil-ex-define-cmd "nmw" 'notmuch-week)
(evil-ex-define-cmd "nms" 'evil-nms)
(evil-ex-define-cmd "nmc" 'notmuch-mua-new-mail)

;; options 

(setq mail-specify-envelope-from t
	  mail-envelope-from 'header
	  message-sendmail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      mm-text-html-renderer 'lynx
	  browse-url-browser-function 'browse-url
	  notmuch-mua-hidden-headers '()
	  notmuch-mua-user-agent-function'notmuch-mua-user-agent-notmuch
	  ; notmuch-crypto-process-mime t
      notmuch-address-command "~/Maildir/notmuch-addrlookup")

(notmuch-address-message-insinuate)
