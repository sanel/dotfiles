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
(evil-ex-define-cmd "mail-sign" 'mml-secure-message-sign-pgpmime)
(evil-ex-define-cmd "mail-encrypt" 'mml-secure-encrypt-pgpmime)
(evil-ex-define-cmd "mail-decrypt" 'epa-decrypt-region)

;; epa-mail-decrypt
;; epa-mail-verify
;; epa-mail-sign/epa-mail-encrypt

;; options 

(setq mail-specify-envelope-from t
	  mail-envelope-from 'header
	  message-sendmail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      mm-text-html-renderer 'lynx
	  notmuch-mua-hidden-headers '()
	  notmuch-mua-user-agent-function 'notmuch-mua-user-agent-notmuch
	  notmuch-message-headers '("Subject" "To" "Cc" "Date" "User-Agent")
	  notmuch-mua-hidden-headers nil
	  notmuch-crypto-process-mime t
      notmuch-address-command "~/Maildir/notmuch-addrlookup"
	  ;message-user-fqdn "foo.com"
	  ;notmuch-search-line-faces '(("unread" :foreground "cyan")
	  ;							  ("flagged" :foreground "blue"))
	  )

(notmuch-address-message-insinuate)
