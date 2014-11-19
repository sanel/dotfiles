(require 'newsticker)

(defun lynx-renderer (start end &optional url charset)
  (interactive)
  (shell-command-on-region start end
						   (format "lynx -force_html -dump -stdin %s" url)
						   (current-buffer)))

;; for browse-url
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setq newsticker-html-renderer 'lynx-renderer)

;; for gnutls to remove warning
(setq gnutls-min-prime-bits 1024)

(set-face-foreground 'newsticker-treeview-face "gray")
(set-face-foreground 'newsticker-treeview-face "gray")
(set-face-foreground 'newsticker-treeview-selection-face "black")
(set-face-foreground 'newsticker-treeview-old-face "gray")

(setq newsticker-keep-obsolete-items nil
	  newsticker-scroll-smoothly     nil
	  newsticker-use-full-width      nil
	  newsticker-sort-method         'sort-by-time
	  newsticker-retrieval-interval  3600
	  newsticker-treeview-treewindow-width 40
	  newsticker-treeview-listwindow-height 10
	  newsticker-automatically-mark-items-as-old t
	  newsticker-automatically-mark-visited-items-as-old t
	  newsticker-url-list-defaults nil)

(setq newsticker-url-list
	  '(("Slashdot" "http://slashdot.org/slashdot.rss")
		("LWN" "http://lwn.net/headlines/newrss")
		("Hacker News" "http://news.ycombinator.com/rss")
		("Reddit Programming" "http://www.reddit.com/r/programming/.rss")
		("Reddit Salvage" "http://www.reddit.com/r/electronicssalvage/.rss")
		("Reddit Scheme" "http://www.reddit.com/r/scheme/.rss")
		("Reddit Lisp" "http://www.reddit.com/r/lisp/.rss")
		("Reddit Arduino" "http://www.reddit.com/r/arduino/.rss")
		("Freshmeat" "http://freecode.com/?format=atom")
		("Planet Clojure" "http://planet.clojure.in/atom.xml")
		("Planet Gnome" "http://planet.gnome.org/atom.xml")
		("Higher Perspective" "http://altering-perspectives.com/feed")
		("NewLISP Forum" "http://www.newlispfanclub.alh.net/forum/feed.php")
		("Slackware Alien" "http://www.slackware.com/~alien/slackbuilds/ChangeLog.rss")
		("Slackbuilds" "http://slackbuilds.org/rss/ChangeLog.rss")
		("Clojure" "https://groups.google.com/forum/feed/clojure/topics/rss.xml?num=25")
		("Disclose.tv" "http://www.disclose.tv/media/rss")
		("ULK Forum" "http://forum.linux.org.ba/extern.php?action=feed&type=atom")))
