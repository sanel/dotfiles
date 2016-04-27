(require 'newsticker)

(defun lynx-renderer (start end &optional url charset)
  (interactive)
  (shell-command-on-region start end
						   (format "lynx -force_html -dump -stdin %s" url)
						   (current-buffer)))

(setq newsticker-html-renderer 'lynx-renderer)

;; for gnutls to remove warning
(setq gnutls-min-prime-bits 1024)

(set-face-foreground 'newsticker-treeview-face "gray")
(set-face-foreground 'newsticker-treeview-new-face "#00afaf")
(set-face-foreground 'newsticker-treeview-selection-face "black")
(set-face-foreground 'newsticker-treeview-old-face "gray")

(setq newsticker-keep-obsolete-items nil
	  newsticker-scroll-smoothly     nil
	  newsticker-use-full-width      nil
	  newsticker-sort-method         'sort-by-time
	  newsticker-retrieval-interval  3600
	  newsticker-treeview-treewindow-width 40
	  newsticker-treeview-listwindow-height 10
	  newsticker-automatically-mark-items-as-old nil
	  newsticker-automatically-mark-visited-items-as-old t
	  newsticker-url-list-defaults nil)

(setq newsticker-url-list
	  '(("KupiKartu" "http://www.kupikartu.ba/home/rss")
		("Slashdot" "http://slashdot.org/slashdot.rss")
		("LWN" "http://lwn.net/headlines/newrss")
		("Hacker News" "http://news.ycombinator.com/rss")
		("Reddit Salvage" "http://www.reddit.com/r/electronicssalvage/.rss")
		("Reddit Arduino" "http://www.reddit.com/r/arduino/.rss")
		("Reddit Lisp" "http://www.reddit.com/r/lisp/.rss")
		("Reddit Programming" "http://www.reddit.com/r/programming/.rss")
		("Reddit Scheme" "http://www.reddit.com/r/scheme/.rss")
		("Reddit Emacs" "http://www.reddit.com/r/emacs/.rss")
		("Reddit Clojure" "http://www.reddit.com/r/clojure/.rss")
		("Planet Clojure" "http://planet.clojure.in/atom.xml")
		("Clojure" "https://groups.google.com/forum/feed/clojure/topics/rss.xml?num=25")
		("NewLISP Forum" "http://www.newlispfanclub.alh.net/forum/feed.php")
		("Slackware Alien" "http://www.slackware.com/~alien/slackbuilds/ChangeLog.rss")
		("Slackbuilds" "http://slackbuilds.org/rss/ChangeLog.rss")
		("CommanlineFu" "http://www.commandlinefu.com/feed/tenup")
		("XE" "http://community.xe.com/taxonomy/term/15/feed")
		("Reuters" "http://feeds.reuters.com/reuters/businessNews?format=xml")
		("Pymnts" "http://www.pymnts.com/feed/")
		("BlackHat" "http://www.blackhatworld.com/blackhat-seo/external.php?type=RSS2")
		("Goremote" "https://goremote.io/rss")
		("SO Careers" "https://careers.stackoverflow.com/jobs/feed?allowsremote=True")
		))
