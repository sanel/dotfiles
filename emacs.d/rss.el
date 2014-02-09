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

(set-face-foreground 'newsticker-treeview-face "gray")
(set-face-foreground 'newsticker-treeview-face "gray")
(set-face-foreground 'newsticker-treeview-selection-face "black")
(set-face-foreground 'newsticker-treeview-old-face "gray")

(setq newsticker-keep-obsolete-items nil
	  newsticker-scroll-smoothly     nil
	  newsticker-use-full-width      nil
	  newsticker-sort-method         'sort-by-time
	  newsticker-retrieval-interval  600
	  newsticker-treeview-treewindow-width 40
	  newsticker-treeview-listwindow-height 10
	  newsticker-automatically-mark-items-as-old t
	  newsticker-automatically-mark-visited-items-as-old t)

(setq newsticker-url-list-defaults
	  '(("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600)
		("Slashdot" "http://slashdot.org/slashdot.rss" nil 3600)
		("LWN" "http://lwn.net/headlines/newrss" nil 3600)
		("Hacker News" "http://news.ycombinator.com/rss" nil 3600)
		("Reddit Programming" "http://www.reddit.com/r/programming/.rss" nil 3600)
		("Reddit Scheme" "http://www.reddit.com/r/scheme/.rss" nil 3600)
		("Reddit Lisp" "http://www.reddit.com/r/lisp/.rss" nil 3600)
		("Freshmeat" "http://freecode.com/?format=atom" nil 3600)
		("Planet Clojure" "http://planet.clojure.in/atom.xml" nil 3600)
		("Planet Gnome" "http://planet.gnome.org/atom.xml" nil 3600)
		("Higher Perspective" "http://altering-perspectives.com/feed" nil 3600)
		("NewLISP Forum" "http://www.newlispfanclub.alh.net/forum/feed.php" nil 3600)
		("Slackware Alien" "http://www.slackware.com/~alien/slackbuilds/ChangeLog.rss" nil 3600)
		("Slackbuilds" "http://slackbuilds.org/rss/ChangeLog.rss" nil 3600)
		("Clojure" "https://groups.google.com/forum/feed/clojure/topics/rss.xml?num=15" nil 3600)
		("ULK Forum" "http://forum.linux.org.ba/extern.php?action=feed&type=atom" nil 3600)))
