(require 'newsticker)

(defun lynx-renderer (start end &optional url charset)
  (interactive)
  (shell-command-on-region start end
						   (format "lynx -force_html -dump -stdin %s" url)
						   (current-buffer)))

(setq newsticker-html-renderer 'lynx-renderer)

(setq newsticker-keep-obsolete-items nil
	  newsticker-scroll-smoothly     nil
	  newsticker-use-full-width      nil
	  newsticker-sort-method         'sort-by-time
	  newsticker-retrieval-interval  600
	  newsticker-treeview-treewindow-width 40
	  newsticker-treeview-listwindow-height 10)

(setq newsticker-url-list-defaults
	  '(("Emacs Wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss" nil 3600)
		("Slashdot" "http://slashdot.org/slashdot.rss" nil 3600)
		("Hacker News" "http://news.ycombinator.com/rss" nil 3600)
		("Reddit Programming" "http://www.reddit.com/r/programming/.rss" nil 3600)
		("Reddit Scheme" "http://www.reddit.com/r/scheme/.rss" nil 3600)
		("Reddit Lisp" "http://www.reddit.com/r/lisp/.rss" nil 3600)
		("Freshmeat" "http://freecode.com/?format=atom" nil 3600)
		("Planet Clojure" "http://planet.clojure.in/atom.xml" nil 3600)
		("Planet Gnome" "http://planet.gnome.org/atom.xml" nil 3600)
		("Hackful Europe" "http://hackful.com/frontpage.rss" nil 3600)
		("Clojure Google Group" "http://groups.google.com/group/clojure/feed/rss_v2_0_msgs.xml" nil 3600)))
