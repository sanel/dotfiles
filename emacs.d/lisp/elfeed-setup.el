(add-to-list 'load-path "~/.emacs.d/modes/elfeed")
(autoload 'elfeed "elfeed" "Runs elfeed." t)

(eval-after-load 'elfeed
  '(progn
	 (defun elfeed-open-url-eww ()
	   "Open url under point in eww. Opened in new buffer."
	   (interactive)
	   (let ((url (get-text-property (point) 'shr-url)))
		 (if url
			 (eww-browse-url url t)
		   (message "No URL under point"))))

	(defun org-elfeed-entry-store-link ()
	  "Captures elfeed entry link; suitable for org-mode links."
	  (when elfeed-show-entry
		(let* ((link (elfeed-entry-link elfeed-show-entry))
			   (title (elfeed-entry-title elfeed-show-entry)))
		  (org-store-link-props
		   :link link
		   :description title))))
	(add-hook 'org-store-link-functions 'org-elfeed-entry-store-link)))

(setq elfeed-feeds
  '(("http://www.klix.ba/rss/svevijesti" news)
	("http://communionafterdark.com/rss.xml" news)
	("http://www.kupikartu.ba/home/rss" news)
	("https://forum.linux.org.ba/posts.rss" tech)
	("http://lwn.net/headlines/newrss" tech)
	("https://www.discoverdev.io/rss.xml" tech)
	("https://blog.acolyer.org/feed/" tech)
	("http://news.ycombinator.com/rss" tech)
	("http://www.reddit.com/r/lisp/.rss" tech)
	("http://www.reddit.com/r/programming/.rss" tech)
	("http://www.reddit.com/r/scheme/.rss" tech)
	("http://www.reddit.com/r/emacs/.rss" tech)
	("http://www.reddit.com/r/clojure/.rss" tech)
	("http://planet.clojure.in/atom.xml" tech)
	("https://groups.google.com/forum/feed/clojure/topics/rss.xml?num=25" tech)
	("http://www.newlispfanclub.alh.net/forum/feed.php" tech)
	("http://www.slackware.com/~alien/slackbuilds/ChangeLog.rss" tech)
	("https://www.exploit-db.com/rss.xml" tech)
	("https://www.quantstart.com/feed" biz quant)
	("http://www.quantnews.com/feed/" biz quant)
	("https://www.kdnuggets.com/feed" biz quant)
	("http://automatedtrader.net/rss/magazine-articles" biz quant)
	("https://seekingalpha.com/feed.xml" biz)
	("https://www.akta.ba/rss" biz)
	("http://www.poslovni.hr/feeds/najnovije.xml" biz)
	("http://community.xe.com/taxonomy/term/15/feed" biz)
	("http://feeds.reuters.com/reuters/businessNews?format=xml" biz)
	("https://www.financemagnates.com/feed/" biz)
	("http://www.pymnts.com/feed/" biz)
	("https://rss.dailyfx.com/feeds/all" biz)
	("https://epchan.blogspot.com/feeds/posts/default?alt=rss" biz quant)
	("https://feeds.feedburner.com/Quantocracy" biz quant)
	("https://fxdiebold.blogspot.com/feeds/posts/default" quant)
	("https://goremote.io/rss" job)
	("https://careers.stackoverflow.com/jobs/feed?allowsremote=True" job)
	("https://alternativedata.org/feed/" data)
	("https://towardsdatascience.com/feed" data tech)
	("https://feeds.feedburner.com/FeaturedBlogPosts-DataScienceCentral" data tech)
	("https://www.reddit.com/r/datasets/.rss" data)
	))
