(defface paren-face
  '((((class color) (background dark))
	 (:foreground "#7f7f7f"))
	(((class color) (background light))
	 (:foreground "grey80")))
  "Face used to dim parentheses.")

(dolist (i '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook clojure-mode-hook))
  (add-hook i
	(lambda ()
	  (font-lock-add-keywords nil
							  '(("(\\|)\\|\\[\\|\\]" . 'paren-face))))))
