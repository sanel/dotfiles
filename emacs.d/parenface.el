(defface paren-face
  '((((class color) (background dark))
	 (:foreground "brightblack"))
	(((class color) (background light))
	 (:foreground "grey80")))
  "Face used to dim parentheses.")

(dolist (i '(emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook))
  (add-hook i
	(lambda ()
	  (font-lock-add-keywords nil
							  '(("(\\|)\\|\\[\\|\\]" . 'paren-face))))))
