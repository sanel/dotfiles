(defface paren-face
  '((((class color) (background dark))
	 (:foreground "grey20"))
	(((class color) (background light))
	 (:foreground "grey80")))
  "Face used to dim parentheses.")

(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("(\\|)" . 'paren-face)))))
