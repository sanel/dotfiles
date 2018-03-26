(define-skeleton new-blog-post
  "Create new blog post."
  "Title: "
  "{:title \"" str "\""\n
  " :layout :post" \n
  ":tags [" _ "]}" \n
  '(progn
	 (markdown-mode)
	 (rename-buffer
	  (format "%s-%s.md"
			  (format-time-string "%Y-%m-%d")
			  (replace-regexp-in-string "[ \\t]+" "-" (downcase str))))))
