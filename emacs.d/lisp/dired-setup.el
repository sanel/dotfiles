(require 'dired-x)

(setq dired-dwim-target t  ;; allow copy between two dired split buffers
	  dired-guess-shell-alist-user
	  '(("\\.\\(?:pdf\\|mobi\\|epub\\)\\'" "/opt/mupdf/bin/mupdf-x11 -D black -P black")
		("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "ede-image-view")
		("\\.\\(?:xcf\\)\\'" "gimp")
		("\\.\\(?:odt\\|doc\\|rtf\\|docx\\|xlsx\\)\\'" "soffice")
		("\\.\\(?:mp4\\|mp3\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'" "vlc")))
