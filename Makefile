CP ?= cp

.PHONY: install install-emacs install-vim install-dotfiles

all: install

install: install-emacs install-vim install-dotfiles

install-emacs:
	$(CP) -R $< $(HOME)/.emacs.d

install-vim:
	$(CP) vimrc $(HOME)/.vimrc
	$(CP) -R vim $(HOME)/.vim

install-dotfiles: Xresources bashrc tmux.conf screenrc
	@for file in $^; do \
		$(CP) $$file $(HOME)/.$$file ; \
	done
