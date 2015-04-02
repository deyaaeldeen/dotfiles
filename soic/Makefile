

top = $(shell pwd)

all:
	ln -s $(top)/.ssh/config ~/.ssh/config
	ln -s $(top)/.bash_profile ~/.bash_profile
	ln -s $(top)/.bashrc ~/.bashrc
	ln -s $(top)/.emacs ~/.emacs
	ln -s $(top)/.gnus ~/.gnus
	ln -s $(top)/.authinfo ~/.authinfo
	ln -s $(top)/.signature ~/.signature
	ln -s $(top)/.gitconfig ~/.gitconfig
	ln -s $(top)/.tmux.conf ~/.tmux.conf

clean :
	-rm ~/.ssh/config ~/.bash_profile ~/.bashrc ~/.emacs ~/.gnus ~/.authinfo ~/.signature ~/.gitconfig ~/.tmux.conf
