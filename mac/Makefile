

top = $(shell pwd)

all:
	ln -s $(top)/.ssh/config ~/.ssh/config
	ln -s $(top)/.bash_profile ~/.bash_profile
	ln -s $(top)/.bashrc ~/.bashrc
	ln -s $(top)/.emacs ~/.emacs
	ln -s $(top)/.gnus ~/.gnus
	ln -s $(top)/.authinfo ~/.authinfo
	ln -s $(top)/.signature ~/.signature
	ln -s $(top)/.signature2 ~/.signature2
	ln -s $(top)/.gitconfig ~/.gitconfig
	ln -s $(top)/.tmux.conf ~/.tmux.conf
	ln -s $(top)/config.fish ~/.config/fish/config.fish
	ln -s $(top)/.latexmkrc ~/.latexmkrc

clean :
	rm -f ~/.ssh/config ~/.bash_profile ~/.bashrc ~/.emacs \
	~/.gnus ~/.authinfo ~/.signature ~/.signature2 ~/.gitconfig  \
	~/.tmux.conf ~/.config/fish/config.fish	~/.latexmkrc
