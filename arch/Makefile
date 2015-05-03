

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
	ln -s $(top)/.Xresources ~/.Xresources
	ln -s $(top)/.xinitrc ~/.xinitrc
	sudo ln -s $(top)/00-keyboard.conf /etc/X11/xorg.conf.d/00-keyboard.conf
	sudo ln -s $(top)/50-synaptics.conf /etc/X11/xorg.conf.d/50-synaptics.conf

clean :
	-sudo rm ~/.ssh/config ~/.bash_profile ~/.bashrc ~/.emacs ~/.gnus ~/.authinfo ~/.signature ~/.gitconfig ~/.tmux.conf ~/.Xresources ~/.xinitrc /etc/X11/xorg.conf.d/00-keyboard.conf /etc/X11/xorg.conf.d/50-synaptics.conf
