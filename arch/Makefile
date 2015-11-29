

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
	ln -s $(top)/xmonad.hs ~/.xmonad/xmonad.hs
	ln -s $(top)/config.fish ~/.config/fish/config.fish
	ln -s $(top)/.xbindkeysrc ~/.xbindkeysrc
	ln -s $(top)/.latexmkrc ~/.latexmkrc
	sudo ln -s $(top)/00-keyboard.conf /etc/X11/xorg.conf.d/00-keyboard.conf
	sudo ln -s $(top)/50-synaptics.conf /etc/X11/xorg.conf.d/50-synaptics.conf
	sudo ln -s $(top)/jre.sh /etc/profile.d/jre.sh
	sudo ln -s $(top)/emc /usr/local/bin/emc
	sudo ln -s $(top)/printers.conf /etc/cups/printers.conf

clean :
	-sudo rm -f ~/.ssh/config ~/.bash_profile ~/.bashrc ~/.emacs \
	~/.gnus ~/.authinfo ~/.signature ~/.signature2 ~/.gitconfig  \
	~/.tmux.conf ~/.Xresources ~/.xinitrc ~/.config/fish/config.fish \
	~/.xmonad/xmonad.hs /etc/X11/xorg.conf.d/00-keyboard.conf \
	/etc/X11/xorg.conf.d/50-synaptics.conf /etc/profile.d/jre.sh \
	/usr/local/bin/emc ~/.xbindkeysrc /etc/cups/printers.conf \
	~/.latexmkrc
