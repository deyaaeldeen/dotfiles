# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

#-----------------------------------------------------------------
set fish_greeting ""

function fish_right_prompt
	  set_color $fish_color_autosuggestion[1]
	  date
	  set_color normal
end

set -x EDITOR "emacs"

umask 022

set -x -g PATH $PATH /usr/local/bin ~/.cabal/bin
set -x -g LANG en_US.UTF-8

balias l 'ls -lhFa'
balias .. 'cd ..'
balias ... 'cd ../../../'
balias .... 'cd ../../../../'
balias ..... 'cd ../../../../'
balias .4 'cd ../../../../'
balias .5 'cd ../../../../..'
balias k "killall -9"
balias h 'cd ~'
balias hh 'history'
balias rm 'rm -rf'
balias silo "ssh -t -X silo '. ~/.bashrc; exec tmux attach'"
balias chris "ssh -t -X chris '. ~/.bashrc; exec tmux attach'"

balias ltx "umask 022;sudo tlmgr update --all"

balias gs "git status"
balias gc "git commit -am"
balias gh "git push"
balias gl "git pull"
balias glg 'git log'
balias gcn 'git clean -xfd'
balias gdr 'git push origin :'

function extract
     if test -e $argv[1]
         switch $argv[1]
         case '*.tar.bz2'
	      tar xjf $argv[1] -C $argv[2]
         case '*.tar.gz'
	      tar xzf $argv[1] -C $argv[2]
         case '*.bz2'
	      bunzip2 $argv[1]
         case '*.rar'
	     unrar e $argv[1] $argv[2]
         case '*.gz'
	      gunzip $argv[1]
         case '*.tar'
	      tar xf $argv[1] -C $argv[2]
         case '*.tbz2'
	      tar xjf $argv[1] -C $argv[2]
         case '*.tgz'
	      tar xzf $argv[1] -C $argv[2]
         case '*.zip'
	      unzip $argv[1] -d $argv[2]
         case '*.Z'
	      uncompress $argv[1]
         case '*.7z'
	      7z x $argv[1]
         case '*.tar.xz'
	      tar -xvJf $argv[1] -C $argv[2]
         case '*'
	      echo "$argv[1] cannot be extracted via extract"
	 end
	 switch $argv[3]
	 case 1
	    rm -rf $argv[1]
	 end
     else
         echo "$argv[1] is not a valid file"
     end
end

function sr
    find $argv[1] -type f -execdir sed -i "s/$argv[2]/$argv[3]/g" '{}' +
end
