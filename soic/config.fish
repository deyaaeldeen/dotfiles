set --erase fish_greeting
# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Path to your custom folder (default path is ~/.oh-my-fish/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

# Custom plugins and themes may be added to ~/.oh-my-fish/custom
# Plugins and themes can be found at https://github.com/oh-my-fish/
Theme 'robbyrussell'
#Theme 'ocean'
#Theme 'bobthefish'
Plugin 'theme'
Plugin 'balias'


#-----------------------------------------------------------------
function fish_right_prompt
	  set_color $fish_color_autosuggestion[1]
	  date
	  set_color normal
end

set -x EDITOR "emc"

umask 077

set -x -g PATH /u/dalmahal/bin /u/dalmahal/.cabal/bin $HOME/bin /u/parfunc/opt/bin $PATH
set -x -g LANG en_US.UTF-8
set -x -g LD_LIBRARY_PATH /u/dalmahal/lib/lib

# Schml compiler
set -x schmlUnderConstruction 1

balias l 'ls -lhFa'
balias c 'reset'
balias .. 'cd ..'
balias ... 'cd ../../../'
balias .... 'cd ../../../../'
balias ..... 'cd ../../../../'
balias .4 'cd ../../../../'
balias .5 'cd ../../../../..'
balias h 'cd ~'
balias rm 'rm -rf'

balias gs "git status"
balias gc "git commit -am"
balias gh "git push"
balias gl "git pull"
balias glg 'git log'
balias gcn 'git clean -xfd'
balias gdr 'git push origin :'

balias rakt 'racket -il xrepl'

function extract
     if test -e $argv[1]
         switch $argv[1]
         case '*.tar.bz2'
	      tar xjf $argv[1]
         case '*.tar.gz'
	      tar xzf $argv[1]
         case '*.bz2'
	      bunzip2 $argv[1]
         case '*.rar'
	      rar x $argv[1]
         case '*.gz'
	      gunzip $argv[1]
         case '*.tar'
	      tar xf $argv[1]
         case '*.tbz2'
	      tar xjf $argv[1]
         case '*.tgz'
	      tar xzf $argv[1]
         case '*.zip'
	      unzip $argv[1]
         case '*.Z'
	      uncompress $argv[1]
         case '*.7z'
	      7z x $argv[1]
         case '*.tar.xz'
	      tar -xvJf $argv[1]
         case '*'
	      echo "$argv[1] cannot be extracted via extract"
	 end
	 switch $argv[2]
	 case 1
	    rm -rf $argv[1]
	 end
     else
         echo "$argv[1] is not a valid file"
     end
end

function shot
	/usr/bin/import -frame -strip -quality 75 "$HOME/"(date +%s)".png"
end

function sr
    find $argv[1] -type f -execdir sed -i "s/$argv[2]/$argv[3]/g" '{}' +
end

function prjcton
    xrandr --output HDMI1 --mode 1280x800
end

function prjctoff
    xrandr --output HDMI1 --off
end
