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
	
# Start X at login
if status --is-login
  if test -z "$DISPLAY" -a $XDG_VTNR = 1
    exec startx -- -keeptty
  end
end

set -x EDITOR "emc"

umask 077

set -x -g PATH $PATH /usr/local/bin /home/deyaa/.cabal/bin /usr/local/texlive/2014/bin/x86_64-linux
set -x -g LANG en_US.UTF-8
set -x -g INFOPATH $INFOPATH /usr/local/texlive/2014/texmf-dist/doc/info
set -x -g MANPATH $MANPATH /usr/local/texlive/2014/texmf-dist/doc/man

# Schml compiler
set -x schmlUnderConstruction 1

balias l 'ls -lhFa'
balias c 'reset'
balias r 'sudo reboot'
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

balias img 'sxiv'
balias 'cdrom' 'sudo mount -a /dev/sr0' # pass the path to mount the cd in
balias wrk 'cd /mnt/disk/Work/Indiana/gradual/'
balias sd 'cd /mnt/disk/Study/Indiana'
balias t 'cd /mnt/disk/Teaching'
balias is "/mnt/disk/Study/Indiana/B522/./isabelle.sh"
balias htop "sudo htop"

balias gs "git status"
balias gc "git commit -am"
balias gh "git push"
balias gl "git pull"
balias glg 'git log'
balias gcn 'git clean -xfd'
balias gdr 'git push origin :'

balias p-s 'sudo pacman -S'      #install
balias p-rs 'sudo pacman -Rs'    #remove plus unused dependencies
balias p-scc 'sudo pacman -Scc'  #clean cache - all pkgs
balias p-sc 'sudo pacman -Sc'    #clean cache - old pkgs only
balias p-ss 'sudo pacman -Ss'    #query database
balias p-qs 'sudo pacman -Qs'    #query installed only
balias p-si 'sudo pacman -Si'    #pkg info
balias p-qi 'sudo pacman -Qi'    #pkg more info
balias p-qe 'sudo pacman -Qe'    #list explicitely installed pkgs
balias p-ql 'sudo pacman -Ql'    #find pkg file list
balias p-qo 'sudo pacman -Qo'    #/path/to/file   find owner
balias p-sf 'sudo pacman -Sf'    #reinstall - for dep problem
balias syu 'sudo pacman -Syu;aurget --deps -Syu' # system upgrade
balias a-s 'aurget --deps -S'

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