#install oh my fish bin/install

# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG "/home/deyaa/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish


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

umask 022

set -x -g PATH $PATH /home/deyaa/.local/bin /usr/local/bin /home/deyaa/.cabal/bin /usr/local/texlive/2014/bin/x86_64-linux
set -x -g LANG en_US.UTF-8
set -x -g INFOPATH $INFOPATH /usr/local/texlive/2014/texmf-dist/doc/info
set -x -g MANPATH $MANPATH /usr/local/texlive/2014/texmf-dist/doc/man
set -x -g WINEPREFIX /mnt/disk/.wine

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
balias silo "ssh -t -X silo '. ~/.bashrc; exec tmux attach'"
balias chris "ssh -t -X chris '. ~/.bashrc; exec tmux attach'"
balias schml "ssh -t -X silo '. ~/.bashrc; exec fish'"

balias img 'sxiv'
balias 'cdrom' 'sudo mount -a /dev/sr0' # pass the path to mount the cd in
balias wrk 'cd /mnt/disk/Work/Indiana/gradual/'
balias sd 'cd /mnt/disk/Study/Indiana'
balias t 'cd /mnt/disk/Teaching'
balias is "/mnt/disk/Study/Indiana/B522/./isabelle.sh"
balias kindle "wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Amazon/Kindle/Kindle For PC/Kindle.exe"
balias wrd "wine /mnt/disk/.wine/drive_c/Program\ Files/Microsoft\ Office/Office14/WINWORD.EXE"
balias pwr "wine /mnt/disk/.wine/drive_c/Program\ Files/Microsoft\ Office/Office14/POWERPNT.EXE"
balias maple "/opt/Citrix/ICAClient/wfica.sh ~/Downloads/launch.ica"
balias nwrk "sudo systemctl restart netctl-auto@wlp2s0.service"
balias vb "sudo modprobe vboxdrv; virtualbox"
balias bat "upower -i (upower -e | grep 'BAT') | grep -E \"state|to\ full|percentage\""
balias sz "du -sh"
balias sound "amixer set Master toggle"
balias disk "ncdu"
balias ltx "umask 022;sudo tlmgr update --all"
balias free "df -h"
balias hib "sudo systemctl hibernate"

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
balias syu 'sudo pacman -Syu;aurget --noconfirm --noedit --deps -Syu' # system upgrade
balias a-s 'aurget --noconfirm --noedit --deps -S'

balias rakt 'racket -il xrepl'

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
