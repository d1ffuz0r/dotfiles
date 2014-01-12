# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Theme
ZSH_THEME="robbyrussell"
# Plugins
plugins=(virtualenvwrapper git osx macports python github pip django mercurial fabric colored-man vagrant)

source $ZSH/oh-my-zsh.sh

# Ports
export PATH=$PATH:/opt/local/bin:/opt/local/sbin
export MANPATH=$MANPATH:/opt/local/share/man

# Virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON=$(which python)
export WORKON_HOME=$HOME/.virtualenvs
source $(which virtualenvwrapper.sh)
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true

# Python
export PATH=$PATH:$HOME/Library/Python/2.7/bin:$HOME/Library/Python/2.7/lib
export PATH=$PATH:$HOME/Library/Python/3.3/bin:$HOME/Library/Python/3.3/lib
export PATH=$PATH:$HOME/.local/bin
export PYTHONSTARTUP=$HOME/.python-startup.py

# Aliases
alias zshconfig="vim ~/.zshrc"
alias hosts="sudo vim /etc/hosts"
alias pgrep="pgrep -fil"
alias pkill="pkill -f"

alias bb="brunch b"
alias bw="brunch w"

alias erl="erl -sname localhost"
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias smtp-python="sudo python -m smtpd -c DebuggingServer -n localhost:25"

alias start-ubuntu="VBoxHeadless -s ubuntu"
