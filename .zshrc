# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Theme
ZSH_THEME="robbyrussell"

# Plugins
plugins=(git os macports python virtualenvwrapper github pip django mercurial)

source $ZSH/oh-my-zsh.sh

# Ports
export PATH=$PATH:/opt/local/bin:/opt/local/sbin
export MANPATH=$MANPATH:/opt/local/share/man

# Virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
export WORKON_HOME=~/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true

# Python
export PATH=$PATH:~/Library/Python/2.7/bin:~/Library/Python/2.7/lib
export PATH=$PATH:~/Library/Python/3.3/bin:~/Library/Python/3.3/lib
export PATH=$PATH:~/.local/bin

# Aliases
alias zshconfig="vim ~/.zshrc"
alias hosts="sudo vim /etc/hosts"

alias erl="erl -sname localhost"
alias ec='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
