# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Theme
ZSH_THEME="robbyrussell"

# Plugins
plugins=(virtualenvwrapper git osx macports python github pip django mercurial fabric colored-man)

source $ZSH/oh-my-zsh.sh

# Ports
export PATH=$PATH:/opt/local/bin:/opt/local/sbin
export MANPATH=$MANPATH:/opt/local/share/man

# Virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
export WORKON_HOME=~/.virtualenvs
source /usr/local/bin/virtualenvwrapper_lazy.sh
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true

# Python
export PATH=$PATH:~/Library/Python/2.7/bin:~/Library/Python/2.7/lib
export PATH=$PATH:~/Library/Python/3.3/bin:~/Library/Python/3.3/lib
export PATH=$PATH:~/.local/bin
export PYTHONSTARTUP=$HOME/.python-startup.py

# Aliases
alias zshconfig="vim ~/.zshrc"
alias hosts="sudo vim /etc/hosts"
alias pgrep="pgrep -L"

alias erl="erl -sname localhost"
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
alias smtp-python="sudo python -m smtpd -c DebuggingServer -n localhost:25"
