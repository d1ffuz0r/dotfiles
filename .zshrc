# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_DISABLE_COMPFIX=true
DISABLE_AUTO_UPDATE=true
# Theme
ZSH_THEME="pygmalion"
ZSH_DISABLE_COMPFIX=true
# Plugins
plugins=(git tmux)

source $ZSH/oh-my-zsh.sh

# Ports
export PATH=$PATH:/opt/local/bin:/opt/local/sbin
export MANPATH=$MANPATH:/opt/local/share/man

# Virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON=$(which python3)
export WORKON_HOME=$HOME/.virtualenvs
source /Library/Frameworks/Python.framework/Versions/Current/bin/virtualenvwrapper.sh
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true

# Python
export PATH=$PATH:$HOME/Library/Python/3.9/bin:$HOME/Library/Python/3.9/lib
export PATH=$PATH:$HOME/.local/bin
export PYTHONSTARTUP=$HOME/.python-startup.py
export PYTHONPATH=$PYTHONPATH:premiere

# Aliases
alias zshconfig="vim ~/.zshrc"
alias hosts="sudo vim /etc/hosts"
alias pgrep="pgrep -fil"
alias pkill="pkill -f"

alias gst="git status -sb"
alias gdc="git diff --cached"
alias glm="git pull origin master"
alias gp="git push origin HEAD"
alias gpf="git push origin HEAD -f"

alias smtp-python="sudo python -m smtpd -c DebuggingServer -n localhost:25"

compinit -u

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

