#!/bin/bash
export PATH=$PATH:~/Library/Python/2.7/bin
export PATH=$PATH:~/Library/Python/3.3/bin
export PATH=$PATH:~/.local/bin

$(which pyflakes) "$1"
$(which pep8) --repeat "$1"
true
