#grep with PCRE support to PATH
export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"

# emacs to PATH
export PATH="$HOME/.config/emacs/bin:$PATH"

# golang to PATH
export GOPATH="$(go env GOPATH)"
export GOROOT="$(go env GOROOT)"
export GOBIN="$(go env GOBIN)"
export PATH="${PATH}:${GOPATH}/bin"
export PATH="${PATH}:${GOROOT}/bin"
export PATH="${PATH}:${GOBIN}/bin"

# sml New Jersey to PATH
export PATH=/usr/local/smlnj/bin:"$PATH"

# brew to PATH
export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/curl/bin:$PATH"
eval "$(rbenv init -)"

# asdf to PATH
. "$HOME/.asdf/asdf.sh"
fpath=(${ASDF_DIR}/completions $fpath)

# python and pip directories to PATH
export PATH="/Users/pavelzhilin/.local/bin:$PATH"
#venv alias
alias venv="if [ -e ./.venv/bin/activate ]; then source ./.venv/bin/activate; else python3 -m venv .venv && source ./.venv/bin/activate; fi"

# initialise completions with ZSH's compinit
autoload -Uz compinit && compinit

# integrate with fzf (probably Ctrl + R integration)
source <(fzf --zsh)

alias brew="arch -arm64 brew"
