# Add `~/bin`, `~/.local/bin`, and `/usr/local/sbin` to the `$PATH`
export PATH="$PATH:$HOME/bin:$HOME/.local/bin:/usr/local/sbin";

# nodebrew path
export PATH="$PATH:$HOME/.nodebrew/current/bin"

# Go path
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"

# Rust path
export PATH="$PATH:$HOME/.cargo/bin"
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"

# anyenv path
anyenv=$(which anyenv)
if [ -x "anyenv" ] ; then
    export PATH="$PATH:$HOME/.anyenv/bin"
    eval "$(anyenv init -)"
fi

# direnv path
direnv=$(which direnv)
if [ -x "direnv" ] ; then
    eval "$(direnv hook zsh)"
fi

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
for file in ~/.{exports,extra}; do
  [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

eval "$(ssh-agent -s)"
