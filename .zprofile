# Add `~/bin` to the `$PATH`
export PATH="$HOME/bin:$PATH";
# Add `~/.local/bin` to the `$PATH`
export PATH="$HOME/.local/bin:$PATH";
# Add `~/usr/local/sbin` to the `$PATH`
export PATH="/usr/local/sbin:$PATH";

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
for file in ~/.{exports,extra}; do
  [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

eval "$(ssh-agent -s)"

# nodebrew path
export PATH="$HOME/.nodebrew/current/bin:$PATH"

# Go path
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"

# Rust path
export PATH="/usr/local/bin:$HOME/bin:$PATH:$HOME/.cargo/bin"
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
