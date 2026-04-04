# Rust path (PATH is set by .cargo/env in .zshenv)
if [[ $(uname -m) == "x86_64" ]]; then
  export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
else
  export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-aarch64-apple-darwin/lib/rustlib/src/rust/src"
fi

# Homebrew location depends on architecture, so discover it once per login shell.
if [[ $(uname -m) == "x86_64" ]]; then
  export PATH="$PATH:/usr/local/opt/texinfo/bin:/usr/local/sbin"
else
  export PATH="$PATH:/opt/homebrew/opt/texinfo/bin:/usr/local/sbin"
fi

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
for file in ~/.{exports,extra}; do
  [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# eval "$(ssh-agent -s)"
if [[ $(uname -m) == "x86_64" ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
else
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

if [ -d ~/.orbstack ]; then
  source ~/.orbstack/shell/init.zsh 2>/dev/null || :
fi

# Added by swiftly
[ -s "$HOME/.swiftly/env.sh" ] && . "$HOME/.swiftly/env.sh"
