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

# Java path
export JAVA_PATH="/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home"
export PATH="$PATH:$JAVA_PATH/bin"

# Scala
export PATH="$PATH:/Users/matsuyoshi/Library/Application Support/Coursier/bin"

# anyenv path
anyenv=$(which anyenv)
if [ -x $anyenv ] ; then
  export PATH="$PATH:$HOME/.anyenv/bin"
  eval "$(anyenv init -)"
fi

rbenv=$(which rbenv)
if [ -x $rbenv ] ; then
  eval "$(rbenv init -)"
fi

# bun
[ -s "/Users/matsuyoshi/.bun/_bun" ] && source "/Users/matsuyoshi/.bun/_bun"
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# texinfo
if [[ $(uname -m) == "x86_64" ]]; then
  export PATH="$PATH:/usr/local/opt/texinfo/bin"
else
  export PATH="$PATH:/opt/homebrew/opt/texinfo/bin"
fi

# Org Sync Root Path
export ORGSYNCROOT="~/Dropbox"

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
for file in ~/.{exports,extra}; do
  [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

eval "$(ssh-agent -s)"
if [[ $(uname -m) == "x86_64" ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
else
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

export AWS_DEFAULT_PROFILE=saml

if [ -d ~/.orbstack ]; then
  source ~/.orbstack/shell/init.zsh 2>/dev/null || :
fi

# Added by swiftly
. "/Users/matsuyoshi/.swiftly/env.sh"