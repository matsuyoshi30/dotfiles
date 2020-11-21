if [ -f ~/.bashrc ]; then
	source ~/.bashrc
fi
export PATH="/usr/local/bin:$HOME/bin:$PATH:$HOME/.cargo/bin"
export RUST_SRC_PATH="$HOME/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
