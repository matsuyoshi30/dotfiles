#!/usr/bin/env bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

BACKUP=false
FORCE=false
DRY_RUN=false

usage() {
  cat << EOF
Usage: $(basename "$0") [OPTIONS]

OPTIONS:
    -b, --backup     Backup existing files with .bak suffix before creating symlinks
    -f, --force      Overwrite existing files without backup
    -n, --dry-run    Show what would be done without making any changes
    -h, --help       Show this help message

EXAMPLES:
    $(basename "$0")              # Interactive mode (asks before overwriting)
    $(basename "$0") -b           # Backup existing files
    $(basename "$0") -f           # Force overwrite without backup
    $(basename "$0") -n           # Dry run mode

EOF
}

while [[ $# -gt 0 ]]; do
  case $1 in
    -b|--backup)
      BACKUP=true
      shift
      ;;
    -f|--force)
      FORCE=true
      shift
      ;;
    -n|--dry-run)
      DRY_RUN=true
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo -e "${RED}Error: Unknown option $1${NC}"
      usage
      exit 1
      ;;
  esac
done

info() {
  echo -e "${BLUE}[INFO]${NC} $1"
}

success() {
  echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warn() {
  echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
  echo -e "${RED}[ERROR]${NC} $1"
}

ensure_dir() {
  local dir="$1"
  if [[ ! -d "$dir" ]]; then
    if [[ "$DRY_RUN" == true ]]; then
      info "Would create directory: $dir"
    else
      mkdir -p "$dir"
      success "Created directory: $dir"
    fi
  fi
}

backup_item() {
  local item="$1"
  local backup="${item}.bak"

  if [[ "$DRY_RUN" == true ]]; then
    info "Would backup $item to $backup"
  else
    mv "$item" "$backup"
    success "Backed up $item to $backup"
  fi
}

create_symlink() {
  local source="$1"
  local target="$2"
  local target_dir="$(dirname "$target")"

  ensure_dir "$target_dir"

  if [[ ! -e "$source" ]]; then
    error "Source does not exist: $source"
    return 1
  fi

  if [[ -e "$target" ]] || [[ -L "$target" ]]; then
    if [[ -L "$target" ]] && [[ "$(readlink "$target")" == "$source" ]]; then
      info "Already linked: $target -> $source"
      return 0
    fi

    if [[ "$BACKUP" == true ]]; then
      backup_item "$target"
    elif [[ "$FORCE" == true ]]; then
      if [[ "$DRY_RUN" == true ]]; then
        info "Would remove: $target"
      else
        rm -rf "$target"
        info "Removed: $target"
      fi
    elif [[ "$DRY_RUN" == true ]]; then
      warn "Target already exists: $target (would ask for confirmation)"
      info "Skipped in dry-run mode: $target"
      return 0
    else
      warn "Target already exists: $target"
      read -p "Overwrite? [y/N] " -n 1 -r
      echo
      if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        info "Skipped: $target"
        return 0
      fi
      rm -rf "$target"
    fi
  fi

  if [[ "$DRY_RUN" == true ]]; then
    success "Would link: $target -> $source"
  else
    ln -s "$source" "$target"
    success "Linked: $target -> $source"
  fi
}

main() {
  echo -e "${BLUE}=====================================${NC}"
  echo -e "${BLUE}  Dotfiles Installation Script${NC}"
  echo -e "${BLUE}=====================================${NC}"
  echo

  if [[ "$DRY_RUN" == true ]]; then
    warn "DRY RUN MODE - No changes will be made"
    echo
  fi

  info "Installing Bash configuration..."
  create_symlink "$SCRIPT_DIR/.bash/.bashrc" "$HOME/.bashrc"
  create_symlink "$SCRIPT_DIR/.bash/.bash_profile" "$HOME/.bash_profile"
  echo

  info "Installing Zsh configuration..."
  create_symlink "$SCRIPT_DIR/.zsh/.zshrc" "$HOME/.zshrc"
  create_symlink "$SCRIPT_DIR/.zsh/.zprofile" "$HOME/.zprofile"
  echo

  info "Installing Tmux configuration..."
  create_symlink "$SCRIPT_DIR/.tmux.conf" "$HOME/.tmux.conf"
  echo

  info "Installing WezTerm configuration..."
  create_symlink "$SCRIPT_DIR/.wezterm.lua" "$HOME/.wezterm.lua"
  echo

  info "Installing Emacs configuration..."
  create_symlink "$SCRIPT_DIR/.emacs.d" "$HOME/.emacs.d"
  echo

  info "Installing Git configuration..."
  create_symlink "$SCRIPT_DIR/git/config" "$HOME/.config/git/config"
  create_symlink "$SCRIPT_DIR/git/ignore" "$HOME/.config/git/ignore"
  echo

  info "Installing Ghostty configuration..."
  create_symlink "$SCRIPT_DIR/ghostty/config" "$HOME/.config/ghostty/config"
  echo

  info "Installing Helix configuration..."
  create_symlink "$SCRIPT_DIR/helix/config.toml" "$HOME/.config/helix/config.toml"
  echo

  info "Installing Claude configuration..."
  create_symlink "$SCRIPT_DIR/.claude/settings.json" "$HOME/.claude/settings.json"
  create_symlink "$SCRIPT_DIR/.claude/statusline.sh" "$HOME/.claude/statusline.sh"
  create_symlink "$SCRIPT_DIR/.claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"
  create_symlink "$SCRIPT_DIR/.claude/agents" "$HOME/.claude/agents"
  create_symlink "$SCRIPT_DIR/.claude/commands" "$HOME/.claude/commands"
  create_symlink "$SCRIPT_DIR/.claude/skills" "$HOME/.claude/skills"

  echo -e "${GREEN}=====================================${NC}"
  if [[ "$DRY_RUN" == true ]]; then
    echo -e "${GREEN}  Dry run completed!${NC}"
  else
    echo -e "${GREEN}  Installation completed!${NC}"
  fi
  echo -e "${GREEN}=====================================${NC}"
}

main
