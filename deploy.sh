set -o nounset
set -o errexit

# Define a function to handle cleanup on errors
cleanup() {
    echo -e "\nAn error occurred. Exiting script."
}

# Set up a trap to call `cleanup` on EXIT if an error occurs
trap cleanup EXIT

copy_dotfiles() {
    # Define the source directory (current directory) and the target directory ($HOME)
    local dotfiles_dir="$PWD"
    local target_dir="$HOME"
    local script_name="$(basename "$0")"  # Get the name of the script itself

    # Check if $HOME exists
    if [ ! -d "$target_dir" ]; then
        echo "Error: HOME directory ($target_dir) does not exist."
        return 1
    fi

    printf "\n"
    # Find files in the dotfiles directory, excluding .git, .gitignore, emacs/ directory, and the script itself
    find "$dotfiles_dir" -type f \
        ! -path "$dotfiles_dir/.git/*" \
        ! -name ".gitignore" \
        ! -path "$dotfiles_dir/emacs/*" \
        ! -name "$script_name" \
        -exec cp {} "$target_dir" \;

    printf "\n\nDotfiles copied to $HOME, excluding .git, .gitignore, emacs/, and this script ($script_name)\n\n"
}

# Run the function
copy_dotfiles

# Disable the trap on successful completion
trap - EXIT
