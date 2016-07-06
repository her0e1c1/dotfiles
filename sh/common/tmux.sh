
alias tmux-capture="tmux capture-pane"
alias tmux-show-current-pane-id="tmux display-message -p '#D'"
alias tmux-show-current-window-flag="tmux display-message -p '#F'"
alias tmux-list-buffers="tmux list-buffers"
# tmux set-option -g status-left "#[fg=colour255,bg=colour241]Session: #S #[default]"
tmux-show-all-buffers() { perl -E 'say `tmux show-buffer -b $_ ` for 1..20' }

# watchmedo shell-command -WR . -p "*.py" -c 'docker exec -it backend py.test tests.py || echo ERROR > /dev/pts/0'
