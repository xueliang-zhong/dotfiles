# Create following window/pane layout out the screen:
#
# +------------------------------------+-----+
# |                                    |date |
# |                                    +-----+
# |               VIM                  |todo |
# |                                    +-----+
# |                                    |     |
# +------------------------------+-----|vimrc|
# |             build            | dbg |     |
# +------------------------------+-----+-----+
#

# right panes are utility panes
split-window -h -l 16 "watch -n 10 date"

# three panes in utility panes
split-window -v -p 87 "vim /tmp/todo"    # for todo list today.
split-window -v       "vim -RM ~/.vimrc" # for vim key mapping references.

# back to the left pane
select-pane -L

# create the build & dgb pane
split-window -v -p 15
# split-window -h -p 20  # dbg pane, not so useful.
select-pane -t 1
