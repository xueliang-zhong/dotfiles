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
split-window -h -p 15

# three panes in utility panes
split-window -v -p 87
split-window -v

# back to the left pane
select-pane -L

# create the build & dgb pane
split-window -v -p 15
split-window -h -p 20
select-pane -t 1
