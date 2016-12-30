# right panes are utility panes
split-window -h -l 15

# three panes in utility panes
split-window -v
split-window -v

# back to the left pane
select-pane -L

# create the command line pane
split-window -v -l 5
select-pane -U
