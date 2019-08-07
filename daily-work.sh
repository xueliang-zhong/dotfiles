#!/bin/bash
cd ~/Dropbox/vim
cat xzhong-work-links.txt | sed "s/\[/ /g" | sed "s/\]/ /g" | grep -v "#" | grep -v "^$" | awk '{print $1}' | sort | uniq > /tmp/focus.txt
FOCUS_TASK=$(cat /tmp/focus.txt | fzf)
grep $FOCUS_TASK xzhong-work-links.txt | fzf | awk '{print $2}' | xargs google-chrome
