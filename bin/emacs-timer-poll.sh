#!/usr/bin/env sh

# Use this script if you need to display
# Emacs Org timer in your bar. (eww, waybar, etc)

emacsclient -e "(org-timer-value-string)"
