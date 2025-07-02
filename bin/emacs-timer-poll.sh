#!/usr/bin/env sh

# Use this script if you need to display
# Emacs Org timer in your bar. (eww, waybar, etc)
#
# It's also don't display timer in Emacs mode-line/frame-title

emacsclient -e "(progn (setq org-timer-display nil) (org-timer-value-string))"
