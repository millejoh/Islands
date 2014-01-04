"""Utility functions for printing formatted text in a window.
"""

import tcod

def make_colored_string(string, dialog=False, window=None):
    
def draw_string(win, x, y, string, dialog=False, background_flag = tcod.BKGND_SET):
    xstr = colorize_string(string, dialog, window=win)


