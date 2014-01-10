"""Utility functions for working with colors.
"""

from collections import namedtuple
from warnings import warn

RGBColor = namedtuple('RGBColor', ['r', 'g', 'b'])
COLOR_TABLE = { 'true-black':   (0x00, 0x00, 0x00),
                'true-pink':    (0xFF, 0x00, 0xFF),
                'true-white':   (0xFF, 0xFF, 0xFF),
                'true-red':     (0xFF, 0x00, 0x00),
                'true-green':   (0x00, 0xFF, 0x00),
                'true-blue':    (0x00, 0x00, 0xFF),
                'black':        (0x00, 0x00, 0x00),
                'dark-grey':    (96, 96, 96),
                'grey':         (196, 196, 196),
                'white':        (255, 255, 255),
                'blue':         (13, 103, 196),
                'dark-blue':    (40, 40, 128),
                'light-blue':   (120, 120, 255),
                'dark-red':     (128, 0, 0),
                'light-red':    (255, 100, 50),
                'dark-brown':   (32, 16, 0),
                'light-yellow': (255, 255, 150),
                'yellow':       (255, 255, 0),
                'dark-yellow':  (164, 164, 0),
                'green':        (0, 220, 0),
                'cyan':         (86, 163, 205),
                'orange':       (255, 150, 0),
                'red':          (255, 0, 0),
                'silver':       (203, 203, 203),
                'gold':         (255, 255, 102),
                'purple':       (204, 51, 153),
                'dark-purple':  (51, 0, 51),
                'slate-grey':   (0x80, 0x80, 0x80),
                'umber':        (0x80, 0x40, 0),
                'pink':         (0xFF, 0x00, 0xFF),
                'chocolate':    (210, 105, 30)}


def compose_color(r, g, b):
    """Given r, g, b color values return 3 byte integer whose value is
    0xBBGGRR.

    Parameters
    ----------
    r : Red component, integer 0-255.
    g : Green component, integer 0-255.
    b : Blue component, integer 0-255.
    """
    return r + (g << 8) + (b << 16)

def decompose_color(color):
    """Given an integer value for a color, return RGB components as a tuple.

    Parameters
    ----------
    color : Integer number 0xBBGGRR.

    """
    return RGBColor((color & 0x0000ff), (color & 0x00ff00) >> 8,
                    (color & 0xff0000) >> 16)

def string_to_colornum(color):
    """Given a color namestring (i.e. 'red' or 'blue'), return
    a integer representing that color for use with TCOD.
    """
    try:
        return compose_color(*COLOR_TABLE[color])
    except:
        warn('Could not find color {0}, return white instead.'.format(color))
        return compose_color(*COLOR_TABLE['white'])
