"""Utility functions for working with colors.
"""

import tcod
from collections import namedtuple

RGBColor = namedtuple('RGBColor', ['r', 'g', 'b'])


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
