"""Utility functions for working with colors.
"""

from collections import namedtuple
from warnings import warn
from colour import Color


def string_to_colornum(name):
    c = Color(name)
    r = int(c.get_red()*255)
    g = int(c.get_green()*255)
    b = int(c.get_blue()*255)
    return (r, g, b)

def color_from_name(name):
    c = Color(name)
    r = int(c.get_red()*255)
    g = int(c.get_green()*255)
    b = int(c.get_blue()*255)
    return compose_color(r, g, b)

def color_from_rgb_int(r, g, b):
    return Color(rgb=(r/255.0, g/255.0, b/255.0))

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
    return (color & 0x0000ff), (color & 0x00ff00) >> 8, (color & 0xff0000) >> 16
