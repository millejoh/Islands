"""Utility functions for printing formatted text in a window.
"""

import re
from collections import namedtuple

import tcod
from gui.color import decompose_color, Color


TEXT = r'(?P<TEXT>[^{}]+)'
MARKUP = r'(?P<COLOR>\{[a-zA-Z:,]+?\})'
CLICK = r'(?P<CLICK>\{click:[a-zA-Z]+\})'
ACCENT = r"""(?P<ACCENT>\{[:'`^0][a-zA-Z]\})"""
TERMINATOR = r'(?P<MU_TERM>\{/\})'
master_pat = re.compile('|'.join([TEXT, MARKUP, ACCENT, CLICK, TERMINATOR]))

FG_COLOR = r'(?P<FG_COLOR>fg:|foreground:)'
BG_COLOR = r'(?P<BG_COLOR>bg:|background:)'
COLOR = r'(?P<COLOR>[a-zA-Z]+)'
color_pat = re.compile('|'.join([FG_COLOR, BG_COLOR, COLOR]))



Token = namedtuple('Token', ['type', 'value'])

def next_token(text, pat=master_pat):
    scanner = pat.scanner(text)
    for m in iter(scanner.match, None):
        yield Token(m.lastgroup, m.group())

def process_color_directive(fmt):
    toks = [tok for tok in next_token(fmt[1:-1], color_pat)]
    if toks[0].type == 'FG_COLOR':
        return color_to_control_string(Color(toks[1].value),
                                       False)
    elif toks[0].type == 'BG_COLOR':
        return color_to_control_string(Color(toks[1].value),
                                       True)
    else:
        return color_to_control_string(Color(toks[0].value),
                                       False)


def make_colored_string(string, dialog=False, window=None):
    s = []
    for tok in next_token(string, master_pat):
        if tok.type == 'COLOR':
            s.append(process_color_directive(tok.value))
        elif tok.type == 'MU_TERM':
            s.append('{:c}'.format(tcod.COLCTRL_STOP))
        else:
            s.append(tok.value)
    return ''.join(s)


def color_to_control_string(color, bg_color_p=False):
    """Given a color integer, return a color control string suitable for use
    with `tcod.console_print` format string.

    """
    r, g, b = decompose_color(color)
    return '{:c}{:c}{:c}{:c}'.format(tcod.COLCTRL_BACK_RGB if bg_color_p else tcod.COLCTRL_FORE_RGB,
                                     max(r, 1), max(g, 1), max(b, 1))


def draw_string(win, x, y, string, dialog=False):#, background_flag = tcod.BKGND_SET):
    xstr = make_colored_string(string, dialog, window=win)
    return xstr
