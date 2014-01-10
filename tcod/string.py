"""Utility functions for printing formatted text in a window.
"""

import tcod
from tcod.color import decompose_color
import re
from collections import namedtuple

TEXT = r'(?P<TEXT>[^{}]+)'
MARKUP = r'(?P<COLOR>\{[a-zA-Z:,]+?\})'
CLICK = r'(?P<CLICK>\{click:[a-zA-Z]+\})'
ACCENT = r"""(?P<ACCENT>\{[:'`^0][a-zA-Z]\})"""
TERMINATOR = r'(?P<MU_TERM>\{/\})'

master_pat = re.compile('|'.join([TEXT, MARKUP, ACCENT, CLICK, TERMINATOR]))

Token = namedtuple('Token', ['type', 'value'])


def next_token(text, pat=master_pat):
    scanner = pat.scanner(text)
    for m in iter(scanner.match, None):
        yield Token(m.lastgroup, m.group())

FG_COLOR = r'fg:[a-zA-Z]+|foreground:[a-zA-Z]+|[^:]'
BG_COLOR = r'bg:[a-ZA-Z]+|background:[a-zA-Z]+'


def process_color_directive(col_fmt):
    pass


def make_colored_string(string, dialog=False, window=None):
    s = []
    for tok in next_token(string, master_pat):
        if tok.type != 'MU_TERM':
            if tok.type == 'COLOR':
                s.append(process_color_directive(tok.value))
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


def draw_string(win, x, y, string, dialog=False, background_flag = tcod.BKGND_SET):
    xstr = colorize_string(string, dialog, window=win)
