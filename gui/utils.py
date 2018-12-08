"""
A refactoring of non-class definitions from the gui module.
"""

import tcod
import gui.console as tc
import gui
from gui.events import MouseEvent, MouseDragEvent, MouseMoveEvent, MouseReleaseEvent, MousePressEvent, KeyEvent
from gui.gameloop import BasicEventLoop

def clamp(low, high, expr):
    """Return the numeric value of EXPR, constrained to the range
    [LOW ... HIGH].
    """
    return min((high, max((low, expr))))

def root_to_win_coord(win, root_x, root_y):
    """Given coordinate pair (root_x, root_y) on the root console,
    return location of same point relative to upper left corner of
    window win.
    """
    wx = root_x - win.tlx
    wy = root_y - win.tly
    return wx, wy

def translate_negative_coords(x, y, window=None):
    assert tc.R.screen_width() > 0
    screen_w = tc.R.screen_width()
    screen_h = tc.R.screen_height()

    if x in ['center', 'centered', 'centre', 'centred']:
        if window:
            x = window.width // 2
        else:
            x = screen_w // 2
    if y in ['center', 'centered', 'centre', 'centred']:
        if window:
            y = window.height // 2
        else:
            y = screen_h // 2
    if x < 0:
        if window:
            x += window.width
        else:
            x += screen_w
    if y < 0:
        if window:
            y += window.width
        else:
            y += screen_w

    return x, y


def screen_to_win_coord(window, coord):
    """Given screen coordinate <coord> (an x,y tuple) return the coordinates of
    the same point relative to the top left corner of <window>.

    >>> w = Window(8,8,10,10)
    >>> x, y = screen_to_win_coord(w, (10, 10))
    >>> print(x)
    2
    >>> print(y)
    2
    """
    win_x = coord[0] - window.tlx
    win_y = coord[1] - window.tly

    return win_x, win_y


def rectangle_overlaps_p(rect1, rect2):
    """Do the two rectangles <rect1> and <rect2> overlap?

    rect1 = Tuple (tlx, tly, brx, bry) specifying the top left and bottom
            right corners of the first area.

    rect2 = Tuple (tlx, tly, brx, bry) specifying the top left and bottom
            right corners of the second area.

    """
    # Translate negative coordinates?
    tlx_a, tly_a, brx_a, bry_a = rect1
    tlx_b, tly_b, brx_b, bry_b = rect2

    return not ((brx_a < tlx_b) or
                (bry_a < tly_b) or
                (tlx_a > brx_b) or
                (tly_a > bry_b))


def transparency_to_fade(transparency):
    return 1.0 - (transparency / 100.0)


def copy_windows_to_console(windows, console):
    for win in reversed(windows):
        win.copy_to_console(console)


def mouse_click_type(mouse_event):
    if mouse_event.lbutton:
        return 'lclick'
    elif mouse_event.rbutton:
        return 'rclick'
    elif mouse_event.mbutton:
        return 'mclick'
    else:
        return None


def check_for_event(mask=tcod.EVENT_ANY):
    key = tcod.Key()
    mouse = tcod.Mouse()
    event_type = tcod.sys_check_for_event(mask, key, mouse)
    return event_type, mouse, key


