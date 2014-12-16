"""
A refactoring of non-class definitions from the gui module.
"""

import tcod
import tcod.console as tc
from tcod.events import MouseEvent, MouseDragEvent, KeyEvent


AUTO_REDRAW = True
OPAQUE = 1
INVISIBLE = 0
DIMMED = 75                # Common values for window_transparency
BOLD_FACTOR = 1.4          # Multiplier for producing a "bold" version
                           # of a color.
CURRENT_MOUSE_EVENT = None
MOUSE_X, MOUSE_Y = 0, 0
FOCUS_CHANGED = False      # Transiently set to true when a window has
                           # taken over focus
WINDOW_STACK = []
HIDDEN_WINDOW_STACK = []
FOCUS_FADE_MODE = 'together'
TOPWIN = None
LAST_TOPWIN = None
DRAG_DELAY = 0.05          # Drag delay time in seconds
LAST_MOUSE_CLICK = None
DOUBLE_CLICK_SPEED = 1000  # Double click speed in milliseconds


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


def redraw_all_windows(exclude=None):
    """Copy all visible windows onto the root console.

    exclude = list of window instances to exclude, defaults to None."""
    windows = [win for win in WINDOW_STACK if win not in ([] if exclude is None else exclude)]
    tc.R.active_root.clear()
    if windows:
        for win in windows.reverse():
            win.prepare()
            win.copy_to_console(tc.R.active_root)
        tcod.console_set_dirty(0, 0,
                               tc.R.screen_width(),
                               tc.R.screen_height())


def copy_windows_to_console(windows, console):
    for win in reversed(windows):
        win.copy_to_console(console)


def top_window_at(x, y, stack=WINDOW_STACK, override_modal=False):
    """Return the window nearest the top of *WINDOW-STACK* which touches X,Y.

    X, Y = Coordinates of a point on the screen (root console).
    stack =  List of window objects to consider (default is to consider
             all non-hidden windows).
    override-modal = Boolean.

    Return the window nearest the top of *WINDOW-STACK* which touches X,Y.  If
    OVERRIDE-MODAL? is true, then disregard whether a window is modal or not in
    deciding which window to return. If this parameter is nil (default) then
    whenever a modal window is at the top of WINDOW-STACK we can only return
    that window from this function. If that window does not touch X,Y then NIL
    is returned.

    """
    if len(stack) == 0:
        return None
    else:
        x, y = translate_negative_coords(x, y)
        top = stack[0]
        if not override_modal and top.is_modal:
            if top.touches_spot(x, y):
                return top
            else:
                return None
        else:
            touching = [win for win in stack
                        if win.touches_spot(x, y) and
                        not isinstance(win, GhostWindow)]
            return touching[0] if len(touching) > 0 else touching


def window_with_mouse_focus():
    """Return the topmost window under the mouse pointer."""
    return top_window_at(MOUSE_X, MOUSE_Y)


def window_with_key_focus():
    """Return the window where keyboard events will/are sent to."""
    if len(WINDOW_STACK) > 0 and WINDOW_STACK[0].is_modal:
        return WINDOW_STACK[0]
    else:
        return window_with_mouse_focus()


def transparency_to_fade(transparency):
    return 1.0 - (transparency / 100.0)


def windows_at(x, y):
    """All windows that overlie the point at (x, y).
    """
    x, y = translate_negative_coords(x, y)
    return [win for win in WINDOW_STACK if win.touches_spot(x, y)]


def all_windows(exclude=None):
    if exclude is None:
        exclude = []
    return [w for w in WINDOW_STACK + HIDDEN_WINDOW_STACK if w not in exclude]


def process_windows():
    """Call process_window() for all windows that are not hidden.
    Meant to be called as part of main event loop."""
    windows = all_windows()
    if len(windows) > 0:
        for win in reversed(windows):
            win.process_window()


def fade_for_window(win):
    focus = window_with_mouse_focus()
    if win.transparency_unfocused is None or win is focus:
        return transparency_to_fade(win.transparency)
    elif FOCUS_FADE_MODE == 'together' and not isinstance(focus, BackgroundWindow):
        return transparency_to_fade(win.transparency)
    elif isinstance(focus, Window) and win.raise_children_with_parent_p and \
            (win in focus.children or focus in win.children):
        return transparency_to_fade(win.transparency)
    else:
        return transparency_to_fade(win.transparency_unfocused)


def send_mouse_move_event(window, event):
    pass


def mouse_click_type(mouse_event):
    if mouse_event.lbutton:
        return 'lclick'
    elif mouse_event.rbutton:
        return 'rclick'
    elif mouse_event.mbutton:
        return 'mclick'
    else:
        return None


def send_mouse_click_event(window, event):
    global LAST_MOUSE_CLICK
    double_click = False
    if window:
        if event.lbutton and event.cy == window.tly and event.cx == window.brx and window.can_close_p:
            window.hide()
        elif LAST_MOUSE_CLICK and mouse_click_type(event) == mouse_click_type(LAST_MOUSE_CLICK.state) and \
          (tcod.sys_elapsed_milli() - LAST_MOUSE_CLICK.time) < DOUBLE_CLICK_SPEED:
            double_click = True
        LAST_MOUSE_CLICK = MouseEvent(winx=event.cx - window.tlx,
                                      winy=event.cy - window.tly,
                                      mouse_state=event,
                                      window=window,
                                      double_click=double_click)
        window.on_mouse_event(LAST_MOUSE_CLICK)


def send_key_event(window, key, x, y):
    kevent = KeyEvent(key, window=window, winx=x, winy=y)
    window.on_key_event(kevent)


def handle_drag_event(mouse):
    """Check for and handle (if necessary) drag events.
    """
    start = tcod.sys_elapsed_milli()
    dragged = False
    ox, oy = mouse.cx, mouse.cy
    key = tcod.Key()
    while mouse.lbutton:
        tcod.sys_check_for_event(tcod.EVENT_MOUSE, key, mouse)
        mouse.dx, mouse.dy = ox - mouse.cx, oy - mouse.cy
        if (tcod.sys_elapsed_milli() > (DRAG_DELAY * 1000 + start)) and \
                (mouse.dx != 0 or mouse.dy != 0):
            dragged = True
            # Are we dragging on the window title (move!)?
            if TOPWIN.can_drag_p and \
                TOPWIN.on_upper_window_border(mouse.cx - TOPWIN.tlx,
                                              mouse.cy - TOPWIN.tly):
                TOPWIN.mouse_drag(mouse)

            # Are we dragging on the bottom right corner (resize!)?
            elif TOPWIN.can_resize_p and \
                ox == (TOPWIN.tlx + TOPWIN.width - 1) and \
                oy == (TOPWIN.tly + TOPWIN.height - 1):
                TOPWIN.mouse_resize(mouse)
            else:
                TOPWIN.on_mouse_event(MouseDragEvent(winx=mouse.cx - TOPWIN.tlx,
                                                     winy=mouse.cy - TOPWIN.tly,
                                                     mouse_state=mouse))

def check_for_event(mask=tcod.EVENT_ANY):
    key = tcod.Key()
    mouse = tcod.Mouse()
    event_type = tcod.sys_check_for_event(mask, key, mouse)
    return event_type, mouse, key


def gui_loop():
    global CURRENT_MOUSE_EVENT, MOUSE_X, MOUSE_Y, TOPWIN, LAST_TOPWIN, FOCUS_CHANGED 
    # Go through any (all) events in the queue
    CURRENT_MOUSE_EVENT = tcod.mouse_get_status()
    FOCUS_CHANGED = False
    event_type, mouse, key = check_for_event()
    #for (event_type, event) in events:
    if event_type & tcod.EVENT_MOUSE:
        CURRENT_MOUSE_EVENT = mouse
        MOUSE_X, MOUSE_Y = mouse.cx, mouse.cy
        if event_type == tcod.EVENT_MOUSE_MOVE:
            FOCUS_CHANGED = window_with_mouse_focus() != LAST_TOPWIN
            send_mouse_move_event(window_with_mouse_focus(), mouse)
        elif event_type in (tcod.EVENT_MOUSE_PRESS, tcod.EVENT_MOUSE_RELEASE):
            TOPWIN = window_with_mouse_focus()
            FOCUS_CHANGED = TOPWIN != LAST_TOPWIN
            send_mouse_click_event(TOPWIN, mouse)
    elif event_type & tcod.EVENT_KEY:
        TOPWIN = window_with_key_focus()
        if TOPWIN:
            send_key_event(TOPWIN, key,
                           MOUSE_X - TOPWIN.tlx,
                           MOUSE_Y - TOPWIN.tly)
        # That's done, now look at the state of the mouse
    if CURRENT_MOUSE_EVENT and CURRENT_MOUSE_EVENT.lbutton and \
            TOPWIN and not TOPWIN.hidden_p:
        TOPWIN.raise_window()
        handle_drag_event(CURRENT_MOUSE_EVENT)

    if FOCUS_CHANGED:
        LAST_TOPWIN = TOPWIN

    process_windows()

    
def destroy_all_windows():
    """Destroy all existing window objects."""
    for win in WINDOW_STACK:
        win.destroy()
