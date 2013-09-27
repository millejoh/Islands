"""
A simple GUI using the TCOD library. Very much inspired by cl-dormouse.
One might even go so far as to say "stolen from." Seems like a good way
to learn a language (and library) to me.
"""

import tcod
from tcod.console import Console, RootConsole

OPAQUE = 0
INVISIBLE = 100
DIMMED = 75 # Common values for window_transparency
BOLD_FACTOR = 1.4 # Multiplier for producing a "bold" version of a color.
MOUSE_X, MOUSE_Y = 0, 0
FOCUS_CHANGED = False # Transiently set to true when a window has taken over focus
WINDOW_STACK = []
HIDDEN_WINDOW_STACK = []
FOCUS_FADE_MODE = 'together'

def clamp(low, high, expr):
    """Return the numeric value of EXPR, constrained to the range [MIN ... MAX]."""
    return min((high, max((low, expr))))

def translate_negative_coords(x, y, window = None):
    assert RootConsole.screen_width() > 0
    screen_w = RootConsole.screen_width()
    screen_h = RootConsole.screen_height()
    
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
    """Given screen coordinate <coord> (an x,y tuple) return the coordinates of the same
    point relative to the top left corner of <window>.

    >>> w = Window(8,8,10,10)
    >>> x, y = screen_to_win_coord(w, (10, 10))
    >>> print x
    2
    >>> print y
    2
    """
    win_x = coord[0] - window.tlx
    win_y = coord[1] - window.tly 

    return win_x, win_y

def rectangle_overlaps_p(rect1, rect2):
    """Do the two rectanges <rect1> and <rect2> overlap?

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

         
def redraw_all_windows(exclude=[]):
    """Copy all visible windows onto the root console.

    exclude = list of window instances to exclude, defaults to None."""
    windows = [win for win in WINDOW_STACK if win not in exclude]
    RootConsole.active_root.clear()
    if windows:
        for win in windows.reverse():
            win.prepare()
            win.copy_to_console(RootConsole.active_root)
        tcod.console_set_dirty( 0, 0, 
                                RootConsole.screen_width(),
                                RootConsole.screen_height() )

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
    x, y = translate_negative_coords(x, y)
    top = stack[0]
    if not override_modal and top.is_modal:
        if top.touches_spot(x, y):
            return top
        else:
            return None
    else:
        [win for win in stack if (win.touches_spot(x, y) and not type(win) is GhostWindow)]

def window_with_mouse_focus():
    """Return the topmost window under the mouse pointer."""
    return top_window_at(MOUSE_X, MOUSE_Y)

def window_with_key_focus():
    """Return the window where keyboard events will/are sent to."""
    if WINDOW_STACK[0].is_modal:
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

def all_windows(exclude=[]):
    return [w for w in WINDOW_STACK+HIDDEN_WINDOW_STACK if w not in exclude]

def process_windows():
    """Call process_window() for all windows that are not hidden. Meant to be called as
    part of main event loop."""
    windows = all_windows()
    if windows:
        for win in windows:
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
        

def gui_loop(key, mouse):
    tcod.console_clear(tcod.root_console)
    tcod.console_print(tcod.root_console, 0, 0, "Current key pressed is {0}.".format(key.vk))
    tcod.console_print(tcod.root_console, 0, 1, "Cursor at ({0}, {1}).".format(mouse.cx, mouse.cy))
    tcod.console_print(tcod.root_console, 0, 2, "FPS = {0}".format(tcod.sys_get_fps()))
    process_windows()
    tcod.console_flush()

# ======================== #

class Event(object):
    def __init__(self):
        self.window = None
        self.winx = 0
        self.winy = 0
        self.time = 0

    def __repr__(self):
        return 'Event({0}) at ({1}, {2})'.format(self.type,
                                                 self.winx, self.winy)
class KeyEvent(Event):
    def __init__(self):
        self.keypress = None

class MouseEvent(Event):
    def __init__(self):
        self.state = None

def destroy_all_windows():
    """Destroy all existing window objects."""
    for win in WINDOW_STACK:
        win.destroy()

class Window(Console):
    def __init__(self, tlx, tly, width, height, hidden=False, parent=None, title=""):
        super().__init__(width, height)
        self.tlx = tlx
        self.tly = tly
        self.is_modal =False
        self.foreground_highlight = tcod.white
        self.background_highlight = tcod.black
        self.children = []
        self.raise_children_with_parent_p = True
        self.auto_redraw_p = False
        self.auto_redraw_time = 10
        self.framed_p = False
        self.can_resize_p = True
        self.can_drag_p = True
        self.can_close_p = True
        self.close_on_escape_p = False
        self.ephemeral_p = False
        self.draw_function = None
        self.event_handler= None
        self.title = title
        self.transparency = 1
        self.transparency_unfocused = 100
        self.hidden_p = hidden
        self.changed_p = False
        self.last_update_time = 0
        self.alive_p = True
        self.touching = []
        if parent:
            self.parent = parent
            self.parent.children.append(self)
        if self.hidden_p:
            HIDDEN_WINDOW_STACK.insert(0, self)
        else:
            self._touch_windows()
            WINDOW_STACK.insert(0, self)

    @property
    def brx(self):
        return self.tlx + (self.width-1)

    @property
    def bry(self):
        return self.tly + (self.height-1)
    
    def process_window(self):
        if self.hidden_p:
            return
        if (self.changed_p and self.auto_redraw_p) or \
            (self.auto_redraw_time and
             tcod.sys_elapsed_milli() > (self.auto_redraw_time +
                                         self.last_update_time)):
            self.prepare()
            self.redraw_area(draw_window=True)
            self.dirty_window()
            self.changed_p = False
            self.last_update_time = tcod.sys_elapsed_milli()
        elif FOCUS_CHANGED:
            self.redraw_window_area(draw_window=True)

    def window_did_change(self):
        self.changed_p = True
        
    def send_to_window(self, event):
        if self.hidden_p:
            return
        return self.event_handler(event)

    def dirty_window(self):
        tlx, tly = self.tlx, self.tly
        w, h = self.width, self.height
        tcod.console_set_dirty( clamp(0, (RootConsole.screen_width() - 1), tlx),
                                clamp(0, (RootConsole.screen_height() - 1), tly),
                                clamp(0, (RootConsole.screen_width() - tlx), w),
                                clamp(0, (RootConsole.screen_height() - tly), w) )

    def destroy(self):
        """Destroy the window object, hiding it first if it is not already
        hidden.
        """
        if self in WINDOW_STACK:
            self.hide()
        self.parent.children.remove(self)
        if len(self.children) > 0:
            for child in self.children:
                child.destroy()
        HIDDEN_WINDOW_STACK.remove(self)
        self.alive_p = False

    def _touch_windows(self):
        """Make window refresh it's list of windows it is currently touching/overlapping."""
        touching = [win for win in WINDOW_STACK if not (win is self) and self.is_touching(win)]
        for win in touching:
            if win not in self.touching:
                self.touching.append(win)
            if self not in win.touching:
                win.touching.append(self)

    def _untouch_windows(self):
        for win in self.touching:
            win.touching.remove(self)
        self.touching = []
        
    def move_window(self, tlx, tly):
        """Move window so top left corner is located at (TLX, TLY), relative
        to the top left corner of the screen.
        """
        # translate_negative_coords(tlx, tly)
        self._untouch_windows()
        self.tlx = tlx
        self.tly = tly
        self._touch_windows()

    def touches_spot(self, x, y):
        """True if any part of the window covers or touches the point at (x, y)."""
        x, y = translate_negative_coords(x, y)
        return (self.tlx <= x <= self.brx) and (self.tly <= y <= self.bry)

    def is_touching(self, win):
        """True if window is touching or overlapping <win>."""
        return rectangle_overlaps_p( (self.tlx, self.tly, self.brx, self.bry),
                                     (win.tlx, win.tly, win.brx, win.bry) )

    def windows_below(self):
        """Return windows (if any) below current window in the window stack."""
        return WINDOW_STACK[WINDOW_STACK.index(self):]
        

    def windows_above(self):
        """Return windows (if any) above current window in the stack."""
        return WINDOW_STACK[:WINDOW_STACK.index(self)]

    def windows_overlying(self):
        """List windows that both overlap current window and are above it in the window stack.

        """
        return [win for win in self.windows_above() if win in self.touching]        

    def windows_underlying(self):
        """List of windows that both overlap current window and are below it in
        the stack.
        """
        return [win for win in self.windows_below() if win in self.touching]

    def windows_overlapping(self, include_window=True):
        return [win for win in WINDOW_STACK \
                if self.is_touching(win) and (include_window and self is win)]
                

    def copy_to_console(self, console):
        """Copy contents of current window to console object <console>."""
        self.blit(console, 0, 0, self.width, self.height, self.tlx, self.tly,
                  self.transparency, self.transparency)

    def redraw(self):
        """Update window onto the root console."""
        self.copy_to_console(RootConsole.active_root)

    def redraw_area(self, draw_window=True):
        if not draw_window:
            RootConsole.active_root.background = tcod.black
            RootConsole.active_root.draw_rect(self.tlx, self.tly, self.width, self.height, True, tcod.BKGND_SET)
        if draw_window and self.children:
            for win in self.children:
                if not win.hidden_p:
                    win.redraw_area(draw_window=True)
        for w in self.windows_overlapping(include_window=draw_window):
            self.redraw()
            #self.redraw_intersection(w, fade=fade_for_window(w))

        self.dirty_window()

    def redraw_intersection(self, window, fade):
        self.redraw_in_area(window.tlx, window.tly, window.brx, window.bry, fade=fade)
    
    def redraw_at(self, root_x, root_y):
        """Copy contents of window into rectangle with top left corner at (root_x, root_y) on root console.

        """
        pass

    def redraw_in_area(self, x1, y1, x2, y2, fade=None):
        """
        """
        x1, y1 = translate_negative_coords(x1, y1)
        x2, y2 = translate_negative_coords(x2, y2)
        tlx = max([self.tlx, x1])
        tly = max([self.tly, y1])
        brx = max([self.brx, x2])
        bry = max([self.bry, y2])
        RootConsole.active_root.fill_char(' ', tlx, tly, brx-(tlx-1), bry-(tly-1))    
        winx, winy = screen_to_win_coord(self, (tlx, tly))
        self.blit(RootConsole.active_root, winx, winy, (brx-tlx), (bry-tly), tlx, tly,
                  (fade or transparency_to_fade(self.transparency)),
                  (fade or transparency_to_fade(self.transparency)))

    def prepare(self):
        if self.framed_p:
            if WINDOW_STACK[0] is self:
                self.print_double_frame(0, 0, self.width, self.height, True,
                                        tcod.BKGND_SET, bytes(self.title,'utf-8') if
                                        self.title else 0)
            else:
                self.print_frame(0, 0, self.width, self.height, True,
                                 tcod.BKGND_SET, bytes(self.title,'utf-8') if
                                 self.title else 0)
            if self.can_close_p:
                self[self.width-1, 0] = 'X'
            if self.can_resize_p: 
                self[self.width-1, self.height-1]= 29
        else:
            self.draw_rect(0, 0, self.width, self.height, True, tcod.BKGND_SET)

    def resize(self, new_width, new_height):
        self._untouch_windows()
        self._c = tcod.console_new(new_width, new_height)
        self.width = new_width
        self.height = new_height
        self._touch_windows()

        
class GhostWindow(Window):
    """Window that cannot be interacted with. Athough it may be raised to the top of
    the window stack, it cannot receive any messages from the mouse or
    keyboard. All such messages pass through to the window below it.
            
    """

    pass

class BackgroundWindow(Window):
    pass

class WindowTheme(object):
    def __init__(self, fore, back, fore_highlight, back_highlight,
                 dialog_button_fore, dialog_button_back,
                 hyperlink_fore, hyperlink_back,
                 input_fore, input_back, framed, transparency,
                 transparency_unfocused):
        self.foreground = fore
        self.background = back
        self.foreground_highlight = fore_highlight
        self.background_highlight = back_highlight
        self.dialog_button_foreground = dialog_button_fore
        self.dialog_button_background = dialog_button_back
        self.hyperlink_foreground = hyperlink_fore
        self.hyperlink_background = hyperlink_back
        self.input_foreground = input_fore
        self.input_background = input_back
        self.framed_p = framed
        self.transparency = transparency
        self.transparency_unfocused = transparency_unfocused

