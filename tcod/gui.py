"""
A simple GUI using the TCOD library. Very much inspired by cl-dormouse.
One might even go so far as to say "stolen from." Seems like a good way
to learn a language (and library) to me.
"""

import tcod
from console import Console, RootConsole

OPAQUE = 0
INVISIBLE = 100
DIMMED = 75 # Common values for window_transparency
BOLD_FACTOR = 1.4 # Multiplier for producing a "bold" version of a color.

def clamp(min, max, val):
    if min > val:
        return min
    elif val > max:
        return max
    else:
        return val

def translate_negative_coords(x, y, window = None):
    assert RootConsole.screen_width() > 0
    screen_w = RootConsole.screen_width()
    screen_h = RootConsole.screen_height()
    
    if x in ['center', 'centered', 'centre', 'centred']:
        if window:
            x = win.width // 2
        else:
            x = screen_w // 2
    if y in ['center', 'centered', 'centre', 'centred']:
        if window:
            y = win.height // 2
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

WindowStack = []
HiddenWindowStack = []

def destroy_all_windows():
    """Destroy all existing window objects."""
    for win in WindowStack:
        win.destroy()

class Window(Console):
    def __init__(self, tlx, tly, width, height, parent=None):
        self.tlx = tlx
        self.tly = tly
        self.foreground = tcod.white
        self.background = tcod.black
        self.foreground_highlight = tcod.white
        self.background_highlight = tcod.black
        self.children = []
        self.raise_children_with_parent_p = True
        self.auto_redraw_p = False
        self.auto_redraw_time = 0
        self.framed_p = False
        self.can_resize_p = True
        self.can_drag_p = True
        self.can_close_p = True
        self.close_on_escape_p = False
        self.ephemeral_p = False
        self.draw_function = None
        self.event_handler= None
        self.title = ""
        self.transparency = 0
        self.transparency_unfocused = 100
        self.hidden_p = False
        self.changed_p = False
        self.last_update_time = 0
        self.alive_p = True
        self.touching = []
        if parent:
            self.parent = parent
            self.parent.children.append(self)

    def process_window(self):
        if self.hidden_p:
            return
        if (self.changed_p and self.auto_redraw_p) or \
           (self.auto_redraw_time and 
            tcod.sys_elapsed_milli() > (self.auto_redraw_time +
                                        self.last_update_time)):
            self.prepare_window()
            self.redraw_window_area(draw_window=True)
            self.dirty_window()
            self.changed_p = False
        elif self.focusChanged:
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
        
        tcod.console_set_dirty( clamp(tlx, 0, (RootConsole.screen_width() - 1)),
                                clamp(tly, 0, (RootConsole.screen_height() - 1)),
                                clamp(w, 0, (RootConsole.screen_width() - tlx)),
                                clamp(h, 0, (RootConsole.screen_height() - tly)) )

    def destroy(self):
        """Destroy the window object, hiding it first if it is not already
        hidden.
        """
        if self is in WindowStack:
            self.hide()
        self.parent.children.remove(self)
        if len(self.children) > 0:
            for child in self.children:
                child.destroy()
        HiddenWindowStack.remove(self)
        self.alive_p = False

    def _touch_windows(self):
        """Make window refresh it's list of windows it is currently touching/overlapping."""
        touching = [win for win in WindowStack if not (win is self) and self.touching(win)]
        for win in touching:
            if win is not in self.touching:
                self.touching.append(win)
            if self is not in win.touching:
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

    def touches_spot_p(self, x, y):
        """True if any part of the window covers or touches the point at (x, y)."""
        pass

    def touching_p(self, win):
        """True if window is touching or overlapping <win>."""
        pass

    @property
    def parent(self):
        """Find and return parent of window, if it has one."""
        pass

    def windows_below(self):
        """Return windows (if any) below current window in the window stack."""
        pass

    def windows_above(self):
        """Return windows (if any) above current window in the stack."""
        pass

    def windows_underlying(self):
        """List of windows that both overlap current window and are below it in
        the stack.
        """
        pass

    def copy_to_console(self, console):
        """Copy contents of current window to console object <console>."""
        pass

    def redraw(self):
        """Update window onto the root console."""
        pass

    def redraw_at(self, root_x, root_y):
        """Copy contents of window into rectangle with top left corner at (root_x, root_y) on root console.

        """
        pass

    def redraw_in_area(self, x1, y1, x2, y2, fade=None):
        """
        """
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

