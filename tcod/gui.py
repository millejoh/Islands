"""
A simple GUI using the TCOD library. Very much inspired by cl-dormouse.
One might even go so far as to say "stolen from." Seems like a good way
to learn a language (and library) to me.
"""

import tcod.console as tc
import tcod
from tcod.events import *
from math import floor

AUTO_REDRAW = True
OPAQUE = 1
INVISIBLE = 0
DIMMED = 75                # Common values for window_transparency
BOLD_FACTOR = 1.4          # Multiplier for producing a "bold" version of a color.
CURRENT_MOUSE_EVENT = None
MOUSE_X, MOUSE_Y = 0, 0
FOCUS_CHANGED = False      # Transiently set to true when a window has taken over focus
WINDOW_STACK = []
HIDDEN_WINDOW_STACK = []
FOCUS_FADE_MODE = 'together'
TOPWIN = None
LAST_TOPWIN = None
DRAG_DELAY = 0.05          # Drag delay time in seconds
LAST_MOUSE_CLICK = None
DOUBLE_CLICK_SPEED = 1000  # Double click speed in milliseconds


def clamp(low, high, expr):
    """Return the numeric value of EXPR, constrained to the range [MIN ... MAX]."""
    return min((high, max((low, expr))))


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


def redraw_all_windows(exclude=[]):
    """Copy all visible windows onto the root console.

    exclude = list of window instances to exclude, defaults to None."""
    windows = [win for win in WINDOW_STACK if win not in exclude]
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
            touching = [win for win in stack if (win.touches_spot(x, y) and not type(win) is GhostWindow)]
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


def send_mouse_move_event(event):
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


def send_mouse_click_event(event):
    global LAST_MOUSE_CLICK
    double_click = False
    if TOPWIN:
        if event.lbutton and event.cy == TOPWIN.tly and event.cx == TOPWIN.brx and TOPWIN.can_close_p:
            TOPWIN.hide()
        elif LAST_MOUSE_CLICK and mouse_click_type(event) == mouse_click_type(LAST_MOUSE_CLICK.state) and \
          (tcod.sys_elapsed_milli() - LAST_MOUSE_CLICK.time) < DOUBLE_CLICK_SPEED:
            double_click = True
        LAST_MOUSE_CLICK = MouseEvent(winx=event.cx - TOPWIN.tlx,
                                      winy=event.cy - TOPWIN.tly,
                                      mouse_state=event,
                                      window=TOPWIN,
                                      double_click=double_click)
        TOPWIN.send_to_window(LAST_MOUSE_CLICK)


def send_key_to_window(window, key, x, y):
    pass


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
                TOPWIN.send_to_window(MouseDragEvent(winx=mouse.cx - TOPWIN.tlx,
                                                     winy=mouse.cy - TOPWIN.tly,
                                                     mouse_state=mouse))


def gui_loop(events):
    global CURRENT_MOUSE_EVENT, MOUSE_X, MOUSE_Y, TOPWIN, LAST_TOPWIN, FOCUS_CHANGED
    # Go through any (all) events in the queue
    CURRENT_MOUSE_EVENT = tcod.mouse_get_status()
    for (event_type, event) in events:
        if isinstance(event, tcod.Mouse):
            CURRENT_MOUSE_EVENT = event
            MOUSE_X, MOUSE_Y = event.cx, event.cy
            if event_type == tcod.EVENT_MOUSE_MOVE:
                FOCUS_CHANGED = window_with_mouse_focus() != LAST_TOPWIN
                send_mouse_move_event(event)
            elif event_type in (tcod.EVENT_MOUSE_PRESS, tcod.EVENT_MOUSE_RELEASE):
                TOPWIN = window_with_mouse_focus()
                FOCUS_CHANGED = TOPWIN != LAST_TOPWIN
                send_mouse_click_event(event)
        if isinstance(event, tcod.Key):
            TOPWIN = window_with_key_focus()
            if TOPWIN:
                send_key_to_window(TOPWIN, event,
                                   MOUSE_X - TOPWIN.tlx,
                                   MOUSE_Y - TOPWIN.tly)
        # That's done, now look at the state of the mouse
    if CURRENT_MOUSE_EVENT and CURRENT_MOUSE_EVENT.lbutton and \
            TOPWIN and not TOPWIN.hidden_p:
        TOPWIN.raise_window()
        handle_drag_event(CURRENT_MOUSE_EVENT)

    if FOCUS_CHANGED:
        LAST_TOPWIN = TOPWIN

    tcod.console_print(tcod.root_console, 0, 0,
                       "Event(s) {0} detected.".format(events))
    tcod.console_print(tcod.root_console, 0, 1,
                       "Mouse position, lbutton = ({0}, {1}, {2})".format(MOUSE_X,
                                                                          MOUSE_Y,
                                                                          CURRENT_MOUSE_EVENT.lbutton if CURRENT_MOUSE_EVENT else None))
    
    tcod.console_print(tcod.root_console, 0, 2,
                       "FPS = {0}".format(tcod.sys_get_fps()))
    tcod.console_print(tcod.root_console, 0, 3,
                       "Top window = {0}".format(TOPWIN))
    process_windows()

# ======================== #


def destroy_all_windows():
    """Destroy all existing window objects."""
    for win in WINDOW_STACK:
        win.destroy()


class Window(tc.Console):
    def __init__(self, tlx, tly, width, height, hidden=False,
                 parent=None, title="", framed=False):
        super().__init__(width, height)
        self.tlx = tlx
        self.tly = tly
        self.is_modal = False
        self.foreground_highlight = tcod.white
        self.background_highlight = tcod.black
        self.children = []
        self.raise_children_with_parent_p = True
        self.auto_redraw_p = False
        self.auto_redraw_time = 10
        self.framed_p = framed
        self.can_resize_p = True
        self.can_drag_p = True
        self.can_close_p = True
        self.close_on_escape_p = False
        self.ephemeral_p = False
        self.draw_function = None
        self.event_handler = None
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
        else:
            self.parent = None

        if self.hidden_p:
            HIDDEN_WINDOW_STACK.insert(0, self)
        else:
            self._touch_windows()
            WINDOW_STACK.insert(0, self)

    def __repr__(self):
        return "Window({w.tlx},{w.tly},{w.width},{w.height},title='{w.title}')".format(w=self)

    @property
    def brx(self):
        return self.tlx + (self.width - 1)

    @property
    def bry(self):
        return self.tly + (self.height - 1)

    def process_window(self):
        if self.hidden_p:
            return
        if (self.changed_p and self.auto_redraw_p) or \
                (self.auto_redraw_time and
                 tcod.sys_elapsed_milli() >
                 (self.auto_redraw_time + self.last_update_time)):
            self.prepare()
            self.redraw_area(draw_window=True)
            self.dirty_window()
            self.changed_p = False
            self.last_update_time = tcod.sys_elapsed_milli()
        elif FOCUS_CHANGED:
            self.redraw_area(draw_window=True)

    def raise_window(self, redraw=AUTO_REDRAW, simple_redraw=False, **keys):
        """
        """
        assert not self.hidden_p
        if self.changed_p:
            self.prepare()
        WINDOW_STACK.remove(self)
        WINDOW_STACK.insert(0, self)
        self.window_did_change()
        self.dirty_window()
        for child in self.children:
            if self.raise_children_with_parent_p and not child.hidden_p:
                child.raise_window(redraw=redraw, simple_redraw=simple_redraw)
        if redraw:
            if simple_redraw:
                self.copy_to_console(tc.R.active_root)
            else:
                self.redraw_area()

    def hide(self, redraw=AUTO_REDRAW):
        """
        """
        if redraw:
            self.redraw_area(draw_window=False)
        self._untouch_windows()
        WINDOW_STACK.remove(self)
        if self.raise_children_with_parent_p and self.children:
            for win in self.children:
                win.hide()
        if self.ephemeral_p:
            self.destroy()
        else:
            self.hidden_p = True
            if self not in HIDDEN_WINDOW_STACK:
                HIDDEN_WINDOW_STACK.insert(0, self)

    def unhide(self, redraw=AUTO_REDRAW):
        self.hidden_p = False
        HIDDEN_WINDOW_STACK.remove(self)
        WINDOW_STACK.insert(0, self)
        if redraw:
            self.redraw_area(draw_window=True)
        if self.raise_children_with_parent_p and self.children:
            for win in self.children:
                win.unhide()

    def window_did_change(self):
        self.changed_p = True

    def send_to_window(self, event):
        if self.hidden_p or self.event_handler is None:
            return
        return self.event_handler(event)

    def dirty_window(self):
        tlx, tly = self.tlx, self.tly
        w, h = self.width, self.height
        tcod.console_set_dirty(clamp(0, (tc.R.screen_width() - 1), tlx),
                               clamp(0, (tc.R.screen_height() - 1), tly),
                               clamp(0, (tc.R.screen_width() - tlx), w),
                               clamp(0, (tc.R.screen_height() - tly), h))

    def destroy(self):
        """Destroy the window object, hiding it first if it is not already
        hidden.
        """
        if self in WINDOW_STACK:
            self.hide()
        if self.parent:
            self.parent.children.remove(self)
        if len(self.children) > 0:
            for child in self.children:
                child.destroy()
        HIDDEN_WINDOW_STACK.remove(self)
        self.alive_p = False

    def _touch_windows(self):
        """Make window refresh it's list of windows it is currently
        touching/overlapping."""

        touching = [win for win in WINDOW_STACK
                    if not (win is self) and self.is_touching(win)]
        for win in touching:
            if win not in self.touching:
                self.touching.append(win)
            if self not in win.touching:
                win.touching.append(self)

    def _untouch_windows(self):
        for win in self.touching:
            win.touching.remove(self)
        self.touching = []

    def on_upper_window_border(self, x, y):
        return y == 0 and x > 0 and x < (tc.R.screen_width() - self.width)

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
        """True if any part of the window covers or touches
        the point at (x, y)."""
        x, y = translate_negative_coords(x, y)
        return (self.tlx <= x <= self.brx) and (self.tly <= y <= self.bry)

    def is_touching(self, win):
        """True if window is touching or overlapping <win>."""
        return rectangle_overlaps_p((self.tlx, self.tly, self.brx, self.bry),
                                    (win.tlx, win.tly, win.brx, win.bry))

    def windows_below(self):
        """Return windows (if any) below current window in the window stack."""
        return WINDOW_STACK[WINDOW_STACK.index(self):]

    def windows_above(self):
        """Return windows (if any) above current window in the stack."""
        return WINDOW_STACK[:WINDOW_STACK.index(self)]

    def windows_overlying(self):
        """List windows that both overlap current window and are above
        it in the window stack.

        """
        return [win for win in self.windows_above() if win in self.touching]

    def windows_underlying(self):
        """List of windows that both overlap current window and are below it in
        the stack.
        """
        return [win for win in self.windows_below() if win in self.touching]

    def windows_overlapping(self, include_window=True):
        return [win for win in WINDOW_STACK
                if self.is_touching(win) and (include_window and self is win)]

    def copy_to_console(self, console):
        """Copy contents of current window to console object <console>."""
        self.blit(console, 0, 0, self.width, self.height, self.tlx, self.tly,
                  self.transparency, self.transparency)

    def redraw(self):
        """Update window onto the root console."""
        self.copy_to_console(tc.R.active_root)

    def redraw_area(self, draw_window=True):
        if draw_window and self.children:
            for win in self.children:
                if not win.hidden_p:
                    win.redraw_area(draw_window=True)

        for w in self.windows_overlapping(include_window=draw_window):
            self.redraw()
            #self.redraw_intersection(w, fade=fade_for_window(w))

        if not draw_window:
            tc.R.active_root.background = tcod.black
            tc.R.active_root.draw_rect(self.tlx, self.tly,
                                       self.width, self.height,
                                       True, tcod.BKGND_SET)

        self.dirty_window()

    def redraw_intersection(self, window, fade):
        self.redraw_in_area(window.tlx, window.tly, window.brx,
                            window.bry, fade=fade)

    def redraw_at(self, root_x, root_y):
        """Copy contents of window into rectangle with top left corner at
        (root_x, root_y) on root console.

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
        tc.R.active_root.fill_char(' ', tlx, tly, brx - (tlx - 1), bry - (tly - 1))
        winx, winy = screen_to_win_coord(self, (tlx, tly))
        self.blit(tc.R.active_root, winx, winy, (brx - tlx), (bry - tly),
                  tlx, tly,
                  (fade or transparency_to_fade(self.transparency)),
                  (fade or transparency_to_fade(self.transparency)))

    def prepare(self):
        if self.framed_p:
            if WINDOW_STACK[0] is self:
                self.print_double_frame(0, 0, self.width, self.height, True,
                                        tcod.BKGND_SET,
                                        bytes(self.title, 'utf-8') if self.title else 0)
            else:
                self.print_frame(0, 0, self.width, self.height, True,
                                 tcod.BKGND_SET,
                                 bytes(self.title, 'utf-8') if self.title else 0)
            if self.can_close_p:
                self[self.width - 1, 0] = 'X'
            if self.can_resize_p:
                self[self.width - 1, self.height - 1] = 29
        else:
            self.draw_rect(0, 0, self.width, self.height, True, tcod.BKGND_SET)

    def resize(self, new_width, new_height):
        self._untouch_windows()
        resized_console = tcod.console_new(new_width, new_height)
        if self.framed_p:
            tcod.console_blit(self._c, 1, 1, self.width-2, self.height-2,
                              resized_console, 1, 1)
        else:
            tcod.console_blit(self._c, 0, 0, self.width, self.height,
                              resized_console, 0, 0)
        self._c = resized_console
        self.width = new_width
        self.height = new_height
        self._touch_windows()

    def mouse_drag(self, mouse):
        root = tc.R.active_root
        width, height = self.width, self.height
        offset_x, offset_y = mouse.cx - self.tlx, mouse.cy - self.tly
        tlx, tly = 0, 0
        swidth = tc.R.screen_width()
        sheight = tc.R.screen_height()
        tc.R.scratch.clear()
        tc.R.temp_console.clear()

        self.raise_window()
        copy_windows_to_console([win for win in WINDOW_STACK if win is not self],
                                tc.R.scratch)
        tc.R.scratch.blit(tc.R.temp_console, self.tlx, self.tly,
                          self.width, self.height,
                          0, 0, 1.0, 1.0)
        tc.R.scratch.copy_to_console(root)  # copy-console-to-console
        self.copy_to_console(root)          # copy-window-to-console
        root.flush()
        key = tcod.Key()
        while mouse.lbutton:
            tcod.sys_check_for_event(tcod.EVENT_MOUSE, key, mouse)
            tlx = clamp(0, swidth - self.width - 1, mouse.cx - offset_x)
            tly = clamp(0, sheight - self.height - 1, mouse.cy - offset_y)
            if not (tlx == self.tlx and tly == self.tly):
                # Erase window
                tc.R.temp_console.blit(root, 0, 0, width, height,
                                       self.tlx, self.tly, 1.0, 1.0)
                self.move_window(tlx, tly)
                # Save part of root console that win is covering
                tc.R.scratch.blit(tc.R.temp_console, tlx, tly, width, height, 0, 0,
                                  1.0, 1.0)
                # Copy win to root
                self.copy_to_console(root)
            root.flush()

    def mouse_resize(self, mouse):
        brx, bry = 0, 0
        root = tc.R.active_root

        self.raise_window()
        copy_windows_to_console([win for win in WINDOW_STACK if win is not self],
                                tc.R.scratch)
        tc.R.scratch.blit(tc.R.temp_console, self.tlx, self.tly, self.width,
                          self.height, 0, 0, 1.0, 1.0)
        tc.R.scratch.copy_to_console(root)  # copy-console-to-console
        self.copy_to_console(root)  # copy-window-to-console
        root.flush()
        key = tcod.Key()
        while mouse.lbutton:
            tcod.sys_check_for_event(tcod.EVENT_MOUSE, key, mouse)
            brx = clamp(self.tlx, tc.R.screen_width() - 1, mouse.cx)
            bry = clamp(self.tly, tc.R.screen_height() - 1, mouse.cy)
            if not (brx == self.brx and bry == self.bry):
                # Erase window
                tc.R.temp_console.blit(root, 0, 0, self.width, self.height,
                                       self.tlx, self.tly, 1.0, 1.0)
                self.resize(brx - self.tlx, bry - self.tly)
                self.prepare()
                # Save part of root console that win is covering
                tc.R.scratch.blit(tc.R.temp_console, self.tlx, self.tly,
                                  self.width, self.height,
                                  0, 0, 1.0, 1.0)
                # Copy win to root
                self.copy_to_console(root)
            tcod.console_print(0, 0, 0,
                               "Br(x,y) = ({0}, {1})".format(brx, bry))
            root.flush()


class GhostWindow(Window):
    """Window that cannot be interacted with. Although it may be raised to the
    top of the window stack, it cannot receive any messages from the
    mouse or keyboard. All such messages pass through to the window
    below it.
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


class Viewport(Window):
    """
    """

    def __init__(self, map_width, map_height, view_tlx, view_tly, **keys):
        """
        """

        super().__init__(**keys)
        self.transparency = OPAQUE
        self.mwidth = map_width
        self.mheight = map_height
        self.map_console = tc.Console(map_width, map_height)
        self.view_tlx = view_tlx
        self.view_tly = view_tly

    def __repr__(self):
        return "Viewport({w.tlx},{w.tly},{w.width},{w.height},title='{w.title}')".format(w=self)

    @property
    def view_width(self):
        if self.framed_p:
            return self.width - 2
        else:
            return self.width

    @property
    def view_height(self):
        if self.framed_p:
            return self.height - 2
        else:
            return self.height

    @property
    def view_brx(self):
        return self.view_tlx + self.view_width - 1

    @property
    def view_bry(self):
        return self.view_tly + self.view_height - 1

    def in_view_bounds(self, x, y):
        """Is the coordinate (x, y) in the bounds of the viewable portion
        of the viewport?
        """

        return (self.view_tlx < x < self.view_brx) and (self.view_tly < y < self.view_bry)

    def in_viewport_contents(self, x, y):
        """Is the coordinate (x, y) within the bounds of the mapped content
        of the viewport?
        """

        return (-1 < x < self.mwidth) and (-1 < y < self.mheight)

    def prepare(self):
        super().prepare()
        if self.map_console:
            self.copy_map_to_viewport()

    def copy_map_to_viewport(self):
        """Copy the visible portion of the viewport contents
        (as set by view_tlx and view_tly) to the root console.
        """

        vtlx, vtly = self.view_tlx, self.view_tly
        wtlx = 1 if self.framed_p else 0
        wtly = 1 if self.framed_p else 0
        width = self.width - 2 if self.framed_p else 0
        height = self.height - 2 if self.framed_p else 0
        edges_showing = False

        if vtlx < 0:
            wtlx = wtlx + abs(vtlx)
            width = width + vtlx
            vtlx = 0
            edges_showing = True
        elif vtlx >= (self.view_width - width):
            width = self.view_width - vtlx
            edges_showing = True

        if vtly < 0:
            wtly = wtly + abs(vtly)
            height = height + vtly
            vtly = 0
            edges_showing = True
        elif vtly >= (self.view_height - height):
            height = self.view_height - vtly
            edges_showing = True

        if edges_showing:
            self.map_console.draw_rect(1 if self.framed_p else 0,
                                       1 if self.framed_p else 0,
                                       self.width - 2 if self.framed_p else 0,
                                       self.height - 2 if self.framed_p else 0,
                                       clear=False,
                                       flag=tcod.BKGND_NONE)
        self.map_console.blit(self, vtlx, vtly, width, height,
                              wtlx, wtly, 1.0, 1.0)

    def clear_map(self, auto_redraw=False):
        self.map_console.clear()
        self.copy_map_to_viewport()
        if redraw:
            self.redraw_area()

    def center_view(self, x, y):
        """Center viewport on map coordinate (x, y). Does not cause the view
        to refresh.
        """
        self.view_tlx = x - floor(self.view_width / 2)
        self.view_tly = y - floor(self.view_height / 2)
