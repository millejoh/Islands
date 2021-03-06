__author__ = 'millejoh'

import logging

import gui.window
import tcod
from gui.console import RootConsole, default_font
from gui.events import *
from gui.gameloop import BasicEventLoop
from gui.utils import translate_negative_coords, transparency_to_fade
from threading import Timer

class WindowManager(object):
    def __init__(self, screen_width, screen_height, auto_redraw=True, opaque=1, invisible=0, dimmed=75, bold_factor=1.4,
                 focus_fade_mode='together', font=default_font,
                 font_flags=tcod.FONT_LAYOUT_TCOD|tcod.FONT_TYPE_GREYSCALE,
                 font_width=0, font_height=0):
        """
        Create a libtcodpy root console and GUI window manager with a given configuration.

        :param screen_width: Width of root console, in screen pixels.
        :param screen_height:  Height of root console, in screen pixels.
        :param auto_redraw: When TRUE, automatically redraw GUI elements when changes occur.
        :param opaque: Default opacity for GUI elements, 0 = transparent, 1 = opaque.
        :param invisible: If TRUE, GUI elements by default will not be shown upon instantiation.
        :param dimmed:
        :param bold_factor:
        :param focus_fade_mode:
        :param font: Font to use for root console.
        :param font_flags:
        :param font_width:
        :param font_height:
        """
        #assert root_console is not None
        self.rootc = RootConsole(screen_width, screen_height, font_file=font, font_flags=font_flags,
                                 font_width=font_width, font_height=font_height)
        self.window_stack = []
        self.hidden_window_stack = []
        self.topwin = None
        self.last_topwin = None
        self.auto_redraw = auto_redraw
        self.opaque = opaque
        self.invisible = invisible
        self.dimmed = dimmed
        self.bold_factor = bold_factor
        self.focus_fade_mode = focus_fade_mode

    @property
    def screen_width(self):
        return self.rootc.width

    @property
    def screen_height(self):
        return self.rootc.height

    def screen_to_window_coord(self, window, screen_x: int, screen_y: int):
        """Convert screen tile coordinate to window local coordinate system.

        window = Window whose coordinates we are trying to calculate.
        screen_x = X tile coordinate on screen.
        screen_y = Y tile coordinate on screen."""

        win_tlx, win_tly = window.tlx, window.tly
        return screen_x - win_tlx, screen_y - win_tly

    def top_window_at(self, x, y, stack=None, override_modal=False):
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
        stack = stack or self.window_stack
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
                            not isinstance(win, gui.window.GhostWindow)]
                return touching[0] if len(touching) > 0 else touching

    def redraw_all_windows(self, exclude=None):
        """Copy all visible windows onto the root console.

        exclude = list of window instances to exclude, defaults to None."""
        windows = [win for win in self.window_stack if win not in ([] if exclude is None else exclude)]
        self.rootc.clear()
        if windows:
            for win in windows.reverse():
                win.prepare()
                win.copy_to_console(self.rootc)
            tcod.console_set_dirty(0, 0,
                                   self.rootc.screen_width(),
                                   self.rootc.screen_height())

    # def top_window_at(self, x, y, override_modal=False):
    #     """Return the window nearest the top of *WINDOW-STACK* which touches X,Y.

    #     X, Y = Coordinates of a point on the screen (root console).
    #     stack =  List of window objects to consider (default is to consider
    #              all non-hidden windows).
    #     override-modal = Boolean.

    #     Return the window nearest the top of *WINDOW-STACK* which touches X,Y.  If
    #     OVERRIDE-MODAL? is true, then disregard whether a window is modal or not in
    #     deciding which window to return. If this parameter is nil (default) then
    #     whenever a modal window is at the top of WINDOW-STACK we can only return
    #     that window from this function. If that window does not touch X,Y then NIL
    #     is returned.

    #     """
    #     if len(self.window_stack) == 0:
    #         return None
    #     else:
    #         x, y = translate_negative_coords(x, y)
    #         top = self.window_stack[0]
    #         if not override_modal and top.is_modal:
    #             if top.touches_spot(x, y):
    #                 return top
    #             else:
    #                 return None
    #         else:
    #             touching = [win for win in self.window_stack
    #                         if win.touches_spot(x, y) and
    #                         not isinstance(win, gui.window.GhostWindow)]
    #             return touching[0] if len(touching) > 0 else touching


    def windows_at(self, x, y):
        """All windows that overlie the point at (x, y).
        """
        x, y = translate_negative_coords(x, y)
        return [win for win in self.window_stack if win.touches_spot(x, y)]


    def all_windows(self, exclude=None):
        if exclude is None:
            exclude = []
        return [w for w in self.window_stack + self.hidden_window_stack if w not in exclude]


    def process_windows(self):
        """Call process_window() for all windows that are not hidden.
        Meant to be called as part of main event loop."""
        windows = self.all_windows()
        if len(windows) > 0:
            for win in reversed(windows):
                win.process_window()

    def destroy_all_windows(self):
        """Destroy all existing window objects."""
        for win in self.window_stack:
            win.destroy()


class GUIEventLoop(BasicEventLoop):
    def __init__(self, window_manager, drag_delay=0.05, double_click_speed=1000):
        """

        :param window_manager:
        :param drag_delay:
        :param double_click_speed:
        """
        self.active_dragged_window = None
        self.active_resize_window = None
        self.last_mouse_click = None
        self.drag_delay = drag_delay
        self.double_click_speed = double_click_speed
        self.window_manager = window_manager
        self.end_game = False
        super().__init__()

    def run(self):
        root = self.window_manager.rootc
        root.init_root()
        while (not self.end_game) and (not tcod.console_is_window_closed()):
            root.clear()
            self.step(root)
            root.flush()
            if self.ipykernel:
                self.step_kernel()

    # def window_with_mouse_focus(self):
    #     """Return the topmost window under the mouse pointer."""
    #     x, y = last_mouse_click.tile[0], last_mouse_click.tile[1]
    #     return self.window_manager.top_window_at(x, y)


    def window_with_key_focus(self):
        """Return the window where keyboard events will/are sent to."""
        if len(self.window_manager.window_stack) > 0 and self.window_manager.window_stack[0].is_modal:
            return self.window_manager.window_stack[0]
        else:
            return self.window_with_mouse_focus()



    def send_key_event(self, window, key, x, y):
        key.window = window
        key.winx = x
        key.winy = y
        window.on_key_event(key)


    def fade_for_window(self, win):
        focus = self.window_with_mouse_focus()
        if win.transparency_unfocused is None or win is focus:
            return transparency_to_fade(win.transparency)
        elif self.window_manager.focus_fade_mode == 'together' and not isinstance(focus, gui.window.BackgroundWindow):
            return transparency_to_fade(win.transparency)
        elif isinstance(focus, gui.window.Window) and win.raise_children_with_parent_p and \
                (win in focus.children or focus in win.children):
            return transparency_to_fade(win.transparency)
        else:
            return transparency_to_fade(win.transparency_unfocused)


    def ev_mousemotion(self, mouse):
        t_x, t_y = mouse.tile
        win = self.window_manager.top_window_at(t_x, t_y)
        if win:
            win.on_mouse_motion(mouse)


    def ev_mousebuttondown(self, mouse):
        wm = self.window_manager
        t_x, t_y = mouse.tile
        win = wm.top_window_at(t_x, t_y)
        if win:
            wm.topwin = win
            wm.focus_changed = wm.topwin != wm.last_topwin
            win.on_mouse_buttondown(mouse)
            # TODO: Handle double clicks!


    def ev_mousebuttonup(self, mouse):
        t_x, t_y = mouse.tile
        win = self.window_manager.top_window_at(t_x, t_y)
        if win:
            win.on_mouse_buttonup(mouse)


    def ev_keyboardevent(self, key):
        wm = self.window_manager
        wm.topwin = self.window_with_key_focus()
        if wm.topwin:
            self.send_key_event(wm.topwin, key,
                                self.mouse_x - wm.topwin.tlx,
                                self.mouse_y - wm.topwin.tly)

    def on_key_event(self, key, x, y):
        pass

    def step(self, root):
        wm = self.window_manager
        wm.focus_changed = False
        for event in tcod.event.get():
            self.current_event = event
            self.dispatch(event)
        if wm.focus_changed:
            wm.last_topwin = wm.topwin
        wm.process_windows()
