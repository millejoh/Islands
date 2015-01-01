__author__ = 'millejoh'

import tcod
import gui.window
from tcod.gameloop import BasicEventLoop
from tcod.events import *

class WindowManager(object):

    def __init__(self, auto_redraw=True, opaque=1, invisible=0, dimmed=75, bold_factor=1.4, focus_fade_mode='together'):
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

    def top_window_at(x, y, stack=None, override_modal=False):
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


class GuiEventLoop(BasicEventLoop):

    def __init__(self, drag_delay=0.05, double_click_speed=1000, window_manager=None):
        self.mouse_x = 0
        self.mouse_y = 0
        self.last_mouse_click = None
        self.drag_delay = drag_delay
        self.double_click_speed = double_click_speed
        if window_manager is None:
            self.window_manager = WindowManager()
        else:
            self.window_manager = window_manager

    def window_with_mouse_focus(self):
        """Return the topmost window under the mouse pointer."""
        return self.window_manager.top_window_at(self.mouse_x, self.mouse_y)

    def send_mouse_click_event(self, window, event):
        double_click = False
        if window:
            if event.lbutton and event.cy == window.tly and event.cx == window.brx and window.can_close_p:
                window.hide()
            elif self.last_mouse_click and type(event) == type(self.last_mouse_click) and \
                (tcod.sys_elapsed_milli() - self.last_mouse_click.time) < self.double_click_speed:
                double_click = True
            self.last_mouse_click = MouseEvent(winx=event.cx - window.tlx,
                                               winy=event.cy - window.tly,
                                               mouse_state=event,
                                               window=window,
                                               double_click=double_click)
            window.on_mouse_event(self.last_mouse_click)

    def send_key_event(self, window, key, x, y):
        kevent = KeyEvent(key, window=window, winx=x, winy=y)
        window.on_key_event(kevent)

    def step(self, root):
        wm = self.window_manager
        self.current_mouse_event = tcod.mouse_get_status()
        wm.focus_changed = False
        self.poll_for_event(tcod.EVENT_ANY)
        event_type = type(self.current_event)
        #for (event_type, event) in events:
        if issubclass(event_type, MouseEvent):
            mouse = self.current_event
            self.mouse_x, self.mouse_y = mouse.cx, mouse.cy
            if event_type is MouseMoveEvent:
                self.focus_changed = self.window_with_mouse_focus() != wm.last_topwin
                wm.send_mouse_move_event(self.window_with_mouse_focus(), mouse)
            elif event_type in (MousePressEvent, MouseReleaseEvent):
                self.topwin = self.window_with_mouse_focus()
                self.focus_changed = self.topwin != self.last_topwin
                self.send_mouse_click_event(self.topwin, mouse)
        elif issubclass(event_type, KeyEvent):
            self.topwin = window_with_key_focus()
            if self.topwin:
                self.send_key_event(self.topwin, self.current_event,
                                    self.mouse_x - self.topwin.tlx,
                                    self.mouse_y - self.topwin.tly)
        # That's done, now look at the state of the mouse
        if self.current_mouse_event and self.current_mouse_event.lbutton and \
                self.topwin and not self.topwin.hidden_p:
            self.topwin.raise_window()
            handle_drag_event(self.current_event)

        if self.focus_changed:
            self.last_topwin = self.topwin

        self.process_windows()