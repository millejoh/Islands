__author__ = 'millejoh'

import tcod
import gui.window
from tcod.console import RootConsole
from tcod.gameloop import BasicEventLoop
from tcod.events import *
from gui.utils import translate_negative_coords, transparency_to_fade

try:
    from ipykernel.zmqshell.eventloops import register_integration
    support_ipy = True
except ImportError:
    support_ipy = False

class WindowManager(object):
    def __init__(self, screen_width, screen_height, auto_redraw=True, opaque=1, invisible=0, dimmed=75, bold_factor=1.4,
                 focus_fade_mode='together', font=tcod.default_font,
                 font_flags=tcod.FONT_LAYOUT_TCOD|tcod.FONT_TYPE_GREYSCALE,
                 font_width=0, font_height=0):
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

    def top_window_at(self, x, y, override_modal=False):
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
        if len(self.window_stack) == 0:
            return None
        else:
            x, y = translate_negative_coords(x, y)
            top = self.window_stack[0]
            if not override_modal and top.is_modal:
                if top.touches_spot(x, y):
                    return top
                else:
                    return None
            else:
                touching = [win for win in self.window_stack
                            if win.touches_spot(x, y) and
                            not isinstance(win, gui.window.GhostWindow)]
                return touching[0] if len(touching) > 0 else touching


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


class GuiEventLoop(BasicEventLoop):
    def __init__(self, window_manager, drag_delay=0.05, double_click_speed=1000, ipykernel=None):
        self.mouse_x = 0
        self.mouse_y = 0
        self.last_mouse_click = None
        self.drag_delay = drag_delay
        self.double_click_speed = double_click_speed
        self.window_manager = window_manager
        self.ipykernel = ipykernel
        self.end_game = False
        super().__init__()


    def ipy_kernel_callback(dt, kernel):
        kernel.do_one_iteration()

    def run(self):
        root = self.window_manager.rootc
        root.init_root()
        while (not self.end_game) and (not tcod.console_is_window_closed()):
            #events = sys_get_events()
            #self.handle_keys(key)
            if self.ipykernel:
                self.ipy_kernel_callback(self.ipykernel)

            root.clear()
            self.step(root)
            root.flush()

    def window_with_mouse_focus(self):
        """Return the topmost window under the mouse pointer."""
        return self.window_manager.top_window_at(self.mouse_x, self.mouse_y)


    def window_with_key_focus(self):
        """Return the window where keyboard events will/are sent to."""
        if len(self.window_manager.window_stack) > 0 and self.window_manager.window_stack[0].is_modal:
            return self.window_manager.window_stack[0]
        else:
            return self.window_with_mouse_focus()


    def send_mouse_click_event(self, window, event):
        double_click = False
        if window:
            if event.state.lbutton and event.state.cy == window.tly and event.state.cx == window.brx and window.can_close_p:
                window.hide(True)
            elif self.last_mouse_click and type(event) == type(self.last_mouse_click) and \
                            (tcod.sys_elapsed_milli() - self.last_mouse_click.time) < self.double_click_speed:
                double_click = True
            self.last_mouse_click = MouseEvent(winx=event.state.cx - window.tlx,
                                               winy=event.state.cy - window.tly,
                                               mouse_state=event,
                                               window=window,
                                               double_click=double_click)
            window.on_mouse_event(self.last_mouse_click)

    def send_key_event(self, window, key, x, y):
        kevent = KeyEvent(key, window=window, winx=x, winy=y)
        window.on_key_event(kevent)

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


    def send_mouse_move_event(self, window, event):
        pass

    def handle_drag_event(self, mouse_event):
        """Check for and handle (if necessary) drag events.
        """
        start = tcod.sys_elapsed_milli()
        dragged = False
        mouse = mouse_event.state
        ox, oy = mouse.cx, mouse.cy
        key = tcod.Key()
        while mouse.lbutton:
            tcod.sys_check_for_event(tcod.EVENT_MOUSE, key, mouse)
            mouse.dx, mouse.dy = ox - mouse.cx, oy - mouse.cy
            if (tcod.sys_elapsed_milli() > (self.drag_delay * 1000 + start)) and \
                    (mouse.dx != 0 or mouse.dy != 0):
                dragged = True
                # Are we dragging on the window title (move!)?
                if self.window_manager.topwin.can_drag_p and \
                    self.window_manager.topwin.on_upper_window_border(mouse.cx - self.window_manager.topwin.tlx,
                                                       mouse.cy - self.window_manager.topwin.tly):
                    self.window_manager.topwin.mouse_drag(mouse)

                # Are we dragging on the bottom right corner (resize!)?
                elif self.window_manager.topwin.can_resize_p and \
                    ox == (self.window_manager.topwin.tlx + self.window_manager.topwin.width - 1) and \
                    oy == (self.window_manager.topwin.tly + self.window_manager.topwin.height - 1):
                    self.window_manager.topwin.mouse_resize(mouse)
                else:
                    self.window_manager.topwin.on_mouse_event(MouseDragEvent(winx=mouse.cx - self.window_manager.topwin.tlx,
                                                              winy=mouse.cy - self.window_manager.topwin.tly,
                                                              mouse_state=mouse_event))


    def step(self, root):
        wm = self.window_manager
        self.current_mouse_event = tcod.mouse_get_status()
        wm.focus_changed = False
        self.poll_for_event(tcod.EVENT_ANY)
        event_type = type(self.current_event)
        # for (event_type, event) in events:
        if issubclass(event_type, MouseEvent):
            mouse = self.current_event
            self.mouse_x, self.mouse_y = mouse.state.cx, mouse.state.cy
            if event_type is MouseMoveEvent:
                wm.focus_changed = self.window_with_mouse_focus() != wm.last_topwin
                self.send_mouse_move_event(self.window_with_mouse_focus(), mouse)
            elif event_type in (MousePressEvent, MouseReleaseEvent):
                wm.topwin = self.window_with_mouse_focus()
                wm.focus_changed = wm.topwin != wm.last_topwin
                self.send_mouse_click_event(wm.topwin, mouse)
        elif issubclass(event_type, KeyEvent):
            wm.topwin = self.window_with_key_focus()
            if wm.topwin:
                self.send_key_event(wm.topwin, self.current_event,
                                    self.mouse_x - wm.topwin.tlx,
                                    self.mouse_y - wm.topwin.tly)
        # That's done, now look at the state of the mouse
        if self.current_mouse_event and self.current_mouse_event.lbutton and \
                wm.topwin and not wm.topwin.hidden_p:
            wm.topwin.raise_window(redraw=self.window_manager.auto_redraw)
            self.handle_drag_event(self.current_event)

        if wm.focus_changed:
            wm.last_topwin = wm.topwin

        wm.process_windows()

if support_ipy:
    @register_integration('tcod_gui')
    def init_gui_manager(kernel):
        global window_manager, gui_loop
        window_manager = WindowManager(80, 60)
        gui_loop = GuiEventLoop(wm, kernel)
        gui_loop.run()
    
