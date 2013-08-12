"""
A simple GUI using the TCOD library. Very much inspired by cl-dormouse.
One might even go so far as to say "stolen from." Seems like a good way
to learn a language (and library) to me.
"""

import tcod
from console import Console

def clamp(min, max, val):
    if min > val:
        return min
    elif val > max:
        return max
    else:
        return val

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

class Window(Console):
    def __init__(self, tlx, tly, width, height):
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
        elif focusChanged:
            self.redraw_window_area(draw_window=True)

    def send_to_window(self, event):
        if self.hidden_p:
            return
        return self.event_handler(event)

    def dirty_window(self):
        tlx, tly = self.tlx, self.tly
        w, h = self.width, self.height

        self.console_set_dirty(
            clamp(tlx, 0, (screend_width() - 1)),
            clamp(tly, 0, (screen_height() - 1)),
            clamp(w, 0, (screen_width() - tlx)),
            clamp(h, 0, (screen_height() - tly)))

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

