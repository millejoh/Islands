
import os.path
from warnings import warn
import tcod

# Make a subclass of Tuple?
class ConsoleCell(object):
    """Represent a console cell as a 3-element tuple: symbol (or character), foreground color,
and background color."""
    def __init__(self, symbol=' ', foreground=None, background=None):
        self.symbol = symbol
        self.foreground = foreground
        self.background = background

    def __repr__(self):
        return 'ConsoleCell({0},{1},{2})'.format(self.symbol, self.foreground, self.background)

class Console(object):
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self._c = tcod.console_new(width, height)
        self.foreground = tcod.white
        self.background = tcod.black

    def __del__(self):
        tcod.console_delete(self._c)

    def __len__(self):
        return self.width*self.height

    def __getitem__(self, index):
        if isinstance(index, slice):
            raise TypeError('Console objects do not support slices. Yet.')
        x, y = index
        if x > self.width or x < 0 or y > self.height or y < 0:
            raise IndexError('Attempt to access cell ({0}, {1}), which is out of range. Console size is ({2}, {3}).'.format(x, y, self.width, self.height))

        return (chr(tcod.console_get_char(self._c, x, y)),
                tcod.console_get_char_foreground(self._c, x, y),
                tcod.console_get_char_background(self._c, x, y))
    
    def __setitem__(self, index, cell):
        if isinstance(index, slice):
            raise TypeError('Console objects do not support slices. Yet.')
        x, y = index
        if x > self.width or x < 0 or y > self.height or y < 0:
            raise IndexError('Attempt to access cell ({0}, {1}), which is out of range. Console size is ({2}, {3}).'.format(x, y, self.width, self.height))
        
        if cell is tuple and len(cell) > 3:
            symbol, foreground, background = cell
        elif cell is not tuple:
            symbol = cell
            foreground = self.foreground
            background = self.background
        else:
            symbol = cell[0]
            foreground = self.foreground
            background = self.background

        tcod.console_put_char_ex(self._c, x, y, symbol, foreground, background)

    @property
    def keyboard_repeat(self):
        return self._keyrepeat

    @keyboard_repeat.setter
    def keyboard_repeat(self, val):
        self._keyrepeat = val
        tcod.console_set_keyboard_repeat(self._c, *val)

    @property
    def background(self):
        return tcod.console_get_default_background(self._c)

    @background.setter
    def background(self, color):
        tcod.console_set_default_background(self._c, color)

    @property
    def foreground(self):
        return tcod.console_get_default_foreground(self._c)

    @foreground.setter
    def foreground(self, color):
        tcod.console_set_default_foreground(self._c, color)

    @property
    def alignment(self):
        return tcod.console_get_alignment(self._c)

    @alignment.setter
    def alignment(self, alignment):
        tcod.console_set_alignment(self._c, alignment)

    @property
    def background_flag(self):
        tcod.console_get_background_flag(self._c)

    @background_flag.setter
    def background_flag(self, flag):
        tcod.console_set_background_flag(self._c, flag)

    @property
    def window_closed(self):
        return tcod.console_is_window_closed()

    def put_cell(self, x, y, sym, flag=tcod.BKGND_NONE):
        tcod.console_put_char(self._c, x, y, sym, flag)

    def write(self, x, y, fmt):
        tcod.console_print(self._c, x, y, fmt)

    def write_rect(self, x, y, w, h, fmt):
        tcod.console_print_rect(self._c, x, y, w, h, fmt)

    def get_text_height(self, x, y, w, h, fmt):
        return tcod.console_get_height_rect(self._c, x, y, w, h, fmt)

    def draw_rect(self, x, y, w, h, clear=False, flag=tcod.BKGND_NONE):
        tcod.console_rect(self._c, x, y, w, h, clear, flag)

    def hline(self, x, y, length, flag=tcod.BKGND_NONE):
        tcod.console_hline(self._c, x, y, length, flag)

    def vline(self, x, y, length, flag=tcod.BKGND_NONE):
        tcod.console_vline(self._c, x, y, length, flag)
    
    def frame(self, x, y, w, h, text, clear=True, flag=tcod.BKGND_NONE):
        tcod.console_print_frame(self._c, x, y, w, h, clear, flag, text)

    def clear(self):
        tcod.console_clear(self._c)
    
    def print_double_frame(self, x, y, width, height, clear=True,
                           flag=tcod.BKGND_DEFAULT, fmt=0):
        """Draw a rectangle like print_frame but draws using `double-line` characters.
        
        """
        tcod.console_print_double_frame(self._c, x, y, width, height, clear,
                                        flag, fmt)

    def print_frame(self, x, y, width, height, clear=True,
                    flag=tcod.BKGND_DEFAULT, fmt=0):
        """Draw a rectangle with size `width` and `height` located at position (

        """
        tcod.console_print_frame(self._c, x, y, width, height, clear, flag, fmt)
        pass

    def blit(self, dest, sx, sy, width, height, dx, dy, fore_alpha, back_alpha):
        """Blit rectangular region of a console to a specific position in another console.

        dest = Destination console.
        sx, sy = Upper left corner of rectangle in source console to be blitted.
        width, height = Size of region in source console to be blitted.
        dx, dy = Location in destination console where rectangle will be blitted.
        fore_alpha, back_alpha = Foreground and background transparency parameters."""
        tcod.console_blit(self._c, sx, sy, width, height, dest._c, dx, dy,
                          fore_alpha, back_alpha)


class RootConsole(Console):
    active_root = None
    mouse_x = 0
    mouse_y = 0

    @classmethod
    def screen_width(cls):
        if cls.active_root:
            return cls.active_root.width
        else:
            return -1

    @classmethod
    def screen_height(cls):
        if cls.active_root:
            return cls.active_root.height
        else:
            return -1
        
    def __init__(self, width=80, height=50, title=b'Stage', background = tcod.darker_sepia, font_file=tcod.default_font, datax='', fullscreen=False, renderer=tcod.RENDERER_GLSL, max_fps=30):
        if RootConsole.active_root:
            warn('Root console already initialized. Any parameters supplied with call are being ignored.')
        else:
            if os.path.exists(font_file):
                tcod.console_set_custom_font(bytes(font_file, 'utf-8'), tcod.FONT_LAYOUT_TCOD |
                                             tcod.FONT_TYPE_GREYSCALE)
            else:
                raise OSError("Font file {0} not found.".format(font_file))

            tcod.console_init_root(width, height, title,
                                   fullscreen, renderer)
            tcod.sys_set_fps(max_fps)
            self._c = tcod.root_console
            self.width = width
            self.height = height
            self.title = title
            self.fullscreen = fullscreen
            self.renderer = renderer
            self.end_game = False
            self.max_fps = max_fps
            self.background = background
            RootConsole.active_root = self

    def flush(self):
        tcod.console_flush()

    def handle_events(self):
        mouse, key = tcod.Mouse(), tcod.Key()
        tcod.sys_check_for_event(tcod.EVENT_ANY, key, mouse)
        return key, mouse
    
    def run(self):

        while (not self.end_game) and (not tcod.console_is_window_closed()):
            key, mouse = self.handle_events()
            self.handle_keys(key)
            if key.vk == tcod.KEY_ESCAPE:
                tcod.console_clear(tcod.root_console)
                tcod.console_print(tcod.root_console, 0, 0, "Exiting...")
                tcod.console_flush()
                break
            tcod.console_clear(tcod.root_console)
            tcod.console_print(tcod.root_console, 0, 0, "Current key pressed is {0}.".format(key.vk))
            tcod.console_print(tcod.root_console, 0, 1, "Cursor at ({0}, {1}).".format(mouse.cx, mouse.cy))
            tcod.console_flush()



def gui_loop():
    credits_end = False
    key = tcod.Key()
    mouse = tcod.Mouse()

    while not tcod.console_is_window_closed():
        tcod.sys_check_for_event(tcod.EVENT_KEY_PRESS | tcod.EVENT_MOUSE, key, mouse)
        tcod.console_set_default_foreground(0, tcod.white)
        if not credits_end:
            credits_end = tcod.console_credits_render(60, 43, 0)
        
        tcod.console_print(0, 0, 1, "Key pressed:{0!a}".format(key.c))
        tcod.console_flush()

        


