
import os.path
from threading import Thread
import tcod
import tcod.color as color
from collections import namedtuple
from tcod.string import make_colored_string


def sys_get_events():
    mouse = tcod.Mouse()
    key = tcod.Key()
    events = []
    while 1:
        event = tcod.sys_check_for_event(tcod.EVENT_ANY, key, mouse)
        if event == tcod.EVENT_NONE:
            break
        elif event == tcod.EVENT_KEY_PRESS or event == tcod.EVENT_KEY_RELEASE:
            events.append((event, key))
        elif event in (tcod.EVENT_MOUSE_MOVE,
                       tcod.EVENT_MOUSE_PRESS,
                       tcod.EVENT_MOUSE_RELEASE):
            events.append((event, mouse))
    return events


ConsoleCell = namedtuple('ConsoleCell', ['symbol', 'foreground', 'background'])


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

        return ConsoleCell(chr(tcod.console_get_char(self._c, x, y)),
                           tcod.console_get_char_foreground(self._c, x, y),
                           tcod.console_get_char_background(self._c, x, y))

    def __setitem__(self, index, cell):
        if isinstance(index, slice):
            raise TypeError('Console objects do not support slices. Yet.')
        x, y = index
        if x > self.width or x < 0 or y > self.height or y < 0:
            raise IndexError('Attempt to access cell ({0}, {1}), which is out of range. Console size is ({2}, {3}).'.format(x, y, self.width, self.height))

        if isinstance(cell, tuple) and len(cell) >= 3:
            symbol, foreground, background = cell
            if isinstance(foreground, str):
                foreground = color.string_to_colornum(foreground)
            if isinstance(background, str):
                background = color.string_to_colornum(background)
        elif cell is not tuple:
            symbol = cell
            foreground = self.get_char_foreground(x, y)
            background = self.get_char_background(x, y)
        else:
            symbol = cell[0]
            foreground = cell[2]
            background = self.get_char_background(x, y)

        if background is None:
            background = self.get_char_background(x, y)

        if foreground is None:
            foreground = self.get_char_foreground(x, y)

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
        return tcod.console_get_background_flag(self._c)

    @background_flag.setter
    def background_flag(self, flag):
        tcod.console_set_background_flag(self._c, flag)

    @property
    def window_closed(self):
        return tcod.console_is_window_closed()

    def get_char_foreground(self, x, y):
        return tcod.console_get_char_foreground(self._c, x, y)

    def get_char_background(self, x, y):
        return tcod.console_get_char_background(self._c, x, y)

    def put_cell(self, x, y, sym, flag=tcod.BKGND_NONE):
        tcod.console_put_char(self._c, x, y, sym, flag)

    def fill_char(self, char, tlx, tly, brx, bry):
        for x in range(tlx, brx):
            for y in range(tly, bry):
                self[x, y] = char

    def write(self, x, y, fmt):
        xstr = make_colored_string(fmt)
        tcod.console_print(self._c, x, y, xstr)

    def draw_string(self, x, y, fmt, bg_color=None,
                    background_flag=tcod.BKGND_SET,
                    align=tcod.LEFT):
        """Draw the string STR on the window object WIN at position X,Y. The string
        STR can contain color-changing directives - see the
        documentation for {defun dormouse:make-coloured-string} for
        details.

        Parameters
        ----------
        string : A string which may contain formatting directives (see below).

        x, y : Coordinates where the string should be printed, relative
               to the top left corner of WIN. If the coordinates are
               negative then they are taken as relative to the bottom
               right corner of WIN. If the coordinates are :CENTRE then
               the start of the string is taken as the centre of the
               window.

        fg, bg : Foreground and background colours for the string.

        align : One of `'left'`, `'right'` or `'center'`. If alignment
                is `'right'`, the string is drawn so that its last
                character is located at X, Y; if `'centre'` so that the
                middle character is at X, Y.

        Examples
        --------
        window.draw_string_at(`"Hello {blue}world!{/}"` 1 1 :fg :green)

        """
        xstr = make_colored_string(fmt)
        prev_bg_color = self.background
        if bg_color is not None: # Hear the cries for a context manager!
            self.background = bg_color
        tcod.console_print_ex(self._c, x, y, background_flag, align, xstr)
        self.background = prev_bg_color

    def write_rect(self, x, y, w, h, fmt):
        xstr = make_colored_string(fmt)
        tcod.console_print_rect(self._c, x, y, w, h, xstr)

    def get_text_height(self, x: int, y: int, w: int, h: int,
                        fmt: str):
        """Return the expected height of the autowrapped string `fmt` without
        actually drawing the screen to the console.

        Parameters
        ----------
        x, y : Upper left coordinate of text box.
        w, h : Width and height of text box.
        fmt : Format string to be printed in the text box.
        """
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
        """Draw a rectangle like print_frame but draws using `double-line`
        characters.

        """

        tcod.console_print_double_frame(self._c, x, y, width, height, clear,
                                        flag, fmt)

    def print_frame(self, x, y, width, height, clear=True,
                    flag=tcod.BKGND_DEFAULT, fmt=0):
        """Draw a rectangle with size `width` and `height` located at
        position (x, y).
        """
        tcod.console_print_frame(self._c, x, y,
                                 width, height,
                                 clear, flag, fmt)

    def blit(self, dest, sx, sy, width, height, dx, dy,
             fore_alpha, back_alpha):
        """Blit rectangular region of a console to a specific position
        in another console.

        dest = Destination console.

        sx, sy = Upper left corner of rectangle in source console to be
        blitted.

        width, height = Size of region in source console to be blitted.

        dx, dy = Location in destination console where rectangle will be
        blitted.

        fore_alpha, back_alpha = Foreground and background transparency
        parameters."""

        tcod.console_blit(self._c, sx, sy, width, height, dest._c, dx, dy,
                          fore_alpha, back_alpha)

    def copy_to_console(self, console):
        """Copy entire contents to another console.
        """

        self.blit(console, 0, 0, self.width, self.height, 0, 0, 1.0, 1.0)

# TODO Implement singletons via instance().


class RootConsole(Thread, Console):
    """
    """

    active_root = None
    scratch = None
    temp_console = None
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

    def __init__(self, width=80, height=50, title=b'Stage',
                 background=tcod.darker_sepia,
                 font_file=tcod.default_font, datax='', fullscreen=False,
                 renderer=tcod.RENDERER_GLSL, max_fps=30):
        super().__init__()
        if os.path.exists(font_file):
            tcod.console_set_custom_font(bytes(font_file, 'utf-8'),
                                         tcod.FONT_LAYOUT_TCOD |
                                         tcod.FONT_TYPE_GREYSCALE)
        else:
            raise OSError("Font file {0} not found.".format(font_file))

        self._c = tcod.root_console
        self.width = width
        self.height = height
        self.title = title
        self.fullscreen = fullscreen
        self.renderer = renderer
        self.end_game = False
        self.max_fps = max_fps
        self.background = background

    def flush(self):
        tcod.console_flush()

    def run(self):
        RootConsole.active_root = self
        RootConsole.scratch = Console(self.width, self.height)
        RootConsole.temp_console = Console(self.width, self.height)

        tcod.console_init_root(self.width, self.height, self.title,
                               self.fullscreen, self.renderer)
        tcod.sys_set_fps(self.max_fps)

        while (not self.end_game) and (not tcod.console_is_window_closed()):
            #events = sys_get_events()
            #self.handle_keys(key)
            tcod.console_clear(tcod.root_console)
            tcod.gui.gui_loop()
            tcod.console_flush()

R = RootConsole
