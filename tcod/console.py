import os.path
from threading import Thread
from collections import namedtuple
import numpy as np
import tcod
import color as color
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

class ConsoleBuffer(object):
    def __init__(self, console):
        self.console = console
        self.width = console.width
        self.height = console.height
        self.background = np.empty((self.width, self.height, 3), dtype=np.int32)
        self.foreground = np.empty((self.width, self.height, 3), dtype=np.int32)
        self.char = np.empty_like([[' '] * self.height] * self.width)
        self.clear()

    def clear(self):
        for i in range(self.width):
            for j in range(self.height):
                self.background[i, j] = tcod.black
                self.foreground[i, j] = tcod.white
                self.char[i, j] = '.'

    def flush_background(self):
        _c = self.console._c
        br = self.background[..., 0].ravel(order='F')
        bg = self.background[..., 1].ravel(order='F')
        bb = self.background[..., 2].ravel(order='F')
        tcod.console_fill_background(_c, br, bg, bb)

    def flush_foreground(self):
        _c = self.console._c
        fr = self.foreground[..., 0].ravel(order='F')
        fg = self.foreground[..., 1].ravel(order='F')
        fb = self.foreground[..., 2].ravel(order='F')
        tcod.console_fill_foreground(_c, fr, fg, fb)

    def flush_char(self):
        _c = self.console._c
        tcod.console_fill_char(_c, self.char.ravel())

    def blit(self, fill_fore=True, fill_back=True):
        if fill_back:
            self.flush_background()
        if fill_fore:
            self.flush_foreground()
            self.flush_char()


class Console(object):
    """Abstract the TCOD console object to be more Pythonic.
    """

    def __init__(self, width, height, buffered=False):
        self.width = width
        self.height = height
        self._c = tcod.console_new(width, height)
        self.default_background_color = tcod.black
        self.default_foreground_color = tcod.white
        # tcod.console_set_default_background(self._c, tcod.black)
        # tcod.console_set_default_foreground(self._c, tcod.white)
        tcod.console_clear(self._c)
        if buffered:
            self.buffer = ConsoleBuffer(self)

    def __del__(self):
        if self._c:
            tcod.console_delete(self._c)

    def __len__(self):
        return self.width * self.height

    def __getitem__(self, index):
        if isinstance(index, slice):
            raise TypeError('Console objects do not support slices. Yet.')
        x, y = index
        if x > self.width or x < 0 or y > self.height or y < 0:
            raise IndexError(
                'Attempt to access cell ({0}, {1}), which is out of range. Console size is ({2}, {3}).'.format(x, y,
                                                                                                               self.width,
                                                                                                               self.height))

        return ConsoleCell(chr(tcod.console_get_char(self._c, x, y)),
                           tcod.console_get_char_foreground(self._c, x, y),
                           tcod.console_get_char_background(self._c, x, y))

    def __setitem__(self, index, cell):
        if isinstance(index, slice):
            raise TypeError('Console objects do not support slices. Yet.')
        x, y = index
        if x > self.width or x < 0 or y > self.height or y < 0:
            raise IndexError(
                'Attempt to access cell ({0}, {1}), which is out of range. Console size is ({2}, {3}).'.format(x, y,
                                                                                                               self.width,
                                                                                                               self.height))

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

        tcod.console_set_char_background(self._c, x, y, background)
        tcod.console_set_char_foreground(self._c, x, y, foreground)
        tcod.console_put_char(self._c, x, y, symbol)

    @property
    def keyboard_repeat(self):
        return self._keyrepeat

    @keyboard_repeat.setter
    def keyboard_repeat(self, val):
        self._keyrepeat = val
        tcod.console_set_keyboard_repeat(self._c, *val)

    @property
    def default_background_color(self):
        return tcod.console_get_default_background(self._c)

    @default_background_color.setter
    def default_background_color(self, color):
        tcod.console_set_default_background(self._c, color)

    @property
    def default_foreground_color(self):
        return tcod.console_get_default_foreground(self._c)

    @default_foreground_color.setter
    def default_foreground_color(self, color):
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

    def fill_from_buffer(self, fill_foreground=True, fill_background=True):
        self.buffer.blit(fill_foreground, fill_background)

    # TODO: These low-level access routines should either be renamed
    #       or refactored to use new numpy arrays.
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
        prev_bg_color = self.default_background_color
        if bg_color is not None:  # Hear the cries for a context manager!
            self.default_background_color = bg_color
        tcod.console_print_ex(self._c, x, y, background_flag, align, xstr)
        self.default_background_color = prev_bg_color

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
                 font_file=tcod.default_font, font_flags=tcod.FONT_LAYOUT_TCOD | tcod.FONT_TYPE_GREYSCALE,
                 font_width=0, font_height=0,
                 datax='', fullscreen=False,
                 renderer=tcod.RENDERER_SDL, max_fps=30):
        """
        Create a RootConsole object.

        :param width:
        :param height:
        :param title:
        :param background:
        :param font_file:
        :param datax:
        :param fullscreen:
        :param renderer:
        :param max_fps:
        :return:
        """
        super().__init__()
        if not os.path.exists(font_file):
            path = os.path.join(os.path.dirname(os.path.abspath(__file__)), os.path.normpath('data/fonts/'))
            font_file = os.path.join(path, font_file)
        if os.path.exists(font_file):
            tcod.console_set_custom_font(bytes(font_file, 'utf-8'),
                                         flags=font_flags,
                                         nb_char_horiz=font_width,
                                         nb_char_vertic=font_height)
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
        self.default_background_color = background

    def flush(self):
        tcod.console_flush()

    def init_root(self, show_credits=False):
        RootConsole.active_root = self
        RootConsole.scratch = Console(self.width, self.height)
        RootConsole.temp_console = Console(self.width, self.height)

        tcod.console_init_root(self.width, self.height, self.title,
                               self.fullscreen, self.renderer)
        if show_credits:
            tcod.console_credits()
        tcod.sys_set_fps(self.max_fps)

    def run(self, gameloop_manager):
        while (not self.end_game) and (not tcod.console_is_window_closed()):
            # events = sys_get_events()
            # self.handle_keys(key)
            tcod.console_clear(tcod.root_console)
            gameloop_manager.step(self)
            tcod.console_flush()


R = RootConsole
