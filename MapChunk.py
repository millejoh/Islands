# Try to find libtcodpy
import sys, platform
import os.path

linux_path = '/home/millejoh/Documents/libtcod/python'
win_path = 'c:/Users/E341194/Applications/libtcod-1.5.2/python'

if platform.system() == 'Linux':
    if linux_path not in sys.path:
        sys.path.append(linux_path)
elif platform.system() == 'Windows':
    if win_path not in sys.path:
        sys.path.append(win_path)

try:
    import libtcodpy as tcod
except:
    print('I am running on {0} and I cannot find libtcodpy.'.format(platform.system()))

# tcod_dir = 'c:/Users/E341194/Applications/libtcod-1.5.2'
# default_font = os.path.join(tcod_dir,b'data', b'fonts', b'consolas10x10_gs_tc.png')

default_font = b'arial10x10.png'
root_console = 0 # This means NULL for you C folk.

class MapChunk(object):
    def __init__(self, width, height, offset=[0, 0]):
        self._offset = offset
        self._width = width
        self._height = height
        self._hmap = tcod.heightmap_new(width, height)

class gObject(object):
    def __init__(self, x, y, char, color):
        self._x, self._y = x, y
        self._char = char
        self._color = color

    def draw(self, console):
        tcod.console_set_default_foreground(console, self._color)
        tcod.console_put_char(console, self._x, self._y, self._char, tcod.BKGND_NONE)

    def clear(self, console):
        tcod.console_put_char(console, self._x, self._y, ' ', tcod.BKGND_NONE)

class Player(gObject):
    def __init__(self, x, y):
        self._x, self._y = x, y
        self._char = '@'
        self._color = tcod.white


