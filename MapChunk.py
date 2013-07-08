# Try to find libtcodpy
import sys, platform
import os.path
import tcod
from tcod.tools import Heightmap

class MapChunk(object):
    def __init__(self, width, height, offset=[0, 0]):
        self._offset = offset
        self._width = width
        self._height = height
        self._hmap = Heightmap(width, height)

class gObject(object):
    def __init__(self, x, y, char, color):
        self._x, self._y = x, y
        self._char = char
        self._color = color

    def draw(self, console):
        console[self._x, self._y] = (self._char, self._color, tcod.BKGND_NONE)

    def clear(self, console):
        console[self._x, self._y] = (' ', self._color, tcod.BKGND_NONE)

class Player(gObject):
    def __init__(self, x, y):
        self._x, self._y = x, y
        self._char = '@'
        self._color = tcod.white


