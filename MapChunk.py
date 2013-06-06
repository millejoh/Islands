# Try to find libtcodpy
import sys, platform
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

class MapChunk(object):
    def __init__(self, width, height, offset=[0, 0]):
        self._offset = offset
        self._width = width
        self._height = height
        self._hmap = tcod.heightmap_new(width, height)
