import libtcodpy as tcod

class MapChunk(object):
    def __init__(self, width, height, offset=[0, 0]):
        self._offset = offset
        self._width = width
        self._height = height
        self._hmap = tcod.heightmap_new(width, height)
        
        
