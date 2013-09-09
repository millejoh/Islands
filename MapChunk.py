# Try to find libtcodpy
import tcod
from tcod.tools import Heightmap
import random as r
import numpy as np

biomes = [ ['SNOW', 'SNOW', 'SNOW', 'TUNDRA', 'BARE', 'SCORCHED'],
       	   ['TAIGA', 'TAIGA', 'SHRUBLAND', 'SHRUBLAND', 'TEMPERATE_DESERT', 'TEMPERATE_DESERT'],
	       ['TEMPERATE_RAIN_FOREST', 'TEMPERATE_DECIDUOUS_FOREST', 'TEMPERATE_DECIDUOUS_FOREST', 'GRASSLAND', 'GRASSLAND', 'TEMPERATE_DESERT'],
	   ['TROPICAL_RAIN_FOREST', 'TROPICAL_RAIN_FOREST', 'TROPICAL_SEASONAL_FOREST', 'TROPICAL_SEASONAL_FOREST', 'GRASSLAND', 'SUBTROPICAL_DESERT'] ]

biome_colors = { 'SNOW'      : tcod.Color(248,248,248),
		 'TUNDRA'    : tcod.Color(221,221,187),
		 'BARE'      : tcod.Color(187,187,187),
		 'SCORCHED'  : tcod.Color(153,153,153),
		 'TAIGA'     : tcod.Color(204,212,187),
		 'SHRUBLAND' : tcod.Color(194,204,187),
		 'GRASSLAND' : tcod.Color(192,212,170),
		 'TEMPERATE_DESERT'           : tcod.Color(228,232,202),
		 'TEMPERATE_RAIN_FOREST'      : tcod.Color(164,196,168),
		 'TEMPERATE_DECIDUOUS_FOREST' : tcod.Color(180,201,169),
		 'TROPICAL_RAIN_FOREST'       : tcod.Color(156,187,169),
		 'TROPICAL_SEASONAL_FOREST'   : tcod.Color(169,204,164),
		 'SUBTROPICAL_DESERT'         : tcod.Color(233,221,199) }

class Tile(object):
    def __init__(self):
        self.elevation = 0.0
        self.precipitation = 0.0
        self.temperature = 0.0
        self.color = self.determine_color()

    def determine_color(self):
        pass

class MapChunk(object):
    def __init__(self, width, height, offset=[0, 0]):
        self._offset = offset
        self._width = width
        self._height = height
        self.__rng = tcod.random_get_instance()
        self.elevation = Heightmap(width, height)
        self.elevation.clear_map()
        self.precipitation = Heightmap(width, height)
        self.temperature = Heightmap(width, height)
        self.terrain = np.zeros((width, height))

    def __del__(self):
        tcod.random_delete(self.__rng)
        
    def random_island(self, sx, sy, width, height, roughness=1.0):
        """Add an island to the map.

        sx, sy        = Top left coordinates of the island box.
        width, height = Dimensions of the island box.
        roughness     = Fractional value to control roughness of the island's
                        elevational transitions.
        """
        e = self.elevation
        max_radius = 0.2 * (width+height/2.0)
        # Blob the map with hills
        for i in range(int(roughness*20)):
            radius = r.randint(0, max_radius)
            x, y = r.randint(sx, sx+width), r.randint(sy, sy+height)
            e.add_hill(x, y, radius, radius*radius)

    def draw_terrain_region(self, rect, console):
        ox, oy, w, h = rect
        for i in range(w):
            x = ox + i
            for j in range(h):
                y = oy + j
                console[x,y] = (' ', tcod.foreground, biome_colors[self.terrain[x,y]])


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


