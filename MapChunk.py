# Try to find libtcodpy
import tcod
from tcod.tools import Heightmap
from tcod.gui import Viewport
from pyDatalog import pyDatalog
from uuid import uuid1
import random as r
import numpy as np
from pyDatalog import pyDatalog

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


class MapChunk(Viewport):
    def __init__(self, offset=[0, 0], **keys):
        """
        """
        super().__init__(**keys)
        mwidth, mheight = self.map_width, self.map_height
        self._offset = offset
        self.__rng = tcod.random_get_instance()
        self.elevation = Heightmap(mwidth, mheight)
        self.elevation.clear_map()
        self.precipitation = Heightmap(mwidth, mheight)
        self.temperature = Heightmap(mwidth, mheight)
        self.terrain = np.zeros((mwidth, mheight))
        self.draw_mode = 'elevations'
        self.props = []
        self.actors = []

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

    def draw_terrain(self):
        for x, y in np.ndinndex(*self.terrain.shape()):
            self.map_console[x, y] = (' ', tcod.white, biome_colors[self.terrain[x,y]])

    def draw_elevations(self, as_color=True):
        for x, y in np.ndindex(*self.elevation.shape()):
            if as_color:
                intensity = tcod.color_lerp(tcod.black, tcod.white,
                                            self.elevation[x, y]/100.0)
            self.map_console[x, y] = (' ', tcod.white, intensity)

    def prepare(self):
        if self.draw_mode == 'elevations':
            self.draw_elevations()
        else:
            self.draw_terrain()
        super().prepare()

    def on_key_event(self, event):
        pass


# With inspiration from Chapter 8.11 of Python Cookbook, 3rd Edition.

class DefaultStructure(object):
    _fields = {}

    def __init__(self, **kwargs):
        """A class that eneralizes slot initialization of an object. Allows
        for specification of default values.

        Simplify writing of __init__ methods by specifying a
        dictionary in the class slot _fields as is similarly done in
        Chapter 8.11 of the Python Cookbook, 3rd Edition.

        _fields is a dictionary of slot names and default values. The
        new class only accepts keyword arguments during object
        initialization (it's a feature, not a bug!).

        """

        for name in kwargs.keys():
            setattr(self, name, kwargs[name])

        for name, value in self._fields.items():
            if not hasattr(self, name):
                setattr(self, name, value)


class gEntity(DefaultStructure):  # , pyDatalog.Mixin):
    _fields = {'x': 0, 'y': 0, 'z': 0, 'eclass': 'entity',
               'char': ' ', 'color': tcod.white}

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._id = uuid1()

    @property
    def pos(self):
        return (self.x, self.y, self.z)

    def draw(self, console):
        console[self._x, self._y] = (self._char, self._color, tcod.BKGND_NONE)

    def clear(self, console):
        console[self._x, self._y] = (' ', self._color, tcod.BKGND_NONE)

    def __repr__(self):
        return '<gEntity {}>'.format(self.eclass)
