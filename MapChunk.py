# Try to find libtcodpy
import random as rand
from uuid import uuid1

from colormath.color_objects import sRGBColor

#from colormath.color_conversions import convert_color
import numpy as np
import tcod
from gui.window import Viewport
from tcod.tools import Heightmap

biomes = [['SNOW', 'SNOW', 'SNOW', 'TUNDRA', 'BARE', 'SCORCHED'],
          ['TAIGA', 'TAIGA', 'SHRUBLAND', 'SHRUBLAND', 'TEMPERATE_DESERT', 'TEMPERATE_DESERT'],
          ['TEMPERATE_RAIN_FOREST', 'TEMPERATE_DECIDUOUS_FOREST', 'TEMPERATE_DECIDUOUS_FOREST', 'GRASSLAND',
           'GRASSLAND', 'TEMPERATE_DESERT'],
          ['TROPICAL_RAIN_FOREST', 'TROPICAL_RAIN_FOREST', 'TROPICAL_SEASONAL_FOREST', 'TROPICAL_SEASONAL_FOREST',
           'GRASSLAND', 'SUBTROPICAL_DESERT']]

biome_colors = {'SNOW': sRGBColor(248, 248, 248),
                'TUNDRA': sRGBColor(221, 221, 187),
                'BARE': sRGBColor(187, 187, 187),
                'SCORCHED': sRGBColor(153, 153, 153),
                'TAIGA': sRGBColor(204, 212, 187),
                'SHRUBLAND': sRGBColor(194, 204, 187),
                'GRASSLAND': sRGBColor(192, 212, 170),
                'TEMPERATE_DESERT': sRGBColor(228, 232, 202),
                'TEMPERATE_RAIN_FOREST': sRGBColor(164, 196, 168),
                'TEMPERATE_DECIDUOUS_FOREST': sRGBColor(180, 201, 169),
                'TROPICAL_RAIN_FOREST': sRGBColor(156, 187, 169),
                'TROPICAL_SEASONAL_FOREST': sRGBColor(169, 204, 164),
                'SUBTROPICAL_DESERT': sRGBColor(233, 221, 199)}

def color_lerp(color_1, color_2, coef):
    c1 = color_1.convert_to('rgb')
    c2 = color_2.convert_to('rgb')
    orig = np.array([c1.rgb_r, c1.rgb_g, c1.rgb_b])
    delta = np.array([c2.rgb_r - c1.rgb_r,
                          c2.rgb_g - c1.rgb_g,
                          c2.rgb_b - c1.rgb_b])
    interp = orig + delta * coef
    return sRGBColor(*interp)


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
        self.props = {}
        self.actors = {}

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
        max_radius = 0.2 * (width + height / 2.0)
        # Blob the map with hills
        for i in range(int(roughness * 20)):
            radius = rand.randint(0, max_radius)
            x, y = rand.randint(sx, sx + width), rand.randint(sy, sy + height)
            e.add_hill(x, y, radius, radius * radius)

    def draw_terrain(self):
        for x, y in np.ndinndex(*self.terrain.shape()):
            self.map_console[x, y] = (' ', tcod.white, biome_colors[self.terrain[x, y]])

    def draw_elevations(self, as_color=True):
        for x, y in np.ndindex(*self.elevation.as_ndarray().shape):
            if as_color:
                intensity = tcod.color_lerp(tcod.black, tcod.white,
                                            self.elevation[x, y] / 100.0)
            self.map_console[x, y] = (' ', tcod.white, intensity)

    def on_update(self):
        self.clear_map()
        if self.draw_mode == 'elevations':
            self.draw_elevations()
        for prop in self.props.values():
            prop.draw(self.map_console)
        for actor in self.actors.values():
            actor.draw(self.map_console)

    def on_key_event(self, event):
        key = event.key_info.vk
        p = self.actors['Player']
        if p and event.key_info.pressed:
            p.clear(self.map_console)
            if key == tcod.KEY_UP:
                p.y = 0 if p.y == 0 else p.y - 1
            elif key == tcod.KEY_DOWN:
                p.y = self.map_height if p.y == self.map_height else p.y + 1
            elif key == tcod.KEY_LEFT:
                p.x = 0 if p.x == 0 else p.x - 1
            elif key == tcod.KEY_RIGHT:
                p.x = self.map_width if p.x == self.map_width else p.x + 1
            self.center_view(p.x, p.y)
            p.draw(self.map_console)

    def add_actor(self, actor_obj):
        self.actors[actor_obj.name] = actor_obj

    def add_prop(self, prop_obj):
        self.props[prop_obj.name] = prop_obj


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
               'char': ' ', 'color': tcod.white,
               'name': 'generic entity'}

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._id = uuid1()

    @property
    def pos(self):
        return (self.x, self.y, self.z)

    def draw(self, console):
        back = console[self.x, self.y].default_background_color
        console[self.x, self.y] = (self.char, self.color, back)

    def clear(self, console):
        back = console[self.x, self.y].default_background_color
        console[self.x, self.y] = (' ', self.color, back)

    def __repr__(self):
        return '<gEntity {}>'.format(self.eclass)


if __name__ == '__main__':
    r = tcod.console.RootConsole(80, 60)
    map = MapChunk(tlx=10, tly=10, width=40, height=40, framed=True, map_width=120,
                   map_height=120, view_tlx=0, view_tly=0, title='The Map')
    map.random_island(0, 0, 60, 60)
    player = gEntity(char='@', name='Player', color = 'blue')
    map.add_actor(player)
    map.on_update()
    r.run()

