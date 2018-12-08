__author__ = 'millejoh'

import tcod
from tdl.noise import Noise
import numpy as np
from numbers import Number

#from numba import jit

class Heightmap(object):
    def __init__(self, width, height):
        ""
        self._data = tcod.heightmap_new(width, height)

    @property
    def width(self):
        return np.shape(self._data)[0]

    @property
    def height(self):
        return np.shape(self._data)[1]

    def __getitem__(self, index):
        # Bounds checking?
        x, y = index
        assert x < self.width and y < self.height
        if type(x) is float and type(y) is float:
            return tcod.heightmap_interpolated_value(self._data, x, y)
        else:
            return tcod.heightmap_get_value(self._data, x, y)

    def __setitem__(self, index, value):
        x, y = index
        assert x < self.width and y < self.height
        if type(x) is float and type(y) is float:
            x, y = tcod.heightmap_interpolated_value(x, y)
        tcod.heightmap_set_value(self._data, x, y, value)

    def copy(self):
        h_new = Heightmap(self.width, self.height)
        tcod.heightmap_copy(self._data, h_new._data)
        return h_new

    def add(self, value):
        h_new = self.copy()
        tcod.heightmap_add(h_new._data, value)
        return h_new

    def add_hm(self, h):
        if self.width != h.width or self.height != h.height:
            raise ValueError('Heightmaps must be of same dimension to support addition.')
        h_new = Heightmap(self.width, self.height)
        tcod.heightmap_add_hm(self._data, h._data, h_new._data)

        return h_new

    def scale(self, value):
        h_new = self.copy()
        tcod.heightmap_scale(h_new._data, value)
        return h_new

    def mult_hm(self, h):
        if self.width != h.width or self.height != h.height:
            raise ValueError('Heightmaps must be of same dimension to support addition.')
        h_new = Heightmap(self.width, self.height)
        tcod.heightmap_multiply_hm(self._data, h._data, h_new._data)
        return h_new

    def __add__(self, value):
        if isinstance(value, Number):
            return self.add(value)
        elif isinstance(value, Heightmap):
            return self.add_hm(value)

    def __iadd__(self, value):
        if isinstance(value, Number):
            tcod.heightmap_add(self._data, value)
            return self
        elif isinstance(value, Heightmap):
            tcod.heightmap_add(self._data, value._data, self._data)
            return self

    def __sub__(self, value):
        if isinstance(value, Number):
            return self.add(-value)
        elif isinstance(value, Heightmap):
            return self.add_hm(-value)

    def __mul__(self, value):
        if isinstance(value, Number):
            return self.scale(value)
        elif isinstance(value, Heightmap):
            return self.mult_hm(value)

    def __iter__(self):
        for i in range(self.width):
            for j in range(self.height):
                yield self[i,j]

    def __neg__(self):
        for i in range(self.width):
            for j in range(self.height):
                self[i, j] = -self[i, j]
        return self

    # def __array__(self ,*args):
    #     return np.array(list(iter(self)), *args).reshape([self.width, self.height])

    def normalize(self, min=0.0, max=1.0):
        tcod.heightmap_normalize(self._data, min, max)
        return self

    def add_hill(self, x, y, radius, height):
        tcod.heightmap_add_hill(self._data, x, y, radius, height)
        return self

    def dig_hill(self, x, y, radius, height):
        tcod.heightmap_dig_hill(self._data, x, y, radius, height)
        return self

    def rain_erosion(self, nb_drops, erosion_coef, sedimentation_coef, rnd=None):
        tcod.heightmap_rain_erosion(self._data, nb_drops, erosion_coef, sedimentation_coef, rnd)
        return self

    def kernel_transform(self, kernel_size, dx, dy, weight, min_level, max_level):
        tcod.heightmap_kernel_transform(self._data, kernel_size, dx, dy, weight, min_level, max_level)
        return self

    def add_voronoi(self, nb_points, nb_coef, coef, rnd=None):
        """This function adds values from a Voronoi diagram to the map.

        :param: nb_points : Number of Voronoi sites.
        :param: nb_coef : The diagram value is calculated from the nbCoef closest sites.
        :param: coef : The distance to each site is scaled by the corresponding coef.
                       Closest site : coef[0], second closest site : coef[1], ...
        :param: rnd : RNG to use, NULL for default generator.
        """

        tcod.heightmap_add_voronoi(self._data, nb_points, nb_coef, coef, rnd)
        return self

    def add_fbm(self, noise, mulx, muly, addx, addy, octaves, delta, scale):
        tcod.heightmap_add_fbm(self._data, noise, mulx, muly, addx, addy, octaves, delta, scale)
        return self

    def scale_fbm(self, noise, mulx, muly, addxy, addy, octaves, delta, scale):
        if isinstance(noise, Noise):
            noise = noise._noise
        tcod.heightmap_scale_fbm(self._data, noise, mulx, muly, addxy, addy, octaves, delta, scale)
        return self

    def get_minmax(self):
        return tcod.heightmap_get_minmax(self._data)

    def __repr__(self):
        return 'Heightmap({}, {})'.format(self.width, self.height)


