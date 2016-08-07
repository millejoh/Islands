import numpy as np
import tcod


def test_tcod():
    r = console.RootConsole(80, 60)
    map = MapChunk(tlx=0, tly=0, width=60, height=40, framed=False, map_width=120,
                   map_height=120, view_tlx=0, view_tly=0, title='The Map')
    map.random_island(0, 0, 20, 20)
    map.random_island(30, 5, 140, 40)
    player = gEntity(char='@', name='Player', color='blue')
    map.add_actor(player)
    map.on_update()
    list_view = ListWindow(tlx=15, tly=15, width=20, height=5, title='List Window', framed=True)
    list_view.add_item('An item.', 'An item.')
    list_view.add_item('Another item.', 'Uhuh.')
    list_view.add_item('Keep going.', 'Number.')
    list_view.add_item('Scrolling yet?', 'Scroll')
    list_view.add_item('last one', 'the end.')
    r.run()

class Heightmap(object):
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self._data = tcod.heightmap_new(width, height)

    def __getitem__(self, index):
        # Bounds checking?
        x, y = index
        assert x < self.width and y < self.height
        if type(x) is float and type(y) is float:
            return self.interpolated_value(x, y)
        else:
            return tcod.heightmap_get_value(self._data, x, y)

    def __setitem__(self, index, value):
        x, y = index
        assert x < self.width and y < self.height
        if type(x) is float and type(y) is float:
            x, y = self.interpolated_value(x, y)
        tcod.heightmap_set_value(self._data, x, y, value)

    def copy(self):
        nhm = Heightmap(self.width, self.height)
        tcod.heightmap_copy(self._data, nhm._data)
        return nhm

    def clear_map(self):
        self.data = np.zeros((self.width, self.height), dtype=np.float64)

    def clamp(self, min, max):
        self.data.clip(min, max)

    def normalize(self, min=0.0, max=1.0):
        """Normalize heightmap values between <min> and <max>.

        The heightmap is translated and scaled so that the lowest
        cell value becomes min and the highest cell value becomes max
        min < max."""
        # max(data)*m+b = max
        # max-max(data)*m = b
        # -
        # min(data)*m+b = min
        # min(data)*m+max-max(data)*m=min
        # m*(min(data)-max(data)) = min-max
        # m = (min-max)/(min(data)-max(data))
        tcod.heightmap_normalize(self._data, min, max)

    def lerp(self, hm2, hm_result, coef):
        return tcod.heightmap_lerp_hm(self._data, self._data, hm_result, coef)

    def interpolated_value(self, x, y):
        """Return interpolated height for non-integer coordinates.

        x, y -- Coordinates of map cell."""
        return tcod.heightmap_get_interpolated_value(self._data, x, y)

    def normal(self, x, y, waterlevel):
        return tcod.heightmap_get_normal(self._data, x, y, waterlevel)

    def add_hill(self, cx, cy, radius, height):
        tcod.heightmap_add_hill(self._data, cx, cy, radius, height)

    def add_fbm(self, noise, mulx, muly, addx, addy, octaves, delta, scale):
        "Perturb  by adding fbm noise values."
        tcod.heightmap_add_fbm(self._data, noise, mulx, muly, addx, addy, octaves, delta, scale)

    def mid_point_displacement(self, rng, roughness):
        """Generates a realistic fractal heightmap.

        rng -- Random number generator to use, 0 to use the default generator.
        roughness -- Map roughness (should be between 0.4 and 0.6.

        Generates a realistic fractal heightmap using the
        diamond-square (or random midpoint displacement) algorithm.
        The roughness range should be comprised between 0.4 and
        0.6.
        """
        tcod.heightmap_mid_point_displacement(self._data, rng, roughness)


    def kernel_transform(self, kernel_size, dx, dy, weight, min_level, max_level):
        """Apply a generic transformation to the heightmap.

        This function allows you to apply a generic transformation on
        the map, so that each resulting cell value is the weighted sum of
        several neighbour cells. This can be used to smooth/sharpen the
        map. 

        kernel_size = Size of transformation.  

        dx, dy = Array of kernel_size coordinates. The coordinates are
                 relative to the current cell (0,0) is current cell,
                 (-1,0) is west cell, (0,-1) is north cell, (1,0) is
                 east cell, (0,1) is south cell, ...

        weight = Array of kernel_size weights. The value of each
                 neighbour cell is scaled by its corresponding weight.

        min_level = The transformation is only applied to cells which
                    value is >= minLevel.

        max_level = The transformation is only applied to cells which
                    value is <= maxLevel."""
        tcod.heightmap_kernel_transform(self._data, kernel_size, dx, dy, weight,
                                        min_level, max_level)
    def as_ndarray(self):
        """Return an ndarray of holding the contents of the Heightmap.

        :return: Numpy ndarray representation of the tcod Heightmap.
        """
        w, h = self.width, self.height
        ndarray = np.zeros((self.width,self.height),dtype=float)
        for i in range(w):
            for j in range(h):
                ndarray[i,j] = self[i,j]
        return ndarray
