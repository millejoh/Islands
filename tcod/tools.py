import tcod

class Heightmap(object):
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self._data = tcod.heightmap_new(width, height)

    def __del__(self):
        tcod.heightmap_delete(self._data)

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
        tcod.heightmap_set_value(self._data, x, y, value)

    def __add__(self, value):
        if isinstance(value, Heightmap):
            tcod.heightmap_add_hm(self._data, value, self._data)
        else:
            tcod.heightmap_add(self._data, value)
        return self

    def __mul__(self, value):
        if isinstance(value, Heightmap):
            tcod.heightmap_multiply_hm_hm(self._data, value, self._data)
        else:
            tcod.heightmap_scale(self._data, value)
        return self

    def __sub__(self, value):
        tcod.heightmap_add(self._data, -value)
        return self

    def copy(self):
        nhm = Heightmap(self.width, self.height)
        tcod.heightmap_copy(self._data, nhm._data)
        return nhm

    def clear_map(self):
        tcod.heightmap_clear(self._data)

    def clamp(self, min, max):
        tcod.heightmap_clamp(self._data, min. max)

    def normalize(self, min=0.0, max=1.0):
        tcod.heightmap_normalize(self._data, min, max)

    def lerp(self, hm2, hm_result, coef):
        return tcod.heightmap_lerp_hm(self._data, self._data, hm_result, coef)

    def interpolated_value(self, x, y):
        """Return interpolated height for noninteger coordinates.

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

