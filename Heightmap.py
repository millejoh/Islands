from math import floor, ceil

__author__ = 'millejoh'
import numpy as np


def lerp(c0, c1, dx):
    """Calculated interpolated value between `c0` and `c1` given dx that is
     between 0 and 1.
    """
    return (1.0-dx)*c0 + dx*c1

class Heightmap(object):
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.data = np.zeros((width, height), dtype=np.float64)

    def __getitem__(self, index):
        # Bounds checking?
        x, y = index
        assert x < self.width and y < self.height
        if type(x) is float and type(y) is float:
            return self.interpolated_value(x, y)
        else:
            return self.data[x, y]

    def randomize(self, scale = 1.0, translate = 0.0):
        self.data = (np.random.rand(*self.data.shape) * scale) + translate

    def copy(self):
        nhm = Heightmap(self.width, self.height)
        nhm.data = np.copy(self.data)
        return nhm

    def clear_map(self):
        self.data = np.zeros((self.width, self.height), dtype=np.float64)

    def clamp(self, min, max):
        self.data.clip(min, max)

    def normalize(self, min=0.0, max=1.0):
        """Normalize heightmap values between <min> and <max>.

        The heightmape is translated and scaled so that the lowest
        cell value becomes min and the highest cell value becomes max
        min < max."""
        h_max, h_min = np.amax(self.data), np.amin(self.data)
        m = min - max / (h_min - h_max)
        b = max - h_max * m
        self.data = self.data*m + b

    def lerp(self, hm, coef):
        """Calculate linear interpolation between two maps.

        hm -- Second map.
        coef -- Interpolation coefficient.
        """
        a, b = self.data, hm.data
        return a + (b - a)*coef

    def interpolated_value(self, x, y):
        """Return interpolated height for noninteger coordinates.

        x, y -- Floating point coordinates of map cell."""
        x_imin, x_imax = floor(x), ceil(x)
        y_imin, y_imax = floor(y), ceil(y)
        x_max, y_max = self.data.shape
        dx = x - x_imin
        dy = y - y_imin
        arr = self.data
        if x_imax == x_max:
            x_imin -= 1
        else:
            x_imax += 1
        if y_imax == y_max:
            y_imin -= 1
        else:
            y_imax += 1
        corners = arr[x_imin:x_imax, y_imin:y_imax]
        top = lerp(corners[0,0], corners[0,1], dx)
        bottom = lerp(corners[1,0], corners[1,1], dx)
        return lerp(top, bottom, dy)


    def normal(self, x, y, waterlevel):
        pass

    def add_hill(self, cx, cy, radius, height):
        radius2 = radius*radius
        coef = height / radius2
        minx = max(0,cx-radius)
        maxx = min(self.width, cx+radius)
        miny = max(0, cy-radius)
        maxy = min(self.height, cy+radius)
        for x in range(minx, maxx):
            xdist = (x-cx)*(x-cx)
            for y in range(miny, maxy):
                z = radius2 - xdist - (y-cy)*(y-cy)
                if z > 0.0:
                    self.data[x,y] += z * coef

    def add_fbm(self, noise, mulx, muly, addx, addy, octaves, delta, scale):
        "Perturb  by adding fbm noise values."
        pass

    def mid_point_displacement(self, rng, roughness):
        """Generates a realistic fractal heightmap.

        rng -- Random number generator to use, 0 to use the default generator.
        roughness -- Map roughness (should be between 0.4 and 0.6.

        Generates a realistic fractal heightmap using the
        diamond-square (or random midpoint displacement) algorithm.
        The roughness range should be comprised between 0.4 and
        0.6.
        """
        pass


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
        pass

