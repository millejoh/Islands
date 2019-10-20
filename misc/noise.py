from collections import namedtuple
from math import sqrt, floor
import numpy as np
#from numba import jit

__author__ = 'millejoh'

DEFAULT_HURST = 0.5
DEFAULT_LACUNARITY = 2.0

NOISE_MAX_OCTAVES = 128
SIMPLEX_SCALE = 0.5

DELTA = 1E-6

WAVELET_TILE_SIZE = 32
WAVELET_ARAD = 16
WAVELET_SCALE = 2.0


def cubic(x):
    return x * x * (3 - 2 * x)


def lerp(c0, c1, dx):
    """Calculated interpolated value between `c0` and `c1` given dx that is
     between 0 and 1.
    """
    return (1.0 - dx) * c0 + dx * c1


def clamp(min, max, val):
    return min if val < min else max if val > max else val

# simplex noise, adapted from libtcod, which itself is adapted from
# Ken Perlin's presentation at Siggraph 2001
# and Stefan Gustavson implementation
simplex = np.array([
    [0, 1, 2, 3], [0, 1, 3, 2], [0, 0, 0, 0], [0, 2, 3, 1], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [1, 2, 3, 0],
    [0, 2, 1, 3], [0, 0, 0, 0], [0, 3, 1, 2], [0, 3, 2, 1], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [1, 3, 2, 0],
    [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0],
    [1, 2, 0, 3], [0, 0, 0, 0], [1, 3, 0, 2], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [2, 3, 0, 1], [2, 3, 1, 0],
    [1, 0, 2, 3], [1, 0, 3, 2], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [2, 0, 3, 1], [0, 0, 0, 0], [2, 1, 3, 0],
    [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0],
    [2, 0, 1, 3], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [3, 0, 1, 2], [3, 0, 2, 1], [0, 0, 0, 0], [3, 1, 2, 0],
    [2, 1, 0, 3], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [3, 1, 0, 2], [0, 0, 0, 0], [3, 2, 0, 1], [3, 2, 1, 0]
])


def simplex_gradient_1d(h, x):
    h &= 0xF
    grad = 1.0 + (h & 7)
    if ( h & 8 ):
        grad = -grad
    return grad * x


def simplex_gradient_2d(h, x, y):
    h &= 0x7
    if h < 4:
        u = x
        v = 2.0 * y
    else:
        u = y
        v = 2.0 * x
    return ((h & 1) if u < 0 else u) + ((h & 2) if v < 0 else v)


def simplex_gradient_3d(h, x, y, z):
    h &= 0xF
    u = (x if h < 8 else y)
    v = (y if h < 4 else (x if h == 12 or h == 14 else z))

    return (-u if (h & 1) else u) + (-v if (h & 2) else v)


def simplex_gradient_4d(h, x, y, z, t):
    h &= 0x1F
    u = x if h < 24 else y
    v = y if h < 16 else z
    w = z if h < 8 else t
    return (-u if (h & 1) else u ) + (-v if (h & 2) else v) + (-w if (h & 4) else w)


# wavelet noise, adapted libtcod which is adapted from Robert L. Cook and Tony Derose 'Wavelet noise' paper
acoeffs = np.array([
    0.000334, -0.001528, 0.000410, 0.003545, -0.000938, -0.008233, 0.002172, 0.019120,
    -0.005040, -0.044412, 0.011655, 0.103311, -0.025936, -0.243780, 0.033979, 0.655340,
    0.655340, 0.033979, -0.243780, -0.025936, 0.103311, 0.011655, -0.044412, -0.005040,
    0.019120, 0.002172, -0.008233, -0.000938, 0.003546, 0.000410, -0.001528, 0.000334,
])


def absmod(x, n):
    m = x % n
    return m + n if m < 0 else m


def noise_wavelet_downsample(wav_from, wav_to, stride):
    for i in range(WAVELET_TILE_SIZE / 2):
        wav_to[i * stride] = 0
        for k in range(2 * i - WAVELET_ARAD, 2 * i + WAVELET_ARAD):
            wav_to[i * stride] += acoeffs[k - 2 * i] * wav_from[absmod(k, WAVELET_TILE_SIZE) * stride]


class NoiseGenerator(object):
    def __init__(self, dimensions, lacunarity=DEFAULT_LACUNARITY, hurst=DEFAULT_HURST, random=np.random.random, noise_type='PERLIN'):
        """

        @param dimensions:
        @param lacunarity:
        @param hurst:
        @param random:
        """
        self.noise_type = noise_type
        self.ndim = dimensions
        self.lacunarity = lacunarity
        self.hurst = hurst
        self.rng = random
        self.map = np.arange(256)
        self.buffer = np.zeros((256, dimensions))
        for i in range(256):
            self.buffer[i, :] = self.normalize(self.rng(self.ndim) - 0.5)
        np.random.shuffle(self.map)
        self.exponent = np.zeros(NOISE_MAX_OCTAVES)
        f = 1.0
        for i in range(NOISE_MAX_OCTAVES):
            self.exponent[i] = 1.0 / f
            f *= self.lacunarity

    def normalize(self, f):
        magnitude = 0
        for i in range(self.ndim):
            magnitude += f[i] * f[i]
        magnitude = 1.0 / sqrt(magnitude)
        for i in range(self.ndim):
            f[i] *= magnitude
        return f

    # @jit
    def lattice(self, ix, fx, iy, fy, iz, fz, iw, fw):
        n = np.array([ix, iy, iz, iw])
        f = np.array([fx, fy, fz, fw])
        n_index = 0
        value = 0
        for i in range(self.ndim):
            n_index = self.map[(n_index + floor(n[i])) & 0xFF]
        for i in range(self.ndim):
            value += self.buffer[n_index, i] * f[i]
        return value

    def perlin_noise(self, f):
        n = np.floor(f)
        r = f - n
        w = cubic(r)
        if self.ndim == 1:
            return clamp(-0.99999, 0.99999, self.perlin_noise_1d(n, r, w))
        elif self.ndim == 2:
            return clamp(-0.99999, 0.99999, self.perlin_noise_2d(n, r, w))
        elif self.ndim == 3:
            return clamp(-0.99999, 0.99999, self.perlin_noise_3d(n, r, w))
        elif self.ndim == 4:
            return clamp(-0.99999, 0.99999, self.perlin_noise_4d(n, r, w))

    def perlin_noise_1d(self, n, r, w):
        return lerp(self.lattice(n[0], r[0], 0, 0, 0, 0, 0, 0),
                    self.lattice(n[0] + 1, r[0] - 1, 0, 0, 0, 0, 0, 0),
                    w[0])

    def perlin_noise_2d(self, n, r, w):
        return lerp(lerp(self.lattice(n[0], r[0], n[1], r[1], 0, 0, 0, 0),
                         self.lattice(n[0] + 1, r[0] - 1, n[1], r[1], 0, 0, 0, 0),
                         w[0]),
                    lerp(self.lattice(n[0], r[0], n[1] + 1, r[1] - 1, 0, 0, 0, 0),
                         self.lattice(n[0] + 1, r[0] - 1, n[1] + 1, r[1] - 1, 0, 0, 0, 0),
                         w[0]),
                    w[1])

    def perlin_noise_3d(self, n, r, w):
        return lerp(lerp(lerp(self.lattice(n[0], r[0], n[1], r[1], n[2], r[2], 0, 0),
                              self.lattice(n[0] + 1, r[0] - 1, n[1], r[1], n[2], r[2], 0, 0),
                              w[0]),
                         lerp(self.lattice(n[0], r[0], n[1] + 1, r[1] - 1, n[2], r[2], 0, 0),
                              self.lattice(n[0] + 1, r[0] - 1, n[1] + 1, r[1] - 1, n[2], r[2], 0, 0),
                              w[0]),
                         w[1]),
                    lerp(lerp(self.lattice(n[0], r[0], n[1], r[1], n[2] + 1, r[2] - 1, 0, 0),
                              self.lattice(n[0] + 1, r[0] - 1, n[1], r[1], n[2] + 1, r[2] - 1, 0, 0),
                              w[0]),
                         lerp(self.lattice(n[0], r[0], n[1] + 1, r[1] - 1, n[2] + 1, r[2] - 1, 0, 0),
                              self.lattice(n[0] + 1, r[0] - 1, n[1] + 1, r[1] - 1, n[2] + 1, r[2] - 1, 0, 0),
                              w[0]),
                         w[1]),
                    w[2])

    def perlin_noise_4d(self, n, r, w):
        return lerp(lerp(lerp(lerp(self.lattice(n[0], r[0], n[1], r[1], n[2], r[2], n[3], r[3]),
                                   self.lattice(n[0] + 1, r[0] - 1, n[1], r[1], n[2], r[2], n[3], r[3]),
                                   w[0]),
                              lerp(self.lattice(n[0], r[0], n[1] + 1, r[1] - 1, n[2], r[2], n[3], r[3]),
                                   self.lattice(n[0] + 1, r[0] - 1, n[1] + 1, r[1] - 1, n[2], r[2], n[3], r[3]),
                                   w[0]),
                              w[1]),
                         lerp(lerp(self.lattice(n[0], r[0], n[1], r[1], n[2] + 1, r[2] - 1, n[3], r[3]),
                                   self.lattice(n[0] + 1, r[0] - 1, n[1], r[1], n[2] + 1, r[2] - 1, n[3], r[3]),
                                   w[0]),
                              lerp(self.lattice(n[0], r[0], n[1] + 1, r[1] - 1, n[2] + 1, r[2] - 1, 0, 0),
                                   self.lattice(n[0] + 1, r[0] - 1, n[1] + 1, r[1] - 1, n[2] + 1, r[2] - 1, n[3],
                                                r[3]),
                                   w[0]),
                              w[1]),
                         w[2]),
                    lerp(lerp(lerp(self.lattice(n[0], r[0], n[1], r[1], n[2], r[2], n[3] + 1, r[3] - 1),
                                   self.lattice(n[0] + 1, r[0] - 1, n[1], r[1], n[2], r[2], n[3] + 1, r[3] - 1),
                                   w[0]),
                              lerp(self.lattice(n[0], r[0], n[1] + 1, r[1] - 1, n[2], r[2], n[3] + 1, r[3] - 1),
                                   self.lattice(n[0] + 1, r[0] - 1, n[1] + 1, r[1] - 1, n[2], r[2], n[3] + 1,
                                                r[3] - 1),
                                   w[0]),
                              w[1]),
                         lerp(lerp(self.lattice(n[0], r[0], n[1], r[1], n[2] + 1, r[2] - 1, n[3] + 1, r[3] - 1),
                                   self.lattice(n[0] + 1, r[0] - 1, n[1], r[1], n[2] + 1, r[2] - 1, n[3] + 1,
                                                r[3] - 1),
                                   w[0]),
                              lerp(self.lattice(n[0], r[0], n[1] + 1, r[1] - 1, n[2] + 1, r[2] - 1, 0, 0),
                                   self.lattice(n[0] + 1, r[0] - 1, n[1] + 1, r[1] - 1, n[2] + 1, r[2] - 1,
                                                n[3] + 1,
                                                r[3] - 1),
                                   w[0]),
                              w[1]),
                         w[2]),
                    w[3])

    def simplex_noise(self, f):
        if self.ndim == 1:
            return self.simplex_noise_1d(f)
        if self.ndim == 2:
            return self.simplex_noise_2d(f)
        if self.ndim == 3:
            return self.simplex_noise_3d(f)
        if self.ndim == 4:
            return self.simplex_noise_4d(f)

    def simplex_noise_1d(self, f):
        i0 = floor(f * SIMPLEX_SCALE)
        i1 = i0 + 1
        x0 = f * SIMPLEX_SCALE - i0
        x1 = x0 - 1.0
        t0 = 1.0 - x0 * x0
        t1 = 1.0 - x1 * x1
        t0 *= t0
        t1 *= t1
        i0 = self.map[i0 & 0xFF]
        n0 = simplex_gradient_1d(i0, x0)
        n0 *= t0 * t0
        i1 = self.map[i1 & 0xFF]
        n1 = simplex_gradient_1d(i1, x1)
        n1 *= t1 * t1
        return 0.25 * (n0 + n1)

    def simplex_noise_2d(self, f):
        F2 = 0.366025403  # 0.5 * (sqrt(3.0)-1.0)
        g2 = 0.211324865  # (3.0 - sqrt(3.0)/6.0

        s = (f[0] + f[1]) * F2 * SIMPLEX_SCALE
        xs = f[0] * SIMPLEX_SCALE + s
        ys = f[1] * SIMPLEX_SCALE + s
        i = floor(xs)
        j = floor(ys)
        t = (i + j) * g2
        xo = i - t
        yo = j - t
        x0 = f[0] * SIMPLEX_SCALE - xo
        y0 = f[1] * SIMPLEX_SCALE - yo
        ii = i % 256
        jj = j % 256
        if x0 > y0:
            i1 = 1
            j1 = 0
        else:
            i1 = 0
            j1 = 1
        x1 = x0 - i1 + g2
        y1 = y0 - j1 + g2
        x2 = x0 - 1.0 + 2.0 * g2
        y2 = y0 - 1.0 + 2.0 * g2
        t0 = 0.5 - x0 * x0 - y0 * y0

        if t0 < 0.0:
            n0 = 0.0
        else:
            idx = (ii + self.map[jj]) & 0xFF
            t0 *= t0
            idx = self.map[idx]
            n0 = simplex_gradient_2d(idx, x0, y0)
            n0 *= t0 * t0

        t1 = 0.5 - x1 * x1 - y1 * y1

        if t1 < 0.0:
            n1 = 0.0
        else:
            idx = (ii + i1 + self.map[(jj + j1) & 0xFF]) & 0xFF
            t1 *= t1
            idx = self.map[idx]
            n1 = simplex_gradient_2d(idx, x1, y1)
            n1 *= t1 * t1

        t2 = 0.5 - x2 * x2 - y2 * y2

        if t2 < 0.0:
            n2 = 0.0
        else:
            idx = (ii + 1 + self.map[(jj + 1) & 0xFF]) & 0xFF
            t2 *= t2
            idx = self.map[idx]  # Redundant?
            n2 = simplex_gradient_2d(idx, x2, y2)
            n2 *= t2 * t2

        return 40.0 * (n0 + n1 + n2)

    def simplex_noise_3d(self, f):
        F3 = 0.333333333
        G3 = 0.166666667
        s = (f[0] + f[1] + f[2]) * F3 * SIMPLEX_SCALE
        xs = f[0] * SIMPLEX_SCALE + s
        ys = f[1] * SIMPLEX_SCALE + s
        zs = f[2] * SIMPLEX_SCALE + s
        i = floor(xs)
        j = floor(ys)
        k = floor(zs)
        t = (i + j + k) * G3
        xo = i - t
        yo = j - t
        zo = k - t
        x0 = f[0] * SIMPLEX_SCALE - xo
        y0 = f[1] * SIMPLEX_SCALE - yo
        z0 = f[2] * SIMPLEX_SCALE - zo

        if x0 >= y0:
            if y0 >= z0:
                i1, j1, k1, i2, j2, k2 = 1, 0, 0, 1, 1, 0
            elif x0 >= z0:
                i1, j1, k1, i2, j2, k2 = 1, 0, 0, 1, 0, 1
            else:
                i1, j1, k1, i2, j2, k2 = 0, 0, 1, 1, 0, 1
        else:
            if y0 < z0:
                i1, j1, k1, i2, j2, k2 = 0, 0, 1, 0, 1, 1
            elif x0 < z0:
                i1, j1, k1, i2, j2, k2 = 0, 1, 0, 0, 1, 1
            else:
                i1, j1, k1, i2, j2, k2 = 0, 1, 0, 1, 1, 0

        x1 = x0 - i1 + G3
        y1 = y0 - j1 + G3
        z1 = z0 - k1 + G3
        x2 = x0 - i2 + 2.0 * G3
        y2 = y0 - j2 + 2.0 * G3
        z2 = z0 - k2 + 2.0 * G3
        x3 = x0 - 1.0 + 3.0 * G3
        y3 = y0 - 1.0 + 3.0 * G3
        z3 = z0 - 1.0 + 3.0 * G3
        ii = i % 256
        jj = j % 256
        kk = k % 256
        t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0

        if t0 < 0.0:
            n0 = 0.0
        else:
            idx = self.map[(ii + self.map[(jj + self.map[kk]) & 0xFF]) & 0xFF]
            t0 *= t0
            n0 = simplex_gradient_3d(idx, x0, y0, z0)
            n0 *= t0 * t0

        t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1

        if t1 < 0.0:
            n1 = 0.0
        else:
            idx = self.map[(ii + i1 + self.map[(jj + j1 + self.map[(kk + k1) & 0xFF]) & 0xFF]) & 0xFF]
            t1 *= t1
            n1 = simplex_gradient_3d(idx, x1, y1, z1)
            n1 *= t1 * t1

        t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2

        if t2 < 0.0:
            n2 = 0.0
        else:
            idx = self.map[(ii + i2 + self.map[(jj + j2 + self.map[(kk + k2) & 0xFF]) & 0xFF]) & 0xFF]
            t2 *= t2
            n2 = simplex_gradient_3d(idx, x2, y2, z2)
            n2 *= t2 * t2

        t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3

        if t3 < 0.0:
            n3 = 0.0
        else:
            idx = self.map[(ii + 1 + self.map[(jj + 1 + self.map[(kk + 1) & 0xFF]) & 0xFF]) & 0xFF]
            t3 *= t3
            n3 = simplex_gradient_3d(idx, x3, y3, z3)
            n3 *= t3 * t3

        return 32.0 * (n0 + n1 + n2 + n3)

    def simplex_noise_4d(self, f):
        F4 = 0.309016994  # (sqrtf(5.0f)-1.0f)/4.0f
        G4 = 0.138196601  # (5.0f - sqrtf(5.0f))/20.0f

        s = (f[0] + f[1] + f[2] + f[3]) * F4 * SIMPLEX_SCALE
        xs = f[0] * SIMPLEX_SCALE + s
        ys = f[1] * SIMPLEX_SCALE + s
        zs = f[2] * SIMPLEX_SCALE + s
        ws = f[3] * SIMPLEX_SCALE + s
        i = floor(xs)
        j = floor(ys)
        k = floor(zs)
        l = floor(ws)
        t = (i + j + k + l) * G4
        xo = i - t
        yo = j - t
        zo = k - t
        wo = l - t
        x0 = f[0] * SIMPLEX_SCALE - xo
        y0 = f[1] * SIMPLEX_SCALE - yo
        z0 = f[2] * SIMPLEX_SCALE - zo
        w0 = f[3] * SIMPLEX_SCALE - wo
        c1 = 32 if x0 > y0 else 0
        c2 = 16 if x0 > z0 else 0
        c3 = 8 if y0 > z0 else 0
        c4 = 4 if x0 > w0 else 0
        c5 = 2 if y0 > w0 else 0
        c6 = 1 if z0 > w0 else 0
        c = c1 + c2 + c3 + c4 + c5 + c6

        i1 = 1 if simplex[c][0] >= 3 else 0
        j1 = 1 if simplex[c][1] >= 3 else 0
        k1 = 1 if simplex[c][2] >= 3 else 0
        l1 = 1 if simplex[c][3] >= 3 else 0

        i2 = 1 if simplex[c][0] >= 2 else 0
        j2 = 1 if simplex[c][1] >= 2 else 0
        k2 = 1 if simplex[c][2] >= 2 else 0
        l2 = 1 if simplex[c][3] >= 2 else 0

        i3 = 1 if simplex[c][0] >= 1 else 0
        j3 = 1 if simplex[c][1] >= 1 else 0
        k3 = 1 if simplex[c][2] >= 1 else 0
        l3 = 1 if simplex[c][3] >= 1 else 0

        x1 = x0 - i1 + G4
        y1 = y0 - j1 + G4
        z1 = z0 - k1 + G4
        w1 = w0 - l1 + G4
        x2 = x0 - i2 + 2.0 * G4
        y2 = y0 - j2 + 2.0 * G4
        z2 = z0 - k2 + 2.0 * G4
        w2 = w0 - l2 + 2.0 * G4
        x3 = x0 - i3 + 3.0 * G4
        y3 = y0 - j3 + 3.0 * G4
        z3 = z0 - k3 + 3.0 * G4
        w3 = w0 - l3 + 3.0 * G4
        x4 = x0 - 1.0 + 4.0 * G4
        y4 = y0 - 1.0 + 4.0 * G4
        z4 = z0 - 1.0 + 4.0 * G4
        w4 = w0 - 1.0 + 4.0 * G4

        ii = i % 256
        jj = j % 256
        kk = k % 256
        ll = l % 256

        t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0 - w0 * w0

        if t0 < 0.0:
            n0 = 0.0
        else:
            idx = self.map[(ii + self.map[(jj + self.map[(kk + self.map[ll] ) & 0xFF]) & 0xFF]) & 0xFF]
            t0 *= t0
            n0 = simplex_gradient_4d(idx, x0, y0, z0, w0)
            n0 *= t0 * t0

        t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1 - w1 * w1

        if t1 < 0.0:
            n1 = 0.0
        else:
            idx = self.map[
                (ii + i1 + self.map[(jj + j1 + self.map[(kk + k1 + self.map[(ll + l1) & 0xFF]) & 0xFF]) & 0xFF]) & 0xFF]
            t1 *= t1
            n1 = simplex_gradient_4d(idx, x1, y1, z2, w1)
            n1 *= t1 * t1

        t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2 - w2 * w2

        if t2 < 0.0:
            n2 = 0.0
        else:
            idx = self.map[(ii + i2 + self.map[
                (jj + j2 + self.map[(kk + k2 + self.map[(ll + l2) & 0xFF]) & 0xFF]) & 0xFF]) & 0xFF]
            t2 *= t2
            n2 = simplex_gradient_4d(idx, x2, y2, z2, w2)
            n2 *= t2 * t2

        t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3 - w3 * w3

        if t3 < 0.0:
            n3 = 0.0
        else:
            idx = self.map[(ii + i3 + self.map[
                (jj + j3 + self.map[(kk + k3 + self.map[(ll + l3) & 0xFF]) & 0xFF]) & 0xFF]) & 0xFF]
            t3 *= t3
            n3 = simplex_gradient_4d(idx, x3, y3, z3, w3)
            n3 *= t3 * t3

        t4 = 0.6 - x4 * x4 - y4 * y4 - z4 * z4 - w4 * w4

        if t4 < 0.0:
            n4 = 0.0
        else:
            idx = self.map[
                (ii + 1 + self.map[(jj + 1 + self.map[(kk + 1 + self.map[(ll + 1) & 0xFF]) & 0xFF]) & 0xFF]) & 0xFF]
            t4 *= t4
            n4 = simplex_gradient_4d(idx, x4, y4, z4, w4)
            n4 *= t4 * t4

        return 27.0 * (n0 + n1 + n2 + n3 + n4)

    def noise_fbm_int(self, f, octaves, func):
        tf = np.array(f)
        value = 0
        for i in range(octaves):
            value += func(tf) * self.exponent[i]
            tf *= self.lacunarity
        octaves -= floor(octaves)
        if octaves > DELTA:
            value += octaves * func(tf) * self.exponent[i]
        return clamp(-0.99999, 0.99999, value)

    def noise_fbm_perlin(self, f, octaves):
        return self.noise_fbm_int(f, octaves, self.perlin_noise)

    def noise_fbm_simplex(self, f, octaves):
        return self.noise_fbm_int(f, octaves, self.simplex_noise)

    def get_fbm(self, coord, octaves, type=None):
        if type is None:
            type = self.noise_type
        if type == 'PERLIN':
            return self.noise_fbm_perlin(coord, octaves)
        elif type == 'SIMPLEX':
            return self.noise_fbm_simplex(coord, octaves)
        elif type == 'WAVELET':
            return self.noise_fbm_wavelet(coord, octaves)
