# A direct translation of Jice's worldgen tool.
#import tcod
#from tcod.tools import Heightmap
import math
import numpy as np
import random
import noise
import Heightmap as hm
from tcod import Color
from itertools import count
from numba import jit

noise1d = noise.NoiseGenerator(1)
noise2d = noise.NoiseGenerator(2)

# Height and Biome Constants
# --------------------------

biome_colors = {'SNOW': Color(248, 248, 248),
                'TUNDRA': Color(221, 221, 187),
                'BARE': Color(187, 187, 187),
                'SCORCHED': Color(153, 153, 153),
                'TAIGA': Color(204, 212, 187),
                'SHRUBLAND': Color(194, 204, 187),
                'GRASSLAND': Color(192, 212, 170),
                'TEMPERATE_DESERT': Color(228, 232, 202),
                'TEMPERATE_RAIN_FOREST': Color(164, 196, 168),
                'TEMPERATE_DECIDUOUS_FOREST': Color(180, 201, 169),
                'TROPICAL_RAIN_FOREST': Color(156, 187, 169),
                'TROPICAL_SEASONAL_FOREST': Color(169, 204, 164),
                'SUBTROPICAL_DESERT': Color(233, 221, 199)}

biomeDiagram = [
    # artic/alpine climate (below -5degC)
    ['TUNDRA', 'TUNDRA', 'TUNDRA', 'TUNDRA', 'TUNDRA'],
    # cold climate (-5 / 5 degC)
    ['COLD_DESERT', 'GRASSLAND', 'BOREAL_FOREST', 'BOREAL_FOREST', 'BOREAL_FOREST'],
    # temperate climate (5 / 15 degC)
    ['COLD_DESERT', 'GRASSLAND', 'TEMPERATE_FOREST', 'TEMPERATE_FOREST', 'TROPICAL_MONTANE_FOREST'],
    # warm climate (15 - 20 degC)
    ['HOT_DESERT', 'SAVANNA', 'TROPICAL_DRY_FOREST', 'TROPICAL_EVERGREEN_FOREST', 'TROPICAL_EVERGREEN_FOREST'],
    # tropical climate (above 20 degC)
    ['HOT_DESERT', 'THORN_FOREST', 'TROPICAL_DRY_FOREST', 'TROPICAL_EVERGREEN_FOREST', 'TROPICAL_EVERGREEN_FOREST']]

SAND_HEIGHT = 0.12
GRASS_HEIGHT = 0.16  # 0.315f;
ROCK_HEIGHT = 0.655
SNOW_HEIGHT = 0.905  # 0.785f;

# TCOD's land color map
# ---------------------

COLOR_KEY_MAX_SEA = round(SAND_HEIGHT * 255) - 1
COLOR_KEY_MIN_LAND = round(SAND_HEIGHT * 255)
keyIndex = [0,
            COLOR_KEY_MAX_SEA,
            COLOR_KEY_MIN_LAND,
            round(GRASS_HEIGHT * 255),
            round(GRASS_HEIGHT * 255) + 10,
            round(ROCK_HEIGHT * 255),
            round(ROCK_HEIGHT * 255) + 10,
            round(SNOW_HEIGHT * 255),
            round(SNOW_HEIGHT * 255) + 10,
            255]

def color_rgba (color, alpha=0):
    return color + (alpha,)

keyrgb_color_int = [(0, 0, 50,),  # deep water
                    (20, 20, 200),  # water-sand transition
                    (134, 180, 101),  # sand
                    (80, 120, 10),  # sand-grass transition
                    (17, 109, 7),  # grass
                    (30, 85, 12),  # grass-rock transisiton
                    (64,  70, 20),  # rock
                    (120, 140, 40),  # rock-snow transisiton
                    (208, 208, 239),  # snow
                    (255, 255, 255)]

# Altitude color map
# ------------------

altIndexes = [0, 15, round(SAND_HEIGHT * 255), round(SAND_HEIGHT * 255) + 1,
              80, 130, 195, 255]
altitudes = [-2000, -1000, -100, 0, 500, 1000, 2500, 4000]  # in meters

altrgb_color_ints = [(24, 165, 255),  # -2000
                     (132, 214, 255),  # -1000
                     (247, 255, 255),  # -100
                     (49, 149, 44),  # 0
                     (249, 209, 151),  # 500
                     (165, 148, 24),  # 1000
                     (153, 110, 6),  # 2500
                     (172, 141, 138)]  # 4000


# Precipitation color map
# -----------------------
precIndexes = [4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 50, 60, 70, 80, 100, 120, 140, 160, 255]
precipitations = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 15, 18, 20, 25, 30, 35, 40]  # cm / m2 / year

precrgb_color_ints = [(128, 0, 0),  # < 4
                      (173, 55, 0),  # 4-8
                      (227, 102, 0),  # 8-12
                      (255, 149, 0),  # 12-16
                      (255, 200, 0),  # 16-20
                      (255, 251, 0),  # 20-24
                      (191, 255, 0),  # 24-28
                      (106, 251, 0),  # 28-32
                      (25, 255, 48),  # 32-36
                      (48, 255, 141),  # 36-40
                      (28, 255, 232),  # 40-50
                      (54, 181, 255),  # 50-60
                      (41, 71, 191),  # 60-70
                      (38, 0, 255),  # 70-80
                      (140, 0, 255),  # 80-100
                      (221, 0, 255),  # 100-120
                      (255, 87, 255),  # 120-140
                      (255, 173, 255),  # 140-160
                      (255, 206, 255)]  # > 160

# Temperature color map
# ---------------------

tempIndexes = [0, 42, 84, 126, 168, 210, 255]
temperatures = [-30, -20, -10, 0, 10, 20, 30]
tempKeyrgb_color_int = [(180, 8, 130),  # -30 degC
                        (32, 1, 139),  # -20 degC
                        (0, 65, 252),  # -10 degC
                        (37, 255, 236),  # 0 degC
                        (255, 255, 1),  # 10 degC
                        (255, 29, 4),  # 20 degC
                        (80, 3, 0)]  # 30 degC

# What are these? They appear just before erode_map
# ---------------

dirx = [0, -1, 0, 1, -1, 1, -1, 0, 1]
diry = [0, -1, -1, -1, 0, 0, 1, 1, 1]
dircoef = [1.0, 1.0 / 1.414, 1.0, 1.0 / 1.414, 1.0, 1.0, 1.0 / 1.414, 1.0, 1.0 / 1.414]
oppdir = [0, 8, 7, 6, 5, 4, 3, 2, 1]


def in_rectangle(x,y,w,h):
    return (x < w) and (y < h)

def sqrdist(x1,y1,x2,y2):
    return (((x1)-(x2))*((x1)-(x2))+((y1)-(y2))*((y1)-(y2)))

def clamp(cmin, cmax, val):
    if cmin > val:
        return cmin
    elif val > cmax:
        return cmax
    else:
        return val


def find_index(indices, val):
    for bound, i in zip(indices, range(len(indices))):
        if val < bound:
            return i - 1, i
    return len(indices), len(indices) - 1


def rand_float_range(fmin, fmax, random=random):
    return (random.random() + fmin/fmax) * fmax

def get_interpolated_float(arr, x, y, width, height):
    wx = clamp(0.0, width-1, x)
    wy = clamp(0.0, height-1, y)
    iwx = round(wx)
    iwy = round(wy)
    dx = wx - iwx
    dy = wy - iwy
    iNW = arr[iwx  , iwy]
    iNE = arr[iwx+1, iwy]   if (iwx < width-1) else iNW
    iSW = arr[iwx  , iwy+1] if (iwy < height-1) else iNW
    iSE = arr[iwx+1, iwy+1] if (iwy < height-1) and (iwx < width-1) else iNW
    iN = (1.0-dx)*iNW + dx*iNE
    iS = (1.0-dx)*iSW + dx*iSE
    return (1.0-dy)*iN + dy * iS


class WorldGenerator(object):
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self._hm = hm.new(width, height) # World Heightmap
        self._hm2 = hm.new(width, height) # World map without erosion
        self._precipitation = hm.new(width, height)
        self._hm_temperature = hm.new(width, height)
        self._biome_map = np.zeros((width, height))
        self._clouds = np.zeros((width, height))
        self.random = random.Random() # Random number generator

    def generate(self, hill_cnt=6):
        # TODO: Add progress indication/messages.
        print("Building heightmap...")
        self.build_base_map(hill_cnt)
        print("Calculating precipitation...")
        self.compute_precipitation()
        print("Eroding...")
        self.erode_map()
        print("Smoothing...")
        self.smooth_map()
        self.set_land_mass(0.6, SAND_HEIGHT)
        print("Generating rivers...")
        for i in range(round(self.width*self.height/3000)):
            self.generate_rivers()
        print("Smoothing precipitations...")
        self.smooth_precipitations()
        print("Determining temperature and biomes...")
        self.compute_temperatures_and_biomes()
        print("Setting colors...")
        self.compute_colors()
        
    def altitude(self, x, y):
        return self._hm[x, y]

    def interpolated_altitude(self, x, y):
        return hm.interpolated_value(self._hm, x, y)

    def real_altitude(self, x, y):
        ih = clamp(0, 255, 256 * round(self.interpolated_altitude(x, y)))
        (i0, i1) = find_index(altIndexes, ih)

        return altitudes[i0] + (altitudes[i1] - altitudes[i0]) * (ih - altIndexes[i0]) / (
            altIndexes[i1] - altIndexes[i0])

    def precipitation(self, x, y):
        iprec = clamp(0, 255, 256 * round(self._precipitation[x, y]))  # tcod.heightmap_get_value(self._precipitation, x,y)))
        (i0, i1) = find_index(precIndexes, iprec)

        return precipitations[i0] + (precipitations[i1] - precipitations[i0]) * (iprec - precIndexes[i0]) / \
            (precIndexes[i1] - precIndexes[i0])

    def temperature(self, x, y):
        return self._hm_temperature[x, y]

    def biome(self, x, y):
        return self._biome_map[round(x), round(y)]

    def interpolated_normal(self, x, y):
        return self._hm2.normal(x, y, SAND_HEIGHT)

    def on_sea_p(self, x, y):
        return self.interpolated_altitude(x, y) <= SAND_HEIGHT

    @jit
    def add_land(self, cnt, base_radius, radius_var, height):
        for i in range(cnt):
            min_radius = base_radius * (1.0 - radius_var)
            max_radius = base_radius * (1.0 + radius_var)
            radius = self.random.randrange(int(min_radius), int(max_radius))
            xh = self.random.randrange(0, self.width)
            yh = self.random.randrange(0, self.height)
            hm.add_hill(self._hm, xh, yh, radius, height)

    @jit
    def set_land_mass(self, land_mass, water_level):
        "Ensure that a proportion <land_mass | [0,1]> of the map is above sea level."
        heightcount = np.zeros(256)
        for x in range(self.width):
            for y in range(self.height):
                h = self._hm[x, y]
                ih = round(h * 255)
                ih = clamp(0, 255, ih)
                heightcount[ih] += 1

        tcnt = 0
        i = 0 
        while tcnt < (self.width * self.height * (1.0 - land_mass)):
            tcnt += heightcount[i]
            i += 1

        new_water_level = i / 255.0
        land_coef = (1.0 - water_level) / (1.0 - new_water_level)
        water_coef = water_level / new_water_level
        for x in range(self.width):
            for y in range(self.height):
                h = self._hm[x, y]
                if h > new_water_level:
                    h = water_level + (h - new_water_level) * land_coef
                else:
                    h = h * water_coef
                self._hm[x, y] = h

    def build_base_map(self, hill_cnt=60):
        self.add_land(hill_cnt, 16 * self.width / 200, 0.7, 0.3)
        self._hm = hm.normalize(self._hm)
        hm.add_fbm(self._hm, noise2d, 2.20 * self.width / 400, 2.2 * self.width / 400, 0, 0, 10, 1.0, 2.05)
        self._hm = hm.normalize(self._hm)
        self._hm2 = self._hm.copy()
        self.set_land_mass(0.6, SAND_HEIGHT)
        # Fix land/mountain ratio using x^3 curve above sea level
        for x in range(self.width):
            for y in range(self.height):
                h = self._hm[x, y]
                if h >= SAND_HEIGHT:
                    coef = (h - SAND_HEIGHT) / (1.0 - SAND_HEIGHT)
                    h = SAND_HEIGHT + coef * coef * coef * (1.0 - SAND_HEIGHT)
                    self._hm[x, y] = h

        f = [0, 0]
        for x in range(self.width):
            f[0] = 6.0 * x / self.width
            for y in range(self.height):
                f[1] = 6.0 * y / self.height
                self._clouds[x, y] = 0.5 * (1.0 + 0.8 * noise2d.get_fbm(f, 4, 'SIMPLEX'))

    def smooth_map(self):
        # 3x3 kernel for smoothing operations
        smooth_ks = 9
        smooth_dx = [-1, 0, 1, -1, 0, 1, -1, 0, 1]
        smooth_dy = [-1, -1, -1, 0, 0, 0, 1, 1, 1]
        smooth_weight = [2, 8, 2, 8, 20, 8, 2, 8, 2]

        hm.kernel_transform(self._hm, smooth_ks, smooth_dx, smooth_dy,
                            smooth_weight, -1000, 1000)
        hm.kernel_transform(self._hm2, smooth_ks, smooth_dx, smooth_dy,
                            smooth_weight, -1000, 1000)
        self._hm = hm.normalize(self._hm)

    def compute_precipitation(self):
        water_add, slope_coef, base_precip = 0.03, 2.0, 0.01
        # // north/south winds
        for dir_y in [-1, 1]:
            for x in range(self.width-1):
                noisex = x*5/self.width
                water_amount = 1.0 + noise1d.get_fbm(noisex, 3, 'SIMPLEX')
                start_y = self.height-1 if dir_y == -1 else 0
                end_y = -1 if dir_y == -1 else self.height
                for y in range(start_y, end_y, dir_y):
                    h = self._hm[x, y]
                    if x < SAND_HEIGHT:
                        water_amount += water_add
                    elif water_amount > 0.0:
                        if (y+dir_y) < self.height:
                            slope = self._hm[x, y+dir_y]-h
                        else:
                            slope = h - self._hm[x, y-dir_y]
                        if slope >= 0:
                            precip = water_amount * (base_precip + slope * slope_coef)
                            self._precipitation[x, y] += precip
                            water_amount -= precip
                            water_amount = max(0.0, water_amount)

        # east/west winds
        for dir_x in [-1, 1]:
            for y in range(self.height-1):
                noisey = y*5/self.height
                water_amount = 1.0 + noise1d.get_fbm(noisey, 3, 'SIMPLEX')
                start_x = self.width-1 if dir_x == -1 else 0
                end_x = -1 if dir_x == -1 else self.width
                for x in range(start_x, end_x, dir_x):
                    h = self._hm[x, y]
                    if x < SAND_HEIGHT:
                        water_amount += water_add
                    elif water_amount > 0.0:
                        if (x+dir_x) < self.height:
                            slope = self._hm[x+dir_x, y]-h
                        else:
                            slope = h - self._hm[x-dir_x, y]
                        if slope >= 0:
                            precip = water_amount * (base_precip + slope * slope_coef)
                            self._precipitation[x, y] += precip
                            water_amount -= precip
                            water_amount = max(0.0, water_amount)

        fmin, fmax = np.amin(self._precipitation), np.amax(self._precipitation)
        # latitude impact
        for y in range(self.height//4, 3*self.height//4):
            lat = y - (self.height/4) * (2/self.height)
            coef = math.sin(2*math.pi*lat)
            #     // latitude (0 : equator, -1/1 : pole)
            for x in range(self.width):
                f = [x/self.width, y/self.height]
                xcoef = coef + 0.5 * noise2d.get_fbm(f, 3, 'SIMPLEX')
                self._precipitation[x,y] += (fmax-fmin)*xcoef*0.1
        # very fast blur by scaling down and up
        factor = 8
        small_width = int((self.width+factor-1)/factor)
        small_height = int((self.height+factor-1)/factor)
        low_res_map = np.zeros((small_width, small_height))
        for x in range(self.width):
            for y in range(self.height):
                v = self._precipitation[x, y]
                ix = x / factor
                iy = y / factor
                low_res_map[ix, iy] += v
        coef = 1.0 / factor
        for x in range(self.width):
            for y in range(self.height):
                 v = get_interpolated_float(low_res_map, x*coef, y*coef, small_width, small_height)
                 self._precipitation[x, y] = v

    def erode_map(self):
        pass

    def generate_rivers(self):
        pass

    def smooth_precipitations(self):
        temphm = self._precipitation.copy()
        for i in range(4, 0, -1):
            for x in range(self.width):
                minx = max((0, x - 2))
                maxx = min((self.width-1, x + 2))
                miny = 0
                maxy = 2
                psum = np.sum(self._precipitation[minx:maxx, miny:maxy])
                pcount = (maxy-miny)*(maxx-minx)
                temphm[x, 0] = psum/pcount
                for y in range(1, self.height):
                    if (y-2) >= 0:
                        psum -= np.sum(self._precipitation[minx:maxx, y-2])
                        pcount -= (maxx-minx)
                    if (y+2) < self.height:
                        psum += np.sum(self._precipitation[minx:maxx, y+2])
                        pcount += (maxx-minx)
                    temphm[x, y] = sum/count
        self._precipitation = self.temphm.copy()
        hm.normalize(self._precipitation)

    def compute_temperatures_and_biomes(self):
        pass

    def biome_color(self, biome, x, y):
        r = biome_colors[biome].r
        g = biome_colors[biome].g
        b = biome_colors[biome].b
        count = 0
        for i in range(4):
            ix = x + random.randint(-10, 10)
            iy = y + random.randint(-10, 10)
            if in_rectangle(ix, iy, self.width, self.height):
                c = biome_colors(self.biome_map[ix, iy])
                r += c.r + random.randint(-10,10)
                g += c.g + random.randint(-10,10)
                b += c.b + random.randint(-10,10)
                count += 1
        r /= count
        g /= count
        b /= count
        r, g, b = clamp(0,255,r), clamp(0, 255, g), clamp(0, 255, b)
        return Color(r, g, b)

    def compute_colors(self):
        pass


# Tests
# -----

def test_find_index():
    assert find_index(tempIndexes, 32) == (0, 1)
    assert find_index(tempIndexes, 120) == (2, 3)
    assert find_index(tempIndexes, 0) == (0, 1)
    assert find_index(tempIndexes, 255) == (4, 5)


def test_clamp():
    assert clamp(0, 10, 5) == 5
    assert clamp(0, 10, -1) == 0
    assert clamp(0, 10, 50) == 10

