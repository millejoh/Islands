# A direct translation of Jice's worldgen tool.
import math
import numpy as np
import random
import noise
import Heightmap as hm
import tcod
from tcod import Color
from itertools import count
from collections import namedtuple
from enum import Enum

noise1d = tcod.noise_new(1) # noise.NoiseGenerator(1)
noise2d = tcod.noise_new(2) # noise.NoiseGenerator(2)

math.pi

# Height and Biome Constants
# --------------------------

class Climate(Enum):
    arctic_alpine = 0
    cold = 1
    temperate = 2
    warm = 3
    tropical = 4


class Biome(Enum):
    snow = 0
    tundra = 1
    bare = 2
    scorched = 3
    taiga = 4
    shrubland = 5
    grassland = 6
    temperate_desert = 7
    temperate_rain_forest = 8
    temperate_deciduous_forest = 9
    tropical_mountain_forest = 10
    tropical_seasonal_forest = 11
    subtropical_desert = 12
    cold_desert = 13
    boreal_forest = 14
    hot_desert = 15
    savanna = 16
    tropical_dry_forest = 17
    tropical_evergreen_forest = 18
    thorn_forest = 19
    tropical_rain_forest = 20
    temperate_forest = 21

biome_colors = {Biome.snow: Color(248, 248, 248),
                Biome.tundra: Color(221, 221, 187),
                Biome.bare: Color(187, 187, 187),
                Biome.scorched: Color(153, 153, 153),
                Biome.taiga: Color(204, 212, 187),
                Biome.shrubland: Color(194, 204, 187),
                Biome.grassland: Color(192, 212, 170),
                Biome.temperate_desert: Color(228, 232, 202),
                Biome.temperate_rain_forest: Color(164, 196, 168),
                Biome.temperate_deciduous_forest: Color(180, 201, 169),
                Biome.tropical_rain_forest: Color(156, 187, 169),
                Biome.tropical_seasonal_forest: Color(169, 204, 164),
                Biome.subtropical_desert: Color(233, 221, 199)}

biome_diagram = [
    # artic/alpine climate (below -5degC)
    [Biome.tundra, Biome.tundra, Biome.tundra, Biome.tundra, Biome.tundra],
    # cold climate (-5 / 5 degC)
    [Biome.cold_desert, Biome.grassland, Biome.boreal_forest, Biome.boreal_forest, Biome.boreal_forest],
    # temperate climate (5 / 15 degC)
    [Biome.cold_desert, Biome.grassland, Biome.temperate_forest, Biome.temperate_forest, Biome.tropical_mountain_forest],
    # warm climate (15 - 20 degC)
    [Biome.hot_desert, Biome.savanna, Biome.tropical_dry_forest, Biome.tropical_evergreen_forest, Biome.tropical_evergreen_forest],
    # tropical climate (above 20 degC)
    [Biome.hot_desert, Biome.thorn_forest, Biome.tropical_dry_forest, Biome.tropical_evergreen_forest, Biome.tropical_evergreen_forest]]

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
    return len(indices)-2, len(indices) - 1


def rand_float_range(fmin, fmax, random=random):
    return (random.random() + fmin/fmax) * fmax


def get_interpolated_float(arr, x, y, width, height):
    wx = clamp(0.0, width-1, x)
    wy = clamp(0.0, height-1, y)
    iwx = int(wx) # Or is round better?
    iwy = int(wy)
    dx = wx - iwx
    dy = wy - iwy
    iNW = arr[iwx  , iwy]
    iNE = arr[iwx+1, iwy]   if (iwx < width-1) else iNW
    iSW = arr[iwx  , iwy+1] if (iwy < height-1) else iNW
    iSE = arr[iwx+1, iwy+1] if (iwy < height-1) and (iwx < width-1) else iNW
    iN = (1.0-dx)*iNW + dx*iNE
    iS = (1.0-dx)*iSW + dx*iSE
    return (1.0-dy)*iN + dy * iS


class MapData(object):
    __slots__ = ['slope', 'area', 'flow_dir', 'up_dir', 'in_flags', 'river_id', 'river_length']
    def __init__(self, slope=0.0, area=0.0, flow_dir=0, up_dir=0, in_flags=0, river_id=0, river_length=0.0):
        self.slope = slope
        self.area = area
        self.flow_dir = flow_dir
        self.up_dir = up_dir
        self.in_flags = in_flags
        self.river_id = river_id
        self.river_length = river_length

class River(object):
    __slots__ = ['coords', 'strength']
    def __init__(self, coords=0, strength=0.0):
        self.coords = 0
        self.strength = strength

class WorldGenerator(object):
    def __init__(self, width, height, erosion_factor=0.01, max_erosion_alt = 0.9,
                 sedimentation_factor=0.01, mudslide_coef=0.4, seed=None):
        self.width = width
        self.height = height
        self._hm = hm.Heightmap(width, height) # World Heightmap
        self._hm2 = hm.Heightmap(width, height) # World map without erosion
        self._precipitation = hm.Heightmap(width, height)
        self._hm_temperature = hm.Heightmap(width, height)
        self._biome_map = np.zeros((width, height))
        self.cloud_dx, self.cloud_tot_dx = 0.0, 0.0
        self._clouds = np.zeros((width, height))
        self.random = random.Random(seed) # Random number generator
        self.erosion_factor = erosion_factor
        self.max_erosion_alt = max_erosion_alt
        self.sedimentation_factor = sedimentation_factor
        self.mudslide_coef = mudslide_coef
        self.noise = tcod.noise_new(2) # noise.NoiseGenerator(2)
        #
        map_data = [MapData() for _ in range(width*height)] #np.full((width, height),dtype=MapData)
        self.map_data = np.array(map_data)
        self.map_data.shape = (width, height)

    def generate(self, hill_cnt=600):
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
        for i in range(self.width*self.height//3000):
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
        ih = clamp(0, 255, 256 * (self.interpolated_altitude(x, y)))
        (i0, i1) = find_index(altIndexes, ih)

        return altitudes[i0] + (altitudes[i1] - altitudes[i0]) * (ih - altIndexes[i0]) / (
            altIndexes[i1] - altIndexes[i0])

    def precipitation(self, x, y):
        iprec = clamp(0, 255, 256 * (self._precipitation[x, y]))  # tcod.heightmap_get_value(self._precipitation, x,y)))
        (i0, i1) = find_index(precIndexes, iprec)

        return precipitations[i0] + (precipitations[i1] - precipitations[i0]) * (iprec - precIndexes[i0]) / \
            (precIndexes[i1] - precIndexes[i0])

    def temperature(self, x, y):
        return self._hm_temperature[x, y]

    def biome(self, x, y):
        return self._biome_map[int(x), int(y)]

    def interpolated_normal(self, x, y):
        return self._hm2.normal(x, y, SAND_HEIGHT)

    def on_sea_p(self, x, y):
        return self.interpolated_altitude(x, y) <= SAND_HEIGHT

    def add_land(self, cnt, base_radius, radius_var, height):
        for i in range(cnt):
            min_radius = base_radius * (1.0 - radius_var)
            max_radius = base_radius * (1.0 + radius_var)
            radius = self.random.randrange(round(min_radius), round(max_radius))
            xh = self.random.randrange(0, self.width)
            yh = self.random.randrange(0, self.height)
            self._hm.add_hill(xh, yh, radius, height)

    def set_land_mass(self, land_mass, water_level):
        "Ensure that a proportion <land_mass | [0,1]> of the map is above sea level."
        heightcount = np.zeros(256)
        for x in range(self.width):
            for y in range(self.height):
                h = self._hm[x, y]
                if (h < 0):
                    h = 0
                ih = int(h * 255)
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
        self.add_land(hill_cnt, 16 * self.width / 200, 0.7, 0.6)
        self._hm.normalize()
        self._hm.add_fbm(noise2d, 2.20 * self.width / 400, 2.2 * self.width / 400, 0, 0, 10, 1.0, 2.05)
        self._hm.normalize()
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
                self._clouds[x, y] = 0.5 * (1.0 + 0.8 * tcod.noise_get_fbm(noise2d, f, 4.0, tcod.NOISE_SIMPLEX))

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
                noisex = [x*5/self.width]
                water_amount = 1.0 + tcod.noise_get_fbm(noise1d, noisex, 3.0, tcod.NOISE_SIMPLEX)
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
                noisey = [y*5/self.height]
                water_amount = 1.0 + tcod.noise_get_fbm(noise1d, noisey, 3.0, tcod.NOISE_SIMPLEX)
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
                xcoef = coef + 0.5 * tcod.noise_get_fbm(noise2d, f, 3.0, tcod.NOISE_SIMPLEX)
                self._precipitation[x,y] += (fmax-fmin)*xcoef*0.1
        # very fast blur by scaling down and up
        factor = 8
        small_width = int((self.width+factor)/factor)
        small_height = int((self.height+factor)/factor)
        low_res_map = np.zeros((small_width, small_height))
        for x in range(self.width):
            for y in range(self.height):
                v = self._precipitation[x, y]
                ix = int(x / factor)
                iy = int(y / factor)
                low_res_map[ix, iy] += v
        coef = 1.0 / factor
        for x in range(self.width):
            for y in range(self.height):
                 v = get_interpolated_float(low_res_map, x*coef, y*coef, small_width, small_height)
                 self._precipitation[x, y] = v

    def erode_map(self):
        new_map = hm.Heightmap(self.width, self.height)
        # Compute flow and slope maps
        for i in range(5, 1, -1):
            for y in range(self.height):
                for x in range(self.width):
                    h = self._hm[x, y]
                    md = self.map_data[x, y]
                    hmin, hmax = h, h
                    min_dir, max_dir = 0, 0
                    for i in range(0, 8):
                        ix = x + dirx[i]
                        iy = y + diry[i]
                        if in_rectangle(ix, iy, self.width, self.height):
                            h2 = self._hm[ix, iy]
                            if h2 < hmin:
                                hmin = h2
                                min_dir = i
                            elif h2 > hmax:
                                hmax = h2
                                max_dir = i
                    md.flow_dir = min_dir
                    md.up_dir = max_dir
                    md.slope = (hmin - h) * dircoef[min_dir] # Is (hmin-h) only once?

            for y in range(self.height):
                for x in range(self.width):
                    md = self.map_data[x,y]
                    md2 = self.map_data[x,y]
                    sediment, end, ix, iy, old_flow = 0, False, x, y, md.flow_dir
                    while not end:
                        h = self._hm[ix, iy]
                        if h < (SAND_HEIGHT - 0.01):
                            break
                        if md2.flow_dir == oppdir[old_flow]:
                            h += self.sedimentation_factor * sediment
                            self._hm[ix, iy] = h
                            end = True
                        else:
                            h += self.precipitation(ix, iy) * self.erosion_factor * md2.slope
                            h = max(h, SAND_HEIGHT)
                            sediment -= md2.slope
                            self._hm[ix, iy] = h
                            old_flow = md2.flow_dir
                            ix += dirx[old_flow]
                            iy += diry[old_flow]
                            md2 = self.map_data[ix,iy]

        # Mudslides and smoothing
        sand_coef = 1 / (1-SAND_HEIGHT)
        for x in range(self.width):
            for y in range(self.height):
                h = self._hm[x, y]
                if (h < SAND_HEIGHT - 0.01) or (h >= self.max_erosion_alt):
                    new_map[x, y] = h
                    continue
                sum_delta1, sum_delta2 = 0.0, 0.0
                nb1, nb2 = 1, 1
                for i in range(1,8):
                    ix = x+dirx[i]
                    iy = y+diry[i]
                    if (in_rectangle(ix, iy, self.width, self.height)):
                        ih = self._hm[ix, iy]
                        if ih < h:
                            # Diagonal neighbors
                            if (i==1) or (i==3) or (i==6) or (i==8):
                                sum_delta1 += (ih - h) * 0.4
                                nb1 += 1
                            else: # Adjacent neighbor
                                sum_delta2 += (ih - h) * 1.6
                                nb2 += 1
                dh = sum_delta1/nb1 + sum_delta2/nb2
                dh *= self.mudslide_coef
                hcoef = (h - SAND_HEIGHT) * sand_coef
                dh *= (1.0-hcoef*hcoef*hcoef) # Smoothing decrease as altitude increases
                new_map[x, y] = h+dh
        self._hm = np.copy(new_map)

    def update_clouds(self, elapsed_time):
        self.cloud_tot_dx += elapsed_time*5
        self.cloud_dx += elapsed_time*5
        if self.cloud_dx >= 1.0:
            cols_to_translate = int(self.cloud_dx)
            self.cloud_dx -= cols_to_translate
            for x in range(cols_to_translate, self.width):
                for y in range(0,self.height):
                    self._clouds[x-cols_to_translate, y] = self._clouds[x,y]
        f = [0,0]
        for x in range(self.width - cols_to_translate, self.width):
            for y in range(0, self.height):
                f[0] = 6.0 * (x+self.cloud_tot_dx) / self.width
                f[1] = 6.0 * y / self.height
                self._clouds[x, y] = 0.5 * (1.0+0.8*tcod.noise_get_fbm(self.noise, f, 4.0, tcod.NOISE_SIMPLEX))

    @property
    def cloud_thickness(self, x, y):
        x += self.cloud_dx
        ix, iy = int(x), int(y)
        ix1 = min([self.width-1, ix+1])
        iy1 = min([self.height-1, iy+1])
        fdx, fdy = x - ix, y - iy
        v1 = self._clouds[ix, iy]
        v2 = self._clouds[ix1, iy1]
        v3 = self._clouds[ix, iy1]
        v4 = self._clouds[ix1, iy1]
        vx1 = (1.0-fdx)*v1 + fdx*v2
        vx2 = (1.0-fdx)*v3 + fdx*v4
        return (1.0-fdy)*vx1 + fdy*vx2

    def generate_rivers(self):
        river_id = 0
        # Source = sx, sy
        # Destination = dx, dy

        # Pick a random point near the coast
        sx = random.randint(0, self.width)
        sy = random.randint(self.height/5, 4*self.height/5)
        h = self._hm[sx, sy]
        while (h < SAND_HEIGHT - 0.02) or ( h >= SAND_HEIGHT):
            sx += 1
            if sx == self.width:
                sx = 0
                sy += 1
                if sy == self.height:
                    sy = 0
            h = self._hm[sx, sy]

        tree, rand_pt = [], []
        tree.insert(0, (sx, sy))
        river_id += 1
        dx, dy = sx, sy
        for i in range(random.randint(50, 200)):
            rx = random.randint(sx-200, sx+200)
            ry = random.randint(sy-200, sy+200)
            rand_pt.insert(0, (rx, ry))
        for (rx, ry) in rand_pt:
            min_dist = 1E10
            best_x, best_y = -1, -1
            for (tx, ty) in tree:
                dist = (tx-rx)*(tx-rx) + (ty-ry)*(ty-ry)
                if dist < min_dist:
                    min_dist = dist
                    best_x, best_y = tx, ty
            tcod.line_init(best_x, best_y, rx, ry)
            len , cx, cy = 3, best_x, best_y
            md = self.map_data[cx, cy]
            if md.river_id == river_id:
                md.river_id = 0
            while len > 0:
                md = self.map_data[cx, cy]
                if md.river_id > 0:
                    break
                h = self._hm[cx, cy]
                if h >= SAND_HEIGHT:
                    md.river_id = river_id
                    self._precipitation[cx, cy] = 1.0
                if cx == 0 or cx == self.width-1 or cy ==0 or cy == self.height-1:
                    len = 0
                else:
                    cx, cy = tcod.line_step()
                    if cx is None or cy is None:
                        len = 0
                len -= 1
            if (cx+cy*self.width != best_x+best_y*self.width):
                tree.insert(0, (cx, cy))

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
                    temphm[x, y] = psum / pcount
        self._precipitation = temphm.copy()
        hm.normalize(self._precipitation)

    def compute_temperatures_and_biomes(self):
        sand_coef = 1.0 / (1.0 - SAND_HEIGHT)
        water_coef = 1.0 / SAND_HEIGHT
        for y in range(self.height):
            lat = (y-self.height/2) * 2 / self.height
            lat_temp = 0.5*(1.0+math.pow(math.sin(math.pi*(lat+0.5)),5))
            lat_temp = math.sqrt(lat_temp) if lat_temp > 0.0 else lat_temp
            lat_temp = -30 + lat_temp*60
            for x in range(self.width):
                h0 = self._hm[x, y]
                h = h0 - SAND_HEIGHT
                h = (h*water_coef) if h < 0.0 else h*sand_coef
                alt_shift = h * -35
                temp = lat_temp + alt_shift
                self._hm_temperature[x, y] = temp
                humid = self._precipitation[x, y]
                climate = self.get_climate_from_temp(temp)
                i_humid = round(humid * 5)
                i_humid = min(4, i_humid)
                self._biome_map[x, y] = biome_diagram[climate][i_humid]




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

if __name__ == "__main__":
    wg = WorldGenerator(400,400)
    wg.generate()
