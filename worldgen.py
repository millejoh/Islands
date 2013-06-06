# A direct translation of Jice's worlden tool.
import libtcodpy as tcod
import numpy as np
from itertools import count

### Height and Biome Constants
### --------------------------

biomeDiagram = [ 
    # artic/alpine climate (below -5°C)
    [ 'TUNDRA', 'TUNDRA', 'TUNDRA', 'TUNDRA', 'TUNDRA' ],
    # cold climate (-5 / 5 °C)
    [ 'COLD_DESERT', 'GRASSLAND', 'BOREAL_FOREST', 'BOREAL_FOREST', 'BOREAL_FOREST' ],
    # temperate climate (5 / 15 °C)
    [ 'COLD_DESERT', 'GRASSLAND', 'TEMPERATE_FOREST', 'TEMPERATE_FOREST', 'TROPICAL_MONTANE_FOREST' ],
    # warm climate (15 - 20°C)
    [ 'HOT_DESERT', 'SAVANNA', 'TROPICAL_DRY_FOREST', 'TROPICAL_EVERGREEN_FOREST', 'TROPICAL_EVERGREEN_FOREST' ],
    # tropical climate (above 20 °C)
    [ 'HOT_DESERT', 'THORN_FOREST', 'TROPICAL_DRY_FOREST', 'TROPICAL_EVERGREEN_FOREST', 'TROPICAL_EVERGREEN_FOREST' ] ]

sandHeight  = 0.12
grassHeight = 0.16 #0.315f;
rockHeight  = 0.655
snowHeight  = 0.905 #0.785f;

### TCOD's land color map
### ---------------------

COLOR_KEY_MAX_SEA = int(sandHeight*255)-1
COLOR_KEY_MIN_LAND = int(sandHeight*255)
keyIndex = [ 0,
             COLOR_KEY_MAX_SEA,
             COLOR_KEY_MIN_LAND,
             int(grassHeight*255),
             int(grassHeight*255)+10,
             int(rockHeight*255),
             int(rockHeight*255)+10,
             int(snowHeight*255),
             int(snowHeight*255)+10,
             255 ]

keyColor = [ tcod.Color(0,0,50), # deep water
             tcod.Color(20,20,200), # water-sand transition
             tcod.Color(134,180,101), # sand
             tcod.Color(80,120,10), # sand-grass transition
             tcod.Color(17,109,7), # grass
             tcod.Color(30,85,12), # grass-rock transisiton
             tcod.Color(64,70,20), # rock
             tcod.Color(120,140,40), # rock-snow transisiton
             tcod.Color(208,208,239), # snow
             tcod.Color(255,255,255) ]

### Altitude color map
### ------------------

altIndexes = [ 0,15,(int)(sandHeight*255),(int)(sandHeight*255)+1,
               80,130,195,255 ]
altitudes = [ -2000,-1000,-100,0,500,1000,2500,4000 ]	# in meters
	
altColors = [ tcod.Color(24,165,255), # -2000
              tcod.Color(132,214,255), # -1000
              tcod.Color(247,255,255), # -100
              tcod.Color(49,149,44), # 0
              tcod.Color(249,209,151), # 500
              tcod.Color(165,148,24), # 1000
              tcod.Color(153,110,6), # 2500
              tcod.Color(172,141,138) ] # 4000


### Precipitation color map
### -----------------------
precIndexes = [	4,8,12,16,20,24,28,32,36,40,50,60,70,80,100,120,140,160,255 ]
precipitations = [ 0,1,2,3,4,5,6,7,8,9,10,13,15,18,20,25,30,35,40 ]  # cm / m² / year

precColors = [ tcod.Color(128,0,0), # < 4
               tcod.Color(173,55,0), # 4-8
               tcod.Color(227,102,0), # 8-12
               tcod.Color(255,149,0), # 12-16
               tcod.Color(255,200,0), # 16-20
               tcod.Color(255,251,0), # 20-24
               tcod.Color(191,255,0), # 24-28
               tcod.Color(106,251,0), # 28-32
               tcod.Color(25,255,48), # 32-36
               tcod.Color(48,255,141), # 36-40
               tcod.Color(28,255,232), # 40-50
               tcod.Color(54,181,255), # 50-60
               tcod.Color(41,71,191), # 60-70
               tcod.Color(38,0,255), # 70-80
               tcod.Color(140,0,255), # 80-100
               tcod.Color(221,0,255), # 100-120
               tcod.Color(255,87,255), # 120-140
               tcod.Color(255,173,255), # 140-160
               tcod.Color(255,206,255) ] # > 160

### Temperature color map
### ---------------------

tempIndexes = [ 0,42,84,126,168,210,255 ]
temperatures = [ -30,-20,-10,0,10,20,30 ]
tempKeyColor = [ tcod.Color(180,8,130), # -30 °C
                 tcod.Color(32,1,139), # -20 °C
                 tcod.Color(0,65,252),# -10 °C
                 tcod.Color(37,255,236),# 0 °C
                 tcod.Color(255,255,1), # 10 °C
                 tcod.Color(255,29,4), # 20 °C
                 tcod.Color(80,3,0) ] # 30 °C

def clamp(min, max, val):
    if min > val:
        return min
    elif val > max:
        return max
    else:
        return val

def find_index(indices, val):
    for bound, i in zip(indices, range(len(indices))):
        if val < bound:
            return (i-1, i)
    return (len(indices), len(indices)-1)

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
        return tcod.heightmap_get_interpolated_value(self._data, x, y)

    def normal(self, x, y, waterlevel):
        return tcod.heightmap_get_normal(self._data, x, y, waterlevel)

    def add_hill(self, cx, cy, radius, height):
        tcod.heightmap_add_hill(self._data, cx, cy, radius, height)

    def add_fbm(self, noise, mulx, muly, addx, addy, octaves, delta, scale):
        "Perturb  by adding fbm noise values."
        tcod.heightmap_add_fbm(self._data, noise, mulx, muly, addx, addy, octaves, delta, scale)


class WorldGenerator(object):
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self._hm = Heightmap(width, height)
        self._hm2 = Heightmap(width, height)
        self._hm_precip = Heightmap(width, height)
        self._hm_temperature = Heightmap(width, height)
        self._biome_map = np.zeros((width, height))
        self._clouds = np.zeros((width, height))
        self.wgRng = tcod.random_get_instance()
        self.noise = tcod.noise_new(2, random=self.wgRng)

    def altitude(self, x, y):
        return self._hm[x, y]
        
    def interpolated_altitude(self, x, y):
        return self._hm.interpolated_value(x, y)

    def real_altitude(self, x, y):
        ih = clamp(0, 255, 256*int(self.getInterpolatedAltitude(x,y)))
        (i0, i1) = find_index(altIndexes, ih)

        return altitudes[i0] + (altitudes[i1]-altitudes[i0]) * (ih-altIndexes[i0])/(altIndexes[i1]-altIndexes[i0])

    def precipitation(self, x, y):
        iprec = clamp(0, 255, 256*int(self._hm_precip[x,y])) #tcod.heightmap_get_value(self._hm_precip, x,y)))
        (i0, i1) = find_index(precIndexes, iprec)

        return precipitations[i0] + (precipitations[i1]-precipitations[i0]) * (iprec-precIndexes[i0])/(precIndexes[i1]-precIndexes[i0])

    def temperature(self, x, y):
        return self._hm_temperature[x, y]

    def biome(self, x, y):
        return self._biome_map[int(x), int(y)]

    def interpolated_normal(self, x, y):
        return self._hm2.normal(x, y, sandHeight)

    def on_sea_p(self, x, y):
        return self.interpolated_altitude(x, y) <= sandHeight

    def add_hill(self, cnt, base_radius, radius_var, height):
        for i in range(cnt):
            min_radius = base_radius * (1.0-radius_var)
            max_radius = base_radius * (1.0+radius_var)
            radius = tcod.random_get_float(self.wgRng, min_radius, max_radius)
            xh = tcod.random_get_int(self.wgRng, 0, self.width-1)
            yh = tcod.random_get_int(self.wgRng, 0, self.height-1)
            self._hm.add_hill(float(xh), float(yh), radius, height)
    
    def set_land_mass(self, land_mass, water_level):
        "Ensure that a proportion <land_mass | [0,1]> of the map is above sea level."
        heightcount = np.zeros(256)
        for x in range(self.width):
            for y in range(self.height):
                h = self._hm[x, y]
                ih = int(h*255)
                ih = clamp(0, 255, ih)
                heightcount[ih] += 1

        tcnt = 0
        for i in count(0):
            if tcnt >= self.width*self.height*(1.0-land_mass):
                break
            tcnt += heightcount[i]
        
        new_water_level = i / 255.0
        land_coef = (1.0 - water_level) / (1.0 - new_water_level)
        water_coef = water_level / new_water_level
        for x in range(self.width):
            for y in range(self.height):
                h = self._hm[x, y]
                if h > new_water_level:
                    h = water_level + (h - new_water_level)*land_coef
                else:
                    h = h * water_coef
                self._hm[x, y] = h

    def build_base_map(self):
        self.add_hill(600, 16*self.width/200, 0.7, 0.3)
        self._hm.normalize()
        self._hm.add_fbm(self.noise, 2.20*self.width/400, 2.2*self.width/400, 0, 0, 10.0, 1.0, 2.05)
        self._hm.normalize()
        self._hm2 = self._hm.copy()
        self.set_land_mass(0.6, sandHeight)
        # Fix land/mountain ratio using x^3 curve above sea level
        for x in range(self.width):
            for y in range(self.height):
                h = self._hm[x,y]
                if h >= sandHeight:
                    coef = (h - sandHeight) / (1.0 - sandHeight)
                    h = sandHeight + coef*coef*coef*(1.0 - sandHeight)
                    self._hm[x,y] = h

        f = [0,0]
        for x in range(self.width):
            f[0] = 6.0 * x / self.width
            for y in range(self.height):
                f[1] = 6.0 * y / self.height
                self._clouds[x,y] = 0.5 * (1.0 + 0.8*tcod.noise_get_fbm(self.noise, f, 4.0, tcod.NOISE_SIMPLEX))
        

                                    
           
        


### Tests
### -----

def test_find_index():
    assert findIndex(tempIndexes, 32) == (0, 1)
    assert findIndex(tempIndexes, 120) == (2, 3)
    assert findIndex(tempIndexes, 0) == (0, 1)
    assert findIndex(tempIndexes, 255) == (4, 5)

def test_clamp():
    assert clamp(0, 10, 5) == 5
    assert clamp(0, 10, -1) == 0
    assert clamp(0, 10, 50) == 10

