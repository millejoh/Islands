import random as rand
import numpy as np
import networkx as nx
import attr
import esper
#import tcod # Replace with bearlibterminal colors
from bearlibterminal.terminal import color_from_argb, color_from_name
from uuid import uuid1, uuid4
from scipy.spatial import Voronoi, voronoi_plot_2d, KDTree

biomes = [['SNOW', 'SNOW', 'SNOW', 'TUNDRA', 'BARE', 'SCORCHED'],
          ['TAIGA', 'TAIGA', 'SHRUBLAND', 'SHRUBLAND', 'TEMPERATE_DESERT', 'TEMPERATE_DESERT'],
          ['TEMPERATE_RAIN_FOREST', 'TEMPERATE_DECIDUOUS_FOREST', 'TEMPERATE_DECIDUOUS_FOREST', 'GRASSLAND',
           'GRASSLAND', 'TEMPERATE_DESERT'],
          ['TROPICAL_RAIN_FOREST', 'TROPICAL_RAIN_FOREST', 'TROPICAL_SEASONAL_FOREST', 'TROPICAL_SEASONAL_FOREST',
           'GRASSLAND', 'SUBTROPICAL_DESERT']]

biome_colors = {'SNOW': color_from_argb(255,248, 248, 248),
                'TUNDRA': color_from_argb(255,221, 221, 187),
                'BARE': color_from_argb(255,187, 187, 187),
                'SCORCHED': color_from_argb(255,153, 153, 153),
                'TAIGA': color_from_argb(255,204, 212, 187),
                'SHRUBLAND': color_from_argb(255,194, 204, 187),
                'GRASSLAND': color_from_argb(255,192, 212, 170),
                'TEMPERATE_DESERT': color_from_argb(255,228, 232, 202),
                'TEMPERATE_RAIN_FOREST': color_from_argb(255,164, 196, 168),
                'TEMPERATE_DECIDUOUS_FOREST': color_from_argb(255,180, 201, 169),
                'TROPICAL_RAIN_FOREST': color_from_argb(255,156, 187, 169),
                'TROPICAL_SEASONAL_FOREST': color_from_argb(255,169, 204, 164),
                'SUBTROPICAL_DESERT': color_from_argb(255,233, 221, 199)}


def relax(pts, n=2):
    for _ in range(n):
        v = Voronoi(pts)
        newpts = []
        for idx in range(len(v.points)):
            pt = v.points[idx,:]
            region = v.regions[v.point_region[idx]]
            if -1 in region:
                newpts.append(pt)
            else:
                vxs = np.asarray([v.vertices[i,:] for i in region])
                vxs[vxs < 0] = 0
                vxs[vxs > 1] = 1
                newpts.append(np.mean(vxs,0))
        pts = newpts
    return np.asarray(pts)


@attr.s
class TerrainFeatures(object):
    idx = attr.ib()
    parent = attr.ib()
    elevation = attr.ib(default=0.0)
    water = attr.ib(default=False)
    ocean = attr.ib(default=False)
    precipitation = attr.ib(default=0.0)
    temperature = attr.ib(default=0.0)
    color = attr.ib(default=0.0)


class PolygonMap(object):
    def __init__(self, width=10.0, height=10.0, feature_cnt=None, relax_iter=4):
        """Create a PolygonMap based on passed parameters.
        :param width: Width, in distance units, of feature graph. Centers and
                      corners will have x values between 0 and ``width``.
        :param height: Height, in distance units, of feature graph. Centers and
                       corners will have y values between 0 and ``width``.
        :param feature_cnt: Number of elevation/terrain 'features', or grid points in the map.
                            Defaults to ``width*height``.
        """
        if not feature_cnt:
            feature_cnt = int(width*height)
        self.width, self.height, = width, height
        seed = np.random.random((feature_cnt, 2))
        self.points = relax(seed, relax_iter)*[width, height]
        voronoi_part = Voronoi(self.points)
        self.vertices = voronoi_part.vertices
        self.dist_map = KDTree(self.points)
        self.centers = nx.Graph()
        self.corners = nx.Graph()
        for p in range(len(voronoi_part.points)):
            pr = voronoi_part.point_region[p]
            f = TerrainFeatures(p, self)
            self.centers.add_node(p, center=self.points[p],
                                  corners=voronoi_part.regions[pr],
                                  terrain_data=f)

        for e in voronoi_part.ridge_points:
            self.centers.add_edge(*e.tolist())

        for c in range(len(voronoi_part.vertices)):
            self.corners.add_node(c, corner=self.vertices)

        for r in voronoi_part.ridge_vertices:
            self.corners.add_edge(*r)

    def terrain(self, x, y, feature_name=None):
        """Return terrain feature data at position (x, y).

        :param x, y: 2-D coordinate.
        :param feature_name: OPTIONAL Specific feature to query, i.e. elevation, temperature, etc."""
        dist, idx = self.dist_map.query([x, y])
        if feature_name is None:
            return self.centers.node[idx]['terrain_data']
        else:
            return self.centers.node[idx]['terrain_data'].__getattribute__(feature_name)

    def set_terrain(self, x, y, feature_name, new_value):
        """Set terrain data at position (x, y) for a given feature.

        :param x, y: 2-D coordinate.
        :param feature_name: The specific terrain feature to set.
        :param new_value: New value for the given terrain feature.
        """
        dist, idx = self.dist_map.query([x, y])
        self.centers.node[idx]['terrain_data'].__setattr__(feature_name, new_value)

    def terrain_to_hm(self, feature_name, x0=None, y0=None, x1=None, y1=None):
        """Convert the data in a given region of the map for a given terrain feature to
        a tcod heighmap object.

        :param feature_name: The terrain feature to convert (see the TerrainFeatures class).
        :param x0, y0, x1, y1: A rectangular region bounded by ([x0, x1], [y0,y1]) to convert.
        """
        x0 = int(x0 or 0)
        y0 = int(y0 or 0)
        x1 = int(x1 or self.width)
        y1 = int(y1 or self.height)
        new_hm = np.zeros((x1-x0), (y1-y0))
        for i in range(x0, x1):
            for j in range(y0, y1):
                new_hm[i, j] = self.terrain(i, j, feature_name)
        return new_hm

    def hm_to_terrain(self, hm, feature_name, x0=None, y0=None, x1=None, y1=None):
        """Update the data for a given terrain feature in a user-specified region from
        the data in a tcod heightmap object.

        :param hm: The tcod heightmap object (see HeightMap class)
        :param feature_name: The terrain feature to convert (see the TerrainFeatures class).
        :param x0, y0, x1, y1: A rectangular region bounded by ([x0, x1], [y0,y1]) to convert.
        """

        x0 = x0 or 0
        y0 = y0 or 0
        x1 = x1 or hm.width
        y1 = y1 or hm.height
        for i in range(x0, x1):
            for j in range(y0, y1):
                self.set_terrain(i, j, feature_name, hm[i,j])

    def generate(self):
        pass

    def __getitem__(self, index):
        if isinstance(index, slice):
            raise TypeError('PolygonMap objects do not support slices. Yet.')
        x, y = index
        return self.terrain(x, y)

class MapDisplay(object):
    def __init__(self, map, unit_distance, grid_density):
        """

        :param grid_density: Number of grids per unit distance. This will result in
                             a grid size of ``grid_density*[width_scale,
                             height_scale]``.
        """
        self._map = map
        self.unit_distance = distance
        self.grid_density= grid_density


@attr.s
class Display(object):
    glyph = attr.ib(default = ' ')
    color = attr.ib(default = color_from_name('white'))
    pos = attr.ib(default = np.zeros(3))
    opos = attr.ib(default=np.zeros(3))

@attr.s
class gEntity(object):  # , pyDatalog.Mixin):
    eclass = attr.ib(default='entity')
    name = attr.ib(default='generic entity')

class DisplayProcess(esper.Processor):
    pass

