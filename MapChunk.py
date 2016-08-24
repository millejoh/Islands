# Try to find libtcodpy
import random as rand
import numpy as np
import networkx as nx
import tcod
import attr
from uuid import uuid1
from scipy.spatial import Voronoi, KDTree


biomes = [['SNOW', 'SNOW', 'SNOW', 'TUNDRA', 'BARE', 'SCORCHED'],
          ['TAIGA', 'TAIGA', 'SHRUBLAND', 'SHRUBLAND', 'TEMPERATE_DESERT', 'TEMPERATE_DESERT'],
          ['TEMPERATE_RAIN_FOREST', 'TEMPERATE_DECIDUOUS_FOREST', 'TEMPERATE_DECIDUOUS_FOREST', 'GRASSLAND',
           'GRASSLAND', 'TEMPERATE_DESERT'],
          ['TROPICAL_RAIN_FOREST', 'TROPICAL_RAIN_FOREST', 'TROPICAL_SEASONAL_FOREST', 'TROPICAL_SEASONAL_FOREST',
           'GRASSLAND', 'SUBTROPICAL_DESERT']]

biome_colors = {'SNOW': tcod.Color(248, 248, 248),
                'TUNDRA': tcod.Color(221, 221, 187),
                'BARE': tcod.Color(187, 187, 187),
                'SCORCHED': tcod.Color(153, 153, 153),
                'TAIGA': tcod.Color(204, 212, 187),
                'SHRUBLAND': tcod.Color(194, 204, 187),
                'GRASSLAND': tcod.Color(192, 212, 170),
                'TEMPERATE_DESERT': tcod.Color(228, 232, 202),
                'TEMPERATE_RAIN_FOREST': tcod.Color(164, 196, 168),
                'TEMPERATE_DECIDUOUS_FOREST': tcod.Color(180, 201, 169),
                'TROPICAL_RAIN_FOREST': tcod.Color(156, 187, 169),
                'TROPICAL_SEASONAL_FOREST': tcod.Color(169, 204, 164),
                'SUBTROPICAL_DESERT': tcod.Color(233, 221, 199)}

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
class PolyFeature(object):
    idx = attr.ib()
    parent = attr.ib()
    center = attr.ib()
    elevation = attr.ib(default=0.0)
    precipitation = attr.ib(default=0.0)
    temperature = attr.ib(default=0.0)
    color = attr.ib(default=0.0)


class PolygonMap(object):
    def __init__(self, seed=None, seed_size=100, relax_iter=4):
        """
        """
        if seed is None:
            seed = np.random.random((seed_size, 2))
        self.points = relax(seed, relax_iter)
        self.polymap = Voronoi(self.points)
        self.dist_map = KDTree(self.points)
        self.poly_graph = nx.Graph()
        self.corner_graph = nx.Graph()
        for p in range(len(self.polymap.points)):
            f = PolyFeature(idx=p, parent=self, center=self.points[p])
            self.poly_graph.add_node(p, feature=f)

        for e in self.polymap.ridge_points:
            self.poly_graph.add_edge(*e.tolist())

        for c in range(len(self.polymap.vertices)):
            self.corner_graph.add_node(c)

        for r in self.polymap.ridge_vertices:
            self.corner_graph.add_edge(*r)

    def feature(self, x, y):
        """:param x, y 2-D coordinate."""
        dist, idx = self.dist_map.query([x, y])
        return self.poly_graph.node[idx]['feature']


@attr.s
class gEntity(object):  # , pyDatalog.Mixin):
    eclass = attr.ib(default='entity')
    char = attr.ib(default=' ')
    color = attr.ib(default= tcod.white)
    name = attr.ib(default='generic entity')
    x = attr.ib(default=0)
    y = attr.ib(default=0)
    z = attr.ib(default=0)
    _id = attr.ib(default=uuid1())

    @property
    def pos(self):
        return (self.x, self.y, self.z)

    def draw(self, console):
        back = console[self.x, self.y].default_background_color
        console[self.x, self.y] = (self.char, self.color, back)

    def clear(self, console):
        back = console[self.x, self.y].default_background_color
        console[self.x, self.y] = (' ', self.color, back)


