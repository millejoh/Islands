from unittest import TestCase
from MapChunk import PolyFeature, PolygonMap
import numpy as np

test_points = np.array([[0, 0], [0, 1], [0, 2], [1, 0], [1, 1], [1, 2],
                        [2, 0], [2, 1], [2, 2]])

vor_vertices = np.array([[ 0.5,  0.5],
                         [ 1.5,  0.5],
                         [ 0.5,  1.5],
                         [ 1.5,  1.5]])

class TestPolygonMap(TestCase):
    def test_creation(self):
        map = PolygonMap(seed=test_points, relax_iter=0)
        assert isinstance(map, PolygonMap)
        assert np.all(map.points == test_points)
        assert np.all(map.polymap.vertices == vor_vertices)

    def test_feature(self):
        map = PolygonMap(seed=test_points, relax_iter=0)
        feature1 = map.feature(0.0, 0.0)
        feature2 = map.feature(0.1, 0.1)
        assert feature1 == feature2
