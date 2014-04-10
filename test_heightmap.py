from unittest import TestCase
from Heightmap import Heightmap
import numpy as np

__author__ = 'millejoh'

EPSILON = 0.00001


class TestHeightmap(TestCase):
    def setUp(self):
        a = Heightmap(10, 10)
        a.data = np.random.rand(*a.data.shape)
        self.a = a
        b = Heightmap(2, 2)
        b.data = np.array([[0, 1], [2, 3]])
        self.b = b

    def test_create(self):
        assert (isinstance(self.a, Heightmap))

    def test_copy(self):
        a = self.a
        b = a.copy()
        assert (a.width == b.width and a.height == b.height and (a.data == b.data).all())

    def test_clear_map(self):
        self.a.clear_map()
        assert ((self.a.data == np.zeros((self.a.width, self.a.height))).all())

    def test_clamp(self):
        a = self.a
        a.clamp(0.2, 0.8)
        assert ( not (a.data < 0.2).any() )
        assert ( not (a.data > 0.8).any() )

    def test_normalize(self):
        b = self.b
        b.normalize(0, 10)
        assert ( ((b.data - np.array([[0, 1 / 3], [2 / 3, 1]])) < EPSILON).all() )

    def test_lerp(self):
        self.fail()

    def test_interpolated_value(self):
        assert self.b.interpolated_value(0.5, 0.5) == 1.5
        assert self.b.interpolated_value(0.99, 0.99) - 2.97 < EPSILON

    def test_normal(self):
        self.fail()

    def test_add_hill(self):
        self.fail()

    def test_add_fbm(self):
        self.fail()

    def test_mid_point_displacement(self):
        self.fail()

    def test_kernel_transform(self):
        self.fail()

    def test_as_ndarray(self):
        self.fail()

    def test___getitem__(self):
        assert( self.b[0, 0] == self.b.data[0,0] )
