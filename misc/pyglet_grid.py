# pyglet_grid is a grid interface for pyglet
#
# Copyright 2011 Jacob Conrad Martin
# http://jacobconradmartin.com
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

from collections import namedtuple
from colour import Color
from warnings import warn
import pyglet
import numpy as np

Cell = namedtuple('Cell', ['glyph', 'foreground', 'background'])

class GlyphSet(object):
    def __init__(self, source, rows, cols):
        self.source = source
        self.im_cnt = rows*cols
        self.image_set = pyglet.image.load(source)
        self.image_set_seq  = pyglet.image.ImageGrid(self.image_set, rows, cols)
        # self.image_set_tex_seq = pyglet.image.TextureGrid(self.image_set_seq)

    def __getitem__(self, item):
        if isinstance(item, int) or isinstance(item, tuple):
            return pyglet.sprite.Sprite(self.image_set_seq[item])
        elif isinstance(item, str) and len(item) == 1:
            idx = self.im_cnt - (ord(item)+16)
            return pyglet.sprite.Sprite(self.image_set_seq[idx])
        else:
            raise AttributeError

    def __repr__(self):
        return "<GlyphSet '{0}' with {1} glyphs>".format(self.source, self.im_cnt)

class Grid():
    
    def __init__(self, cols, rows, border = 1, window_width=800, window_height=600):
        "Initialise the grid with some sensible parameters."
        # Window dimensions
        if (window_width % cols != 0) or (window_height % rows != 0):
            warn('Grid size does not fit evenly in window.')
        self.window_width = window_width
        self.window_height = window_height
        # Window
        self.window = None
        # Cell dimensions
        self.cell_height = window_height // rows
        self.cell_width = window_width // cols
        self.cell_border = border
        # Grid size
        self.w = cols
        self.h = rows
        # Vertex List
        self.vertex_list = None
        self._glyphs = {}
        self.glyph_batch = pyglet.graphics.Batch()

    def __repr__(self):
        return '<Grid ({0}x{1} cells)>'.format(self.w, self.h)

    def __getitem__(self, item):
        x, y = item
        c1 = 12 * (x + y * self.w)
        return self.vertex_list.colors[c1:c1+12]

    def __setitem__(self, key, value):
        if isinstance(value, Cell):
            x, y = key
            self.set_cell_glyph(x, y, value.glyph)
            self.set_cell_bg(x, y, value.background)
            self.set_cell_fg(x, y, value.foreground)
        else:
            raise TypeError('Value is of type {}, must be a Cell object.'.format(type(value)))

    def simple_prepare(self):
        "Ready pyglet_grid for action. Best used when the default parameters are OK."
        self.init_window()
        self.init_grid()
        self.init_vertex_list()
        
    def init_window(self):
        "Initialise the window."
        # Make the window
        self.window = pyglet.window.Window(self.window_width, self.window_height)
        # We need to clear the window background or else it will be full of garbage
        self.window.clear()
    
    def init_grid(self):
        "Initialise the grid."
        self.w = int(self.window_width / self.cell_width)
        self.h = int(self.window_height / self.cell_height)
    
    def init_vertex_list(self):
        "Initialise the vertex list."
        self.vertex_list = pyglet.graphics.vertex_list(4 * self.w * self.h, 'v2i/static', 'c3f/stream')
        verts = []
        cell_width = self.cell_width
        cell_height = self.cell_height
        cell_border = self.cell_border
        for x in range(0, self.w):
            for y in range(0, self.h):
                # Calculate box area
                x1,y1 = x*cell_width, y*cell_height
                x2,y2 = x1+cell_width, y1
                x3,y3 = x2, y1+cell_height
                x4,y4 = x1,y3
                # Apply borders to boxes
                if cell_border > 0:
                    x1, y1 = x1+cell_border, y1+cell_border
                    x2, y2 = x2-cell_border, y2+cell_border
                    x3, y3 = x3-cell_border, y3-cell_border
                    x4, y4 = x4+cell_border, y4-cell_border
                # Add the vertext coords to our list of vertices
                verts.extend([x1,y1, x2,y2, x2,y3, x4,y4])
        self.vertex_list.vertices = verts
        # Initialise colours to background colour
        self.vertex_list.colors = [0] * 12 * self.w * self.h
        
    
    def set_cell_bg(self, x, y, color):
        """Set the background color of a cell.
        The variables x and y should be in the range (0, self.w) and (0, self.h) respectively."
        The variable c should be a colour.Color object.
        """
        # Unpack colour values
        r,g,b = color.get_rgb()
        # There are 4 vertices per coordinate, each with 3 values for the colour
        c1 = 12 * (x*self.h + y)
        self.vertex_list.colors[c1:c1+12] = [r,g,b] * 4

    def set_cell_fg(self, x, y, color):
        """Set the foreground color of a cell, i.e. the color of the cell's glyph (if it has one).
        The variables x and y should be in the range (0, self.w) and (0, self.h) respectively."
        The variable c should be a colour.Color object.
        """
        r, g, b = color.get_rgb()
        try:
            self._glyphs[(x,y)].color = (int(r*255), int(g*255), int(b*255))
        except KeyError:
            pass

    def set_cell_glyph(self, x, y, glyph):
        coord = (x, y)
        if isinstance(glyph, pyglet.sprite.Sprite):
            glyph.x, glyph.y = x*self.cell_width, y*self.cell_height
            if coord in self._glyphs:
                self._glyphs[coord].batch = None
            self._glyphs[coord] = glyph
            self._glyphs[coord].batch = self.glyph_batch

    def unset_cell(self, x, y):
        "Unset a cell back to the background colour."
        # There are 4 vertices per coordinate, each with 3 values for the colour
        c1 = 12 * (x + y*self.w)
        self.vertex_list.colors[c1:c1+12] = [0] * 12
        
    def clear_all_cells(self):
        "Clear the color of all cells."
        # There are 4 vertices per coordinate, each with 3 values for the colour
        c_max = 12 * (self.h * self.w)
        self.vertex_list.colors[0:c_max] = [0] * c_max

    def draw(self):
        "Draw the vertex list using the currently assigned colours."
        self.vertex_list.draw(pyglet.gl.GL_QUADS)
        #self.glyph_batch.draw()


