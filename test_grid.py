# A test of pyglet_grid with customised parameters
#
# Copyright 2011 Jacob Conrad Martin
# http://jacobconradmartin.com
#
# This file is part of pyglet_grid.
#
# pyglet_grid is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pyglet_grid is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with pyglet_grid. If not, see <http://www.gnu.org/licenses/>.


from random import random

import pyglet

import pyglet_grid
import colour as c
from pyglet_grid import Cell, GlyphSet


glyphs = GlyphSet('12x12.png', 48, 16)

# How many elements to draw each time through the loop
how_many = 1000

# Get an instance of pyglet_grid
grid = pyglet_grid.Grid(80, 60)
# Set up the grid with custom parameters
# 1. Set up the window
grid.init_window()
# 2. Set up the cell size and border
grid.init_grid()
# 3. Initialise the vertex list
grid.init_vertex_list()

# It's slightly faster not to have to look these up every time through a loop
w = grid.w
h = grid.h

# ----- EXAMPLE ONE -----


def random_glpyh():
    return glyphs[int(random()*48*16)]

def update(dt):
    "Basic example showing simple usage."
    
    # Clear the grid
    grid.clear_all_cells()
    # Set the colours of some random cells in the grid
    for x in range(0, how_many):
        x = int(w * random())
        y = int(h * random())
        r = random()
        g = random()
        b = random()
        grid[x,y]= Cell(random_glpyh(), c.Color('white'), c.Color(red=r, blue=b, green=g))
    # Draw the grid
    grid.draw()

# ----- EXAMPLE TWO -----

# Set up a list to track the "dirty" cells
dirty_cells = []

def update_faster(dt):
    """More complicated example showing how to speed things up slightly.
    We do this by clearing only the colours of specific cells instead of all of them."""
    
    # Unset the colours of the dirty cells
    global dirty_cells
    dc = dirty_cells[:] # Iterate over a copy of the list, but modify the real list
    for x,y in dc:
        # Allow the dots to stick around for a while
        if random() < 0.1:
            # Unset the colour of the cell (to the grid's background_colour)
            grid.unset_cell(x,y)
            dirty_cells.remove([x,y])
    # Set the colours of some random cells in the grid
    for x in range(0, how_many):
        x = int(w * random())
        y = int(h * random())
        r = random()
        g = random()
        b = random()
        grid[x, y] = Cell(random_glpyh(), c.Color('white'), c.Color(red=r, blue=b, green=g))
        # Add this cell to the list of dirty cells
        dirty_cells.append([x,y])
    # Draw the grid
    grid.window.clear()
    grid.draw()

# ----- RUN EXAMPLES -----

# Go!
if __name__ == '__main__':
    # UNCOMMENT FOR EXAMPLE ONE
    #pyglet.clock.schedule(update)
    # UNCOMMENT FOR EXAMPLE TWO
    pyglet.clock.schedule(update)

    pyglet.app.run()