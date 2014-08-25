from math import cos, sin, pi
from random import uniform, sample, randint
from abc import ABCMeta, abstractmethod, abstractproperty
from itertools import chain

import pyglet
from pyglet.gl import *
import numpy as np

from color import COLOR_TABLE


try:
    # Try and create a window with multisampling (antialiasing)
    config = pyglet.gl.Config(sample_buffers=1, samples=4,
                              depth_size=16, double_buffer=True, )
    window = pyglet.window.Window(resizable=True, config=config)
except pyglet.window.NoSuchConfigException:
    # Fall back to no multisampling for old hardware
    window = pyglet.window.Window(resizable=True)

label = pyglet.text.Label('Hello, world',
                          font_name='Times New Roman',
                          font_size=36,
                          x=window.width // 2, y=window.height // 2,
                          anchor_x='center', anchor_y='center')

quad_vertices = [-0.5, -0.5, 0, -0.5, 0.5, 0, 0.5, 0.5, 0, 0.5, -0.5, 0]
quad_texcoords = (0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0)
quad_normals = (0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0)
quad_indices = (0, 1, 2, 0, 2, 3)

def flatten(mlist):
    if type(mlist[0]) is list:
        return flatten(list(chain.from_iterable(mlist)))
    else:
        return mlist

def rot2d(angle):
    """Angle is in radians."""
    c = cos(angle)
    s = sin(angle)
    return np.matrix([[c, -s],
                     [s, c]])

def rot3d_x(angle):
    c = cos(angle)
    s = sin(angle)
    return np.matrix([[1, 0, 0],
                     [0, c, -s],
                     [0, s, c]])

def rot3d_y(angle):
    c = cos(angle)
    s = sin(angle)
    return np.matrix([[c, 0, s],
                     [0, 1, 0],
                     [-s, 0, c]])

def rot3d_z(angle):
    c = cos(angle)
    s = sin(angle)
    return np.matrix([[c, -s, 0],
                     [s, c, 0],
                     [0, 0, 1],])

def translate(dx, dy, dz=0.0):
    return np.matrix([[dx], [dy], [dz]])


def quad_vertices_at(x, y, rot=0.0, scale=1.0):
    q = quad_vertices.copy()
    q[4] = q[4] * scale
    q[6] = q[6] * scale
    q[7] = q[7] * scale
    q[9] = q[9] * scale
    rot = rot3d_z(rot)
    for i in range(0, 12, 3):
        new_vec = rot.dot(np.array([q[i], q[i + 1], q[i + 2]]))[0]
        q[i], q[i + 1], q[i + 2] = new_vec[0, 0], new_vec[0, 1], new_vec[0, 2]
    for i in range(4):
        i0, i1 = i * 3, (i * 3) + 1
        q[i0], q[i1] = q[i0] + x, q[i1] + y

    return q


def quad_at(x, y, scale=1.0, rot=0.0, quad_color='true-white', batch=None):
    if batch:
        vlist = batch.add_indexed(4, pyglet.gl.GL_TRIANGLES, None, quad_indices, 'v3f', 'c3B', 't2f', 'n3f')
    else:
        vlist = pyglet.graphics.vertex_list_indexed(4, quad_indices, 'v3f', 'c3B', 't2f', 'n3f')

    q = quad_vertices_at(x, y, rot, scale)

    vlist.vertices = q
    vlist.tex_coords = quad_texcoords
    vlist.normals = quad_normals
    vlist.colors = COLOR_TABLE[quad_color] * 4

    return vlist, 4

class Shape(metaclass=ABCMeta):
    def draw(self):
        if not self.in_batch:
            self.vlist.draw(pyglet.gl.GL_TRIANGLES)

    @abstractmethod
    def move(self, dx, dy):
        pass

    @abstractproperty
    def vertices(self):
        pass

class Quad(Shape):
    def __init__(self, x, y, size=1.0, rot=0.0, color='true-white', batch=None):
        self.size = size
        self.x = x
        self.y = y
        self.rot = rot
        self.color = color
        self.batch = batch
        self.vlist, self.vcnt = quad_at(x, y, scale=size, rot=rot, quad_color=color, batch=batch)
        self._vertices = self.vertices

    @property
    def vertices(self):
        v = self.vlist.vertices
        vlist = []
        for i in range(self.vcnt):
            n = i*3
            vlist.append(np.matrix([[v[n]], [v[n+1]], [v[n+2]]]))

        return vlist

    def move(self, dx, dy):
        self.x += dx
        self.y += dy
        for i in range(4):
            n = i*3
            self.vlist.vertices[n] += dx
            self.vlist.vertices[n+1] += dy

    def rotate(self, dangle):
        s = self.size / 2.0
        cx, cy = self.x+s, self.y+s
        self.move(-cx, -cy)
        self.rot += dangle
        v = self.vertices
        r = rot3d_z(dangle)
        nv = flatten([(r*x).tolist() for x in v])
        self.vlist.vertices = nv
        self.move(cx, cy)

        return nv

class QuadField():
    def __init__(self, width, height, tile_size, base_color='true-white', ox=0, oy=0):
        """Create a field of colored quads (grid, really) of given `width` and `height`, with
        each quad having size `tile_size`. `width` and `height` must be a multiple of `tile_size`."""
        self.ox, self.oy = ox, oy
        if (width % tile_size != 0) or (height % tile_size != 0):
            raise ValueError('Parameters width ({0}) and height ({1}) must by a multiple of tile_size ({2}).'.format(width, height, tile_size))
        self.width, self.height = width, height
        self.nx, self.ny = (width // tile_size)+1, (height // tile_size)+1
        self.grid_width, self.grid_height = self.nx-1, self.ny-1
        self.quad_size = tile_size
        self.grid_colors = np.array([base_color]*((self.nx-1)*(self.ny-1))).reshape(self.nx-1, self.ny-1)

        verts, indices = self.build_colorfield()
        vcnt = self.nx*self.ny
        self.color_quads = pyglet.graphics.vertex_list_indexed(vcnt, indices, 'v3i', 'c3B')
        self.color_quads.vertices = verts
        self.color_quads.colors = COLOR_TABLE[base_color] * vcnt

        verts = self.build_grid()
        self.grid = pyglet.graphics.vertex_list(len(verts)//2, 'v2i')
        self.grid.vertices = verts

    def build_colorfield(self):
        verts = []
        for iy in range(self.ny):
            for ix in range(self.nx):
                verts.extend(((ix*self.quad_size)+self.ox, (iy*self.quad_size)+self.oy, 0))

        indices = []
        w = self.nx
        for iy in range(self.ny-1):
            for ix in range(self.nx-1):
                indices.extend((ix+iy*w, ix+1+iy*w, (ix+1)+(iy+1)*w, ix+(iy+1)*w))

        return verts, indices

    def colors(self, ix, iy):
        """Return colors for vertices at quad (ix, iy)."""
        # 4 vertices, 12 color entries in total.
        w = self.nx
        indices = [ix+iy*w, ix+1+iy*w, (ix+1)+(iy+1)*w, ix+(iy+1)*w]
        c = []
        for i in indices:
            start, end = i*3, (i*3)+3
            c.extend(self.color_quads.colors[start:end])

        return c

    def set_color(self, ix, iy, color_name):
        c = COLOR_TABLE[color_name]
        w = self.nx
        indices = [ix+iy*w, ix+1+iy*w, (ix+1)+(iy+1)*w, ix+(iy+1)*w]
        for i in indices:
            start, end = i*3, (i*3)+3
            self.color_quads.colors[start:end] = c

    def build_grid(self):
        verts = []
        for ix in range(self.nx):
            x = (ix * self.quad_size) + self.ox
            verts.extend((x, 0+self.oy, x, self.height+self.oy))
        for iy in range(self.ny):
            y = (iy * self.quad_size) + self.oy
            verts.extend((0+self.ox, y, self.width+self.ox, y))

        return verts

    def draw(self):
        glLoadIdentity()
        self.color_quads.draw(GL_QUADS)
        glColor3f(0,0,0)
        self.grid.draw(GL_LINES)

class Entity(object):
    def __init__(self, id, size, x, y, rot, color='true-white', batch=None):
        self.id = id
        self.size = size
        self.x = x
        self.y = y
        self.rot = rot
        self.vlist = quad_at(x, y, scale=size, rot=rot, quad_color=color, batch=batch)
        # if batch:
        #     self.vlist = batch.add_indexed(4, pyglet.gl.GL_TRIANGLES, None, quad_indices, 'v3i', 'c3B', 't2f', 'n3f')
        #     self.in_batch = True
        # else:
        #     self.vlist = pyglet.graphics.vertex_list_indexed(4, quad_indices, 'v3i', 'c3B', 't2f', 'n3f')
        #     self.in_batch = False
        # self.vlist.vertices = quad_vertices
        # self.vlist.tex_coords = quad_texcoords
        # self.vlist.normals = quad_normals
        # self.vlist.colors = COLOR_TABLE[color] * 4

    def draw(self):
        # glLoadIdentity()
        # glTranslatef(self.x, self.y, 0.0)
        # glRotatef(self.rot, 0, 0, 1)
        # glScalef(self.size, self.size, 1.0)
        if not self.in_batch:
            self.vlist.draw(pyglet.gl.GL_TRIANGLES)

    def draw_triangle(self):
        glLoadIdentity()
        glTranslatef(self.x, self.y, 0.0)
        glRotatef(self.rot, 0, 0, 1)
        glScalef(self.size, self.size, 1.0)
        glBegin(GL_TRIANGLES)
        glColor4f(1.0, 0.0, 0.0, 0.0)
        glVertex2f(0.0, 0.5)
        glColor4f(0.0, 0.0, 1.0, 1.0)
        glVertex2f(0.2, -0.5)
        glColor4f(0.0, 0.0, 1.0, 1.0)
        glVertex2f(-0.2, -0.5)
        glEnd()

class World(object):
    def __init__(self):
        self.ents = []
        self.nextEntId = 0
        self.batch = pyglet.graphics.Batch()
        self.quads = QuadField(200,200,10,'grey',-100,-100)
        for i in range(20):
            self.spawnEntity(0.0)
        pyglet.clock.schedule_interval(self.move_entities, 0.05)
        #pyglet.clock.schedule_interval(self.rot_entities, 0.1)
        pyglet.clock.schedule_interval(self.switch_color, 0.5)

    def spawnEntity(self, dt):
        size = uniform(1.0, 5.0)
        x = uniform(-100.0, 100.0)
        y = uniform(-100.0, 100.0)
        rot = uniform(0.0, 360.0) * pi/180.0
        color = sample(COLOR_TABLE.keys(), 1)[0]
        ent = Quad(x, y, size, rot, color=color, batch=self.batch)
        self.ents.append(ent)
        self.nextEntId += 1

        return ent

    def move_entities(self, dt):
        n_max = len(self.ents)-1
        for n in range(randint(0, n_max)):
            ent = self.ents[randint(0, n_max)]
            ent.move(uniform(-1,1), uniform(-1,1))

    def rot_entities(self,dt):
        n_max = len(self.ents)-1
        for n in range(randint(0, n_max)):
            ent = self.ents[randint(0, n_max)]
            ent.rotate(0.1)

    def switch_color(self, dt):
        color = sample(COLOR_TABLE.keys(), 1)[0]
        x = int(uniform(0, self.quads.grid_width))
        y = int(uniform(0, self.quads.grid_height))
        self.quads.set_color(x, y, color)

    def tick(self):
        for ent in self.ents.values():
            ent.rot += 10.0 / ent.size

    def draw(self):
        glClear(GL_COLOR_BUFFER_BIT)
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        self.quads.draw()
        self.batch.draw()
        #for ent in self.ents.values():
        #    ent.draw()


class Camera(object):
    def __init__(self, win, zoom=1.0):
        self.win = win
        self.zoom = zoom

    def worldProjection(self):
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        widthRatio = self.win.width / self.win.height
        gluOrtho2D(
            -self.zoom * widthRatio,
            self.zoom * widthRatio,
            -self.zoom,
            self.zoom)

    def hudProjection(self):
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        gluOrtho2D(0, self.win.width, 0, self.win.height)


class Hud(object):
    def __init__(self, win):
        self.win = win
        self.fps = pyglet.clock.ClockDisplay()


    def draw(self, quad_count):
        self.text = pyglet.text.Label('Number of quads = {0}'.format(quad_count),
                                      font_name='Times New Roman',
                                      font_size = self.win.width / 15.0,
                                      x = self.win.width / 2,
                                      y = self.win.height / 2,
                                      anchor_x = 'center',
                                      anchor_y = 'center',
                                      color = (255, 255, 255, 128))

        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        self.text.draw()
        self.fps.draw()


camera = Camera(window, 100)
hud = Hud(window)
world = World()


@window.event
def on_draw():
    window.clear()
    camera.worldProjection()
    world.draw()

    camera.hudProjection()
    hud.draw(world.nextEntId)

    #quad_verts.draw(pyglet.gl.GL_TRIANGLES)


if __name__ == '__main__':
    pyglet.app.run()