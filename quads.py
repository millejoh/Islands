from math import cos, sin
from random import uniform, sample, randint
from itertools import chain


import pyglet
from pyglet.gl import *
#from pyglet2d import Shape
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

#arial10x10 = pyglet.image.load('arial10x10_alpha.png')
#arial_map = pyglet.image.ImageGrid(arial10x10, 8, 32)

terminal_font = pyglet.image.load('terminal16x16_gs_ro.png')
terminal_map = pyglet.image.ImageGrid(terminal_font, 16, 16)

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
    def __init__(self, id, shape=None):
        self.id = id
        if shape:
            self.shape = shape
        else:
            self.shape = Shape.regular_polygon([0.0, 0.0], 1.0, 3)

    @property
    def pos(self):
        return self.shape.center

    def draw(self):
        if not self.shape:
            self.draw_triangle()
        else:
            self.shape.draw()

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


class Actor(Entity):
    def __init__(self, pos, size, color='true-white', draw_batch=None):
        corners = [np.asarray(pos), np.asarray(pos)+size]
        self.shape = Shape.rectangle(corners, color = COLOR_TABLE[color], batch = draw_batch)

    def on_update(self, world_state, dt):
        nearest = world_state.order_nearest_to(self)
        actor, dist = nearest[1]
        if dist > 15.0:
            vec = -(self.pos - actor.pos)
            self.shape.velocity = vec / dist
        else:
            vec = (self.pos - actor.pos)
            self.shape.velocity = vec

def build_random_actors(count, batch=None):
    return [Actor([uniform(-100.0, 100.0), uniform(-100.0, 100.0)], 5.0, sample(COLOR_TABLE.keys(), 1)[0], batch) for i in range(count)]


class World(object):
    def __init__(self):
        self.ents = []
        self.batch = pyglet.graphics.Batch()
        self.actor_drawables = pyglet.graphics.Batch()
        self.quads = QuadField(200,200,10,'grey',-100,-100)
        self.sprite = pyglet.sprite.Sprite(terminal_map[5,1])
        pyglet.clock.schedule_interval(self.update_entities, 0.1)
        pyglet.clock.schedule_interval(self.update_actors, 0.2)

        # TODO Move to an init_gl style function
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    def update_entities(self, dt):
        [e.shape.update(dt) for e in self.ents]

    def draw(self):
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glClear(GL_COLOR_BUFFER_BIT)
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        self.quads.draw()
        self.batch.draw()
        self.actor_drawables.draw()
        self.sprite.color = (150, 0, 100)
        self.sprite.draw()

    def order_nearest_to(self, entity):
        return sorted([(e, e.shape.distance_to(entity.shape.center)) for e in self.ents], key=lambda x: x[1])

    def update_actors(self, dt):
        for e in self.ents:
            if type(e) is Actor:
                e.on_update(self, dt)



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


    def draw(self):
        # self.text = pyglet.text.Label('Number of quads = {0}'.format(quad_count),
        #                               font_name='Times New Roman',
        #                               font_size = self.win.width / 15.0,
        #                               x = self.win.width / 2,
        #                               y = self.win.height / 2,
        #                               anchor_x = 'center',
        #                               anchor_y = 'center',
        #                               color = (255, 255, 255, 128))

        glMatrixMode(GL_MODELVIEW)
        glLoadIdentity()
        #self.text.draw()
        self.fps.draw()


camera = Camera(window, 100)
hud = Hud(window)
world = World()
world.ents = build_random_actors(20, world.actor_drawables)

@window.event
def on_draw():
    window.clear()
    camera.worldProjection()
    world.draw()

    camera.hudProjection()
    hud.draw()

    #quad_verts.draw(pyglet.gl.GL_TRIANGLES)


if __name__ == '__main__':
    pyglet.app.run()