from random import randint
#from tcod import console
#from tcod.gui import ListWindow
#from MapChunk import MapChunk, gEntity
import pyglet
from pyglet.window import key
from pyglet.gl import *
import numpy as np
import cocos
from cocos.director import director
import cocos.euclid as eu


def test_tcod():
    r = console.RootConsole(80, 60)
    map = MapChunk(tlx=0, tly=0, width=60, height=40, framed=False, map_width=120,
                   map_height=120, view_tlx=0, view_tly=0, title='The Map')
    map.random_island(0, 0, 20, 20)
    map.random_island(30, 5, 140, 40)
    player = gEntity(char='@', name='Player', color='blue')
    map.add_actor(player)
    map.on_update()
    list_view = ListWindow(tlx=15, tly=15, width=20, height=5, title='List Window', framed=True)
    list_view.add_item('An item.', 'An item.')
    list_view.add_item('Another item.', 'Uhuh.')
    list_view.add_item('Keep going.', 'Number.')
    list_view.add_item('Scrolling yet?', 'Scroll')
    list_view.add_item('last one', 'the end.')
    r.run()


def rectangle(width, height, color_rgba=None):
    x, y = width, height
    ox, oy = 0, 0

    vlist = pyglet.graphics.vertex_list(4,
                                        ('v2i', (ox, oy,
                                                 ox, oy + y,
                                                 ox + x, oy + y,
                                                 ox + x, oy)),
                                        ('c4B', (255, 255, 255, 255) * 4))
    if isinstance(color_rgba, tuple):
        vlist.colors[:] = color_rgba * 4

    return vlist


class HelloWorld(cocos.layer.Layer):
    def __init__(self):
        super().__init__()
        label = cocos.text.Label('Hello world.', font_name='Times New Roman', font_size=36,
                                 anchor_x='center', anchor_y='center')
        label.position = 320, 240
        self.add(label)


class CocosWindow(cocos.layer.ColorLayer):
    is_event_handler = True  #: enable pyglet's events

    def __init__(self, tlx=20, tly=20, width=20, height=20, hidden=False,
                 parent=None, title="", framed=False):
        super().__init__(255, 255, 255, 255, width=width, height=height)
        self.position = tlx, tly
        self.hidden = hidden
        self.parent = parent
        self.title = title
        self.framed = framed
        self.border = cocos.layer.ColorLayer(0, 0, 0, 255, width=width - 2, height=height - 2)
        self.border.position = 1, 1
        self.add(self.border)

    def on_mouse_drag(self, x, y, dx, dy, buttons, modifiers):
        cx, cy = self.position
        wx, wy = cocos.director.director.get_window_size()
        if 0 < cx + dx < wx:
            cx += dx
        if 0 < cy + dy < wy:
            cy += dy
        self.position = cx, cy


world_width = 1000 + 4 * 98  #1392
world_height = 1000


class ProbeQuad(cocos.cocosnode.CocosNode):
    def __init__(self, r, color4):
        super(ProbeQuad, self).__init__()
        self.color4 = color4
        self.vertexes = [(r, 0, 0), (0, r, 0), (-r, 0, 0), (0, -r, 0)]

    def draw(self):
        glPushMatrix()
        self.transform()
        glBegin(GL_QUADS)
        glColor4ub(*self.color4)
        for v in self.vertexes:
            glVertex3i(*v)
        glEnd()
        glPopMatrix()


class ElevationView(cocos.layer.ScrollableLayer):
    is_event_handler = True

    def __init__(self, map_elevations, tile_size_px=10):
        self.emap = map_elavations
        super().__init()
        self.px_width = map_elevations.width * tile_size
        self.px_height = map_elevations.height * tile_size


class ColorGrid(cocos.layer.Layer):
    def __init__(self, cell_width, cell_height, cell_size=10):
        super().__init__()
        self.cells = np.zeros((cell_width, cell_height), dtype=object)
        batch = cocos.batch.BatchNode()
        self.add(batch)
        image = pyglet.image.load('block_black.png')
        w, h = image.width, image.height
        scale = cell_size / w if w >= h else cell_size / h
        for i in range(cell_width):
            for j in range(cell_height):
                position = (i * cell_size, j * cell_size)
                cell = cocos.sprite.Sprite(image, position=position, scale=scale, anchor=(0, 0))
                self.cells[i, j] = cell
                batch.add(cell)
        self.batch = batch

    def __iter__(self):
        (w, h) = self.cells.shape
        for i in range(w):
            for j in range(h):
                yield self.cells[i, j]

class SquareLand(cocos.layer.ScrollableLayer):
    is_event_handler = True

    def __init__(self, world_width, world_height):
        self.world_width = world_width
        self.world_height = world_height
        super(SquareLand, self).__init__()
        self.px_width = world_width
        self.px_height = world_height

        #dummy objects in the world: a big framed background and squares
        bg = cocos.layer.ColorLayer(170, 170, 0, 255, width=world_width, height=world_height)
        self.add(bg, z=0)
        margin = int(world_width * 0.01)
        #print 'margin',margin
        self.margin = margin
        bg = cocos.layer.ColorLayer(0, 170, 170, 255, width=world_width - 2 * margin, height=world_height - 2 * margin)
        bg.position = (margin, margin)
        self.add(bg, z=1)

        mod = (world_width - 2.0 * margin) / 10.0
        #print mod
        y = margin + mod
        self.marks_positions = []
        while y < world_height - mod:
            x = margin + mod
            while x < world_width - mod:
                red = 55 + int(200.0 * x / world_width)
                blue = 55 + int(200.0 * y / world_height)
                actor = cocos.layer.ColorLayer(red, 0, blue, 255,
                                               width=2 * int(mod), height=2 * int(mod))
                actor.position = x, y
                self.marks_positions.append((x, y))
                self.add(actor, z=3)
                x += 3 * mod
            y += 3 * mod
        self.marks_positions = self.marks_positions[:3]

        self.player = cocos.sprite.Sprite('grossinis_sister1.png')
        self.player.scale = 0.4
        self.player.model_width = self.player.width
        self.player.model_height = self.player.height

        self.player.position = (mod / 2 + margin, mod / 2 + margin)  #(200,200)
        self.player.fastness = 200
        self.add(self.player, z=4)

        self.bindings = {  #key constant : button name
                           key.LEFT: 'left',
                           key.RIGHT: 'right',
                           key.UP: 'up',
                           key.DOWN: 'down',
                           key.PLUS: 'zoomin',
                           key.MINUS: 'zoomout'
        }
        self.buttons = {  #button name : current value, 0 not pressed, 1 pressed
                          'left': 0,
                          'right': 0,
                          'up': 0,
                          'down': 0,
                          'zoomin': 0,
                          'zoomout': 0
        }
        self.schedule(self.step)

    def on_enter(self):
        super(SquareLand, self).on_enter()
        self.scroller = self.get_ancestor(cocos.layer.ScrollingManager)
        self.scene = self.get_ancestor(cocos.scene.Scene)
        self.scene.f_refresh_marks = self.refresh_marks

    #tracking of keys : keys updates self.buttons, model uses buttons
    def on_key_press(self, k, m):
        binds = self.bindings
        if k in binds:
            self.buttons[binds[k]] = 1
            return True
        return False

    def on_key_release(self, k, m):
        binds = self.bindings
        if k in binds:
            self.buttons[binds[k]] = 0
            return True
        return False

    def on_mouse_press(self, x, y, button, modifiers):
        # test from screen coords
        print('on_mouse_press:')
        vx, vy = self.scroller.pixel_from_screen(x, y)
        print('\tpixel_from_screen(x, y):', vx, vy)

    def clamp(self, actor, new_pos):
        x, y = new_pos
        if x - actor.model_width // 2 < self.margin:
            x = self.margin + actor.model_width // 2
        elif x + actor.model_width // 2 > self.world_width - self.margin:
            x = self.world_width - self.margin - actor.model_width // 2
        if y - actor.model_height // 2 < self.margin:
            y = self.margin + actor.model_height // 2
        elif y + actor.model_height // 2 > self.world_height - self.margin:
            y = self.world_height - self.margin - actor.model_height // 2
        return x, y

    def step(self, dt):
        buttons = self.buttons
        move_dir = eu.Vector2(buttons['right'] - buttons['left'],
                              buttons['up'] - buttons['down'])
        changed = False
        if move_dir:
            new_pos = self.player.position + self.player.fastness * dt * move_dir.normalize()
            new_pos = self.clamp(self.player, new_pos)

            self.player.position = new_pos
            changed = True

        new_zoom = self.scroller.scale + (buttons['zoomin'] - buttons['zoomout']) * 0.2 * dt
        if new_zoom < 0.3:
            new_zoom = 0.3
        elif new_zoom > 2.0:
            new_zoom = 2.0
        if new_zoom != self.scroller.scale:
            self.scroller.scale = new_zoom
            changed = True

        if changed:
            self.update_after_change()

    def update_after_change(self):
        self.scroller.set_focus(*self.player.position)
        self.refresh_marks()

    def refresh_marks(self):
        for mark, position in zip(self.scene.marks, self.marks_positions):
            screen_pos = self.scroller.pixel_to_screen(*position)
            mark.position = screen_pos

    def teleport_player(self, x, y):
        """ only used by autotest """
        self.player.position = x, y
        self.update_after_change()


class TestScene(cocos.scene.Scene):
    def __init__(self):
        super(TestScene, self).__init__()
        self.marks = []
        for i in range(1, 4):
            mark = ProbeQuad(3, (0, 255 // i, 0, 255))
            mark.position = (-20, -20)
            self.marks.append(mark)
            self.add(mark, z=2)

    def on_enter(self):
        director.push_handlers(self.on_cocos_resize)
        super(TestScene, self).on_enter()

    def on_cocos_resize(self, usable_width, usable_height):
        self.f_refresh_marks()


if __name__ == '__main__':
    director.init()
    cg = ColorGrid(100, 100, 20)
    scene = cocos.scene.Scene()
    #world_layer = SquareLand(world_width, world_height)
    #scroller = cocos.layer.ScrollingManager()
    #scroller.add(world_layer)
    scene.add(cg)
    # for cell in cg:
    #     cell.color = (randint(0, 255), randint(0, 255), randint(0, 255))
    director.run(scene)

