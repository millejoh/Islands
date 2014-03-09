from tcod import console
from tcod.gui import ListWindow
from MapChunk import MapChunk, gEntity
import cocos
import pyglet

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
            vlist.colors[:] = color_rgba*4

        return vlist

class HelloWorld(cocos.layers.layer.Layer):
    def __init__(self):
        super().__init__()
        label = cocos.layers.text.Label('Hello world.', font_name='Times New Roman', font_size=36,
                                 anchor_x='center', anchor_y='center')
        label.position = 320, 240
        self.add(label)


class CocosWindow(cocos.layers.layer.ColorLayer):
    is_event_handler = True     #: enable pyglet's events

    def __init__(self, tlx=20, tly=20, width=20, height=20, hidden=False,
                 parent=None, title="", framed=False):
        super().__init__(255, 255, 255, 255, width=width, height=height)
        self.position = tlx, tly
        self.hidden = hidden
        self.parent = parent
        self.title = title
        self.framed = framed
        self.border = cocos.layers.layer.ColorLayer(0, 0, 0, 255, width=width-2, height=height-2)
        self.border.position = 1, 1
        self.add(self.border)

    def on_mouse_drag(self, x, y, dx, dy, buttons, modifiers):
        cx, cy = self.position
        wx, wy = cocos.layers.director.director.get_window_size()
        if 0 < cx + dx < wx:
            cx += dx
        if 0 < cy + dy < wy:
            cy += dy
        self.position = cx, cy


if __name__ == '__main__':
    cocos.layers.director.director.init()
    hello_layer = HelloWorld()
    win = CocosWindow(30,30,50,50)
    main_scene = cocos.layers.scene.Scene(hello_layer, win)
    cocos.layers.director.director.run(main_scene)