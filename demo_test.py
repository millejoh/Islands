__author__ = 'millejoh'

import tcod, tcod.events
from numba import jit
from tcod.gameloop import BasicEventLoop
from tcod.events import KeyPressEvent
from gui.managers import GuiEventLoop, WindowManager
from gui.window import Window, ListWindow, Viewport
from worldgen import WorldGenerator

try:
    import IPython.core

    ipython_enabled = True
except ImportError:
    ipython_enabled = False


class DemoGame(BasicEventLoop):
    def step(self, root):
        fps = tcod.sys_get_fps()
        self.poll_for_event(tcod.EVENT_ANY)
        root.write(0, 0, "This is a string.")
        root.write(0, 1, "FPS = {0}".format(fps))
        root.write(0, 2, "Current event = {0}.".format(self.current_event))
        root.draw_string(0, 3, "This is {blue}blue{/}.", bg_color=tcod.light_flame)
        if issubclass(type(self.current_event),
                      tcod.events.KeyReleaseEvent) and self.current_event.vk == tcod.KEY_ESCAPE:
            root.end_game = True


class DemoGuiGame(GuiEventLoop):
    def initialize(self):
        self.w1 = Window(5, 5, 15, 15, title='Window 1', framed=True, window_manager=self.window_manager)
        self.w2 = Window(10, 10, 20, 30, title='Window 2', framed=True, window_manager=self.window_manager)
        self.list_view = ListWindow(tlx=15, tly=15, width=20, height=5, title='List Window', framed=True,
                                    window_manager=self.window_manager)
        self.list_view.add_item('An item.', 'An item.')
        self.list_view.add_item('Another item.', 'Uhuh.')
        self.list_view.add_item('Keep going.', 'Number.')
        self.list_view.add_item('Scrolling yet?', 'Scroll')
        self.list_view.add_item('last one', 'the end.')

    def print_debug_info(self):
        fps = tcod.sys_get_fps()
        root = self.window_manager.rootc
        root.write(0, 0, "Time elapsed = {0}".format(tcod.sys_elapsed_milli()))
        root.write(0, 1, "FPS = {0}".format(fps))
        root.write(0, 2, "Current event = {0}.".format(self.current_event))

    def step(self, root):
        start = tcod.sys_elapsed_milli()
        if self.current_key_event.pressed == True and self.current_key_event.vk == tcod.KEY_ESCAPE:
            self.end_game = True
        super().step(root)
        self.print_debug_info()
        # self.game_step()
        end = tcod.sys_elapsed_milli()


class WorldView(Viewport):
    def __init__(self, **keys):
        super().__init__(**keys)
        self.world_factory = WorldGenerator(self.map_width, self.map_height)
        self.world_factory.build_base_map(hill_cnt=60)
        self.elevation = self.world_factory._hm
        self.needs_redraw = True

    @jit
    def draw_elevations(self, as_color=True):
        for x in range(self.map_width):
            for y in range(self.map_height):
                if as_color:
                    intensity = tcod.color_lerp(tcod.black, tcod.white,
                                                self.elevation[x, y])
                    self.map_console.buffer.background[x, y, 0] = intensity.r
                    self.map_console.buffer.background[x, y, 1] = intensity.g
                    self.map_console.buffer.background[x, y, 2] = intensity.b
        self.map_console.fill_from_buffer(False, True)

    def on_update(self):
        if self.needs_redraw:
            self.clear_map()
            self.draw_elevations()
            self.needs_redraw = False

    def on_key_event(self, event):
        if issubclass(type(event), KeyPressEvent):
            if event.vkey == tcod.KEY_LEFT:
                if self.view_tlx > 0:
                    self.view_tlx -= 1
            if event.vkey == tcod.KEY_RIGHT:
                if self.view_tlx < (self.map_width-self.view_width-1):
                    self.view_tlx += 1
            if event.vkey == tcod.KEY_UP:
                if self.view_tly > 0:
                    self.view_tly -= 1
            if event.vkey == tcod.KEY_DOWN:
                if self.view_tly < (self.map_height-self.view_height-1):
                    self.view_tly += 1

class WorldGame(GuiEventLoop):
    def initialize(self, world_width=200, world_height=200):
        w, h = self.window_manager.screen_width, self.window_manager.screen_height
        self.world_view = WorldView(tlx=0, tly=0, width=w, height=round(h * 0.8),
                                    map_width=world_width, map_height=world_height,
                                    view_tlx=0, view_tly=0, framed=False,
                                    window_manager=self.window_manager)

    def print_debug_info(self):
        fps = tcod.sys_get_fps()
        root = self.window_manager.rootc
        root.write(0, 0, "Time elapsed = {0}".format(tcod.sys_elapsed_milli()))
        root.write(0, 1, "FPS = {0}".format(fps))
        root.write(0, 2, "Current event = {0}.".format(self.current_event))

    def step(self, root):
        start = tcod.sys_elapsed_milli()
        super().step(root)
        self.world_view.on_update()
        if self.current_key_event.pressed == True and self.current_key_event.vk == tcod.KEY_ESCAPE:
            self.end_game = True
        #self.print_debug_info()
        # self.game_step()
        end = tcod.sys_elapsed_milli()


def make_sim(kernel=None):
    wm = WindowManager(80, 60)
    # font='12x12.png', font_flags=tcod.FONT_LAYOUT_ASCII_INROW|tcod.FONT_TYPE_GREYSCALE,
    #                    font_width=16, font_height=48)
    if kernel:
        game = WorldGame(wm, ipykernel=kernel)
    else:
        game = WorldGame(wm)
    game.initialize()

    return game

def make_demo():
    wm = WindowManager(80, 60)
    demo = DemoGuiGame(wm)
    demo.initialize()

    return demo

if __name__ == '__main__':
    game = make_sim()
    game.run()
    #demo = make_demo()
    #demo.run()

