__author__ = 'millejoh'

import tcod, tcod.events
import numpy as np
from tcod.console import RootConsole
from tcod.gameloop import BasicEventLoop
from gui.managers import GuiEventLoop, WindowManager
from gui.window import Window, ListWindow, Viewport
from worldgen import WorldGenerator

class DemoGame(BasicEventLoop):

    def step(self, root):
        fps = tcod.sys_get_fps()
        self.poll_for_event(tcod.EVENT_ANY)
        root.write(0,0,"This is a string.")
        root.write(0,1,"FPS = {0}".format(fps))
        root.write(0,2, "Current event = {0}.".format(self.current_event))
        root.draw_string(0, 3, "This is {blue}blue{/}.",bg_color=tcod.light_flame)
        if issubclass(type(self.current_event), tcod.events.KeyReleaseEvent) and self.current_event.key_info.vk == tcod.KEY_ESCAPE:
            root.end_game = True

class DemoGuiGame(GuiEventLoop):
    def initialize(self):
        self.w1 = Window(5, 5, 15, 15,title='Window 1', framed=True, window_manager=self.window_manager)
        self.w2 = Window(10, 10, 20, 30, title='Window 2', framed=True, window_manager=self.window_manager)
        self.list_view = ListWindow(tlx=15, tly=15, width=20, height=5, title='List Window', framed=True, window_manager=self.window_manager)
        self.list_view.add_item('An item.', 'An item.')
        self.list_view.add_item('Another item.', 'Uhuh.')
        self.list_view.add_item('Keep going.', 'Number.')
        self.list_view.add_item('Scrolling yet?', 'Scroll')
        self.list_view.add_item('last one', 'the end.')


    def print_debug_info(self):
        fps = tcod.sys_get_fps()
        root = self.window_manager.rootc
        root.write(0,0,"Time elapsed = {0}".format(tcod.sys_elapsed_milli()))
        root.write(0,1,"FPS = {0}".format(fps))
        root.write(0,2, "Current event = {0}.".format(self.current_event))

    def step(self, root):
        start = tcod.sys_elapsed_milli()
        super().step(root)
        self.print_debug_info()
        #self.game_step()
        end = tcod.sys_elapsed_milli()

class WorldView(Viewport):
    def __init__(self, **keys):
        super().__init__(**keys)
        self.world_factory = WorldGenerator(self.map_width, self.map_height)
        self.world_factory.build_base_map()
        self.elevation = self.world_factory._hm

    def draw_elevations(self, as_color=True):
        for x, y in zip(range(self.map_width), range(self.map_height)):
            if as_color:
                intensity = tcod.color_lerp(tcod.black, tcod.white,
                                            self.elevation[x, y] / 100.0)
            self.map_console[x, y] = (' ', tcod.white, intensity)

    def on_update(self):
        self.clear_map()
        self.draw_elevations()


class WorldGame(GuiEventLoop):
    def initialize(self):
        w, h = self.window_manager.screen_width, self.window_manager.screen_height
        self.world_view = WorldView(tlx=0, tly=0, width=round(w*0.6), height=round(h*0.6),
                                    map_width=w, map_height=h,
                                    view_tlx=0, view_tly=0, framed=False,
                                    window_manager=self.window_manager)

    def print_debug_info(self):
        fps = tcod.sys_get_fps()
        root = self.window_manager.rootc
        root.write(0,0,"Time elapsed = {0}".format(tcod.sys_elapsed_milli()))
        root.write(0,1,"FPS = {0}".format(fps))
        root.write(0,2, "Current event = {0}.".format(self.current_event))

    def step(self, root):
        start = tcod.sys_elapsed_milli()
        self.world_view.on_update()
        super().step(root)
        if self.current_key_event.pressed == True and self.current_key_event.vk == tcod.KEY_ESCAPE:
            root.end_game == True
        self.print_debug_info()
        #self.game_step()
        end = tcod.sys_elapsed_milli()


# if __name__ == '__main__':
#     wm = WindowManager(80, 60)
#     game = WorldGame(wm)
#     game.initialize()
#     game.run()
