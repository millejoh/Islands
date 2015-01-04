__author__ = 'millejoh'

import tcod, tcod.events
from tcod.console import RootConsole
from tcod.gameloop import BasicEventLoop
from gui.managers import GuiEventLoop
from gui.window import Window, ListWindow

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
        root.write(0,0,"Time elapsed = {0}".format(tcod.sys_elapsed_milli()))
        root.write(0,1,"FPS = {0}".format(fps))
        root.write(0,2, "Current event = {0}.".format(self.current_event))

    def step(self, root):
        start = tcod.sys_elapsed_milli()
        super().step(root)
        self.print_debug_info()
        self.game_step()
        end = tcod.sys_elapsed_milli()

if __name__ == '__main__':
    root = RootConsole(80, 60)
    game = DemoGuiGame(root)
    game.initialize()
    root.run(game)