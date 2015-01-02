__author__ = 'millejoh'

import tcod, tcod.events
from tcod.console import RootConsole
from tcod.gameloop import BasicEventLoop
from gui.managers import GuiEventLoop
from gui.window import Window
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
    pass


if __name__ == '__main__':
    root = RootConsole(80, 60)
    game = DemoGuiGame(root)
    w1 = Window(5, 5, 15, 15,title='Window 1', framed=True, window_manager=game.window_manager)
    w2 = Window(10, 10, 20, 30, title='Window 2', framed=True, window_manager=game.window_manager)
    root.run(game)