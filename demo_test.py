__author__ = 'millejoh'

import tcod
from tcod.console import RootConsole
from tcod.gameloop import BasicEventLoop


class DemoGame(BasicEventLoop):

    def step(self, root):
        fps = tcod.sys_get_fps()
        self.poll_for_event(tcod.EVENT_ANY)
        root.write(0,0,"This is a string.")
        root.write(0,1,"FPS = {0}".format(fps))
        root.write(0,2, "Current event = {0}.".format(self.current_event))


if __name__ == '__main__':
    root = RootConsole(80, 60)
    game = DemoGame()
    root.run(game)