import tdl, tcod
import pdb
import attr
import numpy as np
from gameclock import GameClock
from tdl.event import App, get
from MapChunk import PolygonMap
from uuid import uuid4

root = None

def unique_name(prefix):
    return '{}-{}'.format(prefix, uuid4().int)

class InteractiveApp(App):
    def __init__(self, kernel):
        self.kernel = kernel
        self.clock = GameClock()
        self.clock.frame_callback = self.frame_update

    def init_root(self, width=80, height=26, show_credits=False):
        self.root = tdl.init(width, height)
        self.canvas = tdl.Console(width, height)
        self.temp_console = tdl.Console(width, height)
        self.width, self.height = width, height
        tdl.set_fps(30)
        if show_credits:
            tcod.console_credits()
            # tcod.sys_set_fps(self.max_fps)

    def draw(self):
        self.root.blit(self.canvas)

    def frame_update(self, dt):
        self.root.clear()
        self.draw()
        tdl.flush()

    def runOnce(self):
        """Pump events to this App instance and then return.

        This works in the way described in L{App.run} except it immediately
        returns after the first L{update} call.

        Having multiple L{App} instances and selectively calling runOnce on
        them is a decent way to create a state machine.
        """
        if self.kernel:
            self.kernel.do_one_iteration()
        self.clock.tick()
        for event in get():
            if event.type:  # exclude custom events with a blank type variable
                # call the ev_* methods
                method = 'ev_%s' % event.type  # ev_TYPE
                getattr(self, method)(event)
            if event.type == 'KEYDOWN':
                # call the key_* methods
                method = 'key_%s' % event.key  # key_KEYNAME
                if hasattr(self, method):  # silently exclude undefined methods
                    getattr(self, method)(event)

class GameApp(InteractiveApp):
    def init_root(self, manager, *args, **kwargs):
        super().init_root(*args, **kwargs)
        self.game_manager = manager
        self.game_manager.world_canvas = self.canvas

    def draw(self):
        self.game_manager.on_draw()
        super().draw()

class WorldState(object):
    def __init__(self, width, height, feature_cnt=None, canvas=None):
        fcnt = int(feature_cnt or (width*height)/10)
        self._dims = (width, height)
        self.wmap = PolygonMap(*self._dims, fcnt)
        self.world_canvas = canvas
        self.actors = {}

    def redraw_chunk(self):
        self.canvas.clear()
        self.draw_terrain()
        self.draw_features()
        self.draw_actors()

    def on_draw(self):
        # Determine what's changed, and only draw that.
        # Implement special effects here?
        pass

    def draw_terrain(self):
        hn = self.wmap.terrain_to_hm('elevation')
        amin, max = hn.get_minmax()
        for i in range(hn.width):
            for j in range(hn.height):
                color = tcod.color_lerp(tcod.dark_blue, tcod.white, hn[i,j]/max)
                self.world_canvas.draw_char(i, j, ' ', fg=tcod.white, bg=color)

    def draw_features(self):
        pass

    def draw_actors(self):
        for _, actor in self.actors.items():
            actor.draw(self.world_canvas)


