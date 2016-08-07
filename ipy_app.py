from ipykernel.eventloops import register_integration
from tdl.event import App, get
import tdl, tcod
from gameclock import GameClock

root = None

class InteractiveApp(App):
    def __init__(self, kernel):
        self.kernel = kernel
        self.clock = GameClock()
        self.clock.frame_callback = self.update

    def init_root(self, width=80, height=26, show_credits=False):
        self.root = tdl.init(width, height)
        self.scratch = tdl.Console(width, height)
        self.temp_console = tdl.Console(width, height)
        self.width, self.height = width, height
        tdl.set_fps(30)
        if show_credits:
            tcod.console_credits()
            # tcod.sys_set_fps(self.max_fps)

    def draw(self):
        self.root.drawStr(1, 2, "No draw method defined, using default method.")
        self.root.drawStr(1, 3, "Time elapsed = {0}".format(tcod.sys_elapsed_milli()))
        self.root.drawStr(1, 4, "FPS = {0}".format(tcod.sys_get_fps()))

    def update(self, dt):
        self.root.clear()
        self.draw()
        tdl.flush()

    def run_once(self):
        """Pump events to this App instance and then return.

        This works in the way described in L{App.run} except it immediately
        returns after the first L{update} call.

        Having multiple L{App} instances and selectively calling runOnce on
        them is a decent way to create a state machine.
        """
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

        self.clock.tick()
        self.kernel.do_one_iteration()


@register_integration('tcod_gui')
def init_interactive_root(kernel):
    global root
    root = InteractiveApp(kernel)
    root.init_root(show_credits=True)
    root.run()