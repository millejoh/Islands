__author__ = 'millejoh'

import gui.events
import tcod.event


class BasicEventLoop(tcod.event.EventDispatch):
    def __init__(self):
        # self.current_key_event = tcod.Key()
        # self.current_mouse_event = tcod.Mouse()
        self.current_event = None

    def wait_for_event(self, mask):
        return [ev for ev in tcod.event.wait() ]

    def poll_for_events(self, mask):
        # event_type = tcod.sys_check_for_event(mask, self.current_key_event,
        #                                       self.current_mouse_event)
        return [ev for ev in tcod.event.get()]

    def step(self, root):
        pass
