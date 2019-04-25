__author__ = 'millejoh'

import tcod
import gui.events

class BasicEventLoop(object):
    def __init__(self):
        self.current_key_event = tcod.Key()
        self.current_mouse_event = tcod.Mouse()
        self.current_event = None

    def wait_for_event(self, mask):
        event_type = tcod.sys_wait_for_event(mask, self.current_key_event, self.current_mouse_event)
        self.current_event = events.event_from(event_type, self.current_key_event, self.current_mouse_event)
        return self.current_event

    def poll_for_event(self, mask):
        event_type = tcod.sys_check_for_event(mask, self.current_key_event, self.current_mouse_event)
        self.current_event = events.event_from(event_type, self.current_key_event, self.current_mouse_event)
        return self.current_event

    def step(self, root):
        pass
