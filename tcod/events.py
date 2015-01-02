import tcod


class Event(object):
    def __init__(self, window=None, winx=0, winy=0):
        self.window = window
        self.winx = winx
        self.winy = winy
        self.time = tcod.sys_elapsed_milli()

        # def __repr__(self):
    #     return "GUIEvent(window={self.window},winx={self.winx},winy={self.winy})".format(self=self)


class MouseEvent(Event):
    def __init__(self, mouse_state=None, double_click=False, **kwargs):
        super().__init__(kwargs)
        self.state = mouse_state
        self.double_click = double_click

    def __getattr__(self, item):
        return self.state.__getattr__(item)

class MousePressEvent(MouseEvent):
    pass

class MouseReleaseEvent(MouseEvent):
    pass

class MouseMoveEvent(MouseEvent):
    pass

class MouseDragEvent(MouseEvent):
    def __init__(self, mouse_state=None, **kwargs):
        super().__init__(kwargs)
        self.state = mouse_state


class KeyEvent(Event):
    def __init__(self, key, **kwargs):
        super().__init__(**kwargs)
        self.key_info = key

class KeyPressEvent(KeyEvent):
    pass

class KeyReleaseEvent(KeyEvent):
    pass


def event_from(type, key, mouse):
    if type == tcod.EVENT_KEY_PRESS:
        return KeyPressEvent(key)
    elif type == tcod.EVENT_KEY_RELEASE:
        return KeyReleaseEvent(key)
    elif type == tcod.EVENT_MOUSE_PRESS:
        return MousePressEvent(mouse)
    elif type == tcod.EVENT_MOUSE_RELEASE:
        return MouseReleaseEvent(mouse)
    elif type == tcod.EVENT_MOUSE_MOVE:
        if mouse.lbutton or mouse.rbutton or mouse.mbutton:
            return MouseDragEvent(mouse)
        else:
            return MouseMoveEvent(mouse)