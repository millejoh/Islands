import tcod


class GUIEvent(object):
    def __init__(self, window=None, winx=0, winy=0):
        self.window = window
        self.winx = winx
        self.winx = winy
        self.time = tcod.sys_elapsed_milli()

    def __repr__(self):
        return "GUIEvent(window={self.window},winx={self.winx},winy={self.winy})".format(self=self)

class MouseEvent(GUIEvent):
    def __init__(self, mouse_state=None, **kwargs):
        super().__init__(kwargs)
        print(kwargs)
        self.state = mouse_state

class MouseDragEvent(MouseEvent):
    def __init__(self,**kwargs):
        super().__init__(kwargs)
