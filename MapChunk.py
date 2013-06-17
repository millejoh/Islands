# Try to find libtcodpy
import sys, platform
import os.path

linux_path = '/home/millejoh/Documents/libtcod/python'
win_path = 'c:/Users/E341194/Applications/libtcod-1.5.2/python'

if platform.system() == 'Linux':
    if linux_path not in sys.path:
        sys.path.append(linux_path)
elif platform.system() == 'Windows':
    if win_path not in sys.path:
        sys.path.append(win_path)

try:
    import libtcodpy as tcod
except:
    print('I am running on {0} and I cannot find libtcodpy.'.format(platform.system()))

# tcod_dir = 'c:/Users/E341194/Applications/libtcod-1.5.2'
# default_font = os.path.join(tcod_dir,b'data', b'fonts', b'consolas10x10_gs_tc.png')

default_font = b'arial10x10.png'
root_console = 0 # This means NULL for you C folk.

class MapChunk(object):
    def __init__(self, width, height, offset=[0, 0]):
        self._offset = offset
        self._width = width
        self._height = height
        self._hmap = tcod.heightmap_new(width, height)

class Player(object):
    def __init__(self, start_x, start_y, color = tcod.white):
        self._x, self._y = start_x, start_y
        self._color = color

    def draw(self, console=root_console):
        tcod.console_set_default_foreground(console, self._color)
        tcod.console_put_char(console, self._x, self._y, '@')

class Console(object):
    pass

class Stage(object):
    def __init__(self, width, height, title=b'Stage', font_file=default_font, datax='', fullscreen=False, renderer=tcod.RENDERER_GLSL):
        self._width = width
        self._height = height
        self._title = title
        self._fullscreen = fullscreen
        self._renderer = renderer
        self.end_game = False
        self.player = Player(0,0)
        if os.path.exists(font_file):
            tcod.console_set_custom_font(font_file, tcod.FONT_LAYOUT_TCOD | tcod.FONT_TYPE_GREYSCALE)
        else:
            raise FileNotFoundError("Font file {0} not found.".format(font_file))

    def handle_keys(self, key_event):
        if key_event.vk == tcod.KEY_UP:
            self.player._y -= 1
        if key_event.vk == tcod.KEY_DOWN:
            self.player._y += 1
        if key_event.vk == tcod.KEY_RIGHT:
            self.player._x += 1
        if key_event.vk == tcod.KEY_LEFT:
            self.player._x -= 1

#    def handle_mouse(self, mouse):
#        if mouse.lbutton:
            

    def handle_events(self):
        mouse, key = tcod.Mouse(), tcod.Key()
        tcod.sys_check_for_event(tcod.EVENT_ANY, key, mouse)
        return key, mouse

    
    def run(self, max_fps=30):
        tcod.console_init_root(self._width, self._height, self._title, self._fullscreen,
                               self._renderer)        
        tcod.sys_set_fps(max_fps)
        tcod.console_set_default_background(root_console, tcod.darker_sepia)
        tcod.console_set_keyboard_repeat(1,1)
        while (not self.end_game) and (not tcod.console_is_window_closed()):
            key, mouse = self.handle_events()
            self.handle_keys(key)
            if key.vk == tcod.KEY_ESCAPE:
                tcod.console_clear(root_console)
                tcod.console_print(root_console, 0, 0, "Exiting...")
                tcod.console_flush()
                break
            tcod.console_clear(root_console)
            self.player.draw(root_console)
            tcod.console_print(root_console, 0, 0, "Current key pressed is {0}.".format(key.vk))
            tcod.console_print(root_console, 0, 1, "Cursor at ({0}, {1}).".format(mouse.cx, mouse.cy))
            tcod.console_flush()



#if __name__ == '__main__':
