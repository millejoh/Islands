import sys, platform
import os.path
from threading import Thread

linux_path = '/home/millejoh/Documents/libtcod/python/'
win_path = 'c:/Users/E341194/Applications/libtcod-1.5.2/python/'

try:
    from IPython import embed_kernel
except ImportError:
    pass

if platform.system() == 'Linux':
    root_dir = b'/home/millejoh/Documents/libtcod/'
    if linux_path not in sys.path:
        sys.path.append(linux_path)
elif platform.system() == 'Windows':
    root_dir = b'c:/Users/E341194/Applications/tcod-1.5.2/'
    if win_path not in sys.path:
        sys.path.append(win_path)

try:
    import libtcodpy as tcod
except:
    print('I am running on {0} and I cannot find tcodpy.'.format(platform.system()))

default_font = b'consolas10x10_gs_tc.png'

font = os.path.join(root_dir, b'data', b'fonts', default_font)

def gui_loop():
    credits_end = False
    key = tcod.Key()
    mouse = tcod.Mouse()

    while not tcod.console_is_window_closed():
        tcod.sys_check_for_event(tcod.EVENT_KEY_PRESS | tcod.EVENT_MOUSE, key, mouse)
        tcod.console_set_default_foreground(0, tcod.white)
        if not credits_end:
            credits_end = tcod.console_credits_render(60, 43, 0)
        
        tcod.console_print(0, 0, 1, "Key pressed:{0!a}".format(key.c))
        tcod.console_flush()

if __name__ == '__main__':
    tcod.console_set_custom_font(font, tcod.FONT_TYPE_GREYSCALE | tcod.FONT_LAYOUT_TCOD)
    tcod.console_init_root(80, 50, b'tcod python sample', False)
    t = Thread(target=gui_loop)
    t.start()
    embed_kernel()
            
            
        


