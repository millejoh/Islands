import sys, platform
import os.path

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
    root_dir = b'c:/Users/E341194/Applications/libtcod-1.5.2/'
    if win_path not in sys.path:
        sys.path.append(win_path)

try:
    from libtcodpy import *
except:
    print('I am running on {0} and I cannot find tcodpy.'.format(platform.system()))

root_console = 0 # This means NULL for you C folk.
default_font = os.path.join(root_dir, b'data/fonts/consolas10x10_gs_tc.png')

key_dispatch_table = {}


__all__ = ['console', 'tools']
