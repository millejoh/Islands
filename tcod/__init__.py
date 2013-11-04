import sys, platform
import os, os.path

# username = os.getlogin()

# linux_path = '/home/millejoh/Documents/libtcod/python/'
# win_path = 'c:/Users/{0}/Applications/libtcod-1.5.2/python/'.format(username)

# try:
#     from IPython import embed_kernel
# except ImportError:
#     pass

# if platform.system() == 'Linux':
#     root_dir = b'/home/millejoh/Documents/libtcod/'
#     if linux_path not in sys.path:
#         sys.path.append(linux_path)
# elif platform.system() == 'Windows':
#     if username == millejoh:
#         root_dir = b'c:/Users/millejoh/Applications/libtcod-1.5.2/'
#     elif username == e34
#     if win_path not in sys.path:
#         sys.path.append(win_path)

print('Tcod load path = ', os.path.dirname(os.path.abspath(__file__)))
os.environ['PATH'] = os.path.dirname(os.path.abspath(__file__)) + ';' + os.environ['PATH']
print('PATH = ', os.environ['PATH'])

from tcod.libtcodpy import *


root_console = 0 # This means NULL for you C folk.
default_font =  os.path.join(os.path.dirname(os.path.abspath(__file__)), os.path.normpath('data/fonts/consolas10x10_gs_tc.png'))

key_dispatch_table = {}

import tcod.console as console
import tcod.gui as gui

__all__ = ['console', 'tools', 'gui', 'libtcodpu']
