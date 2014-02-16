from tcod import console
from tcod.gui import ListWindow
from MapChunk import MapChunk, gEntity

if __name__ == '__main__':
    r = console.RootConsole(80, 60)
    map = MapChunk(tlx=0, tly=0, width=60, height=40, framed=False, map_width=120,
                   map_height=120, view_tlx=0, view_tly=0, title='The Map')
    map.random_island(0,0, 20, 20)
    map.random_island(30, 5, 40, 40)
    player = gEntity(char='@', name='Player', color = 'blue')
    map.add_actor(player)
    map.on_update()
    list_view = ListWindow(tlx=15, tly=15, width=20, height=5, title='List Window', framed=True)
    list_view.add_item('An item.', 'An item.')
    list_view.add_item('Another item.', 'Uhuh.')
    list_view.add_item('Keep going.', 'Number.')
    list_view.add_item('Scrolling yet?', 'Scroll')
    list_view.add_item('last one', 'the end.')
    r.run()
