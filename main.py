from bearlibterminal import terminal
from clubsandwich.blt.state import blt_state
from clubsandwich.director import DirectorLoop, Scene
from clubsandwich.geom import Rect, Point, Size
from clubsandwich.ui import (
    RectView,
    LabelView,
    ButtonView,
    FirstResponderContainerView,
    WindowView,
    SettingsListView,
    LayoutOptions,
    UIScene,
    CyclingButtonView,
    SingleLineTextInputView,
    IntStepperView,
    View,
)

RectView()

LOGO = """
   _____      __
  /_  _/ __  / /             __  __
   / / / _/ / /___ ____  ___/ // _/
 _/ /_ \ \ / /| _  / _ \/ _  / \ \
/____//__//_/ \_,_/_//_/\_,_/ /__/
"""


class MainMenuScene(UIScene):
    def __init__(self, *args, **kwargs):
        views = [
            LabelView(
                LOGO[1:].rstrip(),
                layout_options=LayoutOptions.row_top(0.5)),
            LabelView(
                "Try resizing the window!",
                layout_options=LayoutOptions.centered('intrinsic', 'intrinsic')),
            ButtonView(
                text="Play",
                callback=self.play,
                color_bg='#000000', color_fg='#00ff00',
                layout_options=LayoutOptions.row_bottom(4).with_updates(
                    left=0.2, width=0.2, right=None)),
            ButtonView(
                text="Settings", callback=self.show_settings,
                layout_options=LayoutOptions.row_bottom(4).with_updates(
                    left=0.4, width=0.2, right=None)),
            ButtonView(
                text="[color=red]Quit",
                callback=lambda: self.director.pop_scene(),
                size=Size(4, 1),  # [color=red] messes up auto size calculations
                layout_options=LayoutOptions.row_bottom(4).with_updates(
                    left=0.6, width=0.2, right=None)),
        ]
        super().__init__(views, *args, **kwargs)

    def become_active(self):
        self.ctx.clear()

    def play(self):
        # self.director.push_scene(CharacterCreationScene())
        self.director.push_scene(GameScene())

    def show_settings(self):
        self.director.push_scene(SettingsScene())

class SettingsScene(UIScene):
    OPTIONS = {
        'Difficulty': ["I'm too young to die", "Hey, not too rough", "Hurt me plenty", "Ultra-Violence", "Nightmare!"],
        'Advanced water effects': ['True', 'False'],
        'Sound level (out of 10)': ['Off', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10'],
        'Music level (out of 10)': ['Off', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10'],
        'Permadeath': ['True', 'False'],
        'FXAA': ['True', 'False'],
        'Shadow quality': ['No shadows', 'I have a potato', 'Medium', 'High', 'Ridiculous'],
        'Realtime': ['True', 'False'],
        'Send system analytics to Facebook': ['True', 'False'],
        'Burn extra CPU just for fun': ['True', 'False'],
        'Include EMACS implementation': ['True', 'False'],
        'Include LISP implementation': ['True', 'False'],
        'Include email client implementation': ['True', 'False'],
    }

    def __init__(self, *args, **kwargs):
        view = WindowView(
            'Settings',
            layout_options=LayoutOptions.centered(60, 20),
            subviews=[
                SettingsListView(
                    [
                        (k, CyclingButtonView(v, v[0], callback=lambda _: None, align_horz='left'))
                        for k, v in sorted(SettingsScene.OPTIONS.items())
                    ],
                    value_column_width=20,
                    layout_options=LayoutOptions(bottom=5)),
                ButtonView(
                    text='Apply', callback=self.apply,
                    layout_options=LayoutOptions.row_bottom(5).with_updates(right=0.5)),
                ButtonView(
                    text='Cancel', callback=lambda: self.director.pop_scene(),
                    layout_options=LayoutOptions.row_bottom(5).with_updates(left=0.5)),
            ])
        super().__init__(view, *args, **kwargs)

        # this lets the main screen show underneath
        self.covers_screen = False

    def apply(self):
        print("Your choices are meaningless.")
        self.director.pop_scene()

class GameScene(UIScene):
    def __init__(self, *args, **kwargs):
        self.main_display = MainDisplay(layout_options=LayoutOptions(left=0.2))
        self.side_info_bar = InfoBar(layout_options=LayoutOptions.column_left(width=0.2))
        views = [
            self.main_display,
            self.side_info_bar
        ]
        super().__init__(views, *args, **kwargs)

    def become_active(self):
        self.ctx.clear()

    def terminal_read(self, val):
        super().terminal_read(val)
        if val == 'q' or val =='Q':
            self.director.pop_scene()
        return True

class InfoBar(RectView):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

from clubsandwich.draw import draw_line_horz
from clubsandwich.geom import Point

class WorldView(View):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.view_center = Point()
        self.the_map = None

    def draw(self, ctx):
        w, h = self.bounds.width, self.bounds.height
        draw_line_horz(Point(0,0), w-5, ctx)
        draw_line_horz(Point(0, 10), w-5, ctx)


class MainDisplay(View):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.info_bar_view = LabelView(' Status/Resources',
                                       align_horz='left',
                                       layout_options=LayoutOptions.row_top(1))
        self.world_view = WorldView(layout_options=LayoutOptions(top=2))
        self.message_view = RectView(layout_options=LayoutOptions.row_bottom(10))
        self.add_subviews([self.info_bar_view, self.world_view, self.message_view])

from gameclock import GameClock

class GameLoop(DirectorLoop):
    def __init__(self):
        super().__init__()
        self.clock = GameClock()
        try:
            import IPython.core
            shell = IPython.core.getipython.get_ipython()
            self._kernel = shell.kernel
        except ImportError:
            self._kernel = None

    def terminal_init(self):
        super().terminal_init()
        terminal.set("""
        window.resizeable=true;
        """)

    def get_initial_scene(self):
        return MainMenuScene()

    def terminal_update(self):
        # pdb.set_trace()
        super().terminal_update()
        if self._kernel:
            self._kernel.do_one_iteration()
            self.clock.tick()
        return True

global global_director
global_director = GameLoop()
