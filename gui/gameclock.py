__author__ = 'millejoh'

#!/usr/bin/env python

__version__ = '$Id: gameclock.py 294 2011-05-12 03:55:32Z$'
__author__ = 'Gummbum, (c) 2011'


"""gameclock.py - Game clock for Gummworld2.

GameClock manages time in the following ways:

    1.  Run game engine at a constant speed, independent of variable frame rate.
    2.  Register special callback functions that will be run when the
        update_ready and frame_ready conditions occur. (Directly polling the
        clock is a supported alternate method.)
    3.  Schedule callback items to coincide with tick(), update_ready,
        frame_ready, and elapsed intervals.
    4.  Employ on-demand time dilation.
    5.  Gracefully handles some worst cases.

Note that Python Library docs mention that time functions do not return
fractions of a second on all computer platforms. Therefore this module will not
work on such platforms.

* * *  IMPORTANT  * * *

On the note of granularity, some time sources are far less than ideal. This
module uses the logic: if (windows|cygwin) use time.clock, else use time.time.
For non-Windows systems using pygame is HIGHLY recommended to use
pygame.time.get_ticks for its greatly superior granularity. You can do this like
so:

    if sys.platform in('win32','cygwin'):
        time_source = None
    else:
        time_source = lambda:pygame.time.get_ticks()/1000.
    clock = gameclock.GameClock(time_source=time_source)

USAGE

Old-style direct polling:

    clock = GameClock()
    while 1:
        clock.tick()
        if clock.update_ready:
            update()
        if clock.frame_ready:
            draw()

Special callback:

    clock = GameClock(update_callback=update, frame_callback=draw)
    while 1:
        clock.tick()

Special callbacks can be directly set and cleared at any time:

    clock.update_callback = my_new_update
    clock.frame_callback = my_new_draw

    clock.update_callback = None
    clock.frame_callback = None

Scheduling miscellanous callbacks:

    def my_tick_spammer(dt, secs, message=None):
        print 'secs=%f dt=%f message=%s' % (secs,dt,message)
    clock.schedule(my_tick_spammer, lambda:time.time(), message='Hello again!')

    def update_something(dt):
        "..."
    clock.schedule_update(coincide_with_update)

    def coincide_with_draw(dt):
        "..."
    clock.schedule_frame(coincide_with_draw)

    def every_second_of_every_day(dt):
        "..."
    clock.schedule_interval(every_second_of_every_day, 1.0)

Time dilation (affects DT and interval timers):

    normal = 1.0
    slow_mo = 2.0
    fast_mo = 0.5
    clock.dilation = slow_mo
    clock.dilation = normal

CREDITS

The inspiration for this module came from Koen Witters's superb article
"deWiTTERS Game Loop", aka "Constant Game Speed independent of Variable FPS" at
http://www.koonsolo.com/news/dewitters-gameloop/.

Pythonated by Gummbum. While the builtin demo requires pygame, the module does
not. The GameClock class is purely Python and should be compatible with other
Python-based multi-media and game development libraries.
"""

import sys
import time

class _Item(object):
    """A spammy item runs all the time."""
    __slots__ = ['func', 'pri', 'args', 'kwargs']
    def __init__(self, func, pri, args, kwargs):
        self.func = func
        self.pri = pri
        self.args = args
        self.kwargs = kwargs

class _IntervalItem(object):
    """An interval item runs after an elapsed interval."""
    __slots__ = ['func', 'interval', 'lasttime', 'args', 'kwargs']
    def __init__(self, func, interval, curtime, args, kwargs):
        self.func = func
        self.interval = float(interval)
        self.lasttime = curtime
        self.args = args
        self.kwargs = kwargs
    def sort_key(self):
        return self.lasttime+self.interval

class GameClock(object):
    """Manage time in the following ways:

    1. Run game engine at a constant speed, independent of variable frame rate.
    2. Schedule items to coincide with tick(), update_ready, frame_ready, and
       elapsed intervals.
    3. Employ on-demand time dilation.

    Parameters:
        ticks_per_second -> Positive integer. Constant ticks per second for
            game physics.
        max_fps -> Positive integer. Max frames allowed per second. A value of
            zero allows unlimited frames.
        use_wait -> Boolean. When True, GameClock.tick() uses time.sleep to
            throttle frames per second. This uses less CPU at the postential
            cost of smoothness. When False, GameClock.tick() returns without
            injecting any waits, and can result in smoother frames.
        max_frame_skip -> Positive integer. Max game ticks allowed before
            forcing a frame display.
        update_callback -> Callable. Special callback to invoke when update is
            ready.
        frame_callback -> Callable. Special callback to invoke when frame is
            ready.
        time_source -> Callable. Custom time source, e.g.
            lambda:pygame.time.get_ticks() / 1000.0.
    Properties:
        interpolate -> Read-only. Float (range 0 to 1) factor representing the
            exact point in time between the previous and next ticks.
        update_ready -> Read-only. Boolean indicating it is time to update the
            game logic.
        frame_ready -> Read-only. Boolean indicating it is time to update the
            display.
        dilation -> Read-write. Set the time dilation factor. Normal==1.0,
            Slower>1.0, Faster&lt;1.0. Affects DT and interval timers.
        update_callback -> Read-write. The callback function to invoke at each
            update_ready interval.
        frame_callback -> Read-write. The callback function to invoke at each
            frame_ready interval.
        fps, frame_count, frame_elapsed -> Read-only. Most recent FPS,
            cumulative frames posted during the current second, and time elapsed
            in the previous frame, respectively.
        ups, update_count, update_elapsed -> Read-only. Most recent updates per
            second, cumulative updates posted during the current second, and
            time elapsed in the previous update, respectively.
        tps -> Read-only. Most recently measured tick() calls per second.
        time -> Read-write. The value from the last poll of time source.
        ticks_per_second -> Read-write. See parameter ticks_per_second.
        max_fps -> Read-write. See parameter max_fps.
        use_wait -> Read-write. See parameter use_wait.
        max_frame_skip -> Read-write. See parameter max_frame_skip.
    Methods:
        tick() -> Game loop timer. Call once per game loop.
        get_time() -> Return the milliseconds elapsed in the previous call to tick().
        get_fps() -> Return the frame rate from the previous second.
        get_ups() -> Return the update rate from the previous second.
        schedule(), schedule_update(), schedule_update_priority(),
            schedule_frame(), schedule_frame_priority(),
            schedule_interval() -> Various scheduling facilities.
        unschedule() -> Schedule removal.
    """

    def __init__(self,
        ticks_per_second=25, max_fps=0, use_wait=True, max_frame_skip=5,
        update_callback=None, frame_callback=None, time_source=None,
    ):
        # time sources
        self._wait = time.sleep
        if time_source is not None:
            self._get_ticks = time_source
        elif sys.platform in ('win32','cygwin'):
            self._get_ticks = time.clock
        else:
            self._get_ticks = time.time

        # settings
        self.ticks_per_second = ticks_per_second
        self.max_fps = max_fps
        self.use_wait = use_wait
        self.max_frame_skip = max_frame_skip
        self.update_callback = update_callback
        self.frame_callback = frame_callback
        self.dilation = 1.0

        # counters
        self._elapsed = 0.0
        self._update_elapsed = 0.0
        self._frame_elapsed = 0.0
        self.frame_count = 0
        self.update_count = 0
        self._frames_skipped = 0
        self.time = self._get_ticks()
        self._last_update = self.time
        self._last_frame = self.time

        # schedules: trigger once per call to tick()
        # interval schedules: trigger on elapsed time
        # update schedules: trigger on update_ready
        # frames schedules: trigger on frame_ready
        self.schedules = []
        self.interval_schedules = []
        self.update_schedules = []
        self.frame_schedules = []
        self._need_sort = False  # for interval schedules only

        # stats
        self.tps = 0.0      # calls to tick() per second
        self.fps = 0.0      # frames per second
        self.ups = 0.0      # updates per second
        self.update_elapsed = 0.0
        self.frame_elapsed = 0.0
        self.update_ready = True
        self.frame_ready = True

    @property
    def ticks_per_second(self):
        """Get or set ticks per second."""
        return self._ticks_per_second
    @ticks_per_second.setter
    def ticks_per_second(self, n):
        if n > 0:
            self._ticks_per_second = n
        else:
            self._ticks_per_second = 25
        self._tick_step = 1.0 / self._ticks_per_second

    @property
    def max_fps(self):
        """Get or set max_fps."""
        return self._max_fps
    @max_fps.setter
    def max_fps(self, n):
        if n > 0:
            self._max_fps = n
            self._frame_step = 1.0 / n
        else:
            self._max_fps = 0
            self._frame_step = 0

    @property
    def use_wait(self):
        """Get or set use_wait."""
        return self._use_wait
    @use_wait.setter
    def use_wait(self, enabled):
        self._use_wait = enabled
        self._tps = float(self.max_fps)

    @property
    def max_frame_skip(self):
        """Get or set max_frame_skip."""
        return self._max_frame_skip
    @max_frame_skip.setter
    def max_frame_skip(self, n):
        if n > 0:
            self._max_frame_skip = n
        else:
            self._max_frame_skip = 0

    def tick(self):
        """Game loop timer. Call once per game loop to calculate runtime values.
        After calling, check the update_ready() and frame_ready() methods.
        Sleep cycles are injected if use_wait=True. Returns the number of
        milliseconds that have elapsed since the last call to tick()."""

        TIME = self._get_ticks()
        DT = self._ticks = (TIME - self.time) / self.dilation
        self._elapsed += self._ticks
        self.time = TIME

        # Update runtime stats and counters every second.
        if self._elapsed >= 1.0:
            self._elapsed %= 1.0
            # Save stats and clear counters.
            self.tps = 0.0
            self.fps = self.frame_count
            self.ups = self.update_count
            self.frame_count = self.update_count = 0

        # Process the time slice.
        self._tps += 1
        self._update_elapsed += DT
        self._frame_elapsed += DT
        self.update_ready = self.frame_ready = False

        if TIME >= self._last_update+self._tick_step*self.dilation:
            self.update_ready = True

        if self.max_fps == 0:
            self.frame_ready = True
        elif TIME >= self._last_frame+self._frame_step or \
            self._frames_skipped >= self.max_frame_skip:
            self.frame_ready = True
        elif self._use_wait and self.max_fps > 0:
            wait_sec = self._last_frame + self._frame_step - self._get_ticks()
            if wait_sec > 0.:
                self._wait(wait_sec)
            self.frame_ready = True

        # Schedules cycled every tick.
        for sched in self.schedules:
            sched.func(DT, *sched.args, **sched.kwargs)

        # Schedules cycled when their interval elapses.
        if self._need_sort:
            self.interval_schedules.sort(key=_IntervalItem.sort_key)
        self.need_sort = False
        for sched in self.interval_schedules:
            due = sched.lasttime + sched.interval*self.dilation
            if TIME >= due:
                drift = TIME - due
                if -0.5 < drift < 0.5:
                    dt = sched.interval
                else:
                    dt = TIME - sched.lasttime
                sched.func(dt/self.dilation, *sched.args, **sched.kwargs)
                sched.lasttime += dt * self.dilation
                self._need_sort = True
            else:
                break

        # Schedules cycled every update.
        if self.update_ready:
            # Flip the state variables.
            self.update_count += 1
            self._frames_skipped += 1
            self.update_elapsed = self._update_elapsed
            self._update_elapsed = 0.0
            # Reconcile if we're way too fast or slow.
            self._last_update += self._tick_step
            drift = self._tick_step / 5.0
            if not (TIME-drift < self._last_update < TIME+drift):
                self._last_update = TIME
            # Run the schedules.
            update_called = self.update_callback is None
            for sched in self.update_schedules:
                if update_called:
                    sched.func(self.update_elapsed, *sched.args, **sched.kwargs)
                else:
                    if sched.pri > 0.0:
                        self.update_callback(self.update_elapsed)
                        update_called = True
                    sched.func(self.update_elapsed, *sched.args, **sched.kwargs)
            if not update_called:
                self.update_callback(self.update_elapsed)

        # Schedules cycled every frame.
        if self.frame_ready:
            # Flip the state variables.
            self.frame_count += 1
            self._frames_skipped = 0
            self.frame_elapsed = self._frame_elapsed
            self._frame_elapsed = 0.0
            # Reconcile if we're way too fast or slow.
            if self._frame_step:
                self._last_frame += self._frame_step
                drift = self._frame_step * self.max_frame_skip
                if not (TIME-drift < self._last_frame < TIME+drift):
                    self._last_frame = TIME
            # Run the schedules.
            frame_called = self.frame_callback is None
            for sched in self.frame_schedules:
                if frame_called:
                    sched.func(self.frame_elapsed, *sched.args, **sched.kwargs)
                else:
                    if sched.pri > 0.0:
                        self.frame_callback(self.frame_elapsed)
                        frame_called = True
                    sched.func(self.frame_elapsed, *sched.args, **sched.kwargs)
            if not frame_called:
                self.frame_callback(self.frame_elapsed)

        return DT

    @property
    def interpolate(self):
        """Return a float representing the current position in between the
        previous gametick and the next one. This allows the main game loop to
        make predictive calculations between gameticks."""
        interp = (
            self._get_ticks() - self._last_update
        ) / self._tick_step / self.dilation
        if interp > 1.0:
            interp = 1.0
        return interp

    def get_time(self):
        """Return the milliseconds elapsed in the previous call to tick()."""
        return self._ticks

    def get_fps(self):
        """Return frames per second during the previous second."""
        return self.fps

    def get_ups(self):
        """Return updates per second during the previous second."""
        return self.ups

    def schedule(self, func, *args, **kwargs):
        """Schedule an item to be called back each time tick() is called."""
        self.unschedule(func)
        item = _Item(func, 0, args, kwargs)
        self.schedules.append(item)

    def schedule_update(self, func, *args, **kwargs):
        """Schedule an item to be called back each time update_ready is True."""
        self.unschedule(func)
        item = _Item(func, -1, args, kwargs)
        self.update_schedules.append(item)

    def schedule_update_priority(self, func, pri, *args, **kwargs):
        """Schedule an item to be called back each time update_ready is True.

        Items are called in order of priority, low to high. If the clock's
        update_callback is not None, its priority is always 0.0.
        """
        self.unschedule(func)
        new_item = _Item(func, pri, args, kwargs)
        for i,sched in enumerate(self.update_schedules):
            if sched.pri > new_item.pri:
                self.update_schedules.insert(i, new_item)
                return
        self.update_schedules.append(new_item)

    def schedule_frame(self, func, *args, **kwargs):
        """Schedule an item to be called back each time frame_ready is True."""
        self.unschedule(func)
        item = _Item(func, 0.0, args, kwargs)
        self.frame_schedules.append(item)

    def schedule_frame_priority(self, func, pri, *args, **kwargs):
        """Schedule an item to be called back each time frame_ready is True.

        Items are called in order of priority, low to high. If the clock's
        frame_callback is not None, its priority is always 0.0.
        """
        self.unschedule(func)
        new_item = _Item(func, pri, args, kwargs)
        for i,sched in enumerate(self.frame_schedules):
            if sched.pri > new_item.pri:
                self.frame_schedules.insert(i, new_item)
                return
        self.frame_schedules.append(new_item)

    def schedule_interval(self, func, interval, *args, **kwargs):
        """Schedule an item to be called back each time an interval elapses.

        Parameters:
            interval -> The time in seconds (float).
        """
        self.unschedule(func)
        item = _IntervalItem(func, interval, self._get_ticks(), args, kwargs)
        self.interval_schedules.append(item)
        self._need_sort = True

    def unschedule(self, func):
        """Unschedule a managed function."""
        for sched in (
            self.schedules, self.update_schedules, self.frame_schedules,
            self.interval_schedules,
        ):
            for item in list(sched):
                if item.func == func:
                    sched.remove(item)
