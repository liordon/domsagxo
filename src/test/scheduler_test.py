import pytest
from biblioteko.atomaj_tipoj import *
import kompilajxo.leksisto as lxr
import kompilajxo.abstrakta_sintaksarbo as ast_bld
import datetime
import time
from apscheduler.schedulers.background import BackgroundScheduler

from biblioteko.estra_komponantoj import Horaro


class TimeManagerProvided(object):

    def incTime(self, amount):
        if amount < 0:
            raise ValueError("sleep length must be non-negative")
        self.schedule_time += amount

    @pytest.fixture
    def scd(self):
        self.strt = time.time()
        self.schedule_time = 0
        return Horaro(timefunc=lambda: self.schedule_time, delayfunc=self.incTime)

    @pytest.fixture
    def increaser(self):
        self.counter = 0

        def increase_counter(num=1):
            self.counter += num

        return increase_counter

    @pytest.fixture
    def one_sec(self):
        return TimeSpan(seconds=1)

    @pytest.fixture
    def one_min(self):
        return TimeSpan(minutes=1)

    @pytest.fixture
    def one_day(self):
        return TimeSpan(days=1)

    @pytest.fixture
    def dawn_of_time(self):
        return datetime.datetime.utcfromtimestamp(0)


class TestTimedActions(TimeManagerProvided):

    def test_canRunSetAmountOfTime(self, scd):
        scd.runSetTime(TimeSpan(seconds=3))

        assert 3 == self.schedule_time

    def test_canScheduleAMutationToAVariable(self, scd, one_sec, increaser):
        scd.enter(one_sec, increaser)

        assert 0 == self.counter
        scd.runSetTime(one_sec)
        assert 1 == self.counter

    def test_ifRunForSetTimeAllActionedTimedInBetweenAreExecuted(self, scd, increaser):
        scd.enter(TimeSpan(seconds=1), increaser)
        scd.enter(TimeSpan(seconds=2), increaser)

        scd.runSetTime(TimeSpan(seconds=3))
        assert 2 == self.counter

    def test_eventCanBeScheduledForLaterDay(self, scd, one_sec, one_day, increaser):
        scd.enter(one_day, increaser)

        assert 0 == self.counter
        scd.runSetTime(one_sec)
        assert 0 == self.counter
        scd.runSetTime(one_day)
        assert 1 == self.counter

    def test_eventCanBeScheduledToRepeat(self, scd, one_sec, increaser):
        scd.repeat(one_sec, increaser)
        assert 0 == self.counter
        scd.runSetTime(one_sec)
        assert 1 == self.counter
        scd.runSetTime(TimeSpan(seconds=2))
        assert 3 == self.counter

    def test_eventCanRepeatInDifferentIntervals(self, scd, increaser):
        scd.repeat(TimeSpan(seconds=2), increaser)
        scd.runSetTime(TimeSpan(seconds=4))
        assert 2 == self.counter

    def test_eventCanBeScheduledToDifferentTimeUnits(self, scd, one_min, increaser):
        scd.repeat(one_min, increaser)
        scd.runSetTime(TimeSpan(minutes=3))
        assert 3 == self.counter

    def test_mockSchedulerStartsAt0UnixTime(self, scd, dawn_of_time):
        zero_day = scd.getDate()

        assert dawn_of_time.year == zero_day.year
        assert dawn_of_time.month == zero_day.month
        assert dawn_of_time.day == zero_day.day
        assert dawn_of_time.hour == zero_day.hour
        assert dawn_of_time.minute == zero_day.minute
        assert dawn_of_time.second == zero_day.second

    def test_mockSchedulerAdvancesAccordingToSpecifiedTime(self, scd, dawn_of_time,
                                                           one_sec, one_min, one_day):
        scd.runSetTime(one_sec)
        scd.runSetTime(one_min)
        scd.runSetTime(one_day)

        current_day = scd.getDate()

        assert dawn_of_time.year == current_day.year
        assert dawn_of_time.month == current_day.month
        assert dawn_of_time.day + 1 == current_day.day
        assert dawn_of_time.hour == current_day.hour
        assert dawn_of_time.minute + 1 == current_day.minute
        assert dawn_of_time.second + 1 == current_day.second

    def test_eventCanBeScheduledToTimeOfDay(self, scd, increaser):
        scd.enterAtTimeOfDay(datetime.time(6, 00), increaser)
        scd.runSetTime(TimeSpan(hours=5, minutes=59, seconds=59))

        assert 0 == self.counter
        scd.runSetTime(TimeSpan(seconds=1))
        assert 1 == self.counter

    def test_eventScheduledForPastTimeTodayIsOverflowedToTomorrow(self, scd, increaser):
        scd.runSetTime(TimeSpan(hours=5))
        scd.enterAtTimeOfDay(datetime.time(4, 00), increaser)
        # scd.runSetTime(TimeSpan(hours=1))

        assert 0 == self.counter
        scd.runSetTime(TimeSpan(hours=23))
        assert 1 == self.counter

    def test_repeatingTaskScheduledForPastTimeTodayIsOverflowedToTomorrow(self, scd, increaser):
        scd.runSetTime(TimeSpan(hours=5))
        scd.repeatAt(datetime.time(4, 00), increaser)
        # scd.runSetTime(TimeSpan(hours=1))

        assert 0 == self.counter
        scd.runSetTime(TimeSpan(hours=23))
        assert 1 == self.counter
        scd.runSetTime(TimeSpan(hours=24))
        assert 2 == self.counter

    def test_taskCanBeScheduledForFutureDate(self, scd, dawn_of_time, increaser):
        scd.enterAtFutureTime(datetime.datetime(year=dawn_of_time.year,
                                                month=dawn_of_time.month,
                                                day=3, hour=4), increaser)
        scd.runSetTime(TimeSpan(hours=4))

        assert 0 == self.counter
        scd.runSetTime(TimeSpan(days=3))
        assert 1 == self.counter

    def test_taskScheduledToPastTimeWillThrowException(self, scd, increaser):
        prehistory = datetime.datetime.utcfromtimestamp(-1)
        with pytest.raises(ValueError):
            scd.enterAtFutureTime(prehistory, increaser)
