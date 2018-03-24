import pytest
from biblioteko.atomaj_tipoj import *
import kompilajxo.leksisto as lxr
import kompilajxo.abstrakta_sintaksarbo as ast_bld
import datetime
from apscheduler.schedulers.background import BackgroundScheduler

from biblioteko.estra_komponantoj import Horaro


class TimeManagerProvided(object):

    def incTime(self, amount):
        self.schedule_time += amount

    @pytest.fixture
    def scd(self):
        self.schedule_time = 0
        scheduler = BackgroundScheduler()
        return Horaro(timefunc=lambda: self.schedule_time,
                      delayfunc=self.incTime)

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

    def test_mockSchedulerStartsAt0UnixTime(self, scd):
        zero_day = scd.getDate()

        assert 1970 == zero_day.year
        assert 1 == zero_day.month
        assert 1 == zero_day.day
        assert 0 == zero_day.hour
        assert 0 == zero_day.minute
        assert 0 == zero_day.second

    def test_mockSchedulerAdvancesAccordingToSpecifiedTime(self, scd, one_sec, one_min, one_day):
        scd.runSetTime(one_sec)
        scd.runSetTime(one_min)
        scd.runSetTime(one_day)

        current_day = scd.getDate()

        assert 1970 == current_day.year
        assert 1 == current_day.month
        assert 2 == current_day.day
        assert 0 == current_day.hour
        assert 1 == current_day.minute
        assert 1 == current_day.second

    # def test_eventCanBeScheduledToTimeOfDay(self, scd, increaser):
    #     scd.enterAt(TimePoint(6,00), increaser)
    #     scd.runSetTime(TimeSpan(hours=5, minutes=59, seconds=59))
    #
    #     assert 0 == self.counter
    #     scd.runSetTime(TimeSpan(seconds=1))
    #     assert 1 == self.counter
