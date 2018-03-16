import pytest
import biblioteko.atomaj_tipoj as tipo
import kompilajxo.leksisto as lxr
import kompilajxo.abstrakta_sintaksarbo as ast_bld
import sched
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


class TestTimedActions(TimeManagerProvided):

    def test_canRunSetAmountOfTime(self, scd):
        scd.runSetTime(3)

        assert 3 == self.schedule_time

    def test_canScheduleAMutationToAVariable(self, scd, increaser):
        scd.enter(1, increaser)

        assert 0 == self.counter
        scd.runSetTime(1)
        assert 1 == self.counter

    def test_ifRunForSetTimeAllActionedTimedInBetweenAreExecuted(self, scd, increaser):
        scd.enter(1, increaser)
        scd.enter(2, increaser)

        scd.runSetTime(3)
        assert 2 == self.counter

    def test_eventCanBeScheduledForLaterDay(self, scd, increaser):
        scd.enterInTimeUnits(1, tipo.TimeUnits.DAY, increaser)

        assert 0 == self.counter
        scd.runSetTime(1)
        assert 0 == self.counter
        scd.runSetTime(1, tipo.TimeUnits.DAY)
        assert 1 == self.counter

    def test_eventCanBeScheduledToRepeat(self, scd, increaser):
        scd.repeat(1, tipo.TimeUnits.SECOND, increaser)
        assert 0 == self.counter
        scd.runSetTime(1)
        assert 1 == self.counter
        scd.runSetTime(2)
        assert 3 == self.counter

    def test_eventCanRepeatInDifferentIntervals(self, scd, increaser):
        scd.repeat(2, tipo.TimeUnits.SECOND, increaser)
        scd.runSetTime(4)
        assert 2 == self.counter

    def test_eventCanBeScheduledToDifferentTimeUnits(self, scd, increaser):
        scd.repeat(1, tipo.TimeUnits.MINUTE, increaser)
        scd.runSetTime(3, tipo.TimeUnits.MINUTE)
        assert 3 == self.counter

    # def test_eventCanBeScheduledToTimeOfDay(self, scd, increaser):
    #     scd.enterAt(tipo.TimePoint(6,00))