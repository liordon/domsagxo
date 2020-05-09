import datetime

import pytest

from domsagxo_test.test_utils.providers import TimeManagerWithSimulativeClockProvided


class TestTimedActions(TimeManagerWithSimulativeClockProvided):

    def test_canRunSetAmountOfTime(self, scd):
        scd.runSetTime(datetime.timedelta(seconds=3))

        assert 3 == scd.currentTime()

    def test_canScheduleAMutationToAVariable(self, scd, one_sec, increaser):
        scd.enter(one_sec, increaser)

        assert 0 == self.counter
        scd.runSetTime(one_sec)
        assert 1 == self.counter

    def test_ifRunForSetTimeAllActionedTimedInBetweenAreExecuted(self, scd, increaser):
        scd.enter(datetime.timedelta(seconds=1), increaser)
        scd.enter(datetime.timedelta(seconds=2), increaser)

        scd.runSetTime(datetime.timedelta(seconds=3))
        assert 2 == self.counter

    def test_eventCanBeScheduledForLaterDay(self, scd, one_sec, one_day, increaser):
        scd.enter(one_day, increaser)

        assert 0 == self.counter
        scd.runSetTime(one_sec)
        assert 0 == self.counter
        scd.runSetTime(one_day)
        assert 1 == self.counter

    def test_eventCanBeScheduledToRepeat(self, scd, one_sec, increaser):
        scd.startAtIntervalRepeatAtInterval(one_sec, increaser)
        assert 0 == self.counter
        scd.runSetTime(one_sec)
        assert 1 == self.counter
        scd.runSetTime(datetime.timedelta(seconds=2))
        assert 3 == self.counter

    def test_eventCanRepeatInDifferentIntervals(self, scd, increaser):
        scd.startAtIntervalRepeatAtInterval(datetime.timedelta(seconds=2), increaser)
        scd.runSetTime(datetime.timedelta(seconds=4))
        assert 2 == self.counter

    def test_eventCanBeScheduledToDifferentTimeUnits(self, scd, one_min, increaser):
        scd.startAtIntervalRepeatAtInterval(one_min, increaser)
        scd.runSetTime(datetime.timedelta(minutes=3))
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
        scd.enter(datetime.time(6, 00), increaser)
        scd.runSetTime(datetime.timedelta(hours=5, minutes=59, seconds=59))

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(seconds=1))
        assert 1 == self.counter

    def test_eventScheduledForPastTimeTodayIsOverflowedToTomorrow(self, scd, increaser):
        scd.runSetTime(datetime.timedelta(hours=5))
        scd.enter(datetime.time(4, 00), increaser)

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(hours=23))
        assert 1 == self.counter

    def test_repeatingTaskScheduledForPastTimeTodayIsOverflowedToTomorrow(self, scd, increaser):
        scd.runSetTime(datetime.timedelta(hours=5))
        scd.startAtTimeRepeatAtInterval(datetime.time(4, 00),
            datetime.timedelta(seconds=24 * 60 * 60), increaser)

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(hours=23))
        assert 1 == self.counter
        scd.runSetTime(datetime.timedelta(hours=24))
        assert 2 == self.counter

    def test_taskCanBeScheduledForFutureDate(self, scd, dawn_of_time, increaser):
        scd.enter(datetime.datetime(year=dawn_of_time.year,
            month=dawn_of_time.month,
            day=3, hour=4), increaser)
        scd.runSetTime(datetime.timedelta(hours=4))

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(days=3))
        assert 1 == self.counter

    def test_taskScheduledToPastTimeWillThrowException(self, scd, increaser):
        prehistory = datetime.datetime.utcfromtimestamp(-1)
        with pytest.raises(ValueError):
            scd.enter(prehistory, increaser)

    def test_repeatAtCustomTime(self, scd, increaser):
        scd.runSetTime(datetime.timedelta(hours=5))
        scd.startAtTimeRepeatAtInterval(datetime.time(4, 00),
            datetime.timedelta(hours=2, minutes=10), increaser)

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(hours=1, minutes=10))
        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(hours=21, minutes=50))
        assert 1 == self.counter
        scd.runSetTime(datetime.timedelta(hours=2, minutes=10))
        assert 2 == self.counter

    def test_canDoActionByTrigger(self, scd, increaser):
        def trigger_function():
            return action_time < scd.getDate()

        action_time = scd.getDate() + datetime.timedelta(seconds=2)

        scd.enterAtTrigger(trigger_function, increaser)

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(seconds=3))
        assert 1 == self.counter

    def test_canDoTwoActionsByTrigger(self, scd, increaser):
        def trigger_function1():
            return action_time < scd.getDate()

        def trigger_function2():
            return (scd.getDate().time() > datetime.time(second=2)) and (
                    scd.getDate().time() < datetime.time(hour=12, second=2))

        action_time = scd.getDate() + datetime.timedelta(seconds=2)

        scd.enterAtTrigger(trigger_function1, increaser)
        scd.enterAtTrigger(trigger_function2, increaser)

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(seconds=3))
        assert 2 == self.counter

    def test_repeatsActionByTrigger(self, scd, increaser):
        def trigger_function():
            if (scd.getDate().time() > datetime.time(second=2)) and (
                    scd.getDate().time() < datetime.time(second=5)):
                return True
            return (scd.getDate().time() > datetime.time(second=10)) and (
                    scd.getDate().time() < datetime.time(second=15))

        scd.repeatAtTrigger(trigger_function, increaser)

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(seconds=3))
        assert 1 == self.counter
        scd.runSetTime(datetime.timedelta(seconds=7))
        assert 1 == self.counter
        scd.runSetTime(datetime.timedelta(seconds=1))
        assert 2 == self.counter

    def test_eventCancellationByTime(self, scd, increaser):
        scd.runSetTime(datetime.timedelta(hours=5))
        scd.enter(datetime.time(4, 00), increaser)
        scd.enter(datetime.time(8, 00), increaser)

        scd.cancelEventByTime(datetime.time(hour=4))

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(hours=3))
        assert 1 == self.counter
        scd.runSetTime(datetime.timedelta(hours=21))
        assert 1 == self.counter

    def test_unscheduledEventCancellationGivesError(self, scd, increaser):
        scd.runSetTime(datetime.timedelta(hours=5))
        scd.enter(datetime.time(4, 00), increaser)
        scd.enter(datetime.time(8, 00), increaser)

        with pytest.raises(ValueError):
            scd.cancelEventByTime(datetime.time(hour=3))

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(hours=3))
        assert 1 == self.counter
        scd.runSetTime(datetime.timedelta(hours=21))
        assert 2 == self.counter

    def test_triggeredEventsCanBeCancelled(self, scd, increaser):
        def trigger_function():
            return (scd.getDate().time() > datetime.time(hour=5, second=2)) and (
                    scd.getDate().time() < datetime.time(hour=17, second=2))

        scd.repeatAtTrigger(trigger_function, increaser)

        scd.runSetTime(datetime.timedelta(hours=5))
        scd.enter(datetime.time(8, 00), increaser)

        assert 0 == self.counter
        scd.runSetTime(datetime.timedelta(hours=3))
        scd.cancelAllTriggeredEvents()
        assert 2 == self.counter
        scd.runSetTime(datetime.timedelta(hours=21))
        assert 2 == self.counter
