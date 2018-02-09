import pytest
from biblioteko.antauxdifinitaj_funkcioj import *
import biblioteko.atomaj_tipoj as tipo


# class TestRandomGeneration(object):

class TestTimePointGeneration(object):

    def test_canGenerateTotallyRandomTimePoint(self):
        time_point = generateRandom([Generate.TIME_POINT.value])
        assert isinstance(time_point, tipo.TimePoint)
        assert 24 > time_point.hour
        assert 60 > time_point.minutes

    def test_canGenerateConstrainedRandomTimePoint(self):
        time_point = generateRandom([Generate.TIME_POINT.value,
                                     tipo.TimePoint(9, 20),
                                     tipo.TimePoint(9, 50)])
        assert isinstance(time_point, tipo.TimePoint)
        assert 9 == time_point.hour
        assert 50 > time_point.minutes
        assert 20 <= time_point.minutes

    def test_canGenerateRandomTimePointWithOverflowToNextHour(self):
        time_point = generateRandom([Generate.TIME_POINT.value,
                                     tipo.TimePoint(12, 59),
                                     tipo.TimePoint(13, 50)])
        assert isinstance(time_point, tipo.TimePoint)
        assert (59 == time_point.minutes) if time_point.hour == 12 \
            else (13 == time_point.hour and 50 > time_point.minutes)


class TestTimeSpanGeneration(object):

    def test_canGenerateTotallyRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value])
        assert isinstance(time_span, tipo.TimeSpan) #TODO: infinite loop? kial?!

    def test_canGenerateConstrainedRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    TimeSpan(0, 0, 1),
                                    TimeSpan(0, 1, 0)])
        assert isinstance(time_span, tipo.TimeSpan)
        assert 0 == time_span.hours
        assert 1 >= time_span.minutes
        assert 60 > time_span.seconds
        assert 0 < time_span.seconds

    def test_canGenerateLargeConstrainedRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    TimeSpan(1, 0, 0),
                                    TimeSpan(2, 0, 0)])
        assert isinstance(time_span, tipo.TimeSpan)
        assert 1 == time_span.hours
        assert 0 <= time_span.minutes
        assert 60 > time_span.minutes
        assert 60 > time_span.seconds
        assert 0 <= time_span.seconds

    def test_canGenerateRandomTimeSpanWithOverflow(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    TimeSpan(0, 59, 0),
                                    TimeSpan(2, 0, 0)])
        assert isinstance(time_span, tipo.TimeSpan)
        assert 1 >= time_span.hours
        assert 0 <= time_span.minutes
        assert 60 > time_span.minutes
        assert 60 > time_span.seconds
        assert 0 <= time_span.seconds
