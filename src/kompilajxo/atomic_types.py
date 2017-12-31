from enum import Enum


class TimeUnits(Enum):
    HOUR = "horo"
    MINUTE = "minuto"
    SECOND = "sekundo"


class TimePoint(object):
    def __init__(self, minutes, hour):
        self.minutes = minutes
        self.hour = hour


class TimeSpan(object):
    def __init__(self, hours=0, minutes=0, seconds=0):
        self.seconds = seconds
        self.minutes = minutes
        self.hours = hours

    def addFraction(self, frac):
        if self.seconds > 0:
            res = TimeSpan(0, 0, frac)
        elif self.minutes > 0:
            res = TimeSpan(0, 0, 60 * frac)
        else:
            res = TimeSpan(0, 60 * frac, 0)

        return self.unite(self, res)

    @classmethod
    def unite(cls, span1, span2):
        return TimeSpan(span1.hours + span2.hours,
                        span1.minutes + span2.minutes,
                        span1.seconds + span2.seconds)

    @classmethod
    def uniteTuple(cls, tuple1, tuple2):
        return (tuple1[0] + tuple2[0],
                tuple1[1] + tuple2[1],
                tuple1[2] + tuple2[2])