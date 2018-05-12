import datetime
from random import randrange

from library.atomic_types import *


class Generate(Enum):
    TIME_POINT = 'horo'
    TIME_SPAN = 'tempo'


digitNames = {
    1: "unu",
    2: "du",
    3: "tri",
    4: "kvar",
    5: "kvin",
    6: "ses",
    7: "sep",
    8: "ok",
    9: "naux"
}


def generateRandom(argList):
    if argList[0] == Generate.TIME_POINT.value:
        if len(argList) > 1:
            hour_range = argList[2].hour - argList[1].hour
            minute_range = argList[2].minute - argList[1].minute
            random_total_minutes = randrange(hour_range * 60 + minute_range)
            random_hour = (random_total_minutes + argList[1].minute) // 60 + argList[1].hour
            random_minute = (random_total_minutes + argList[1].minute) % 60
        else:
            random_hour = randrange(0, 24)
            random_minute = randrange(0, 60)
        return datetime.time(hour=random_hour, minute=random_minute)

    elif argList[0] == Generate.TIME_SPAN.value:
        if len(argList) > 1:
            return generateConstrainedTimeSpan(argList[1], argList[2])
        return datetime.timedelta(hours=randrange(0, 24),
                                  minutes=randrange(0, 60),
                                  seconds=randrange(0, 60))

    if len(argList) > 1:
        return randrange(argList[1], argList[2])
    else:
        return randrange(100)


def generateConstrainedTimeSpan(lower_bound, upper_bound):
    return datetime.timedelta(seconds=randrange(lower_bound.seconds,upper_bound.seconds))
