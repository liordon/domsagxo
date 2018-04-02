import datetime
from random import randrange

from biblioteko.atomaj_tipoj import *


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
        return TimeSpan(randrange(0, 24), randrange(0, 60), randrange(0, 60))

    if len(argList) > 1:
        return randrange(argList[1], argList[2])
    else:
        return randrange(100)


def generateConstrainedTimeSpan(lower_bound, upper_bound):
    hour_range = upper_bound.hours - lower_bound.hours
    minute_range = upper_bound.minutes - lower_bound.minutes
    second_range = upper_bound.seconds - lower_bound.seconds
    random_total_seconds = randrange(0, hour_range * 3600
                                     + minute_range * 60
                                     + second_range)
    random_seconds = lower_bound.seconds + random_total_seconds % 60
    random_minutes = lower_bound.minutes + random_total_seconds // 60 % 60 \
                     + (1 if random_seconds >= 60 else 0)
    random_hours = lower_bound.hours + random_total_seconds // 3600 \
                   + (1 if random_minutes >= 60 else 0)

    return TimeSpan(hours=random_hours,
                    minutes=random_minutes % 60,
                    seconds=random_seconds % 60)
