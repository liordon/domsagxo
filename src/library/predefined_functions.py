import datetime
from random import randrange

from library.predefined_values import *

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


def generateRandom(type_to_generate, lower_bound=None, upper_bound=None):
    both_bounds_were_given = lower_bound is not None and upper_bound is not None
    if type_to_generate == RandomizableType.TIME_POINT.value:
        if both_bounds_were_given:
            hour_range = upper_bound.hour - lower_bound.hour
            minute_range = upper_bound.minute - lower_bound.minute
            random_total_minutes = randrange(hour_range * 60 + minute_range)
            random_hour = (random_total_minutes + lower_bound.minute) // 60 + lower_bound.hour
            random_minute = (random_total_minutes + lower_bound.minute) % 60
        else:
            random_hour = randrange(0, 24)
            random_minute = randrange(0, 60)
        return datetime.time(hour=random_hour, minute=random_minute)

    elif type_to_generate == RandomizableType.TIME_SPAN.value:
        if both_bounds_were_given:
            return generateConstrainedTimeSpan(lower_bound, upper_bound)
        return datetime.timedelta(hours=randrange(0, 24),
                                  minutes=randrange(0, 60),
                                  seconds=randrange(0, 60))

    if both_bounds_were_given:
        return randrange(lower_bound, upper_bound)
    else:
        return randrange(100)


def generateConstrainedTimeSpan(lower_bound, upper_bound):
    return datetime.timedelta(seconds=randrange(lower_bound.seconds, upper_bound.seconds))
