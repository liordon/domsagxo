from random import randrange
from biblioteko.atomaj_tipoj import *


def generateRandom(argList):
    if argList[0] == "horo":
        if len(argList) > 1:
            hour_range = argList[2].hour - argList[1].hour
            minute_range = argList[2].minutes - argList[1].minutes
            random_total_minutes = randrange(hour_range*60 + minute_range)
            random_hour = (random_total_minutes + argList[1].minutes) // 60 + argList[1].hour
            random_minute = (random_total_minutes + argList[1].minutes) % 60
        else:
            random_hour = randrange(0, 24)
            random_minute = randrange(0, 60)
        return TimePoint(random_minute, random_hour)

    if len(argList) > 1:
        return randrange(argList[1], argList[2])
    else:
        return randrange(100)

method_dict = {"hazardu":generateRandom}