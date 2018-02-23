from random import randrange
from biblioteko.atomaj_tipoj import *


class Generate(Enum):
    TIME_POINT = 'horo'
    TIME_SPAN = 'tempo'


def generateRandom(argList, smart_house_manager=None):
    if argList[0] == Generate.TIME_POINT.value:
        if len(argList) > 1:
            hour_range = argList[2].hour - argList[1].hour
            minute_range = argList[2].minutes - argList[1].minutes
            random_total_minutes = randrange(hour_range * 60 + minute_range)
            random_hour = (random_total_minutes + argList[1].minutes) // 60 + argList[1].hour
            random_minute = (random_total_minutes + argList[1].minutes) % 60
        else:
            random_hour = randrange(0, 24)
            random_minute = randrange(0, 60)
        return TimePoint(random_hour, random_minute)

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

    return TimeSpan(random_hours,
                    random_minutes % 60,
                    random_seconds % 60)


def addAppliance(device_parameters, smart_house_manager):
    appliance_type = ApplianceTypes(device_parameters[0])
    if len(device_parameters) > 1:
        appliance_name = device_parameters[1]
    else:
        appliance_name = appliance_type.value
    if smart_house_manager.recognizes(appliance_name):
        raise ValueError("appliance " + appliance_name + " already exists.")
    appliance = Appliance(appliance_type, appliance_name)
    smart_house_manager.addAppliance(appliance)


def turnOnDevices(devices, smart_house_manager=None):
    for device in devices:
        if type(device) is list:
            for d in device:
                d.isTurnedOn = True
        else:
            device.isTurnedOn = True


method_dict = {"hazardu": generateRandom,
               "sxaltu": turnOnDevices,
               "aldonu": addAppliance}
