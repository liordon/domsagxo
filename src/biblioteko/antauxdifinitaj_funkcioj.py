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
        for numerator in range(1, 9):
            appliance_name = digitNames[numerator] + "a " + appliance_type.value
            if not smart_house_manager.recognizes(appliance_name):
                break
    if smart_house_manager.recognizes(appliance_name):
        raise KeyError("appliance " + appliance_name + " already exists.")
    appliance = Appliance(appliance_type, appliance_name)
    smart_house_manager.addAppliance(appliance)


def renameAppliance(names, smart_house_manager):
    if smart_house_manager.recognizes(names[1]):
        raise KeyError("name " + names[1] + " is already taken")
    appliance = smart_house_manager.appliances[names[0]]
    appliance.name = names[1]
    smart_house_manager.appliances[names[1]] = appliance
    smart_house_manager.appliances.pop(names[0])


def turnOnDevices(devices, smart_house_manager):
    for d in devices:
        if smart_house_manager.isApplianceName(d):
            device = smart_house_manager.getApplianceOrGroup(d)
            device.isTurnedOn = True
        elif smart_house_manager.isGroupName(d):
            device = smart_house_manager.getApplianceOrGroup(d)
            for d in device:
                d.isTurnedOn = True


def createGroup(group_name, smart_house_manager):
    if group_name[0] in smart_house_manager.groups.keys():
        raise KeyError("group name " + group_name[0] + " is already taken")
    smart_house_manager.groups[group_name[0]] = []


def removeGroup(group_name, smart_house_manager):
    smart_house_manager.groups.pop(group_name[0])


def putApplianceInGroup(appliance_and_group, smart_house_manager):
    appliance = smart_house_manager.appliances[appliance_and_group[0]]
    smart_house_manager.groups[appliance_and_group[1]].append(appliance)


def moveAppliance(appliance_and_groups, smart_house_manager):
    appliance = smart_house_manager.appliances[appliance_and_groups[0]]
    smart_house_manager.groups[appliance_and_groups[1]].remove(appliance)
    smart_house_manager.groups[appliance_and_groups[2]].append(appliance)


method_dict = {"hazardu": generateRandom,
               "sxaltu": turnOnDevices,
               "aldonu": addAppliance}
