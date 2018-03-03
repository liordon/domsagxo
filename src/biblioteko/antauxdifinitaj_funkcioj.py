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
    def turnOnDevice(device):
        device.isTurnedOn = True

    performActionOnAllDevices(devices, smart_house_manager, turnOnDevice)


def performActionOnAllDevices(devices, smart_house_manager, action):
    for d in devices:
        if smart_house_manager.isApplianceName(d):
            device = smart_house_manager.getAppliance(d)
            action(device)
        elif smart_house_manager.isGroupName(d):
            device_group = smart_house_manager.getGroup(d)
            for device in device_group:
                action(device)


def createGroup(group_name, smart_house_manager):
    smart_house_manager.addGroup(group_name[0])


def removeGroup(group_name, smart_house_manager):
    smart_house_manager.removeGroup(group_name[0])


def putApplianceInGroup(appliance_and_group, smart_house_manager):
    smart_house_manager.addApplianceToGroup(appliance_and_group[0], appliance_and_group[1])


def moveAppliance(appliance_and_groups, smart_house_manager):
    if len(appliance_and_groups) < 3:
        raise ValueError("not enough arguments for moving appliance between groups")
    smart_house_manager.removeApplianceFromGroup(appliance_and_groups[0], appliance_and_groups[1])
    smart_house_manager.addApplianceToGroup(appliance_and_groups[0], appliance_and_groups[2])


def getApplianceProperty(appliance_and_property, smart_house_manager):
    return smart_house_manager \
        .getPropertyOfAppliance(appliance_and_property[0], appliance_and_property[1])


def setApplianceProperty(appliance_property_and_value, smart_house_manager):
    def setStateComponentAction(device):
        device.setStateComponent(appliance_property_and_value[1],
                                 appliance_property_and_value[2])
    performActionOnAllDevices([appliance_property_and_value[0]],
                              smart_house_manager, setStateComponentAction)
    # return smart_house_manager\
    #     .setPropertyOfAppliance(appliance_property_and_value[0],
    #                             appliance_property_and_value[1],
    #                             appliance_property_and_value[2])


method_dict = {"hazardu": generateRandom,
               "sxaltu": turnOnDevices,
               "aldonu": addAppliance}
