import sched

import library.predefined_values
from library.atomic_types import Appliance
from library.predefined_functions import *


class Domsagxo(object):
    """this class is a helper for the abstract syntax tree which holds all of the smart-home
    appliances and manages them."""

    appliance_type_group = {
        ApplianceTypes.SWITCH    : "sxaltoj",
        ApplianceTypes.KNOB      : "agordoj",
        ApplianceTypes.LIGHT     : "lumoj",
        ApplianceTypes.THERMOSTAT: "termostatoj",
        ApplianceTypes.CAMERA    : "fotiloj"
    }

    def __init__(self, scheduler=None):
        self.variables = {}
        self.method_dict = {
            "hazardu": generateRandom,
            "sxaltu" : self.requestDeviceActivation,
            "aldonu" : self.requestDeviceAddition
        }
        self.variables = {}
        for enum in [cls for cls in library.predefined_values.all_categories]:
            for predefined_word in enum:
                self.variables[predefined_word.value] = predefined_word.value

        # for appType in Domsagxo.appliance_type_group:
        #     self.variables[Domsagxo.appliance_type_group[appType]] = []
        for appType in library.predefined_values.ApplianceTypes:
            self.variables[appType.value + "j"] = []
        Domsagxo.number_of_reserved_words = len(self.variables)
        self.scheduler = scheduler

    def addAppliance(self, appliance):
        if appliance.name in self.variables:
            raise ValueError("appliance named " + appliance.name + " already exists in this home.")
        self.variables[appliance.name] = appliance
        if appliance.type in Domsagxo.appliance_type_group:
            # self.variables[Domsagxo.appliance_type_group[appliance.type]].append(appliance)
            self.variables[Domsagxo.appliance_type_group[appliance.type]].append(appliance)

    def addGroup(self, group_name):
        if self.isGroupName(group_name):
            raise KeyError("group name " + group_name + " is already taken")
        self.variables[group_name] = []

    def removeGroup(self, group_name):
        self.variables.pop(group_name)

    def recognizes(self, appliance_or_group_name):
        return (appliance_or_group_name in self.variables) or \
               (appliance_or_group_name in self.variables)

    def isApplianceName(self, appliance_name):
        return appliance_name in self.variables

    def isGroupName(self, group_name):
        return group_name in self.variables.keys()

    def getAppliance(self, appliance_name):
        return self.variables[appliance_name]

    def getGroup(self, group_name):
        return self.variables[group_name]

    def addApplianceToGroup(self, appliance_name, group):
        appliance = self.variables[appliance_name]
        self.variables[group].append(appliance)

    def removeApplianceFromGroup(self, appliance_name, group):
        appliance = self.variables[appliance_name]
        self.variables[group].remove(appliance)

    def getPropertyOfAppliance(self, appliance_name, property_name):
        appliance = self.variables[appliance_name]
        return appliance.properties[property_name]

    def setPropertyOfAppliance(self, appliance_name, property_name, value):
        appliance = self.variables[appliance_name]
        appliance.properties[property_name] = value

    def requestDeviceAddition(self, appliance_type, appliance_name=None):
        if appliance_name is None:
            for numerator in range(1, 9):
                appliance_name = digitNames[numerator] + "a " + appliance_type
                if not self.recognizes(appliance_name):
                    break
        if self.recognizes(appliance_name):
            raise KeyError("appliance " + appliance_name + " already exists.")
        appliance = Appliance(ApplianceTypes(appliance_type), appliance_name)
        self.addAppliance(appliance)

    def requestDeviceActivation(self, devices):
        def turnOnDevice(device):
            device.isTurnedOn = True

        self.performActionOnAllDevices(devices, turnOnDevice)

    def requestChangeToDeviceProperty(self, device, property, value):
        def setDeviceProperty(device):
            device.properties[property] = value

        self.performActionOnAllDevices(device, setDeviceProperty)

    def performActionOnAllDevices(self, devices, action):
        for d in devices:
            if isinstance(d, Appliance):
                # if self.isApplianceName(d):
                #     device = self.getAppliance(d)
                action(d)
            elif isinstance(d, list):
                # device_group = self.getGroup(d)
                for device in d:
                    action(device)

    def renameAppliance(self, old_name, new_name):
        if self.recognizes(new_name):
            raise KeyError("name " + new_name + " is already taken")
        appliance = self.variables[old_name]
        appliance.name = new_name
        self.variables[new_name] = appliance
        self.variables.pop(old_name)


class Horaro(object):

    def __init__(self, time_function, delay_function):
        """receives a function that tells the time in each invocation, and a function that
        enables a wait for desired amount of time. returns a Horaro scheduler"""
        self.scheduler = sched.scheduler(time_function, delay_function)
        self.time_check_interval = 1

    def currentTime(self):
        return self.scheduler.timefunc()

    def runSetTime(self, amountOfTime):
        target_time = self.scheduler.timefunc() + amountOfTime.total_seconds()
        while (not self.scheduler.empty()) and \
                (self.scheduler.queue[0][0] <= target_time):
            self.scheduler.delayfunc(self.scheduler.queue[0][0] - self.scheduler.timefunc())
            self.scheduler.run(False)
        self.scheduler.delayfunc(target_time - self.scheduler.timefunc())

    def runUntil(self, time_point):
        time_amount = time_point - self.getDate()
        self.runSetTime(time_amount)

    def getDate(self):
        """returns the current time in datetime format"""
        return datetime.datetime.utcfromtimestamp(self.scheduler.timefunc())

    def timeToSeconds(self, time_point):
        """returns delay in seconds (default for the enter function) from the current time"""
        now = self.getDate()
        if isinstance(time_point, datetime.time):
            delay = datetime.timedelta(hours=time_point.hour - now.hour,
                                       minutes=time_point.minute - now.minute,
                                       seconds=time_point.second - now.second)
            if delay.total_seconds() > 0:
                return delay.total_seconds()
            return delay.total_seconds() + 24 * 60 * 60
        elif isinstance(time_point, datetime.datetime):
            return (time_point - now).total_seconds()
        elif isinstance(time_point, datetime.timedelta):
            return time_point.total_seconds()
        else:
            return time_point

    def run(self, blocking=True):
        self.scheduler.run(blocking)

    def enter(self, time_point_or_date, action, argument=(), kwargs=None):
        """redefine the sched.scheduler enter function"""
        if kwargs is None:
            kwargs = {}
        delay = self.timeToSeconds(time_point_or_date)
        if delay > 0:
            self.scheduler.enter(delay, 1, action, *argument, **kwargs)
        else:
            raise ValueError("cannot schedule events for past time.")

    def startAtIntervalRepeatAtInterval(self, interval, action, argument=(), kwargs=None):
        """performs an action after interval and repeats at intervals"""
        if kwargs is None:
            kwargs = {}

        def repetition():
            action(*argument, **kwargs)
            self.enter(interval, repetition)

        self.enter(interval, repetition)

    def startAtTimeRepeatAtInterval(self, time_point, interval, action, argument=(), kwargs=None):
        """performs an action after delay and repeats at intervals determined by delay."""
        if kwargs is None:
            kwargs = {}

        def repetition():
            action(*argument, **kwargs)
            self.enter(interval.total_seconds(), repetition)

        self.enter(time_point, repetition)

    def enterAtTrigger(self, triggerFunc, action, argument=(), kwargs=None):
        """performs the action action when triggerFunc() becomes True i.e.
        returns True for the first time."""
        if kwargs is None:
            kwargs = {}

        def triggerCheck():
            if triggerFunc():
                action(*argument, **kwargs)
                return True
            self.enter(self.time_check_interval, triggerCheck)

        self.enter(self.time_check_interval, triggerCheck)

    def repeatAtTrigger(self, triggerFunc, action, argument=(), kwargs=None):
        """performs the action action when triggerFunc() becomes True, waits for it to be False and
        repeats this process."""
        if kwargs is None:
            kwargs = {}

        def triggerCheck():
            if triggerFunc():
                action(*argument, **kwargs)
                self.enter(self.time_check_interval, unTriggerCheck)
                return
            self.enter(self.time_check_interval, triggerCheck)

        def unTriggerCheck():
            if not triggerFunc():
                self.enter(self.time_check_interval, triggerCheck)
                return
            self.enter(self.time_check_interval, unTriggerCheck)

        self.enter(self.time_check_interval, triggerCheck)

    def cancelEventByTime(self, time_point):
        """Cancels a past scheduled event. If no event exist return error msg. time_point can be
        any datetime object."""
        delay = self.timeToSeconds(time_point)
        for event in self.scheduler.queue:
            if abs(event[0] - self.scheduler.timefunc() - delay) < self.time_check_interval:
                self.scheduler.cancel(event)
                return
        raise ValueError("Cannot cancel unscheduled event")

    def cancelAllTriggeredEvents(self):
        """Cancels all past scheduled triggered events."""
        for event in self.scheduler.queue:
            if abs(event[0] - self.scheduler.timefunc()) <= self.time_check_interval:
                self.scheduler.cancel(event)
