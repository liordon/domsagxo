from biblioteko.atomaj_tipoj import *
import sched
from datetime import datetime


class Domsagxo(object):
    """this class is a helper for the abstract syntax tree which holds all of the smart-home
    appliances and manages them."""

    appliance_type_group = {
        ApplianceTypes.SWITCH: "sxaltoj",
        ApplianceTypes.KNOB: "agordoj",
        ApplianceTypes.LIGHT: "lumoj",
        ApplianceTypes.THERMOSTAT: "termostatoj",
        ApplianceTypes.CAMERA: "bildiloj"
    }

    def __init__(self):
        self.groups = {}
        for appType in Domsagxo.appliance_type_group:
            self.groups[Domsagxo.appliance_type_group[appType]] = []
        self.appliances = {}

    def addAppliance(self, appliance):
        if appliance.name in self.appliances:
            raise ValueError("appliance named " + appliance.name + " already exists in this home.")
        self.appliances[appliance.name] = appliance
        if appliance.type in Domsagxo.appliance_type_group:
            self.groups[Domsagxo.appliance_type_group[appliance.type]].append(appliance)

    def addGroup(self, group_name):
        if self.isGroupName(group_name):
            raise KeyError("group name " + group_name + " is already taken")
        self.groups[group_name] = []

    def removeGroup(self, group_name):
        self.groups.pop(group_name)

    def recognizes(self, appliance_or_group_name):
        return (appliance_or_group_name in self.appliances) or \
               (appliance_or_group_name in self.groups)

    def isApplianceName(self, appliance_name):
        return appliance_name in self.appliances

    def isGroupName(self, group_name):
        return group_name in self.groups.keys()

    def getAppliance(self, appliance_name):
        return self.appliances[appliance_name]

    def getGroup(self, group_name):
        return self.groups[group_name]

    def addApplianceToGroup(self, appliance_name, group):
        appliance = self.appliances[appliance_name]
        self.groups[group].append(appliance)

    def removeApplianceFromGroup(self, appliance_name, group):
        appliance = self.appliances[appliance_name]
        self.groups[group].remove(appliance)

    def getPropertyOfAppliance(self, appliance_name, property_name):
        appliance = self.appliances[appliance_name]
        return appliance.state_components[property_name]

    def setPropertyOfAppliance(self, appliance_name, property_name, value):
        appliance = self.appliances[appliance_name]
        appliance.state_components[property_name] = value


class Horaro(object):

    def __init__(self, timefunc, delayfunc):
        self.scheduler = sched.scheduler(timefunc, delayfunc)

    def enter(self, delay, action, argument=(), kwargs={}):
        self.scheduler.enter(delay.totalSeconds(), 1, action, argument, kwargs)

    def enterAt(self, time_point, action, argument=(), kwargs={}):
        pass

    def run(self, blocking=True):
        self.scheduler.run(blocking)

    def runSetTime(self, amountOfTime):
        target_time = self.scheduler.timefunc() + amountOfTime.totalSeconds()
        while (not self.scheduler.empty()) and \
                (self.scheduler.queue[0][0] <= target_time):
            self.scheduler.delayfunc(self.scheduler.queue[0][0] - self.scheduler.timefunc())
            self.scheduler.run(False)
        self.scheduler.delayfunc(target_time - self.scheduler.timefunc())

    def getDate(self):
        return datetime.utcfromtimestamp(self.scheduler.timefunc())

    def repeat(self, delay, action):
        def repetition():
            action()
            self.scheduler.enter(delay.totalSeconds(), 1, repetition)
        self.scheduler.enter(delay.totalSeconds(), 1, repetition)
