from biblioteko.atomaj_tipoj import *
from biblioteko.antauxdifinitaj_funkcioj import *
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
        self.variables = {}
        self.method_dict = {"hazardu": generateRandom,
                            "sxaltu": turnOnDevices,
                            "aldonu": addAppliance}
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
        seconds = delay.total_seconds()
        if seconds > 0:
            self.scheduler.enter(seconds, 1, action, argument, kwargs)
        else:
            raise ValueError("cannot schedule events for past time.")

    def enterAtTimeOfDay(self, time_point, action, argument=(), kwargs={}):
        day_offset = 0
        if time_point < self.getDate().time():
            day_offset = 1
        scheduled_time = datetime(self.getDate().year,
                                  self.getDate().month,
                                  self.getDate().day + day_offset,
                                  time_point.hour,
                                  time_point.minute)
        self.enter(scheduled_time - self.getDate(), action, argument, kwargs)

    def enterAtFutureTime(self, date_time_point, action):
        self.enter(date_time_point - self.getDate(), action)

    def run(self, blocking=True):
        self.scheduler.run(blocking)

    def runSetTime(self, amountOfTime):
        target_time = self.scheduler.timefunc() + amountOfTime.total_seconds()
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
            self.scheduler.enter(delay.total_seconds(), 1, repetition)

        self.scheduler.enter(delay.total_seconds(), 1, repetition)

    def repeatAt(self, time_point, action):
        def repetition():
            action()
            self.scheduler.enter(24 * 60 * 60, 1, repetition)

        self.enterAtTimeOfDay(time_point, repetition)
