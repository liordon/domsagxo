from biblioteko.atomaj_tipoj import *
from biblioteko.antauxdifinitaj_funkcioj import *
import sched
import datetime


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
        """create the class object"""
        self.scheduler = sched.scheduler(timefunc, delayfunc)
        self.time_check_interval = 1


    def runSetTime(self, amountOfTime):
        target_time = self.scheduler.timefunc() + amountOfTime.total_seconds()
        while (not self.scheduler.empty()) and \
                (self.scheduler.queue[0][0] <= target_time):
            self.scheduler.delayfunc(self.scheduler.queue[0][0] - self.scheduler.timefunc())
            self.scheduler.run(False)
        self.scheduler.delayfunc(target_time - self.scheduler.timefunc())
    
    def getDate(self):
        """returns the current time in datetime format"""
        return datetime.datetime.utcfromtimestamp(self.scheduler.timefunc())

    def timeEdit(self, time_point):
        """returns delay in seconds (default for the enter function) from the current time"""
        now = self.getDate()
        if isinstance(time_point, datetime.time):
            #if time_point < current daytime then schedual for tommorow
            day_offset = 0
            if time_point < now.time():
                day_offset = 1
            #calculates the absolute time for the action in datetime.datetime format
            absolute_action_time = datetime.datetime(now.year, now.month, now.day + day_offset,\
                                    time_point.hour, time_point.minute, time_point.second)
            return (absolute_action_time-now).total_seconds()
        elif isinstance(time_point, datetime.datetime):
            return (time_point - now).total_seconds()
        elif isinstance(time_point, datetime.timedelta):
            return time_point.total_seconds()
        else:
            return time_point

    def run(self, blocking=True):
        self.scheduler.run(blocking)

    def enter(self, time_point, action, argument=(), kwargs={}):
        """redefine the enter function"""
        delay = self.timeEdit(time_point)
        if delay > 0:
            self.scheduler.enter(delay, 1, action, *argument, **kwargs)
        else:
            raise ValueError("cannot schedule events for past time.")

    def repeatSnooze(self, interval, action, argument=(), kwargs={}):
        """performs an action after interval and repeats at intervals"""
        def repetition():
            action(*argument, **kwargs)
            self.enter(interval, repetition)

        self.enter(interval, repetition)

    def repeatAtTime(self, time_point, interval, action, argument=(), kwargs={}):
        """performs an action after delay and repeats at intervals determined by delay"""
        def repetition():
            action(*argument, **kwargs)
            self.enter(interval.total_seconds(), repetition)

        self.enter(time_point,  repetition)

    def enterAtTrigger(self, triggerFunc, action, argument=(), kwargs={}):
        """performs the action action when triggerFunc() == True"""
        def triggerCheck():
            if triggerFunc():
                action(*argument, **kwargs)
                return True
            self.enter(self.time_check_interval, triggerCheck)

        self.enter(self.time_check_interval, triggerCheck)

    def repeatAtTrigger(self, triggerFunc, action, argument=(), kwargs={}):
        """performs the action action when triggerFunc() == True waits for it to be False and repeats this process"""
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
        """cancels a past scheduled event if no event exist return error msg. time_point can be any datetime object"""
        delay = self.timeEdit(time_point)
        for event in self.scheduler.queue:
            if abs(event[0] - self.scheduler.timefunc() - delay) < self.time_check_interval:
                self.scheduler.cancel(event)
                return
        raise ValueError("Cannot cancel unscheduled event")

    def cancelAllTriggeredEvents(self):
        """cancells a past scheduled event if no event exist return error msg. time_point can be any datetime object"""
        for event in self.scheduler.queue:
            if abs(event[0] - self.scheduler.timefunc()) <= self.time_check_interval:
                self.scheduler.cancel(event)