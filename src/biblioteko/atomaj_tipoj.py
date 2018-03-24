from enum import Enum


# noinspection SpellCheckingInspection
class TimeUnits(Enum):
    DAY = "tago"
    HOUR = "horo"
    MINUTE = "minuto"
    SECOND = "sekundo"

    def in_seconds(self):
        if self == TimeUnits.SECOND:
            return 1
        elif self == TimeUnits.MINUTE:
            return 60
        elif self == TimeUnits.HOUR:
            return 3600
        elif self == TimeUnits.DAY:
            return 86400


class TimeSpan(object):
    def __init__(self, days=0, hours=0, minutes=0, seconds=0):
        self.days = days
        self.seconds = seconds
        self.minutes = minutes
        self.hours = hours

    def addFraction(self, fraction):
        if self.seconds > 0:
            res = TimeSpan(seconds=fraction)
        elif self.minutes > 0:
            res = TimeSpan(seconds=60 * fraction)
        else:
            res = TimeSpan(minutes=60 * fraction)

        return self.unite(self, res)

    def totalSeconds(self):
        hours = self.hours + self.days*24
        minutes = self.minutes + hours*60
        return self.seconds + minutes*60

    def __str__(self):
        components = []
        if self.days > 0:
            components += [self.days, "days"]
        if self.hours > 0:
            components += [self.hours, "hours"]
        if self.minutes > 0:
            components += [self.minutes, "minutes"]
        if self.seconds > 0:
            components += [self.seconds, "seconds"]

        if len(components) > 2:
            components = components[:-2] + "and" + components[-2:]


    @classmethod
    def unite(cls, span1, span2):
        return TimeSpan(days=span1.days + span2.days,
                        hours=span1.hours + span2.hours,
                        minutes=span1.minutes + span2.minutes,
                        seconds=span1.seconds + span2.seconds)



# noinspection SpellCheckingInspection
class ApplianceTypes(Enum):
    # MOST BASIC - TURN ON OR OFF
    SWITCH = 'sxalto'
    # A DIMMER WITH A RANGE OF VALUES
    KNOB = 'agordo'
    # LIKE A KNOB BUT ALSO WITH COLOR
    LIGHT = 'lumo'
    # A SENSOR FOR TEMPERATURE AND HUMIDITY
    THERMOSTAT = 'termostato'
    # A SENSOR FOR VISUAL FOOTAGE
    CAMERA = 'fotilo'


class ApplianceProperties(Enum):
    BRIGHTNESS = 'brilo'

    def defaultValue(self):
        return 1


class Appliance(object):
    """ the most basic parameter of any appliance is it's name.
    beyond that, an appliance should have many other functions determined
    by the components it includes."""
    def __init__(self, app_type, name):
        self.name = name
        self.type = app_type
        self.isTurnedOn = False
        self.createStateComponents()

    def createStateComponents(self):
        if self.type is ApplianceTypes.LIGHT:
            self.state_components = {
                ApplianceProperties.BRIGHTNESS.value: ApplianceProperties.BRIGHTNESS.defaultValue()
            }

    def setStateComponent(self, state_component, value):
        self.state_components[state_component] = value
