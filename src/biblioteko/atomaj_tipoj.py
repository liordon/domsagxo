from enum import Enum


# noinspection SpellCheckingInspection
class TimeUnits(Enum):
    HOUR = "horo"
    MINUTE = "minuto"
    SECOND = "sekundo"


class TimePoint(object):
    def __init__(self, hour, minutes):
        self.minutes = minutes
        self.hour = hour

    def __str__(self):
        return "TimePoint(%02d,%02d)" % (self.hour, self.minutes, )


class TimeSpan(object):
    def __init__(self, hours=0, minutes=0, seconds=0):
        self.seconds = seconds
        self.minutes = minutes
        self.hours = hours

    def addFraction(self, fraction):
        if self.seconds > 0:
            res = TimeSpan(0, 0, fraction)
        elif self.minutes > 0:
            res = TimeSpan(0, 0, 60 * fraction)
        else:
            res = TimeSpan(0, 60 * fraction, 0)

        return self.unite(self, res)

    def __str__(self):
        components = []
        if self.hours > 0:
            components += [self.hours, " hours "]
        if self.minutes > 0:
            components += [self.minutes, " minutes "]
        if self.seconds > 0:
            components += [self.seconds, " hours "]

        return "TimeSpan(%02d,%02d,%02d)" % (self.hours, self.minutes, self.seconds,)

    @classmethod
    def unite(cls, span1, span2):
        return TimeSpan(span1.hours + span2.hours,
                        span1.minutes + span2.minutes,
                        span1.seconds + span2.seconds)

    @classmethod
    def uniteTuple(cls, tuple1, tuple2):
        return (tuple1[0] + tuple2[0],
                tuple1[1] + tuple2[1],
                tuple1[2] + tuple2[2])


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


class Appliance(object):
    ''' the most basic parameter of any appliance is it's name.
    beyond that, an appliance should have many other functions determined
    by the components it includes.'''
    def __init__(self, app_type, name):
        self.name = name
        self.type = app_type
        self.isTurnedOn = False
        self.state_components = {}
