from enum import Enum


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
    COLOR = 'koloro'


class LightColor(Enum):
    WHITE = 'blanko'
    RED = 'rugxo'


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
            self.properties = {
                ApplianceProperties.BRIGHTNESS.value: 1,
                ApplianceProperties.COLOR.value     : LightColor.WHITE
            }

    def setStateComponent(self, state_component, value):
        self.properties[state_component] = value
