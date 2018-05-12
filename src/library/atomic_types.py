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
