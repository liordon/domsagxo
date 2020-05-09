from domsagxo.library.predefined_values import *


class Appliance(object):
    """ the most basic parameter of any appliance is it's name.
    beyond that, an appliance should have many other functions determined
    by the components it includes."""

    def __init__(self, app_type, name):
        self.name = name
        self.type = app_type
        self.properties = {}
        self.stateQueries = {}
        self.createStateComponents()
        self.stateQueries[ApplianceQueries.IS_ON.value] = False

    def createStateComponents(self):
        if self.type is ApplianceTypes.LIGHT:
            self.properties = {
                ApplianceProperties.BRIGHTNESS.value: 1,
                ApplianceProperties.COLOR.value: Color.WHITE.value
            }
        elif self.type is ApplianceTypes.BOILER:
            self.properties = {
                ApplianceProperties.CURRENT_TEMPERATURE.value: 0,
                ApplianceProperties.DESIRED_TEMPERATURE.value: 100
            }

    def setStateComponent(self, state_component, value):
        self.properties[state_component] = value

    def turnOn(self):
        self.stateQueries[ApplianceQueries.IS_ON.value] = True

    def turnOff(self):
        self.stateQueries[ApplianceQueries.IS_ON.value] = False

    def isTurnedOn(self):
        return self.stateQueries[ApplianceQueries.IS_ON.value]
