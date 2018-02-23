from biblioteko.atomaj_tipoj import ApplianceTypes


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
        self.appliances[appliance.name] = appliance
        if appliance.type in Domsagxo.appliance_type_group:
            self.groups[Domsagxo.appliance_type_group[appliance.type]].append(appliance)

    def recognizes(self, appliance_or_group_name):
        return (appliance_or_group_name in self.appliances) or \
               (appliance_or_group_name in self.groups)

    def getApplianceOrGroup(self, appliance_or_group_name):
        if appliance_or_group_name in self.groups:
            return self.groups[appliance_or_group_name]
        elif appliance_or_group_name in self.appliances:
            return self.appliances[appliance_or_group_name]