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
