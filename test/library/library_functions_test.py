import pytest

from library.management_components import *
from library.predefined_values import RandomizableType


class SmartHomeManagerProvided(object):
    @pytest.fixture
    def manager(self):
        return Domsagxo()

    @staticmethod
    def assertNumberOfNewAppliances(number, state):
        assert number == len(state.variables) - state.number_of_reserved_words


app_nm1 = "sxambalulo"
app_nm2 = "bambasxusxo"
app_nm3 = "lamsxabubo"
group_nm1 = "mia dormcxambro"
group_nm2 = "infancxambro"


class TestTimePointGeneration(object):

    def test_canGenerateTotallyRandomTimePoint(self):
        time_point = generateRandom(RandomizableType.TIME_POINT.value)
        assert isinstance(time_point, datetime.time)
        assert 24 > time_point.hour
        assert 60 > time_point.minute

    def test_canGenerateConstrainedRandomTimePoint(self):
        time_point = generateRandom(RandomizableType.TIME_POINT.value,
                                    datetime.time(9, 20),
                                    datetime.time(9, 50))
        assert isinstance(time_point, datetime.time)
        assert 9 == time_point.hour
        assert 50 > time_point.minute
        assert 20 <= time_point.minute

    def test_canGenerateRandomTimePointWithOverflowToNextHour(self):
        time_point = generateRandom(RandomizableType.TIME_POINT.value,
                                    datetime.time(12, 59),
                                    datetime.time(13, 50))
        assert isinstance(time_point, datetime.time)
        assert (59 == time_point.minute) if time_point.hour == 12 \
            else (13 == time_point.hour and 50 > time_point.minute)


class TestTimeSpanGeneration(object):

    def test_canGenerateTotallyRandomTimeSpan(self):
        time_span = generateRandom(RandomizableType.TIME_SPAN.value)
        assert isinstance(time_span, datetime.timedelta)

    def test_canGenerateConstrainedRandomTimeSpan(self):
        time_span = generateRandom(RandomizableType.TIME_SPAN.value,
                                   datetime.timedelta(seconds=1),
                                   datetime.timedelta(minutes=1))
        assert isinstance(time_span, datetime.timedelta)
        assert 60 > time_span.seconds
        assert 1 <= time_span.seconds

    def test_canGenerateLargeConstrainedRandomTimeSpan(self):
        time_span = generateRandom(RandomizableType.TIME_SPAN.value,
                                   datetime.timedelta(hours=1),
                                   datetime.timedelta(hours=2))
        assert isinstance(time_span, datetime.timedelta)
        assert 2 * 3600 > time_span.seconds
        assert 3600 <= time_span.seconds

    def test_canGenerateRandomTimeSpanWithOverflow(self):
        time_span = generateRandom(RandomizableType.TIME_SPAN.value,
                                   datetime.timedelta(minutes=59),
                                   datetime.timedelta(hours=2))
        assert isinstance(time_span, datetime.timedelta)
        assert 2 * 3600 > time_span.seconds
        assert 59 * 60 <= time_span.seconds


class TestApplianceManagement(SmartHomeManagerProvided):

    def test_managerStartsEmpty(self, manager):
        self.assertNumberOfNewAppliances(0, manager)

    def test_canAddApplianceToSmartHouseManagerViaLibraryFunction(self, manager):
        manager.requestDeviceAddition(ApplianceTypes.SWITCH.value, app_nm1)

        self.assertNumberOfNewAppliances(1, manager)
        assert manager.variables[app_nm1].type is ApplianceTypes.SWITCH

    def test_cannotAddApplianceToSmartHouseManagerIfItsNameIsTaken(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        with pytest.raises(KeyError):
            manager.requestDeviceAddition(ApplianceTypes.SWITCH.value, app_nm1)

    def test_canRenameExistingAppliance(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm1))

        manager.renameAppliance(app_nm1, app_nm2)

        self.assertNumberOfNewAppliances(1, manager)
        assert manager.variables[app_nm2].type is ApplianceTypes.KNOB
        assert manager.variables[app_nm2].name is app_nm2

    def test_cannotRenameNonExistingAppliance(self, manager):
        with pytest.raises(KeyError):
            manager.renameAppliance(app_nm1, app_nm2)

    def test_cannotRenameApplianceIntoPreexistingName(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm1))
        manager.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))

        with pytest.raises(KeyError):
            manager.renameAppliance(app_nm1, app_nm2)

    def test_canCreateApplianceGroup(self, manager):
        manager.addGroup(group_nm1)

        assert group_nm1 in manager.variables.keys()
        assert 0 == len(manager.variables[group_nm1])

    def test_creatingAnApplianceGroupRaisesKeyErrorIfGroupExists(self, manager):
        manager.variables[group_nm1] = []

        with pytest.raises(KeyError):
            manager.addGroup(group_nm1)

    def test_canRemoveApplianceGroup(self, manager):
        manager.variables[group_nm1] = []

        manager.removeGroup(group_nm1)

        assert group_nm1 not in manager.variables.keys()

    def test_deletingAnApplianceGroupRaisesKeyErrorIfGroupDoesNotExist(self, manager):
        with pytest.raises(KeyError):
            manager.removeGroup(group_nm1)

    def test_canMoveApplianceIntoGroup(self, manager):
        manager.variables[group_nm1] = []
        appliance = Appliance(ApplianceTypes.LIGHT, app_nm1)
        manager.addAppliance(appliance)

        manager.addApplianceToGroup(app_nm1, group_nm1)

        assert 1 == len(manager.variables[group_nm1])
        assert appliance in manager.variables[group_nm1]

    def test_cannotAddNonExistingApplianceToGroup(self, manager):
        manager.variables[group_nm1] = []
        with pytest.raises(KeyError):
            manager.addApplianceToGroup(app_nm1, group_nm1)

    def test_cannotPutApplianceInNonExistingGroup(self, manager):
        appliance = Appliance(ApplianceTypes.LIGHT, app_nm1)
        manager.addAppliance(appliance)

        with pytest.raises(KeyError):
            manager.addApplianceToGroup(app_nm1, group_nm1)


class TestApplianceCommands(SmartHomeManagerProvided):

    def test_canTurnOnAllAppliancesInGroup(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        manager.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        manager.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))
        manager.addGroup(group_nm1)
        manager.addApplianceToGroup(app_nm1, group_nm1)
        manager.addApplianceToGroup(app_nm2, group_nm1)

        manager.requestDeviceActivation(manager.variables[group_nm1])

        assert manager.variables[app_nm1].isTurnedOn
        assert manager.variables[app_nm2].isTurnedOn
        assert not manager.variables[app_nm3].isTurnedOn

    def test_canTurnOnSeveralAppliancesAtOnce(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        manager.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        manager.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))

        manager.requestDeviceActivation(
            [manager.variables[app_nm2], manager.variables[app_nm3]])

        assert not manager.variables[app_nm1].isTurnedOn
        assert manager.variables[app_nm2].isTurnedOn
        assert manager.variables[app_nm3].isTurnedOn

    def test_canTurnOnBothAppliancesAndGroupsAtOnce(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        manager.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        manager.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))
        manager.addGroup(group_nm1)
        manager.addApplianceToGroup(app_nm1, group_nm1)
        manager.addApplianceToGroup(app_nm2, group_nm1)

        manager.requestDeviceActivation(
            [manager.variables[group_nm1], manager.variables[app_nm3]])

        assert manager.variables[app_nm1].isTurnedOn
        assert manager.variables[app_nm2].isTurnedOn
        assert manager.variables[app_nm3].isTurnedOn

    def test_canQueryLightForItsBrightness(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        brightness = manager.getPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value)

        assert 1 == brightness

    def test_canAlterLightBrightness(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        manager.setPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value, .15)

        assert .15 == manager.getPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value)

    def test_canAlterLightBrightnessForEntireGroup(self, manager):
        manager.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))
        manager.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm2))
        manager.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm3))

        group_of_all_lights = manager.variables[ApplianceTypes.LIGHT.value + "j"]
        manager.requestChangeToDeviceProperty(
            group_of_all_lights, ApplianceProperties.BRIGHTNESS.value, .15)

        assert .15 == manager.getPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value)
        assert .15 == manager.getPropertyOfAppliance(app_nm2, ApplianceProperties.BRIGHTNESS.value)
        assert .15 == manager.getPropertyOfAppliance(app_nm3, ApplianceProperties.BRIGHTNESS.value)
