import datetime

import pytest

from domsagxo.library.atomic_types import Appliance
from domsagxo.library.predefined_functions import generateRandom
from domsagxo.library.predefined_values import PossibleRandomType, ApplianceTypes, ApplianceProperties
from domsagxo_test.test_utils.providers import SmartHomeManagerProvided

app_nm1 = "sxambalulo"
app_nm2 = "bambasxusxo"
app_nm3 = "lamsxabubo"
group_nm1 = "mia dormcxambro"
group_nm2 = "infancxambro"


class TestTimePointGeneration(object):

    def test_canGenerateTotallyRandomTimePoint(self):
        time_point = generateRandom(PossibleRandomType.TIME_POINT.value)
        assert isinstance(time_point, datetime.time)
        assert 24 > time_point.hour
        assert 60 > time_point.minute

    def test_canGenerateConstrainedRandomTimePoint(self):
        time_point = generateRandom(PossibleRandomType.TIME_POINT.value,
            datetime.time(9, 20),
            datetime.time(9, 50))
        assert isinstance(time_point, datetime.time)
        assert 9 == time_point.hour
        assert 50 > time_point.minute
        assert 20 <= time_point.minute

    def test_canGenerateRandomTimePointWithOverflowToNextHour(self):
        time_point = generateRandom(PossibleRandomType.TIME_POINT.value,
            datetime.time(12, 59),
            datetime.time(13, 50))
        assert isinstance(time_point, datetime.time)
        assert (59 == time_point.minute) if time_point.hour == 12 \
            else (13 == time_point.hour and 50 > time_point.minute)


class TestTimeSpanGeneration(object):

    def test_canGenerateTotallyRandomTimeSpan(self):
        time_span = generateRandom(PossibleRandomType.TIME_SPAN.value)
        assert isinstance(time_span, datetime.timedelta)

    def test_canGenerateConstrainedRandomTimeSpan(self):
        time_span = generateRandom(PossibleRandomType.TIME_SPAN.value,
            datetime.timedelta(seconds=1),
            datetime.timedelta(minutes=1))
        assert isinstance(time_span, datetime.timedelta)
        assert 60 > time_span.seconds
        assert 1 <= time_span.seconds

    def test_canGenerateLargeConstrainedRandomTimeSpan(self):
        time_span = generateRandom(PossibleRandomType.TIME_SPAN.value,
            datetime.timedelta(hours=1),
            datetime.timedelta(hours=2))
        assert isinstance(time_span, datetime.timedelta)
        assert 2 * 3600 > time_span.seconds
        assert 3600 <= time_span.seconds

    def test_canGenerateRandomTimeSpanWithOverflow(self):
        time_span = generateRandom(PossibleRandomType.TIME_SPAN.value,
            datetime.timedelta(minutes=59),
            datetime.timedelta(hours=2))
        assert isinstance(time_span, datetime.timedelta)
        assert 2 * 3600 > time_span.seconds
        assert 59 * 60 <= time_span.seconds


class TestApplianceManagement(SmartHomeManagerProvided):

    def test_smartHomeStartsEmpty(self, smart_home):
        self.assertNumberOfNewAppliances(0, smart_home)

    def test_canAddApplianceToSmartHouseManagerViaLibraryFunction(self, smart_home):
        smart_home.requestDeviceAddition(ApplianceTypes.SWITCH.value, app_nm1)

        self.assertNumberOfNewAppliances(1, smart_home)
        assert smart_home.variables[app_nm1].type is ApplianceTypes.SWITCH

    def test_cannotAddApplianceToSmartHouseManagerIfItsNameIsTaken(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        with pytest.raises(KeyError):
            smart_home.requestDeviceAddition(ApplianceTypes.SWITCH.value, app_nm1)

    def test_canRenameExistingAppliance(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm1))

        smart_home.renameAppliance(app_nm1, app_nm2)

        self.assertNumberOfNewAppliances(1, smart_home)
        assert smart_home.variables[app_nm2].type is ApplianceTypes.KNOB
        assert smart_home.variables[app_nm2].name is app_nm2

    def test_cannotRenameNonExistingAppliance(self, smart_home):
        with pytest.raises(KeyError):
            smart_home.renameAppliance(app_nm1, app_nm2)

    def test_cannotRenameApplianceIntoPreexistingName(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm1))
        smart_home.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))

        with pytest.raises(KeyError):
            smart_home.renameAppliance(app_nm1, app_nm2)

    def test_canCreateApplianceGroup(self, smart_home):
        smart_home.addGroup(group_nm1)

        assert group_nm1 in smart_home.variables.keys()
        assert 0 == len(smart_home.variables[group_nm1])

    def test_creatingAnApplianceGroupRaisesKeyErrorIfGroupExists(self, smart_home):
        smart_home.variables[group_nm1] = []

        with pytest.raises(KeyError):
            smart_home.addGroup(group_nm1)

    def test_canRemoveApplianceGroup(self, smart_home):
        smart_home.variables[group_nm1] = []

        smart_home.removeGroup(group_nm1)

        assert group_nm1 not in smart_home.variables.keys()

    def test_deletingAnApplianceGroupRaisesKeyErrorIfGroupDoesNotExist(self, smart_home):
        with pytest.raises(KeyError):
            smart_home.removeGroup(group_nm1)

    def test_canMoveApplianceIntoGroup(self, smart_home):
        smart_home.variables[group_nm1] = []
        appliance = Appliance(ApplianceTypes.LIGHT, app_nm1)
        smart_home.addAppliance(appliance)

        smart_home.addApplianceToGroup(app_nm1, group_nm1)

        assert 1 == len(smart_home.variables[group_nm1])
        assert appliance in smart_home.variables[group_nm1]

    def test_cannotAddNonExistingApplianceToGroup(self, smart_home):
        smart_home.variables[group_nm1] = []
        with pytest.raises(KeyError):
            smart_home.addApplianceToGroup(app_nm1, group_nm1)

    def test_cannotPutApplianceInNonExistingGroup(self, smart_home):
        appliance = Appliance(ApplianceTypes.LIGHT, app_nm1)
        smart_home.addAppliance(appliance)

        with pytest.raises(KeyError):
            smart_home.addApplianceToGroup(app_nm1, group_nm1)


class TestApplianceCommands(SmartHomeManagerProvided):

    def test_canTurnOnAllAppliancesInGroup(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        smart_home.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        smart_home.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))
        smart_home.addGroup(group_nm1)
        smart_home.addApplianceToGroup(app_nm1, group_nm1)
        smart_home.addApplianceToGroup(app_nm2, group_nm1)

        smart_home.requestDeviceActivation(smart_home.variables[group_nm1])

        assert smart_home.variables[app_nm1].isTurnedOn() is True
        assert smart_home.variables[app_nm2].isTurnedOn() is True
        assert smart_home.variables[app_nm3].isTurnedOn() is False

    def test_canTurnOnSeveralAppliancesAtOnce(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        smart_home.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        smart_home.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))

        smart_home.requestDeviceActivation(
            [smart_home.variables[app_nm2], smart_home.variables[app_nm3]])

        assert smart_home.variables[app_nm1].isTurnedOn() is False
        assert smart_home.variables[app_nm2].isTurnedOn() is True
        assert smart_home.variables[app_nm3].isTurnedOn() is True

    def test_canTurnOnBothAppliancesAndGroupsAtOnce(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        smart_home.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        smart_home.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))
        smart_home.addGroup(group_nm1)
        smart_home.addApplianceToGroup(app_nm1, group_nm1)
        smart_home.addApplianceToGroup(app_nm2, group_nm1)

        smart_home.requestDeviceActivation(
            smart_home.variables[group_nm1], smart_home.variables[app_nm3])

        assert smart_home.variables[app_nm1].isTurnedOn() is True
        assert smart_home.variables[app_nm2].isTurnedOn() is True
        assert smart_home.variables[app_nm3].isTurnedOn() is True

    def test_canQueryLightForItsBrightness(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        brightness = smart_home.getPropertyOfAppliance(app_nm1,
            ApplianceProperties.BRIGHTNESS.value)

        assert 1 == brightness

    def test_canAlterLightBrightness(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        smart_home.setPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value, .15)

        assert .15 == smart_home.getPropertyOfAppliance(app_nm1,
            ApplianceProperties.BRIGHTNESS.value)

    def test_canAlterLightBrightnessForEntireGroup(self, smart_home):
        smart_home.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))
        smart_home.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm2))
        smart_home.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm3))

        group_of_all_lights = smart_home.variables[ApplianceTypes.LIGHT.value + "j"]
        smart_home.requestChangeToDeviceProperty(
            group_of_all_lights, ApplianceProperties.BRIGHTNESS.value, .15)

        assert .15 == smart_home.getPropertyOfAppliance(app_nm1,
            ApplianceProperties.BRIGHTNESS.value)
        assert .15 == smart_home.getPropertyOfAppliance(app_nm2,
            ApplianceProperties.BRIGHTNESS.value)
        assert .15 == smart_home.getPropertyOfAppliance(app_nm3,
            ApplianceProperties.BRIGHTNESS.value)
