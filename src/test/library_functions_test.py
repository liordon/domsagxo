import pytest

from library.management_components import *


class SmartHomeManagerProvided(object):
    @pytest.fixture
    def shm(self):
        return Domsagxo()


app_nm1 = "sxambalulo"
app_nm2 = "bambasxusxo"
app_nm3 = "lamsxabubo"
group_nm1 = "mia dormcxambro"
group_nm2 = "infancxambro"


class TestTimePointGeneration(object):

    def test_canGenerateTotallyRandomTimePoint(self):
        time_point = generateRandom([Generate.TIME_POINT.value])
        assert isinstance(time_point, datetime.time)
        assert 24 > time_point.hour
        assert 60 > time_point.minute

    def test_canGenerateConstrainedRandomTimePoint(self):
        time_point = generateRandom([Generate.TIME_POINT.value,
                                     datetime.time(9, 20),
                                     datetime.time(9, 50)])
        assert isinstance(time_point, datetime.time)
        assert 9 == time_point.hour
        assert 50 > time_point.minute
        assert 20 <= time_point.minute

    def test_canGenerateRandomTimePointWithOverflowToNextHour(self):
        time_point = generateRandom([Generate.TIME_POINT.value,
                                     datetime.time(12, 59),
                                     datetime.time(13, 50)])
        assert isinstance(time_point, datetime.time)
        assert (59 == time_point.minute) if time_point.hour == 12 \
            else (13 == time_point.hour and 50 > time_point.minute)


class TestTimeSpanGeneration(object):

    def test_canGenerateTotallyRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value])
        assert isinstance(time_span, datetime.timedelta)

    def test_canGenerateConstrainedRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    datetime.timedelta(seconds=1),
                                    datetime.timedelta(minutes=1)])
        assert isinstance(time_span, datetime.timedelta)
        assert 60 > time_span.seconds
        assert 1 <= time_span.seconds

    def test_canGenerateLargeConstrainedRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    datetime.timedelta(hours=1),
                                    datetime.timedelta(hours=2)])
        assert isinstance(time_span, datetime.timedelta)
        assert 2 * 3600 > time_span.seconds
        assert 3600 <= time_span.seconds

    def test_canGenerateRandomTimeSpanWithOverflow(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    datetime.timedelta(minutes=59),
                                    datetime.timedelta(hours=2)])
        assert isinstance(time_span, datetime.timedelta)
        assert 2 * 3600 > time_span.seconds
        assert 59 * 60 <= time_span.seconds


class TestApplianceManagement(SmartHomeManagerProvided):

    def test_managerStartsEmpty(self, shm):
        assert 0 == len(shm.appliances)

    def test_managerHasPredefinedGroupForEachApplianceType(self, shm):
        assert len(ApplianceTypes) == len(shm.groups)

    def test_canAddApplianceToSmartHouseManagerViaLibraryFunction(self, shm):
        shm.requestDeviceAddition([ApplianceTypes.SWITCH.value, app_nm1])

        assert 1 == len(shm.appliances)
        assert shm.appliances[app_nm1].type is ApplianceTypes.SWITCH

    def test_cannotAddApplianceToSmartHouseManagerIfItsNameIsTaken(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        with pytest.raises(KeyError):
            shm.requestDeviceAddition([ApplianceTypes.SWITCH.value, app_nm1])

    def test_canRenameExistingAppliance(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm1))

        shm.renameAppliance([app_nm1, app_nm2])

        assert 1 == len(shm.appliances)
        assert shm.appliances[app_nm2].type is ApplianceTypes.KNOB
        assert shm.appliances[app_nm2].name is app_nm2

    def test_cannotRenameNonExistingAppliance(self, shm):
        with pytest.raises(KeyError):
            shm.renameAppliance([app_nm1, app_nm2])

    def test_cannotRenameApplianceIntoPreexistingName(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm1))
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))

        with pytest.raises(KeyError):
            shm.renameAppliance([app_nm1, app_nm2])

    def test_canCreateApplianceGroup(self, shm):
        shm.addGroup(group_nm1)

        assert group_nm1 in shm.groups.keys()
        assert 0 == len(shm.groups[group_nm1])

    def test_creatingAnApplianceGroupRaisesKeyErrorIfGroupExists(self, shm):
        shm.groups[group_nm1] = []

        with pytest.raises(KeyError):
            shm.addGroup(group_nm1)

    def test_canRemoveApplianceGroup(self, shm):
        shm.groups[group_nm1] = []

        shm.removeGroup(group_nm1)

        assert group_nm1 not in shm.groups.keys()

    def test_deletingAnApplianceGroupRaisesKeyErrorIfGroupDoesNotExist(self, shm):
        with pytest.raises(KeyError):
            shm.removeGroup(group_nm1)

    def test_canMoveApplianceIntoGroup(self, shm):
        shm.groups[group_nm1] = []
        appliance = Appliance(ApplianceTypes.LIGHT, app_nm1)
        shm.addAppliance(appliance)

        shm.addApplianceToGroup(app_nm1, group_nm1)

        assert 1 == len(shm.groups[group_nm1])
        assert appliance in shm.groups[group_nm1]

    def test_cannotAddNonExistingApplianceToGroup(self, shm):
        shm.groups[group_nm1] = []
        with pytest.raises(KeyError):
            shm.addApplianceToGroup(app_nm1, group_nm1)

    def test_cannotPutApplianceInNonExistingGroup(self, shm):
        appliance = Appliance(ApplianceTypes.LIGHT, app_nm1)
        shm.addAppliance(appliance)

        with pytest.raises(KeyError):
            shm.addApplianceToGroup(app_nm1, group_nm1)

    # def test_canMoveApplianceFrom1GroupToAnother(self, shm):
    #     appliance = Appliance(ApplianceTypes.LIGHT, app_nm1)
    #     shm.addAppliance(appliance)
    #     shm.groups[group_nm1] = [appliance]
    #     shm.groups[group_nm2] = []
    #
    #     moveAppliance([app_nm1, group_nm1, group_nm2], shm)
    #     assert appliance not in shm.groups[group_nm1]
    #     assert appliance in shm.groups[group_nm2]


class TestApplianceCommands(SmartHomeManagerProvided):

    def test_canTurnOnAllAppliancesInGroup(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        shm.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))
        shm.addGroup(group_nm1)
        shm.addApplianceToGroup(app_nm1, group_nm1)
        shm.addApplianceToGroup(app_nm2, group_nm1)

        shm.requestDeviceActivation([group_nm1])

        assert shm.getAppliance(app_nm1).isTurnedOn
        assert shm.getAppliance(app_nm2).isTurnedOn
        assert not shm.getAppliance(app_nm3).isTurnedOn

    def test_canTurnOnSeveralAppliancesAtOnce(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        shm.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))

        shm.requestDeviceActivation([app_nm2, app_nm3])

        assert not shm.getAppliance(app_nm1).isTurnedOn
        assert shm.getAppliance(app_nm2).isTurnedOn
        assert shm.getAppliance(app_nm3).isTurnedOn

    def test_canTurnOnBothAppliancesAndGroupsAtOnce(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.SWITCH, app_nm1))
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, app_nm2))
        shm.addAppliance(Appliance(ApplianceTypes.CAMERA, app_nm3))
        shm.addGroup(group_nm1)
        shm.addApplianceToGroup(app_nm1, group_nm1)
        shm.addApplianceToGroup(app_nm2, group_nm1)

        shm.requestDeviceActivation([group_nm1, app_nm3])

        assert shm.getAppliance(app_nm1).isTurnedOn
        assert shm.getAppliance(app_nm2).isTurnedOn
        assert shm.getAppliance(app_nm3).isTurnedOn

    def test_canQueryLightForItsBrightness(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        brightness = shm.getPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value)

        assert 1 == brightness

    def test_canAlterLightBrightness(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))

        shm.setPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value, .15)

        assert .15 == shm.getPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value)

    def test_canAlterLightBrightnessForEntireGroup(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm1))
        shm.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm2))
        shm.addAppliance(Appliance(ApplianceTypes.LIGHT, app_nm3))

        group_of_all_lights = ApplianceTypes.LIGHT.value + "j"
        shm.requestChangeToDeviceProperty(
            [group_of_all_lights, ApplianceProperties.BRIGHTNESS.value, .15])

        assert .15 == shm.getPropertyOfAppliance(app_nm1, ApplianceProperties.BRIGHTNESS.value)
        assert .15 == shm.getPropertyOfAppliance(app_nm2, ApplianceProperties.BRIGHTNESS.value)
        assert .15 == shm.getPropertyOfAppliance(app_nm3, ApplianceProperties.BRIGHTNESS.value)
