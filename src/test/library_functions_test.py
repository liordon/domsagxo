import pytest
from biblioteko.antauxdifinitaj_funkcioj import *
from biblioteko.atomaj_tipoj import *
from biblioteko.estra_komponantoj import *


class TestTimePointGeneration(object):

    def test_canGenerateTotallyRandomTimePoint(self):
        time_point = generateRandom([Generate.TIME_POINT.value])
        assert isinstance(time_point, TimePoint)
        assert 24 > time_point.hour
        assert 60 > time_point.minutes

    def test_canGenerateConstrainedRandomTimePoint(self):
        time_point = generateRandom([Generate.TIME_POINT.value,
                                     TimePoint(9, 20),
                                     TimePoint(9, 50)])
        assert isinstance(time_point, TimePoint)
        assert 9 == time_point.hour
        assert 50 > time_point.minutes
        assert 20 <= time_point.minutes

    def test_canGenerateRandomTimePointWithOverflowToNextHour(self):
        time_point = generateRandom([Generate.TIME_POINT.value,
                                     TimePoint(12, 59),
                                     TimePoint(13, 50)])
        assert isinstance(time_point, TimePoint)
        assert (59 == time_point.minutes) if time_point.hour == 12 \
            else (13 == time_point.hour and 50 > time_point.minutes)


class TestTimeSpanGeneration(object):

    def test_canGenerateTotallyRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value])
        assert isinstance(time_span, TimeSpan) #TODO: infinite loop? kial?!

    def test_canGenerateConstrainedRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    TimeSpan(0, 0, 1),
                                    TimeSpan(0, 1, 0)])
        assert isinstance(time_span, TimeSpan)
        assert 0 == time_span.hours
        assert 1 >= time_span.minutes
        assert 60 > time_span.seconds
        assert 0 < time_span.seconds

    def test_canGenerateLargeConstrainedRandomTimeSpan(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    TimeSpan(1, 0, 0),
                                    TimeSpan(2, 0, 0)])
        assert isinstance(time_span, TimeSpan)
        assert 1 == time_span.hours
        assert 0 <= time_span.minutes
        assert 60 > time_span.minutes
        assert 60 > time_span.seconds
        assert 0 <= time_span.seconds

    def test_canGenerateRandomTimeSpanWithOverflow(self):
        time_span = generateRandom([Generate.TIME_SPAN.value,
                                    TimeSpan(0, 59, 0),
                                    TimeSpan(2, 0, 0)])
        assert isinstance(time_span, TimeSpan)
        assert 1 >= time_span.hours
        assert 0 <= time_span.minutes
        assert 60 > time_span.minutes
        assert 60 > time_span.seconds
        assert 0 <= time_span.seconds


class TestApplianceManagement(object):
    @pytest.fixture
    def shm(self):
        return Domsagxo()
    
    def test_managerStartsEmpty(self, shm):
        assert 0 == len(shm.appliances)

    def test_managerHasPredefinedGroupForEachApplianceType(self, shm):
        assert len(ApplianceTypes) == len(shm.groups)

    def test_canAddApplianceDirectlyToSmartHouseManager(self, shm):
        shm.addAppliance(Appliance(ApplianceTypes.SWITCH, "sxambalulo"))

        assert 1 == len(shm.appliances)
        assert shm.appliances["sxambalulo"].type is ApplianceTypes.SWITCH

    def test_canAddApplianceToSmartHouseManagerViaLibraryFunction(self, shm):
        addAppliance(["sxalto", "sxambalulo"], shm)

        assert 1 == len(shm.appliances)
        assert shm.appliances["sxambalulo"].type is ApplianceTypes.SWITCH

    def test_cannotAddApplianceToSmartHouseManagerIfItsNameIsTaken(self, shm):
        appliance_name = "sxambalulo"
        shm.addAppliance(Appliance(ApplianceTypes.LIGHT, appliance_name))

        with pytest.raises(KeyError):
            addAppliance(["sxalto", appliance_name], shm)

    def test_canRenameExistingAppliance(self, shm):
        old_name = "sxambalulo"
        new_name = "bambasxusxo"
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, old_name))

        renameAppliance([old_name, new_name], shm)

        assert 1 == len(shm.appliances)
        assert shm.appliances[new_name].type is ApplianceTypes.KNOB
        assert shm.appliances[new_name].name is new_name

    def test_cannotRenameNonExistingAppliance(self, shm):
        with pytest.raises(KeyError):
            renameAppliance(["sxambalulo", "bambasxusxo"], shm)

    def test_cannotRenameApplianceIntoPreexistingName(self, shm):
        old_name = "sxambalulo"
        new_name = "bambasxusxo"
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, old_name))
        shm.addAppliance(Appliance(ApplianceTypes.KNOB, new_name))

        with pytest.raises(KeyError):
            renameAppliance([old_name, new_name], shm)

    def test_canCreateApplianceGroup(self, shm):
        group_name = "mia dormocxambro"

        createGroup([group_name], shm)

        assert group_name in shm.groups.keys()
        assert 0 == len(shm.groups[group_name])

    def test_creatingAnApplianceGroupRaisesKeyErrorIfGroupExists(self, shm):
        group_name = "mia dormocxambro"
        shm.groups[group_name] = []

        with pytest.raises(KeyError):
            createGroup([group_name], shm)

    def test_canRemoveApplianceGroup(self, shm):
        group_name = "mia dormocxambro"
        shm.groups[group_name] = []

        removeGroup([group_name], shm)

        assert group_name not in shm.groups.keys()

    def test_deletingAnApplianceGroupRaisesKeyErrorIfGroupDoesNotExist(self, shm):
        group_name = "mia dormocxambro"

        with pytest.raises(KeyError):
            removeGroup([group_name], shm)

    def test_canMoveApplianceIntoGroup(self, shm):
        group_name = "mia dormocxambro"
        shm.groups[group_name] = []
        appliance_name = "sxambalulo"
        appliance = Appliance(ApplianceTypes.LIGHT, appliance_name)
        shm.addAppliance(appliance)

        putApplianceInGroup([appliance_name, group_name], shm)

        assert 1 == len(shm.groups[group_name])
        assert appliance in shm.groups[group_name]

    def test_cannotMoveNonExistingApplianceIntoGroup(self, shm):
        group_name = "mia dormocxambro"
        shm.groups[group_name] = []
        appliance_name = "sxambalulo"

        with pytest.raises(KeyError):
            putApplianceInGroup([appliance_name, group_name], shm)

    def test_cannotMoveApplianceIntoNonExistingGroup(self, shm):
        group_name = "mia dormocxambro"
        appliance_name = "sxambalulo"
        appliance = Appliance(ApplianceTypes.LIGHT, appliance_name)
        shm.addAppliance(appliance)

        with pytest.raises(KeyError):
            putApplianceInGroup([appliance_name, group_name], shm)

    def test_canMoveApplianceFrom1GroupToAnother(self, shm):
        old_group_name = "mia dormocxambro"
        new_group_name = "infancxambro"
        appliance_name = "sxambalulo"
        appliance = Appliance(ApplianceTypes.LIGHT, appliance_name)
        shm.addAppliance(appliance)
        shm.groups[old_group_name] = [appliance]
        shm.groups[new_group_name] = []

        moveAppliance([appliance_name, old_group_name, new_group_name], shm)
        assert appliance not in shm.groups[old_group_name]
        assert appliance in shm.groups[new_group_name]
