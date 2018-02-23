import pytest
from biblioteko.antauxdifinitaj_funkcioj import *
from biblioteko.atomaj_tipoj import *
from biblioteko.rega_komponantoj import *


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
        addAppliance(["lumo", "sxambalulo"], shm)
        with pytest.raises(ValueError):
            addAppliance(["sxalto", "sxambalulo"], shm)
        assert 1 == len(shm.appliances)
        assert shm.appliances["sxambalulo"].type is ApplianceTypes.LIGHT
