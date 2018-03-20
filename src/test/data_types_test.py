import biblioteko.atomaj_tipoj as tipo
import kompilajxo.leksisto as lxr
import kompilajxo.abstrakta_sintaksarbo as ast_bld
import pytest

lxr.build()


class TestAstTimeSpans(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="timeSpan")

    @staticmethod
    def assertTimeSpan(parse_result, days=0, hours=0, minutes=0, seconds=0):
        assert isinstance(parse_result, tipo.TimeSpan)
        assert days == parse_result.days
        assert hours == parse_result.hours
        assert minutes == parse_result.minutes
        assert seconds == parse_result.seconds

    def test_canFormatSingleHour(self, ast):
        parse_result = ast.parse("horo")
        self.assertTimeSpan(parse_result, hours=1)

    def test_canFormatTwoHours(self, ast):
        parse_result = ast.parse("du horoj")
        self.assertTimeSpan(parse_result, hours=2)

    def test_canFormatSingleMinute(self, ast):
        parse_result = ast.parse("minuto")
        self.assertTimeSpan(parse_result, minutes=1)

    def test_canFormatSingleSecond(self, ast):
        parse_result = ast.parse("sekundo")
        self.assertTimeSpan(parse_result, seconds=1)

    def test_canFormatAnHourAndAMinute(self, ast):
        parse_result = ast.parse("horo kaj minuto")
        self.assertTimeSpan(parse_result, hours=1, minutes=1)

    def test_canFormat2HoursAnd10Minutes(self, ast):
        parse_result = ast.parse("du horoj kaj dek minutoj")
        self.assertTimeSpan(parse_result, hours=2, minutes=10)

    def test_canFormat10Hours10MinutesAnd10Seconds(self, ast):
        parse_result = ast.parse("dek horoj dek minutoj kaj dek sekundoj")
        self.assertTimeSpan(parse_result, hours=10, minutes=10, seconds=10)

    def test_canFormatAnHourAndAHalf(self, ast):
        parse_result = ast.parse("horo kaj duono")
        self.assertTimeSpan(parse_result, hours=1, minutes=30)

    def test_cannotFormatAnHourAndAnHour(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("horo kaj unu")

    def test_canFormatHalfAMinute(self, ast):
        parse_result = ast.parse("duono minuto")
        self.assertTimeSpan(parse_result, seconds=30)

    def test_canFormat2Days(self, ast):
        parse_result = ast.parse("du tagoj")
        self.assertTimeSpan(parse_result, days=2)


class TestAstTimePoints(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="timePoint")

    @staticmethod
    def assertTimePointValues(parse_result, hour, minutes=0):
        assert isinstance(parse_result, tipo.TimePoint)
        assert hour == parse_result.hour
        assert minutes == parse_result.minutes

    def test_canFormatFormalRoundHour(self, ast):
        parse_result = ast.parse("la sesa horo")
        self.assertTimePointValues(parse_result, 6)

    def test_canFormatColloquialRoundHour(self, ast):
        parse_result = ast.parse("la sepa kaj nul")
        self.assertTimePointValues(parse_result, 7)

    def test_canFormatColloquialFracturedHour(self, ast):
        parse_result = ast.parse("la dek dua kaj kvindek ses")
        self.assertTimePointValues(parse_result, 12, 56)

    def test_canFormatColloquialQuarteredHour(self, ast):
        parse_result = ast.parse("la kvara kaj kvarono")
        self.assertTimePointValues(parse_result, 4, 15)

    def test_canFormatColloquial24BasedHour(self, ast):
        parse_result = ast.parse("la dudek tria kaj nul")
        self.assertTimePointValues(parse_result, 23)

    def test_canotFormatFormalFracturedHour_ITriedThatAndGotParsingConflicts(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            parse_result = ast.parse("la deka horo kaj kvardek ses minutoj")

    def test_cannotFormatMoreThan24thHour(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("la kvardek sesa horo kaj nul")

    def test_cannotFormatMoreThan24thHourInOneDigit(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("la nauxdeka horo kaj nul")

    def test_cannotFormatMoreThan60Minutes(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("la kvina kaj cent")

    def test_cannotFormatTimeWithoutHour(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("la kvina minuto")

    def test_cannotFormatReverseOrderTime(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("la kvina minuto kaj dek horoj")


class TestAstRandomGeneration(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="functionCall")

    def test_canGenerateRandomNumber(self, ast):
        ast.parse("hazardu nombro")

    def test_canGenerateRandomNumberWithinBounds(self, ast):
        parse_result = ast.parse("hazardu nombro inter kvar kaj dek")
        assert 4 <= parse_result
        assert 10 > parse_result

    def test_randomNumberBoundedBetween2AdjacentNumbersIsAlwaysLowerNumber(self, ast):
        assert 1 == ast.parse("hazardu nombro inter unu kaj du")

    def test_canGenerateRandomTimePoint(self, ast):
        parse_result = ast.parse("hazardu horon")
        assert isinstance(parse_result, tipo.TimePoint)

    def test_canGenerateRandomTimePointWithinTwoRoundHours(self, ast):
        parse_result = ast.parse("hazardu horon inter la oka horo kaj la nauxa horo")
        assert isinstance(parse_result, tipo.TimePoint)
        assert 9 > parse_result.hour

    def test_canGenerateRandomTimePointWithinASingleHour(self, ast):
        parse_result = ast.parse("hazardu horon inter la oka horo kaj la oka kaj dek")
        assert isinstance(parse_result, tipo.TimePoint)
        assert 8 == parse_result.hour
        assert 10 > parse_result.minutes

    def test_canGenerateRandomTimeSpanWithConstraints(self, ast):
        parse_result = ast.parse("hazardu tempon el du minutoj gxis tri minutoj")
        assert isinstance(parse_result, tipo.TimeSpan)
        assert 2 >= parse_result.minutes
        assert 1 <= parse_result.minutes
        assert 60 > parse_result.seconds
        assert 0 <= parse_result.seconds


class TestAstApplianceManagement(object):
    @pytest.fixture
    def ast(self):
        abstract_syntax_tree = ast_bld.build(start="functionCall")
        return abstract_syntax_tree

    def test_canAddApplianceToSmartHomeViaCode(self, ast):
        appliance = tipo.Appliance(tipo.ApplianceTypes.LIGHT, "shambalulu")
        ast.smart_home_manager.addAppliance(appliance)
        assert appliance.name in ast.smart_home_manager.appliances.keys()

    def test_canTurnOnAllLights(self, ast):
        ast.parse("sxaltu la lumojn")
        for appliance in ast.smart_home_manager.appliances.values():
            if appliance.type is tipo.ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn

    def test_nonLightAppliancesAreUnaffectedByLightsCommand(self, ast):
        manager = ast.smart_home_manager
        manager.addAppliance(tipo.Appliance(tipo.ApplianceTypes.LIGHT, "shamba"))
        manager.addAppliance(tipo.Appliance(tipo.ApplianceTypes.KNOB, "lulu"))

        ast.parse("sxaltu la lumojn")
        for appliance in manager.appliances.values():
            if appliance.type is tipo.ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn
            elif appliance.type is not tipo.ApplianceTypes.LIGHT:
                assert not appliance.isTurnedOn

    def test_canAddAnonymousApplianceToSmartHomeViaSpeech(self, ast):
        ast.parse("aldonu lumon")
        assert 1 == len(ast.smart_home_manager.appliances)
        assert "lumo" not in ast.smart_home_manager.appliances.keys()
        assert "unua lumo" in ast.smart_home_manager.appliances.keys()
        assert ast.smart_home_manager.appliances["unua lumo"].type is tipo.ApplianceTypes.LIGHT

    def test_whenAddingTwoAnonymousAppliancesOneReceivesSerialNumber(self, ast):
        ast.parse("aldonu lumon")
        ast.parse("aldonu lumon")
        assert 2 == len(ast.smart_home_manager.appliances)
        assert "unua lumo" in ast.smart_home_manager.appliances.keys()
        assert "dua lumo" in ast.smart_home_manager.appliances.keys()
        assert ast.smart_home_manager.appliances["dua lumo"].type is tipo.ApplianceTypes.LIGHT
