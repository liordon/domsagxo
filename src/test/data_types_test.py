import datetime

import pytest

import library.atomic_types as tipo
import library.management_components as esk
import compilation.abstrakta_sintaksarbo as ast_bld
import compilation.esp_lexer as lxr

lxr.build()


def parsed_value_of(ast, expr, state=None):
    if state is None:
        state = esk.Domsagxo()
    node = ast.parse(expr)
    return node.evaluate(state)[1]


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
        parse_result = parsed_value_of(ast, "horo")
        self.assertTimeSpan(parse_result, hours=1)

    def test_canFormatTwoHours(self, ast):
        parse_result = parsed_value_of(ast, "du horoj")
        self.assertTimeSpan(parse_result, hours=2)

    def test_canFormatSingleMinute(self, ast):
        parse_result = parsed_value_of(ast, "minuto")
        self.assertTimeSpan(parse_result, minutes=1)

    def test_canFormatSingleSecond(self, ast):
        parse_result = parsed_value_of(ast, "sekundo")
        self.assertTimeSpan(parse_result, seconds=1)

    def test_canFormatAnHourAndAMinute(self, ast):
        parse_result = parsed_value_of(ast, "horo kaj minuto")
        self.assertTimeSpan(parse_result, hours=1, minutes=1)

    def test_canFormat2HoursAnd10Minutes(self, ast):
        parse_result = parsed_value_of(ast, "du horoj kaj dek minutoj")
        self.assertTimeSpan(parse_result, hours=2, minutes=10)

    def test_canFormat10Hours10MinutesAnd10Seconds(self, ast):
        parse_result = parsed_value_of(ast, "dek horoj dek minutoj kaj dek sekundoj")
        self.assertTimeSpan(parse_result, hours=10, minutes=10, seconds=10)

    def test_canFormatAnHourAndAHalf(self, ast):
        parse_result = parsed_value_of(ast, "horo kaj duono")
        self.assertTimeSpan(parse_result, hours=1, minutes=30)

    def test_cannotFormatAnHourAndAnHour(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("horo kaj unu")

    def test_canFormatHalfAMinute(self, ast):
        parse_result = parsed_value_of(ast, "duono minuto")
        self.assertTimeSpan(parse_result, seconds=30)

    def test_canFormat2Days(self, ast):
        parse_result = parsed_value_of(ast, "du tagoj")
        self.assertTimeSpan(parse_result, days=2)


class TestAstTimePoints(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="timePoint")

    @staticmethod
    def assertTimePointValues(parse_result, hour, minutes=0):
        assert isinstance(parse_result, datetime.time)
        assert hour == parse_result.hour
        assert minutes == parse_result.minute

    def test_canFormatFormalRoundHour(self, ast):
        parse_result = parsed_value_of(ast, "la sesa horo")
        self.assertTimePointValues(parse_result, 6)

    def test_canFormatColloquialRoundHour(self, ast):
        parse_result = parsed_value_of(ast, "la sepa kaj nul")
        self.assertTimePointValues(parse_result, 7)

    def test_canFormatColloquialFracturedHour(self, ast):
        parse_result = parsed_value_of(ast, "la dek dua kaj kvindek ses")
        self.assertTimePointValues(parse_result, 12, 56)

    def test_canFormatColloquialQuarteredHour(self, ast):
        parse_result = parsed_value_of(ast, "la kvara kaj kvarono")
        self.assertTimePointValues(parse_result, 4, 15)

    def test_canFormatColloquial24BasedHour(self, ast):
        parse_result = parsed_value_of(ast, "la dudek tria kaj nul")
        self.assertTimePointValues(parse_result, 23)

    def test_canotFormatFormalFracturedHour_ITriedThatAndGotParsingConflicts(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            parse_result = ast.parse("la deka horo kaj kvardek ses minutoj")

    def test_cannotFormatMoreThan24thHour(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            parsed_value_of(ast, "la kvardek sesa horo kaj nul")

    def test_cannotFormatMoreThan24thHourInOneDigit(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            parsed_value_of(ast, "la nauxdeka horo kaj nul")

    def test_cannotFormatMoreThan60Minutes(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            parsed_value_of(ast, "la kvina kaj cent")

    def test_cannotFormatTimeWithoutHour(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            parsed_value_of(ast, "la kvina minuto")

    def test_cannotFormatReverseOrderTime(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            parsed_value_of(ast, "la kvina minuto kaj dek horoj")


class TestAstRandomGeneration(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="functionCall")

    def test_canGenerateRandomNumber(self, ast):
        parsed_value_of(ast, "hazardu nombro")

    def test_canGenerateRandomNumberWithinBounds(self, ast):
        parse_result = parsed_value_of(ast, "hazardu nombro inter kvar kaj dek")
        assert 4 <= parse_result
        assert 10 > parse_result

    def test_randomNumberBoundedBetween2AdjacentNumbersIsAlwaysLowerNumber(self, ast):
        assert 1 == parsed_value_of(ast, "hazardu nombro inter unu kaj du")

    def test_canGenerateRandomTimePoint(self, ast):
        parse_result = parsed_value_of(ast, "hazardu horon")
        assert isinstance(parse_result, datetime.time)

    def test_canGenerateRandomTimePointWithinTwoRoundHours(self, ast):
        parse_result = parsed_value_of(ast, "hazardu horon inter la oka horo kaj la nauxa horo")
        assert isinstance(parse_result, datetime.time)
        assert 9 > parse_result.hour

    def test_canGenerateRandomTimePointWithinASingleHour(self, ast):
        parse_result = parsed_value_of(ast, "hazardu horon inter la oka horo kaj la oka kaj dek")
        assert isinstance(parse_result, datetime.time)
        assert 8 == parse_result.hour
        assert 10 > parse_result.minute

    def test_canGenerateRandomTimeSpanWithConstraints(self, ast):
        parse_result = parsed_value_of(ast, "hazardu tempon el du minutoj gxis tri minutoj")
        assert isinstance(parse_result, tipo.TimeSpan)
        assert 2 >= parse_result.minutes
        assert 1 <= parse_result.minutes
        assert 60 > parse_result.seconds
        assert 0 <= parse_result.seconds


def evaluate_and_return_state(ast, statement, initial_state=None):
    if initial_state is None:
        initial_state = esk.Domsagxo()  # so as not to put a mutable default
    state, nothing = ast.parse(statement).evaluate(initial_state)
    return state


class TestAstApplianceManagement(object):
    @pytest.fixture
    def ast(self):
        abstract_syntax_tree = ast_bld.build(start="functionCall")
        return abstract_syntax_tree

    @pytest.fixture
    def manager(self):
        return esk.Domsagxo()

    def test_canAddApplianceToSmartHomeViaCode(self, manager):
        appliance = tipo.Appliance(tipo.ApplianceTypes.LIGHT, "shambalulu")
        manager.addAppliance(appliance)
        assert appliance.name in manager.appliances.keys()

    def test_canTurnOnAllLights(self, ast):
        state = evaluate_and_return_state(ast, "sxaltu la lumojn")
        for appliance in state.appliances.values():
            if appliance.type is tipo.ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn

    def test_nonLightAppliancesAreUnaffectedByLightsCommand(self, ast, manager):
        manager.addAppliance(tipo.Appliance(tipo.ApplianceTypes.LIGHT, "shamba"))
        manager.addAppliance(tipo.Appliance(tipo.ApplianceTypes.KNOB, "lulu"))

        evaluate_and_return_state(ast, "sxaltu la lumojn", manager)
        for appliance in manager.appliances.values():
            if appliance.type is tipo.ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn
            elif appliance.type is not tipo.ApplianceTypes.LIGHT:
                assert not appliance.isTurnedOn

    def test_canAddAnonymousApplianceToSmartHomeViaSpeech(self, ast):
        state = evaluate_and_return_state(ast, "aldonu lumon")
        assert 1 == len(state.appliances)
        assert "lumo" not in state.appliances.keys()
        assert "unua lumo" in state.appliances.keys()
        assert state.appliances["unua lumo"].type is tipo.ApplianceTypes.LIGHT

    def test_whenAddingTwoAnonymousAppliancesOneReceivesSerialNumber(self, ast):
        state = evaluate_and_return_state(ast, "aldonu lumon")
        state = evaluate_and_return_state(ast, "aldonu lumon", state)
        assert 2 == len(state.appliances)
        assert "unua lumo" in state.appliances.keys()
        assert "dua lumo" in state.appliances.keys()
        assert state.appliances["dua lumo"].type is tipo.ApplianceTypes.LIGHT
