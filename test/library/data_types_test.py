import datetime

import pytest

import compilation.abstract_syntax_tree as ast_bld
from library.atomic_types import Appliance
from library.management_components import Domsagxo
from library.predefined_values import ApplianceTypes, ApplianceQueries, Color


def parsed_value_of(ast, expr, state=None):
    if state is None:
        state = Domsagxo()
    node = ast.parse(expr)
    return node.evaluate(state)[1]


class TestAstTimeSpans(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.Var.TIME_SPAN.value)

    @staticmethod
    def assertTimeSpan(parse_result, days=0, hours=0, minutes=0, seconds=0):
        assert isinstance(parse_result, datetime.timedelta)
        assert days == parse_result.days
        # assert hours == parse_result.hours
        # assert minutes == parse_result.minutes
        assert seconds + minutes * 60 + hours * 3600 == parse_result.seconds

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
        return ast_bld.build(start=ast_bld.Var.TIME_POINT.value)

    @staticmethod
    def assertTimePointValues(parse_result, hour, minutes=0):
        assert isinstance(parse_result, datetime.time)
        assert parse_result.hour == hour
        assert parse_result.minute == minutes

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

    def test_cannotFormatFormalFracturedHour_ITriedThatAndGotParsingConflicts(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("la deka horo kaj kvardek ses minutoj")

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
        return ast_bld.build(start=ast_bld.Var.ROUTINE_INVOCATION.value)

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
        assert isinstance(parse_result, datetime.timedelta)
        assert 180 > parse_result.seconds
        assert 120 <= parse_result.seconds


def evaluate_and_return_state(ast, statement, initial_state=None):
    if initial_state is None:
        initial_state = Domsagxo()  # so as not to put a mutable default
    state, nothing = ast.parse(statement).evaluate(initial_state)
    return state


class TestAstApplianceManagement(object):
    @pytest.fixture
    def ast(self):
        abstract_syntax_tree = ast_bld.build(start=ast_bld.Var.ROUTINE_INVOCATION.value)
        return abstract_syntax_tree

    @pytest.fixture
    def manager(self):
        manager = Domsagxo()
        manager.addAppliance(Appliance(ApplianceTypes.LIGHT, "sxambo"))
        manager.addAppliance(Appliance(ApplianceTypes.KNOB, "lulo"))
        return manager

    @staticmethod
    def assertNumberOfNewAppliances(number, state):
        assert number == len(state.variables) - state.number_of_reserved_words

    def test_canAddApplianceToSmartHomeViaCode(self, manager):
        appliance = Appliance(ApplianceTypes.LIGHT, "sxambalulo")
        manager = Domsagxo()
        manager.addAppliance(appliance)
        assert appliance.name in manager.variables.keys()

    def test_canTurnOnAllLights(self, ast, manager):
        state = evaluate_and_return_state(ast, "sxaltu la lumojn", manager)
        for appliance in [variable for variable in state.variables.values()
            if isinstance(variable, Appliance)]:
            if appliance.type is ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn() is True

    def test_nonLightAppliancesAreUnaffectedByLightsCommand(self, ast, manager):
        evaluate_and_return_state(ast, "sxaltu la lumojn", manager)
        for appliance in [app for app in manager.variables.values() if
            isinstance(app, Appliance)]:
            if appliance.type is ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn() is True
            elif appliance.type is not ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn() is False

    def test_canAddAnonymousApplianceToSmartHomeViaSpeech(self, ast):
        manager = evaluate_and_return_state(ast, "aldonu lumon")
        self.assertNumberOfNewAppliances(1, manager)
        # assert "lumo" not in manager.variables.keys() #lumo became a reserved word.
        assert "unua lumo" in manager.variables.keys()
        assert manager.variables["unua lumo"].type is ApplianceTypes.LIGHT

    def test_whenAddingTwoAnonymousAppliancesOneReceivesSerialNumber(self, ast):
        manager = evaluate_and_return_state(ast, "aldonu lumon")
        manager = evaluate_and_return_state(ast, "aldonu lumon", manager)
        self.assertNumberOfNewAppliances(2, manager)
        assert "unua lumo" in manager.variables.keys()
        assert "dua lumo" in manager.variables.keys()
        assert manager.variables["dua lumo"].type is ApplianceTypes.LIGHT

    def test_canQueryTheStateOfLights(self, ast, manager):
        manager = evaluate_and_return_state(ast, "sxaltu sxambo", manager)
        assert manager.variables["sxambo"].stateQueries[ApplianceQueries.IS_ON.value]
        assert not manager.variables["lulo"].stateQueries[ApplianceQueries.IS_ON.value]


class TestObjectOrientedActions(object):
    @pytest.fixture
    def parser(self):
        abstract_syntax_tree = ast_bld.build(start=ast_bld.Var.STATEMENT.value)
        return abstract_syntax_tree

    @pytest.fixture
    def smart_home(self):
        smart_home = Domsagxo()
        light_bulb = Appliance(ApplianceTypes.LIGHT, "sxambalulo")
        smart_home.addAppliance(light_bulb)
        return smart_home

    def test_canChangeColorFieldOfLightBulb(self, parser, smart_home):
        ast = parser.parse("asignu rugxo al koloro de sxambalulo")
        smart_home, value = ast.evaluate(smart_home)
        assert Color.RED.value == smart_home.variables["sxambalulo"].properties[
            "koloro"]

    def test_canChangeBrightnessFieldOfLightBulb(self, parser, smart_home):
        ast = parser.parse("asignu sepdek al brilo de sxambalulo")
        smart_home, value = ast.evaluate(smart_home)
        assert 70 == smart_home.variables["sxambalulo"].properties["brilo"]

    def test_canChangeDesiredTemperatureOfBoiler(self, parser, smart_home):
        boiler = Appliance(ApplianceTypes.BOILER, "granda kaldrono")
        smart_home.addAppliance(boiler)
        ast = parser.parse("asignu sesdek al volita temperaturo de granda kaldrono")
        smart_home, value = ast.evaluate(smart_home)
        assert 60 == smart_home.variables["granda kaldrono"].properties["volita temperaturo"]
