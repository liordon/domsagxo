import datetime

import pytest

import compilation.abstract_syntax_tree as ast_bld
import compilation.esp_lexer as lxr
import library.atomic_types as atypes
import library.management_components as mcmps

lxr.build()


def parsed_value_of(ast, expr, state=None):
    if state is None:
        state = mcmps.Domsagxo()
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
        return ast_bld.build(start=ast_bld.Var.FUNCTION_INVOCATION.value)

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
        initial_state = mcmps.Domsagxo()  # so as not to put a mutable default
    state, nothing = ast.parse(statement).evaluate(initial_state)
    return state


class TestAstApplianceManagement(object):
    @pytest.fixture
    def ast(self):
        abstract_syntax_tree = ast_bld.build(start=ast_bld.Var.FUNCTION_INVOCATION.value)
        return abstract_syntax_tree

    @pytest.fixture
    def manager(self):
        return mcmps.Domsagxo()

    def test_canAddApplianceToSmartHomeViaCode(self, manager):
        appliance = atypes.Appliance(atypes.ApplianceTypes.LIGHT, "shambalulu")
        manager.addAppliance(appliance)
        assert appliance.name in manager.variables.keys()

    def test_canTurnOnAllLights(self, ast):
        state = evaluate_and_return_state(ast, "sxaltu la lumojn")
        for appliance in state.variables.values():
            if appliance.type is atypes.ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn

    def test_nonLightAppliancesAreUnaffectedByLightsCommand(self, ast, manager):
        manager.addAppliance(atypes.Appliance(atypes.ApplianceTypes.LIGHT, "shamba"))
        manager.addAppliance(atypes.Appliance(atypes.ApplianceTypes.KNOB, "lulu"))

        evaluate_and_return_state(ast, "sxaltu la lumojn", manager)
        for appliance in manager.variables.values():
            if appliance.type is atypes.ApplianceTypes.LIGHT:
                assert appliance.isTurnedOn
            elif appliance.type is not atypes.ApplianceTypes.LIGHT:
                assert not appliance.isTurnedOn

    def test_canAddAnonymousApplianceToSmartHomeViaSpeech(self, ast):
        manager = evaluate_and_return_state(ast, "aldonu lumon")
        assert 1 == len(manager.variables)
        assert "lumo" not in manager.variables.keys()
        assert "unua lumo" in manager.variables.keys()
        assert manager.variables["unua lumo"].type is atypes.ApplianceTypes.LIGHT

    def test_whenAddingTwoAnonymousAppliancesOneReceivesSerialNumber(self, ast):
        manager = evaluate_and_return_state(ast, "aldonu lumon")
        manager = evaluate_and_return_state(ast, "aldonu lumon", manager)
        assert 2 == len(manager.variables)
        assert "unua lumo" in manager.variables.keys()
        assert "dua lumo" in manager.variables.keys()
        assert manager.variables["dua lumo"].type is atypes.ApplianceTypes.LIGHT


class TestObjectOrientedActions(object):
    @pytest.fixture
    def parser(self):
        abstract_syntax_tree = ast_bld.build(start=ast_bld.Var.STATEMENT.value)
        return abstract_syntax_tree

    @pytest.fixture
    def smart_home(self):
        smart_home = mcmps.Domsagxo()
        light_bulb = atypes.Appliance(atypes.ApplianceTypes.LIGHT, "sxambalulo")
        smart_home.addAppliance(light_bulb)
        return smart_home

    def test_canAccessColorFieldOfLightBulb(self, parser, smart_home):
        ast = parser.parse("koloro de sxambalulo")
        state, value = ast.evaluate(smart_home)
        assert isinstance(value, atypes.LightColor)

    def test_canChangeColorFieldOfLightBulb(self, parser, smart_home):
        ast = parser.parse("koloro de sxambalulo estas rugxo")
        smart_home, value = ast.evaluate(smart_home)
        assert atypes.LightColor.RED.value == smart_home.variables["sxambalulo"].properties[
            "koloro"]
