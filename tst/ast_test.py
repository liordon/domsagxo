import biblioteko.atomaj_tipoj as tipo
import kompilajxo.leksisto as lxr
import kompilajxo.abstrakta_sintaksarbo as ast_bld
import pytest


lxr.build()


class TestAstMathExpressions(object):

    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="expression")

    def test_capableOfAddition(self, ast):
        assert 2 == (ast.parse("1+1"))

    def test_capableOfSubtraction(self, ast):
        assert -2 == (ast.parse("5-7"))

    def test_unaryMinusInvertsNumberValue(self, ast):
        assert -1 == ast.parse("-1")

    def test_capableOfMultiplication(self, ast):
        assert 42 == ast.parse("7*6")

    def test_unaryMinusDoesNotNeedParentheses(self, ast):
        assert -42 == ast.parse("7*-6")
        assert 1 == ast.parse("7+-6")

    def test_capableVerbalNumberParsing(self, ast):
        assert 926 == ast.parse("nauxcent dudek ses")
        assert 102 == ast.parse("cent du")

    def test_sameDigitCannotBeSpecifiedTwice(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("naux ses")

    def test_capableUsingAssignedVariable(self, ast):
        ast.variable_table["muso"] = 69
        assert 69 == ast.parse("muso")

    def test_valueOfReservedWordForTrueIsTrue(self, ast):
        assert ast.parse("vero")

    def test_valueOfReservedWordForFalseIsFalse(self, ast):
        assert not ast.parse("malvero")


class TestAstTimeSpans(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="timeSpan")

    @staticmethod
    def assertTimeSpan(parse_result, hours=0, minutes=0, seconds=0):
        assert isinstance(parse_result, tipo.TimeSpan)
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
        parse_result = ast.parse("dek horoj, dek minutoj kaj dek sekundoj")
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


class TestAstStatements(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="statement")

    def test_anExpressionIsAlsoAStatement(self, ast):
        ast.parse("12")

    def test_capableOfNumberAssignment(self, ast):
        ast.parse("kato = 10")
        assert 10 == ast.variable_table["kato"]

    def test_capableOfExpressionAssignment(self, ast):
        ast.parse("kato=2+4*10")
        assert 42 == ast.variable_table["kato"]

    def test_adjectivesJoinNounsToDefineVariableNames(self, ast):
        ast.parse("malgranda kato=7")
        assert 7 == ast.variable_table["malgranda kato"]
        with pytest.raises(KeyError):
            ast.variable_table["kato"]

    def test_definiteNounsBecomeIndefinite(self, ast):
        ast.parse("la kato = 9")
        assert 9 == ast.variable_table["kato"]

    def test_definiteDescribedNounAlsoBecomesIndefinite(self, ast):
        ast.parse("la dika kato = 99")
        assert 99 == ast.variable_table["dika kato"]


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

    def test_canGenerateRandomTimePointWithOverflowIntoNextHour(self, ast):
        parse_result = ast.parse("hazardu horon inter la oka kaj kvindek naux kaj la nauxa kaj dek")
        assert isinstance(parse_result, tipo.TimePoint)
        assert 9 == parse_result.hour
        assert 10 > parse_result.minutes


class TestAstPrograms(object):

    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="program")

    def test_unterminatedCommandIsNotAProgram(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("12")

    def test_canParseASingleCommandAsProgram(self, ast):
        ast.parse("12.")

    def test_consecutiveStatementsPropagateVariableValues(self, ast):
        ast.parse('''kato=2+4*10.
                        hundo = kato/6.''')
        assert 42 == ast.variable_table["kato"]
        assert 7 == ast.variable_table["hundo"]
