import kompilajxo.atomic_types as type
import kompilajxo.lexer_builder as lxr
import kompilajxo.ast_builder as ast_bld
import pytest


class TestAstMathExpressions(object):

    @pytest.fixture
    def ast(self):
        lxr.build()
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


class TestAstTimes(object):
    @pytest.fixture
    def ast(self):
        lxr.build()
        return ast_bld.build(start="timePoint")

    def test_canFormatFormalRoundHour(self, ast):
        assert isinstance(ast.parse("la sesa horo"), type.TimePoint)

    def test_canFormatColloquialRoundHour(self, ast):
        assert isinstance(ast.parse("la sesa"), type.TimePoint)

    # def test_canFormatFormalFracturedHour(self, ast):
    #     assert isinstance(ast.parse("la sesa horo kaj kvardek ses minutoj"), type.TimePoint)
    #
    # def test_canFormatColloquialFracturedHour(self, ast):
    #     assert isinstance(ast.parse("la sesakaj kvardek ses"), type.TimePoint)


# noinspection PyStatementEffect
class TestAstStatements(object):

    @pytest.fixture
    def ast(self):
        lxr.build()
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

    # def test_capableOfIfStatement(self, ast):
    #     ast.parse("se vero: kato = 1")
    #     assert 1 == ast.variable_table["kato"]


class TestAstPrograms(object):

    @pytest.fixture
    def ast(self):
        lxr.build()
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
