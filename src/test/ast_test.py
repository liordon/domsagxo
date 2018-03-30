import biblioteko.atomaj_tipoj as tipo
import kompilajxo.leksisto as lxr
import kompilajxo.abstrakta_sintaksarbo as ast_bld
import kompilajxo.nodo as Node
import pytest

lxr.build()


class TestBasicAstNodes(object):

    def test_parsedNumberReturnsNumberNode(self):
        ast = ast_bld.build(start="expression")
        parse_res = ast.parse("1")
        assert Node.Number == type(parse_res)

    def test_parsedVerbalNumberReturnsNumberNode(self):
        ast = ast_bld.build(start="expression")
        parse_res = ast.parse("unu")
        assert Node.Number == type(parse_res)

    def test_parsedNegativeNumberReturnsOperationNode(self):
        ast = ast_bld.build(start="expression")
        parse_res = ast.parse("-1")
        assert Node.MathOp == type(parse_res)

    def test_multiplicationReturnsOperationNode(self):
        ast = ast_bld.build(start="expression")
        parse_res = ast.parse("1*1")
        assert Node.MathOp == type(parse_res)

    def test_divisionReturnsOperationNode(self):
        ast = ast_bld.build(start="expression")
        parse_res = ast.parse("1/1")
        assert Node.MathOp == type(parse_res)

    def test_additionReturnsOperationNode(self):
        ast = ast_bld.build(start="expression")
        parse_res = ast.parse("1+1")
        assert Node.MathOp == type(parse_res)

    def test_subtractionReturnsOperationNode(self):
        ast = ast_bld.build(start="expression")
        parse_res = ast.parse("1-1")
        assert Node.MathOp == type(parse_res)


def parsed_value_of(ast, expr, state=None):
    node = ast.parse(expr)
    return node.evaluate(state)[1]


class TestAstMathExpressions(object):

    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="expression")

    def test_capableOfAddition(self, ast):
        assert 2 == (parsed_value_of(ast, "1+1"))

    def test_capableOfSubtraction(self, ast):
        assert -2 == (parsed_value_of(ast, "5-7"))

    def test_unaryMinusInvertsNumberValue(self, ast):
        assert -1 == parsed_value_of(ast, "-1")

    def test_capableOfMultiplication(self, ast):
        assert 42 == parsed_value_of(ast, "7*6")

    def test_unaryMinusDoesNotNeedParentheses(self, ast):
        assert -42 == parsed_value_of(ast, "7*-6")
        assert 1 == parsed_value_of(ast, "7+-6")

    def test_capableVerbalNumberParsing(self, ast):
        assert 926 == parsed_value_of(ast, "nauxcent dudek ses")
        assert 102 == parsed_value_of(ast, "cent du")

    def test_sameDigitCannotBeSpecifiedTwice(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("naux ses")

    def test_capableUsingAssignedVariable(self, ast):
        variable_table = {"muso": 69}
        assert 69 == parsed_value_of(ast, "muso", variable_table)

    def test_valueOfReservedWordForTrueIsTrue(self, ast):
        assert parsed_value_of(ast, "vero")

    def test_valueOfReservedWordForFalseIsFalse(self, ast):
        assert not parsed_value_of(ast, "malvero")


def evaluate_and_return_state(ast, statement, initial_state=None):
    if initial_state is None:
        initial_state = {}  # so as not to put a mutable default
    state, nothing = ast.parse(statement).evaluate(initial_state)
    return state


class TestAstStatements(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="statement")

    def test_anExpressionIsAlsoAStatement(self, ast):
        ast.parse("12")

    def test_capableOfNumberAssignment(self, ast):
        assert {'kato': 10} == evaluate_and_return_state(ast, "kato = 10")

    def test_capableOfExpressionAssignment(self, ast):
        assert {'kato': 42} == evaluate_and_return_state(ast, "kato=2+4*10")

    def test_adjectivesJoinNounsToDefineVariableNames(self, ast):
        new_state = evaluate_and_return_state(ast, "malgranda kato=7")
        assert {'malgranda kato': 7} == new_state
        assert 'kato' not in new_state

    def test_definiteNounsBecomeIndefinite(self, ast):
        assert {'kato': 9} == evaluate_and_return_state(ast, "la kato = 9")

    def test_definiteDescribedNounAlsoBecomesIndefinite(self, ast):
        assert {"dika kato": 99} == evaluate_and_return_state(ast, "la dika kato = 99")


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
        new_state = evaluate_and_return_state(ast, '''kato=2+4*10.
                        hundo = kato/6.''')
        assert {'kato': 42, 'hundo': 7} == new_state
