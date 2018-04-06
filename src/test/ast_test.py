import pytest

import biblioteko.estra_komponantoj as esk
import kompilajxo.abstrakta_sintaksarbo as ast_bld
import kompilajxo.leksisto as lxr
import kompilajxo.nodo as Node

lxr.build()


class ExpressionLevelAstProvided(object):

    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="expression")


class TestBasicAstNodes(ExpressionLevelAstProvided):

    def test_parsedNumberReturnsNumberNode(self, ast):
        parse_res = ast.parse("1")
        assert Node.Number == type(parse_res)

    def test_parsedVerbalNumberReturnsNumberNode(self, ast):
        parse_res = ast.parse("unu")
        assert Node.Number == type(parse_res)

    def test_parsedNegativeNumberReturnsOperationNode(self, ast):
        parse_res = ast.parse("-1")
        assert Node.MathOp == type(parse_res)

    def test_multiplicationReturnsOperationNode(self, ast):
        parse_res = ast.parse("1*1")
        assert Node.MathOp == type(parse_res)

    def test_divisionReturnsOperationNode(self, ast):
        parse_res = ast.parse("1/1")
        assert Node.MathOp == type(parse_res)

    def test_additionReturnsOperationNode(self, ast):
        parse_res = ast.parse("1+1")
        assert Node.MathOp == type(parse_res)

    def test_subtractionReturnsOperationNode(self, ast):
        parse_res = ast.parse("1-1")
        assert Node.MathOp == type(parse_res)


def parsed_value_of(ast, expr, state=None):
    node = ast.parse(expr)
    return node.evaluate(state)[1]


class TestAstMathExpressions(ExpressionLevelAstProvided):

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

    def test_valueOfReservedWordForTrueIsTrue(self, ast):
        assert parsed_value_of(ast, "vero")

    def test_valueOfReservedWordForFalseIsFalse(self, ast):
        assert not parsed_value_of(ast, "malvero")

    def test_canUseEqualsSignToCompare(self, ast):
        assert not parsed_value_of(ast, "ses estas sep")
        assert parsed_value_of(ast, "ses estas ses")


class TestAstBooleanExpressions(ExpressionLevelAstProvided):
    @pytest.fixture
    def relation_ast(self):
        return ast_bld.build(start="relation")

    def test_existsAnEqualityRelation(self, relation_ast):
        node = relation_ast.parse("estas egala al")
        assert node is not None

    def test_existsAGreatnessRelation(self, relation_ast):
        node = relation_ast.parse("estas pli granda ol")
        assert node is not None

    def test_existsAnEqualOrGreatnessRelation(self, relation_ast):
        node = relation_ast.parse("estas pli granda aux egala al")
        assert node is not None

    def test_existsASmallnessRelation(self, relation_ast):
        node = relation_ast.parse("estas pli malgranda ol")
        assert node is not None

    def test_existsAnEqualOrSmallnessRelation(self, relation_ast):
        node = relation_ast.parse("estas pli malgranda aux egala al")
        assert node is not None

    def test_canEvaluateEqualityBetweenNumberAndItself(self, ast):
        assert parsed_value_of(ast, "unu estas egala al unu")
        assert not parsed_value_of(ast, "unu estas egala al du")
        assert not parsed_value_of(ast, "du estas egala al unu")

    def test_canEvaluateInequalityBetweenNumberAndItself(self, ast):
        assert not parsed_value_of(ast, "unu ne estas egala al unu")
        assert parsed_value_of(ast, "unu ne estas egala al du")
        assert parsed_value_of(ast, "du ne estas egala al unu")

    def test_canEvaluateGreatnessBetween2Numbers(self, ast):
        assert not parsed_value_of(ast, "unu estas pli granda ol unu")
        assert not parsed_value_of(ast, "unu estas pli granda ol du")
        assert parsed_value_of(ast, "du estas pli granda ol unu")

    def test_canEvaluateUnGreatnessBetween2Numbers(self, ast):
        assert parsed_value_of(ast, "unu ne estas pli granda ol unu")
        assert parsed_value_of(ast, "unu ne estas pli granda ol du")
        assert not parsed_value_of(ast, "du ne estas pli granda ol unu")

    def test_canEvaluateGreatnessOrEqualityBetween2Numbers(self, ast):
        assert parsed_value_of(ast, "unu estas pli granda aux egala al unu")
        assert not parsed_value_of(ast, "unu estas pli granda aux egala al du")
        assert parsed_value_of(ast, "du estas pli granda aux egala al unu")

    def test_canEvaluateUnGreatnessOrEqualityBetween2Numbers(self, ast):
        assert not parsed_value_of(ast, "unu ne estas pli granda aux egala al unu")
        assert parsed_value_of(ast, "unu ne estas pli granda aux egala al du")
        assert not parsed_value_of(ast, "du ne estas pli granda aux egala al unu")

    def test_canEvaluateSmallnessBetween2Numbers(self, ast):
        assert not parsed_value_of(ast, "unu estas pli malgranda ol unu")
        assert parsed_value_of(ast, "unu estas pli malgranda ol du")
        assert not parsed_value_of(ast, "du estas pli malgranda ol unu")

    def test_canEvaluateUnSmallnessBetween2Numbers(self, ast):
        assert parsed_value_of(ast, "unu ne estas pli malgranda ol unu")
        assert not parsed_value_of(ast, "unu ne estas pli malgranda ol du")
        assert parsed_value_of(ast, "du ne estas pli malgranda ol unu")

    def test_canEvaluateSmallnessOrEqualityBetween2Numbers(self, ast):
        assert parsed_value_of(ast, "unu estas pli malgranda aux egala al unu")
        assert parsed_value_of(ast, "unu estas pli malgranda aux egala al du")
        assert not parsed_value_of(ast, "du estas pli malgranda aux egala al unu")

    def test_canEvaluateUnSmallnessOrEqualityBetween2Numbers(self, ast):
        assert not parsed_value_of(ast, "unu ne estas pli malgranda aux egala al unu")
        assert not parsed_value_of(ast, "unu ne estas pli malgranda aux egala al du")
        assert parsed_value_of(ast, "du ne estas pli malgranda aux egala al unu")


def evaluate_and_return_state_variables(ast, statement, initial_state=None):
    if initial_state is None:
        initial_state = esk.Domsagxo()  # so as not to put a mutable default
    state, nothing = ast.parse(statement).evaluate(initial_state)
    return state.variables


class TestAstStatements(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="statement")

    def test_anExpressionIsAlsoAStatement(self, ast):
        ast.parse("12")

    def test_capableOfNumberAssignment(self, ast):
        assert {'kato': 10} == evaluate_and_return_state_variables(ast, "kato = 10")

    def test_capableOfExpressionAssignment(self, ast):
        assert {'kato': 42} == evaluate_and_return_state_variables(ast, "kato=2+4*10")

    def test_adjectivesJoinNounsToDefineVariableNames(self, ast):
        new_state = evaluate_and_return_state_variables(ast, "nigra kato=7")
        assert {'nigra kato': 7} == new_state
        assert 'kato' not in new_state

    def test_reservedAdjectivesCanBeUsedForVariableNamesOutOfReservedContext(self, ast):
        new_state = evaluate_and_return_state_variables(ast, "malgranda kato estas 7")
        assert {'malgranda kato': 7} == new_state
        new_state = evaluate_and_return_state_variables(ast, "granda kato estas 17")
        assert {'granda kato': 17} == new_state

    def test_definiteNounsBecomeIndefinite(self, ast):
        assert {'kato': 9} == evaluate_and_return_state_variables(ast, "la kato = 9")

    def test_definiteDescribedNounAlsoBecomesIndefinite(self, ast):
        assert {"dika kato": 99} == evaluate_and_return_state_variables(ast, "la dika kato = 99")

    def test_ifStatementContentIsNotEvaluatedIfConditionIsFalse(self, ast):
        assert {} == evaluate_and_return_state_variables(
            ast, "se malvero tiam kato estas sep. finu")

    def test_ifStatementContentIsEvaluatedIfConditionIsTrue(self, ast):
        assert {'kato': 7} == evaluate_and_return_state_variables(
            ast, "se vero tiam kato estas sep. finu")

    def test_elseStatementContentIsEvaluatedIfConditionIsFalse(self, ast):
        assert {'kato': 9} == evaluate_and_return_state_variables(
            ast, "se malvero tiam kato estas sep. alie kato estas naux. finu")

    def test_canDefineSimpleWhileLoopThatDoesNotEvaluate(self, ast):
        assert {} == evaluate_and_return_state_variables(
            ast, "dum malvero tiam kato estas sep. finu")

    def test_canDefineWhileLoopThatEvaluatesOnce(self, ast):
        initial_state = esk.Domsagxo()
        initial_state.variables["kato"] = 1
        assert {'kato': 0} == evaluate_and_return_state_variables(
            ast, "dum kato estas pli granda ol nul tiam kato estas kato-1. finu", initial_state)

    def test_canDefineWhileLoopThatEvaluatesFiveTimes(self, ast):
        initial_state = esk.Domsagxo()
        initial_state.variables["kato"] = 5
        assert {'kato': 0} == evaluate_and_return_state_variables(
            ast, "dum kato estas pli granda ol nul tiam kato estas kato-1. finu", initial_state)


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
        new_state = evaluate_and_return_state_variables(ast, '''kato=2+4*10.
                        hundo = kato/6.''')
        assert {'kato': 42, 'hundo': 7} == new_state

    def test_returnStatementReturnsItsDeclaredValue(self, ast):
        value = parsed_value_of(ast, '''revenu ses.''')
        assert 6 == value

    def test_returnStopsProgramFromContinuing(self, ast):
        state = esk.Domsagxo()
        new_state, value = ast.parse('''revenu ses. kato = 10.''').evaluate(state)
        assert 6 == value
        assert {} == new_state.variables

    def test_returnStopsWhileLoopFromContinuing(self, ast):
        initial_state = esk.Domsagxo()
        state, value = ast.parse("""
        kato estas naux.
        dum kato estas pli granda ol nul tiam
            kato estas kato-1.
            se kato estas egala al tri tiam
                revenu kato.
            finu.
        finu.
        """).evaluate(initial_state)
        assert {'kato': 3} == state.variables
        assert 3 == value
