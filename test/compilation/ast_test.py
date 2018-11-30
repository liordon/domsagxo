import pytest

import compilation.abstract_syntax_tree as ast_bld
import compilation.esp_lexer as lxr
import compilation.node as Node
from test_utils import mocks

lxr.build()


class CanAssertNodeType(object):
    @staticmethod
    def assertThatExpressionIsOfNodeType(ast, expr, nodeType):
        assert isinstance(ast.parse(expr), nodeType)


class PartialNameLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.Var.PARTIAL_NAME.value)


class TestNameAndNumeratorAstNodes(PartialNameLevelAstProvided, CanAssertNodeType):
    def test_parsedAdjectiveReturnsDescriptionNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "bela", Node.Description)

    def test_parsedKeywordLaReturnsNoneNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "la", Node.NoneNode)

    def test_twoParsedAdjectivesReturnsDescriptionNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "bela bona", Node.Description)

    def test_parsedDigitalNumeratorReturnsDescriptionNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "unua", Node.Description)


class ExpressionLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.Var.EXPRESSION.value)


class TestBasicAstExpressionNodes(ExpressionLevelAstProvided, CanAssertNodeType):

    def test_parsedStringReturnsStringNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "''", Node.String)

    def test_parsedNumberReturnsNumberNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1", Node.Number)

    def test_parsedVerbalNumberReturnsNumberNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "unu", Node.Number)

    def test_parsedNegativeNumberReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "-1", Node.Subtract)
        self.assertThatExpressionIsOfNodeType(ast, "malpli unu", Node.Subtract)

    def test_multiplicationReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1*1", Node.Multiply)
        self.assertThatExpressionIsOfNodeType(ast, "unu fojoj unu", Node.Multiply)

    def test_divisionReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1/1", Node.Divide)
        self.assertThatExpressionIsOfNodeType(ast, "unu partoj unu", Node.Divide)

    def test_additionReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1+1", Node.Add)
        self.assertThatExpressionIsOfNodeType(ast, "unu pli unu", Node.Add)

    def test_subtractionReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1-1", Node.Subtract)
        self.assertThatExpressionIsOfNodeType(ast, "unu malpli unu", Node.Subtract)

    def test_nounReturnsVariableNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "sxambalulo", Node.VariableName)

    def test_adjectiveAndNounReturnsVariableNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "sxamba lulo", Node.VariableName)

    def test_fieldAccessReturnsDereferenceNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "sxa mbo de lulo", Node.Dereference)

    def test_useOfNumeratorReturnsArrayAccessNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "sxa mba de lulo", Node.ArrayAccess)


class TestReferenceSemantics(ExpressionLevelAstProvided):
    @pytest.fixture
    def state(self):
        return mocks.Bunch(variables={})

    def test_variableSetterCanBeUsedToChangeVariableValue(self, ast, state):
        variable_name = "sxambalulo"
        setter = ast.parse(variable_name).setter
        setter(state, 1)
        assert state.variables[variable_name] == 1

    def test_dereferenceSetterCanBeUsedToChangeVariableValue(self, ast, state):
        variable_name = "sxambo de lulo"
        setter = ast.parse(variable_name).setter
        state.variables["lulo"] = mocks.Bunch(properties={"sxambo": 0})
        setter(state, 2)
        assert 2 == state.variables["lulo"].properties["sxambo"]

    def test_recursiveDereferenceSettersCanStillBeUsedToChangeValue(self, ast, state):
        variable_name = "sxambo de lulo de kahxolo"
        setter = ast.parse(variable_name).setter
        state.variables["kahxolo"] = mocks.Bunch(properties={
            "lulo": mocks.Bunch(properties={
                "sxambo": 0
            })
        })
        setter(state, 3)
        assert 3 == state.variables["kahxolo"].properties["lulo"].properties["sxambo"]

    def test_cannotUseSetterToSetNonExistingProperty(self, ast, state):
        variable_name = "sxambo de lulo"
        setter = ast.parse(variable_name).setter
        state.variables["lulo"] = mocks.Bunch(properties={})
        with pytest.raises(KeyError):
            setter(state, 3)

    def test_cannotUseArrayAccessOnNonIterable(self, ast, state):
        variable_name = "unua de ampoloj"
        setter = ast.parse(variable_name).setter
        state.variables["ampoloj"] = 3
        with pytest.raises(TypeError):
            setter(state, 3)

    def test_outOfBoundsArrayAccessThrowsIndexError(self, ast, state):
        variable_name = "sesa de ampoloj"
        setter = ast.parse(variable_name).setter
        state.variables["ampoloj"] = [1, 2, 3]
        with pytest.raises(IndexError):
            setter(state, 6)

    def test_arrayNumerationStartsAtOneAndNotZero(self, ast, state):
        variable_name = "unua de ampoloj"
        state.variables["ampoloj"] = [1]
        assert 1 == state.variables["ampoloj"][0]
        assert 1 == ast.parse(variable_name).getter(state)
        ast.parse(variable_name).setter(state, 6)
        assert 6 == state.variables["ampoloj"][0]

    def test_canAssignIntoIndexOfExistingArray(self, ast, state):
        variable_name = "dua de ampoloj"
        setter = ast.parse(variable_name).setter
        state.variables["ampoloj"] = [1, 2, 3]
        setter(state, 6)
        assert 6 == ast.parse(variable_name).getter(state)
        assert 6 == state.variables["ampoloj"][1]

    def test_canUseVariableAsIndexOfArray(self, ast, state):
        variable_name = "kata de ampoloj"
        setter = ast.parse(variable_name).setter
        state.variables["ampoloj"] = [1, 2, 3]
        state.variables["kato"] = 2
        setter(state, 5)
        assert 5 == ast.parse(variable_name).getter(state)
        assert 5 == state.variables["ampoloj"][1]


def parsed_value_of(ast, expr, state=None):
    node = ast.parse(expr)
    return node.evaluate(state)[1]


class TestAstMathExpressions(ExpressionLevelAstProvided):

    def test_capableVerbalNumberParsing(self, ast):
        assert 926 == parsed_value_of(ast, "nauxcent dudek ses")
        assert 102 == parsed_value_of(ast, "cent du")

    def test_capableOfAddition(self, ast):
        assert 2 == (parsed_value_of(ast, "1+1"))
        assert 2 == (parsed_value_of(ast, "unu pli unu"))

    def test_capableOfSubtraction(self, ast):
        assert -2 == (parsed_value_of(ast, "5-7"))
        assert -2 == (parsed_value_of(ast, "kvin malpli sep"))

    def test_unaryMinusInvertsNumberValue(self, ast):
        assert -1 == parsed_value_of(ast, "-1")
        assert -1 == parsed_value_of(ast, "malpli unu")

    def test_capableOfMultiplication(self, ast):
        assert 42 == parsed_value_of(ast, "7*6")
        assert 42 == parsed_value_of(ast, "sep fojoj ses")

    def test_unaryMinusDoesNotNeedParentheses(self, ast):
        assert -42 == parsed_value_of(ast, "7*-6")
        assert -42 == parsed_value_of(ast, "sep fojoj malpli ses")
        assert 1 == parsed_value_of(ast, "7+-6")
        assert 1 == parsed_value_of(ast, "sep pli malpli ses")

    def test_sameDigitCannotBeSpecifiedTwice(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("naux ses")


class TestAstBooleanExpressions(ExpressionLevelAstProvided):

    def test_valueOfReservedWordForTrueIsTrue(self, ast):
        assert parsed_value_of(ast, "vero")

    def test_valueOfReservedWordForFalseIsFalse(self, ast):
        assert not parsed_value_of(ast, "malvero")

    def test_functionTableOfOperator_not(self, ast):
        assert not parsed_value_of(ast, "ne vero")
        assert parsed_value_of(ast, "ne malvero")

    def test_functionTableOfOperator_and(self, ast):
        assert parsed_value_of(ast, "vero ambaux vero")
        assert not parsed_value_of(ast, "vero ambaux malvero")
        assert not parsed_value_of(ast, "malvero ambaux vero")
        assert not parsed_value_of(ast, "malvero ambaux malvero")

    def test_functionTableOfOperator_or(self, ast):
        assert parsed_value_of(ast, "vero aux vero")
        assert parsed_value_of(ast, "vero aux malvero")
        assert parsed_value_of(ast, "malvero aux vero")
        assert not parsed_value_of(ast, "malvero aux malvero")


class TestAstRelationalExpressions(ExpressionLevelAstProvided):
    @pytest.fixture
    def relation_ast(self):
        return ast_bld.build(start=ast_bld.Var.RELATION.value)

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
