import pytest

import domsagxo.compilation.abstract_syntax_tree as ast_bld
from domsagxo.compilation import node
from domsagxo.compilation.definitions import EsperantoLocatedSyntaxError, EsperantoSyntaxError, GrammarVariable
from domsagxo.library import mocks
from test.test_utils.providers import PartialNameLevelAstProvided, ExpressionLevelAstProvided, \
    MockSmartHomeStateVariablesProvided, TopLevelAstProvided


def parsed_value_of(ast, expr, state=None):
    ast_node = ast.parse(expr)
    return ast_node.evaluate(state)[1]


class CanAssertNodeType(object):
    @staticmethod
    def assertThatExpressionIsOfNodeType(ast, expr, node_type):
        assert isinstance(ast.parse(expr), node_type)


class TestVerbalNumbers(ExpressionLevelAstProvided):

    def test_canParseDigit0(self, ast):
        assert parsed_value_of(ast, "nul") == 0

    def test_canParseDigit1(self, ast):
        assert parsed_value_of(ast, "unu") == 1

    def test_canParseDigit2(self, ast):
        assert parsed_value_of(ast, "du") == 2

    def test_canParseDigit3(self, ast):
        assert parsed_value_of(ast, "tri") == 3

    def test_canParseDigit4(self, ast):
        assert parsed_value_of(ast, "kvar") == 4

    def test_canParseDigit5(self, ast):
        assert parsed_value_of(ast, "kvin") == 5

    def test_canParseDigit6(self, ast):
        assert parsed_value_of(ast, "ses") == 6

    def test_canParseDigit7(self, ast):
        assert parsed_value_of(ast, "sep") == 7

    def test_canParseDigit8(self, ast):
        assert parsed_value_of(ast, "ok") == 8

    def test_canParseDigit9(self, ast):
        assert parsed_value_of(ast, "naux") == 9

    def test_canParseNumber10(self, ast):
        assert parsed_value_of(ast, "dek") == 10

    def test_canParseNumber20(self, ast):
        assert parsed_value_of(ast, "dudek") == 20

    def test_canParseNumber100(self, ast):
        assert parsed_value_of(ast, "cent") == 100

    def test_canParseNumber248(self, ast):
        assert parsed_value_of(ast, "ducent kvardek ok") == 248

    def test_canParseFractionHalf(self, ast):
        assert parsed_value_of(ast, "duono") == 1 / 2

    def test_canParseFractionQuarter(self, ast):
        assert parsed_value_of(ast, "kvarono") == 1 / 4


class TestNameAndOrdinalAstNodes(PartialNameLevelAstProvided, CanAssertNodeType):
    def test_parsedAdjectiveReturnsDescriptionNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "bela", node.Description)

    def test_twoParsedAdjectivesReturnsDescriptionNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "bela bona", node.Description)

    def test_parsedDigitalOrdinalReturnsDescriptionNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "unua", node.Description)


class TestBasicAstExpressionNodes(ExpressionLevelAstProvided, CanAssertNodeType):

    def test_parsedStringReturnsStringNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "''", node.String)

    def test_parsedNumberReturnsNumberNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1", node.Number)

    def test_parsedVerbalNumberReturnsNumberNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "unu", node.Number)

    def test_parsedNegativeNumberReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "-1", node.Subtract)
        self.assertThatExpressionIsOfNodeType(ast, "malpli unu", node.Subtract)

    def test_multiplicationReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1*1", node.Multiply)
        self.assertThatExpressionIsOfNodeType(ast, "unu fojoj unu", node.Multiply)

    def test_divisionReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1/1", node.Divide)
        self.assertThatExpressionIsOfNodeType(ast, "unu partoj unu", node.Divide)

    def test_additionReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1+1", node.Add)
        self.assertThatExpressionIsOfNodeType(ast, "unu pli unu", node.Add)

    def test_subtractionReturnsOperationNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "1-1", node.Subtract)
        self.assertThatExpressionIsOfNodeType(ast, "unu malpli unu", node.Subtract)

    def test_calculationInParenthesesReturnParenthesesNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "(1-1)", node.Parentheses)
        self.assertThatExpressionIsOfNodeType(ast, "krampo unu malpli unu malkrampo", node.Parentheses)

    def test_nounReturnsVariableNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "sxambalulo", node.VariableName)

    def test_adjectiveAndNounReturnsVariableNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "sxamba lulo", node.VariableName)

    def test_fieldAccessReturnsDereferenceNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "sxa mbo de lulo", node.Dereference)

    def test_useOfOrdinalVariableReturnsArrayAccessNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "sxa mba de lulo", node.ArrayAccess)

    def test_useOfIndefiniteOrdinalNumberReturnsArrayAccessNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "unua de sxambaluloj", node.ArrayAccess)

    def test_useOfDefiniteOrdinalNumberReturnsArrayAccessNode(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "la unua de sxambaluloj", node.ArrayAccess)

    def test_useOfKeywordItIsAValidExpressionUntoItself(self, ast):
        self.assertThatExpressionIsOfNodeType(ast, "gxi", node.VariableName)
        self.assertThatExpressionIsOfNodeType(ast, "gxin", node.VariableName)
        self.assertThatExpressionIsOfNodeType(ast, "ĝi", node.VariableName)
        self.assertThatExpressionIsOfNodeType(ast, "ĝin", node.VariableName)


class TestReferenceSemantics(ExpressionLevelAstProvided, MockSmartHomeStateVariablesProvided):

    def test_nounVariableValueCanBeAssessedAsExpression(self, ast, state):
        variable_name = "sxambalulo"
        state.variables[variable_name] = 1
        assert ast.parse(variable_name).getter(state) == 1

    def test_adjectiveNounVariableCanBeAssessedAsExpression(self, ast, state):
        variable_name = "sxamba lulo"
        state.variables[variable_name] = 2
        assert ast.parse(variable_name).getter(state) == 2

    def test_ordinalNounVariableCanBeAssessedAsExpression(self, ast, state):
        variable_name = "tria sxambalulo"
        state.variables[variable_name] = 3
        assert ast.parse(variable_name).getter(state) == 3

    def test_definiteOrdinalNounVariableCanBeAssessedAsExpression(self, ast, state):
        variable_name = "kvara sxambalulo"
        definite_variable_name = "la " + variable_name
        state.variables[variable_name] = 4
        assert ast.parse(definite_variable_name).getter(state) == 4

    def test_indefiniteLargeOrdinalNounVariableCanBeAssessedAsExpression(self, ast, state):
        variable_name = "kvindek kvara sxambalulo"
        state.variables[variable_name] = 54
        assert ast.parse(variable_name).getter(state) == 54

    def test_definiteLargeOrdinalNounVariableCanBeAssessedAsExpression(self, ast, state):
        variable_name = "kvindek kvina sxambalulo"
        definite_variable_name = "la " + variable_name
        state.variables[variable_name] = 55
        assert ast.parse(definite_variable_name).getter(state) == 55

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

    def test_definiteArticleArrayAccessWorksJustAsWellAsIndefinite(self, ast, state):
        variable_name = "la unua de ampoloj"
        state.variables["ampoloj"] = [1]
        assert ast.parse(variable_name).getter(state) == 1
        assert parsed_value_of(ast, variable_name, state) == 1

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

    def test_parenthesesCanAlterCalculationOrder(self, ast):
        assert 9 == parsed_value_of(ast, "(1+2)*3")
        assert 9 == parsed_value_of(ast, "krampo unu pli du malkrampo fojoj tri")

    def test_sameDigitCannotBeSpecifiedTwice(self, ast):
        with pytest.raises(EsperantoSyntaxError):
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
        return ast_bld.build(start=GrammarVariable.RELATION.value)

    def test_existsAnEqualityRelation(self, relation_ast):
        ast_node = relation_ast.parse("estas egala al")
        assert ast_node is not None

    def test_existsAGreatnessRelation(self, relation_ast):
        ast_node = relation_ast.parse("estas pli granda ol")
        assert ast_node is not None

    def test_existsAnEqualOrGreatnessRelation(self, relation_ast):
        ast_node = relation_ast.parse("estas pli granda aux egala al")
        assert ast_node is not None

    def test_existsASmallnessRelation(self, relation_ast):
        ast_node = relation_ast.parse("estas pli malgranda ol")
        assert ast_node is not None

    def test_existsAnEqualOrSmallnessRelation(self, relation_ast):
        ast_node = relation_ast.parse("estas pli malgranda aux egala al")
        assert ast_node is not None

    def test_unalphabeticComparisonOperatorsAreEquivalentToSpokenOperators(self, relation_ast):
        assert relation_ast.parse("estas pli granda ol") == relation_ast.parse(">")
        assert relation_ast.parse("estas pli malgranda ol") == relation_ast.parse("<")
        assert relation_ast.parse("estas pli granda aux egala al") == relation_ast.parse(">=")
        assert relation_ast.parse("estas pli malgranda aux egala al") == relation_ast.parse("<=")
        assert relation_ast.parse("estas pli granda aux egala al") == relation_ast.parse("≥")
        assert relation_ast.parse("estas pli malgranda aux egala al") == relation_ast.parse("≤")

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


class TestPinPointingOffendingTokenOnSyntaxErrors(TopLevelAstProvided):
    def test_canIndicateProblemInVeryFirstToken(self, ast):
        with pytest.raises(EsperantoLocatedSyntaxError) as exception_info:
            ast.parse("finu")
        assert exception_info.value.index == 0

    def test_canIdentifyProblemAfterArithmeticNode(self, ast):
        with pytest.raises(EsperantoLocatedSyntaxError) as exception_info:
            ast.parse("asignu 1 + 1 + finu")
        assert exception_info.value.index == 5

    def test_canIdentifyProblemAfterMethodParameters(self, ast):
        with pytest.raises(EsperantoLocatedSyntaxError) as exception_info:
            ast.parse("sxambaluli unuo kaj duo kaj trio signifas finu")
        assert exception_info.value.index == 7

    def test_canIdentifyProblemAfterEntireMethodDefinition(self, ast):
        with pytest.raises(EsperantoLocatedSyntaxError) as exception_info:
            ast.parse("sxambaluli unuo kaj duo kaj trio signifas revenu finu finu")
        assert exception_info.value.index == 9
