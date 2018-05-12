import pytest

import compilation.abstract_syntax_tree as ast_bld
import compilation.esp_lexer as lxr
import compilation.node as Node

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
