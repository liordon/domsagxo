from domsagxo.compilation.node import *
from test.test_utils.providers import SyntaxNodesProvided


class TestNodeRepresentation(object):
    def test_abstractSyntaxTreeNodesCanRepresentThemselvesAndTheirArgs(self):
        assert AstNode(1, 2, AstNode()).__repr__() == "AstNode(1, 2, AstNode)"

    def test_abstractSyntaxTreeNodesRepresentationIncludesKwargs(self):
        assert AstNode(1, 2, shambalulu=3).__repr__() == "AstNode(1, 2, shambalulu=3)"

    def test_basicNodesHaveShorterStringRepresentation(self):
        assert str(Number(3)) == "3"
        assert str(String("saluton")) == "`saluton'"
        assert str(Boolean(True)) == "True"
        assert str(Boolean(False)) == "False"

    def test_emptyTimeSpansArePrintedAs0seconds(self):
        assert str(TimeSpan()) == "0s"
        assert str(TimeSpan(seconds=Number(0))) == "0s"

    def test_compositeTimeSpansAreShownAsNumberAndUnitPairs(self):
        assert str(TimeSpan(seconds=Number(42))) == "42s"
        assert str(TimeSpan(minutes=Number(40), seconds=Number(2))) == "40m 2s"
        assert str(TimeSpan(days=Number(40), hours=Number(2))) == "40d 2h"

    def test_binaryOperationNodesWithAtomicArgumentsCanBeSimplyRepresented(self):
        assert Add(Number(4), Number(2)).__repr__() == "Add(Number(4), Number(2))"
        assert Subtract(Number(4), Number(2)).__repr__() == "Subtract(Number(4), Number(2))"


class TestNodePrettyPrinting(object):
    def test_canUsePrettyPrintFunctionToPrintASingleNode(self):
        assert AstNode().pretty_print() == "└- AstNode"

    def test_canUsePrettyPrintFunctionOnAtomicNodeTypes(self):
        assert Number(3).pretty_print() == "└- 3"
        assert String("saluton").pretty_print() == "└- `saluton'"
        assert NoneNode().pretty_print() == "└- NoneNode"

    def test_canUsePrettyPrintOnVariablesAndArrays(self):
        assert VariableName('lulo', NoneNode()).pretty_print() == "└- <lulo>"
        assert VariableName('lulo',
            Description('mba', Description('sxa', NoneNode()))
        ).pretty_print() == \
               "└- <sxa mba lulo>"

    def test_canPrettyPrintVariableAssignments(self):
        varAss = VariableAssignment(VariableName('lulo', NoneNode()),
            VariableName('sxambo', NoneNode()))
        assert varAss.pretty_print() == \
               "└- VariableAssignment" + \
               "\n\t├- <lulo>" + \
               "\n\t└- <sxambo>"

    def test_canUsePrettyPrintFunctionToPrintSingleDescendantNode(self):
        assert AstNode(AstNode()).pretty_print() == \
               "└- AstNode" + \
               "\n\t└- AstNode"

    def test_kwargsAreSpecifiedByNameWhenPrettyPrinted(self):
        assert AstNode(AstNode()).pretty_print() == \
               "└- AstNode" + \
               "\n\t└- AstNode"

    def test_canUsePrettyPrintFunctionToPrintTwoDescendantNodes(self):
        assert AstNode(AstNode(), AstNode()).pretty_print() == \
               "└- AstNode" + \
               "\n\t├- AstNode" + \
               "\n\t└- AstNode"

    def test_canUsePrettyPrintFunctionOnDifferentNodeTypes(self):
        assert Add(Number(4), Number(2)).pretty_print() == \
               "└- Add" + \
               "\n\t├- 4" + \
               "\n\t└- 2"""

    def test_routineInvocationsShouldBePrettyPrintedWithTheirArguments(self):
        assert RoutineInvocation("sxambalulu", (Number(42),)).pretty_print() == \
               "└- RoutineInvocation: sxambalulu" + "\n\t└- 42"

    def test_routineDefinitionsShouldBePrettyPrintedWithTheirStatements(self):
        assert RoutineDefinition("sxambalulu", ["kato", "hundo"],
            VariableAssignment(VariableName('lulo', NoneNode()),
                VariableName('sxambo', NoneNode()))
        ).pretty_print() == \
               "└- RoutineDefinition: sxambalulu" + \
               "\n\t├- Arguments:" + \
               "\n\t│\t├- kato" + \
               "\n\t│\t└- hundo" + \
               "\n\t└- VariableAssignment" + \
               "\n\t\t├- <lulo>" + \
               "\n\t\t└- <sxambo>"

    def test_canUsePrettyPrintFunctionToPrintManyDescendantNodes(self):
        pretty_print = AstNode(
            AstNode(AstNode(),
                AstNode(AstNode(), AstNode(), AstNode())
            )
        ).pretty_print()

        assert pretty_print == \
               "└- AstNode" + \
               "\n\t└- AstNode" + \
               "\n\t\t├- AstNode" + \
               "\n\t\t└- AstNode" + \
               "\n\t\t\t├- AstNode" + \
               "\n\t\t\t├- AstNode" + \
               "\n\t\t\t└- AstNode"

    def test_whenHavingMultipleChildrenTreeRemainsWellConnected(self):
        pretty_print = Add(
            Multiply(Number(2), Number(3)),
            Divide(Number(0), Number(5))
        ).pretty_print()
        assert pretty_print == \
               "└- Add" + \
               "\n\t├- Multiply" + \
               "\n\t│\t├- 2" + \
               "\n\t│\t└- 3" + \
               "\n\t└- Divide" + \
               "\n\t\t├- 0" + \
               "\n\t\t└- 5"


class TestNumberOfTokensPerNode(SyntaxNodesProvided):

    def test_noneNodeHasNoTokensInIt(self):
        assert NoneNode().total_number_of_tokens() == 0

    def test_numberLiteralsAreAlwaysSingleTokens(self):
        assert Number(2).total_number_of_tokens() == 1
        assert Number(42).total_number_of_tokens() == 1

    def test_BooleanNodeIsAlwaysSingeToken(self):
        assert Boolean(True).total_number_of_tokens() == 1
        assert Boolean(False).total_number_of_tokens() == 1

    def test_BooleanOperationNodesAreWorth1TokenPlusArguments(self, tokenless_node, double_token_node):
        assert LogicAnd(Boolean(True), Boolean(False)).total_number_of_tokens() == 3
        assert LogicAnd(tokenless_node,
            LogicAnd(tokenless_node, tokenless_node)
        ).total_number_of_tokens() == 2
        assert LogicAnd(tokenless_node, double_token_node).total_number_of_tokens() == 3

    def test_arithmeticOperatorNodesAreConsideredTokens(self):
        assert Add(Number(40), Number(2)).total_number_of_tokens() == 3
        assert Add(Divide(Number(4), Number(0)), Number(2)).total_number_of_tokens() == 5
        assert Parentheses(Number(42)).total_number_of_tokens() == 3

    def test_descriptionNodeHasAsManyTokensAsAdjectives(self):
        assert Description("dika").total_number_of_tokens() == 1
        assert Description("dika", Description("nigra")).total_number_of_tokens() == 2

    def test_variableNameNodeHasNumberOfTokensEqualToNumberOfWords(self):
        assert VariableName("hundo", NoneNode()).total_number_of_tokens() == 1
        assert VariableName("kato", Description("dika")).total_number_of_tokens() == 2
        assert VariableName("kato", Description("dika", Description("nigra"))).total_number_of_tokens() == 3

    def test_assignmentStatementHas2TokensMoreThanItsComponents(self, tokenless_node, double_token_node):
        assert VariableAssignment(tokenless_node, tokenless_node).total_number_of_tokens() == 2
        assert VariableAssignment(double_token_node, double_token_node).total_number_of_tokens() == 6

    def test_ifStatementHas3TokensIfThereIsNoElseClauseAnd4IfThereIsPlusInternals(self, tokenless_node,
            single_token_node, double_token_node):
        assert ConditionalStatement(tokenless_node, tokenless_node).total_number_of_tokens() == 3
        assert ConditionalStatement(tokenless_node, tokenless_node, tokenless_node).total_number_of_tokens() == 4
        assert ConditionalStatement(single_token_node, double_token_node).total_number_of_tokens() == 6
        assert ConditionalStatement(tokenless_node, single_token_node, double_token_node).total_number_of_tokens() == 7

    def test_whileStatementHas3TokensBesidesItsConditionsAndLoopBody(self, tokenless_node,
            single_token_node, double_token_node):
        assert LoopStatement(tokenless_node, tokenless_node).total_number_of_tokens() == 3
        assert LoopStatement(single_token_node, double_token_node).total_number_of_tokens() == 6

    def test_comparisonOperatorsContain3To6TokensPlusArguments(self, tokenless_node,
            single_token_node, double_token_node):
        assert Comparison(tokenless_node, Comparison.Relation.EQUAL, tokenless_node).total_number_of_tokens() == 3
        assert Comparison(tokenless_node, Comparison.Relation.GREATER_OR_EQUAL,
            tokenless_node).total_number_of_tokens() == 6
        assert Comparison(single_token_node, Comparison.Relation.EQUAL, double_token_node).total_number_of_tokens() == 6
        assert Comparison(double_token_node, Comparison.Relation.GREATER_OR_EQUAL,
            double_token_node).total_number_of_tokens() == 10

    def test_negatedComparisonOperatorsContain4Or7TokensPlusArguments(self, tokenless_node,
            single_token_node, double_token_node):
        assert Comparison(tokenless_node, Comparison.Relation.EQUAL,
            tokenless_node).reverse().total_number_of_tokens() == 4
        assert Comparison(tokenless_node, Comparison.Relation.GREATER_OR_EQUAL,
            tokenless_node).reverse().total_number_of_tokens() == 7
        assert Comparison(single_token_node, Comparison.Relation.EQUAL,
            double_token_node).reverse().total_number_of_tokens() == 7
        assert Comparison(double_token_node, Comparison.Relation.GREATER_OR_EQUAL,
            double_token_node).reverse().total_number_of_tokens() == 11

    def test_dereferenceNodesContain1TokenPlusInternals(self, tokenless_node, single_token_node):
        assert Dereference(tokenless_node, tokenless_node).total_number_of_tokens() == 1
        assert Dereference(single_token_node, single_token_node).total_number_of_tokens() == 3

    def test_arrayAccessNodesContain1TokenPlusInternals(self, tokenless_node, single_token_node):
        assert ArrayAccess(tokenless_node, tokenless_node).total_number_of_tokens() == 1
        assert ArrayAccess(single_token_node, single_token_node).total_number_of_tokens() == 3

    def test_programNodeContainsAdditionalTokenIfBothInternalsArePresent(self, tokenless_node, single_token_node):
        assert Program(None, tokenless_node).total_number_of_tokens() == 0
        assert Program(tokenless_node, tokenless_node).total_number_of_tokens() == 1
        assert Program(single_token_node, single_token_node).total_number_of_tokens() == 3

    def test_routineDefinitionContainsTokensForRoutineNameArgumentsSeparatorsKeywordAndContent(self, tokenless_node,
            single_token_node, double_token_node):
        assert RoutineDefinition("", [tokenless_node], tokenless_node).total_number_of_tokens() == 3
        assert RoutineDefinition("", [single_token_node], tokenless_node).total_number_of_tokens() == 4
        assert RoutineDefinition("", [double_token_node, single_token_node],
            tokenless_node).total_number_of_tokens() == 7
        assert RoutineDefinition("", [tokenless_node], double_token_node).total_number_of_tokens() == 5

    def test_routineInvocationContainsTokensForRoutineNameArgumentsAndSeparators(self, tokenless_node,
            single_token_node, double_token_node):
        assert RoutineInvocation("", [tokenless_node]).total_number_of_tokens() == 1
        assert RoutineInvocation("", [single_token_node]).total_number_of_tokens() == 2
        assert RoutineInvocation("", [double_token_node]).total_number_of_tokens() == 3
        assert RoutineInvocation("", [double_token_node, single_token_node]).total_number_of_tokens() == 5

    def test_returnStatementContains1TokenBesidesReturnValue(self, tokenless_node, double_token_node):
        assert ReturnValue(tokenless_node).total_number_of_tokens() == 1
        assert ReturnValue(double_token_node).total_number_of_tokens() == 3
