from compilation.node import *


def test_abstractSyntaxTreeNodesCanRepresentThemselvesAndTheirArgs():
    assert AstNode(1, 2, AstNode()).__repr__() == "AstNode(1, 2, AstNode)"


def test_abstractSyntaxTreeNodesRepresentationIncludesKwargs():
    assert AstNode(1, 2, shambalulu=3).__repr__() == "AstNode(1, 2, shambalulu=3)"


def test_basicNodesHaveShorterStringRepresentation():
    assert str(Number(3)) == "3"
    assert str(String("saluton")) == "`saluton'"
    assert str(Boolean(True)) == "True"
    assert str(Boolean(False)) == "False"


def test_emptyTimeSpansArePrintedAs0seconds():
    assert str(TimeSpan()) == "0s"
    assert str(TimeSpan(seconds=Number(0))) == "0s"


def test_compositeTimeSpansAreShownAsNumberAndUnitPairs():
    assert str(TimeSpan(seconds=Number(42))) == "42s"
    assert str(TimeSpan(minutes=Number(40), seconds=Number(2))) == "40m 2s"
    assert str(TimeSpan(days=Number(40), hours=Number(2))) == "40d 2h"


def test_binaryOperationNodesWithAtomicArgumentsCanBeSimplyRepresented():
    assert Add(Number(4), Number(2)).__repr__() == "Add(Number(4), Number(2))"
    assert Subtract(Number(4), Number(2)).__repr__() == "Subtract(Number(4), Number(2))"


def test_canUsePrettyPrintFunctionToPrintASingleNode():
    assert AstNode().pretty_print() == "└- AstNode"


def test_canUsePrettyPrintFunctionOnAtomicNodeTypes():
    assert Number(3).pretty_print() == "└- 3"
    assert String("saluton").pretty_print() == "└- `saluton'"
    assert NoneNode().pretty_print() == "└- NoneNode"


def test_canUsePrettyPrintOnVariablesAndArrays():
    assert VariableName('lulo', NoneNode()).pretty_print() == "└- <lulo>"
    assert VariableName('lulo',
                        Description('mba', Description('sxa', NoneNode()))
                        ).pretty_print() == \
           "└- <sxa mba lulo>"
    # assert ArrayAccess(3, 'sxamba').pretty_print() == "└- <sxamba lulo>"


def test_canPrettyPrintVariableAssignments():
    varAss = VariableAssignment(VariableName('lulo', NoneNode()),
                                VariableName('sxambo', NoneNode()))
    assert varAss.pretty_print() == \
           "└- VariableAssignment" + \
           "\n\t├- <lulo>" + \
           "\n\t└- <sxambo>"


def test_canUsePrettyPrintFunctionToPrintSingleDescendantNode():
    assert AstNode(AstNode()).pretty_print() == \
           "└- AstNode" + \
           "\n\t└- AstNode"


def test_kwargsAreSpecifiedByNameWhenPrettyPrinted():
    assert AstNode(AstNode()).pretty_print() == \
           "└- AstNode" + \
           "\n\t└- AstNode"


def test_canUsePrettyPrintFunctionToPrintTwoDescendantNodes():
    assert AstNode(AstNode(), AstNode()).pretty_print() == \
           "└- AstNode" + \
           "\n\t├- AstNode" + \
           "\n\t└- AstNode"


def test_canUsePrettyPrintFunctionOnDifferentNodeTypes():
    assert Add(Number(4), Number(2)).pretty_print() == \
           "└- Add" + \
           "\n\t├- 4" + \
           "\n\t└- 2"""


def test_routineInvocationsShouldBePrettyPrintedWithTheirArguments():
    assert RoutineInvocation("sxambalulu", (Number(42),)).pretty_print() == \
           "└- RoutineInvocation: sxambalulu" + "\n\t└- 42"


def test_routineDefinitionsShouldBePrettyPrintedWithTheirStatements():
    assert RoutineDefinition("sxambalulu", ["kato", "hundo"], VariableAssignment(VariableName('lulo', NoneNode()),
                                VariableName('sxambo', NoneNode()))
    ).pretty_print() == \
           "└- RoutineDefinition: sxambalulu" + \
           "\n\t├- Arguments:" + \
           "\n\t│\t├- kato" + \
           "\n\t│\t└- hundo" + \
           "\n\t└- VariableAssignment" + \
           "\n\t\t├- <lulo>" + \
           "\n\t\t└- <sxambo>"


def test_canUsePrettyPrintFunctionToPrintManyDescendantNodes():
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


def test_whenHavingMultipleChildrenTreeRemainsWellConnected():
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
