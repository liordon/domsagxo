import math

import ply.yacc as yacc

import compilation.node as Node
from compilation.esperanto_lexer import UnalphabeticTerminal as UaTer, PartOfSpeech as POS, \
    ReservedWord as ResWord, tokens
from library.atomic_types import *


class EsperantoSyntaxError(Exception):
    pass


class Var(Enum):
    PROGRAM = 'Vprogram'
    BLOCK = 'Vblock'
    STATEMENT = 'Vstatement'
    IF_STATEMENT = 'Vif'
    WHILE_LOOP = 'Vwhile_loop'
    DELAYED_STATEMENT = 'Vdelayed_statement'
    DELIMITER = 'Vseparator'
    SCHEDULED_STATEMENT = 'Vscheduled_statement'
    REPEATING_STATEMENT = 'Vrepeating_statement'
    RETURN_STATEMENT = 'Vreturn'
    ASSIGN_STATEMENT = 'Vassign'
    EXPRESSION = 'Vexpression'
    TIME_SPAN = 'Vtime_span'
    TIME_POINT = 'Vtime_point'
    NAME = 'Vname'
    VARIABLE = 'Vvariable'
    PARTIAL_NAME = 'Vpartial_name'
    ADJECTIVE = 'Vadjective'
    TERM = 'Vterm'
    FACTOR = 'Vfactor'
    ROUTINE_INVOCATION = 'Vroutine_invocation'
    NUMBER_LITERAL = 'Vnumber_literal'
    HOUR_ORDINAL = 'Vhour_ordinal'
    PARTIAL_TIME_SPAN = 'Vpartial_time_span'
    ROUTINE_ARGUMENTS = 'Varguments'
    ROUTINE_ARGUMENT = 'Vsingle_argument'
    PARAMETERS = 'Vparameters'
    ROUTINE_DEFINITION = 'Vroutine_definition'
    RELATION = 'Vrelation'
    LARGE_ORDINAL = 'Vlarge_ordinal'


def RULE(product, rule_list):
    def set_rule(fun):
        doc = product.value + " : "
        for rule in rule_list:
            doc = doc + ' '.join([item.value for item in rule])
            doc = doc + "\n | "

        fun.__doc__ = doc[:-2]
        return fun

    return set_rule


digitNames = {
    "nul" : 0,
    "unu" : 1,
    "du"  : 2,
    "tri" : 3,
    "kvar": 4,
    "kvin": 5,
    "ses" : 6,
    "sep" : 7,
    "ok"  : 8,
    "naŭ" : 9,
    "naux": 9,
    ""    : 1
}


def parseDigit(name):
    if name[-3:] == "ono":
        name = name[:-3]
        return 1 / digitNames[name]

    multiplier = 1
    if name[-3:] == "dek":
        name = name[:-3]
        multiplier = 10
    elif name[-4:] == "cent":
        name = name[:-4]
        multiplier = 100

    return digitNames[name] * multiplier


def parseEntireNumber(number):
    total = 0
    for word in number:
        current = parseDigit(word)
        if total > 0 and (current > total or numbers_share_digit(total, current)):
            raise EsperantoSyntaxError("illegal verbal number combination: "
                                       + " ".join(number))
        total += current
    return total


def get_digit(number, digit):
    return number // 10 ** digit % 10


def numbers_share_digit(number1, number2):
    for digit in range(min(int(math.log10(number1)), int(math.log10(number2))) + 1):
        if get_digit(number1, digit) != 0 and get_digit(number2, digit) != 0:
            return True
    return False


def build(start=None):
    # noinspection PyUnusedLocal
    allTokenTypes = tokens  # just so the import will be considered meaningful.

    @RULE(Var.PROGRAM, [[Var.BLOCK], ])
    def p_program_block(p):
        p[0] = p[1]

    @RULE(Var.BLOCK, [[Var.STATEMENT],
        [Var.BLOCK, ResWord.AND_THEN, Var.STATEMENT],
        [Var.BLOCK, ResWord.SIMULTANEOUSLY, Var.STATEMENT], ])
    def p_block_separatedStatements(p):
        if len(p) == 2:
            p[0] = Node.Program(None, p[1])
        else:
            p[0] = Node.Program(p[1], p[3])

    @RULE(Var.STATEMENT, [[Var.ROUTINE_DEFINITION],
        [Var.ROUTINE_INVOCATION],
        [Var.ASSIGN_STATEMENT],
        [Var.RETURN_STATEMENT],
        [Var.IF_STATEMENT],
        [Var.WHILE_LOOP],
        [Var.DELAYED_STATEMENT],
        [Var.SCHEDULED_STATEMENT],
        [Var.REPEATING_STATEMENT], ])
    def p_statement_anyKind(p):
        p[0] = p[1]

    # ------------------------     statement definitions     --------------------------#

    @RULE(Var.IF_STATEMENT, [[ResWord.IF, Var.EXPRESSION, ResWord.THEN, Var.BLOCK, ResWord.END],
        [ResWord.IF, Var.EXPRESSION, ResWord.THEN, Var.BLOCK,
            ResWord.ELSE, Var.BLOCK, ResWord.END], ])
    def p_ifStatement_ifCondition(p):
        if len(p) == 6:
            p[0] = Node.ConditionalStatement(p[2], p[4], None)
        else:
            p[0] = Node.ConditionalStatement(p[2], p[4], p[6])

    @RULE(Var.WHILE_LOOP,
        [[ResWord.DURING, Var.EXPRESSION, ResWord.THEN, Var.BLOCK, ResWord.END], ])
    def p_whileLoop_loopBlock(p):
        p[0] = Node.LoopStatement(p[2], p[4])

    @RULE(Var.DELAYED_STATEMENT, [[Var.STATEMENT, ResWord.AFTER, Var.TIME_SPAN], ])
    def p_delayedStatement_delayAndAction(p):
        p[0] = Node.DelayedStatement(p[1], p[3])

    @RULE(Var.SCHEDULED_STATEMENT, [[Var.STATEMENT, ResWord.AT, Var.TIME_POINT], ])
    def p_scheduledStatement_timeAndAction(p):
        p[0] = Node.ScheduledStatement(p[1], p[3])

    @RULE(Var.REPEATING_STATEMENT, [[Var.STATEMENT, ResWord.EVERY, Var.TIME_SPAN], ])
    def p_repeatingStatement_repetitionAndAction(p):
        p[0] = Node.RepeatedStatement(p[1], p[3])

    @RULE(Var.ASSIGN_STATEMENT, [[ResWord.PUT, Var.EXPRESSION, ResWord.TO, Var.VARIABLE], ])
    def p_assignStatement_verbalAssign(p):
        p[0] = Node.VariableAssignment(p[4], p[2])

    @RULE(Var.ASSIGN_STATEMENT, [[Var.VARIABLE, UaTer.ASSIGN, Var.EXPRESSION], ])
    def p_assignStatement_signedAssign(p):
        p[0] = Node.VariableAssignment(p[1], p[3])

    @RULE(Var.RETURN_STATEMENT, [[ResWord.RETURN], ])
    def p_returnStatement_return(p):
        p[0] = Node.ReturnValue(None)

    @RULE(Var.RETURN_STATEMENT, [[ResWord.RETURN, Var.EXPRESSION], ])
    def p_returnStatement_returnValue(p):
        p[0] = Node.ReturnValue(p[2])

    # ---------------------       variable name definitions     ----------------------------#

    @RULE(Var.VARIABLE, [[Var.NAME, ResWord.OF, Var.VARIABLE], ])
    def p_variable_dereference(p):
        p[0] = Node.Dereference(p[1], p[3])

    @RULE(Var.VARIABLE, [[Var.PARTIAL_NAME, ResWord.OF, Var.VARIABLE], ])
    def p_variable_arrayAccessViaVariable(p):
        p[0] = Node.ArrayAccess(p[1], p[3])

    @RULE(Var.LARGE_ORDINAL, [[Var.NUMBER_LITERAL, POS.ORDINAL],
        [POS.ORDINAL], ])
    def p_largeOrdinal_NumberAndLargeOrdinal(p):
        if len(p) > 2:
            p[0] = p[1] + [p[2][:-1]]
        else:
            p[0] = [p[1][:-1]]

    @RULE(Var.VARIABLE, [[Var.LARGE_ORDINAL, ResWord.OF, Var.VARIABLE], ])
    def p_variable_arrayAccessViaOrdinal(p):
        p[0] = Node.ArrayAccess(Node.Number(parseEntireNumber(p[1])), p[3])

    @RULE(Var.VARIABLE, [[Var.NAME], ])
    def p_variable_name(p):
        p[0] = p[1]

    @RULE(Var.VARIABLE, [[ResWord.THE, Var.VARIABLE], ])
    def p_variable_definiteArticle(p):
        p[0] = p[2]

    @RULE(Var.NAME, [[ResWord.IT],
        [POS.NOUN],
        [Var.PARTIAL_NAME, POS.NOUN], ])
    def p_name_partialNameAndNoun(p):
        if len(p) == 3:
            p[0] = Node.VariableName(p[2], p[1])
        else:
            p[0] = Node.VariableName(p[1], Node.NoneNode())

    @RULE(Var.PARTIAL_NAME, [[Var.PARTIAL_NAME, Var.ADJECTIVE],
        [Var.ADJECTIVE], ])
    def p_partialName_partialNameAndAdjective(p):
        if len(p) == 3:
            p[0] = Node.Description(p[2], p[1])
        else:
            p[0] = Node.Description(p[1])

    @RULE(Var.ADJECTIVE, [[ResWord.GREATER],
        [ResWord.SMALLER],
        [POS.ADJECTIVE], ])
    def p_adjective_normalAdjectiveOrReclaimedWeaklyReservedWord(p):
        p[0] = str(p[1])

    @RULE(Var.ADJECTIVE, [[Var.LARGE_ORDINAL], ])
    def p_adjective_reclaimedOrdinal(p):
        p[0] = " ".join(p[1]) + "a"

    # -------------------------   mathematical calculations   -----------------------------#
    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, UaTer.PLUS, Var.TERM],
        [Var.EXPRESSION, ResWord.MORE, Var.TERM], ])
    def p_expression_plus(p):
        p[0] = Node.Add(p[1], p[3])

    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, UaTer.MINUS, Var.TERM],
        [Var.EXPRESSION, ResWord.LESS, Var.TERM], ])
    def p_expression_minus(p):
        p[0] = Node.Subtract(p[1], p[3])

    @RULE(Var.TERM, [[Var.TERM, UaTer.TIMES, Var.FACTOR],
        [Var.TERM, ResWord.TIMES, Var.FACTOR], ])
    def p_term_multiply(p):
        p[0] = Node.Multiply(p[1], p[3])

    @RULE(Var.TERM, [[Var.TERM, UaTer.DIVIDE, Var.FACTOR],
        [Var.TERM, ResWord.PARTS, Var.FACTOR], ])
    def p_term_div(p):
        p[0] = Node.Divide(p[1], p[3])

    @RULE(Var.FACTOR, [[UaTer.MINUS, Var.FACTOR],
        [ResWord.LESS, Var.FACTOR], ])
    def p_factor_unaryMinus(p):
        p[0] = Node.Subtract(Node.Number(0), p[2])

    @RULE(Var.EXPRESSION, [[Var.TERM],
        [Var.TIME_POINT],
        [Var.TIME_SPAN], ])
    def p_expression_term(p):
        p[0] = p[1]

    @RULE(Var.TERM, [[Var.FACTOR], ])
    def p_term_factor(p):
        p[0] = p[1]

    @RULE(Var.FACTOR, [[UaTer.NUMBER], ])
    def p_factor_number(p):
        p[0] = Node.Number(p[1])

    @RULE(Var.FACTOR, [[Var.NUMBER_LITERAL], ])
    def p_factor_numberLiteral(p):
        p[0] = Node.Number(parseEntireNumber(p[1]))

    @RULE(Var.NUMBER_LITERAL, [[ResWord.VERBAL_DIGIT],
        [Var.NUMBER_LITERAL, ResWord.VERBAL_DIGIT], ])
    def p_verbal_number(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[2]]

    @RULE(Var.FACTOR, [[Var.VARIABLE], ])
    def p_factor_noun(p):
        p[0] = p[1]

    @RULE(Var.FACTOR, [[UaTer.L_PAREN, Var.EXPRESSION, UaTer.R_PAREN], ])
    def p_factor_expr(p):
        p[0] = p[2]

    # ----------------------------   boolean calculations    ------------------------- #
    @RULE(Var.RELATION, [[ResWord.IS, ResWord.EQUAL, ResWord.TO], ])
    def p_relation_equal(p):
        p[0] = Node.Comparison.Relation.EQUAL

    @RULE(Var.RELATION, [[ResWord.IS, ResWord.MORE, ResWord.GREATER, ResWord.THAN],
        [ResWord.IS, ResWord.MORE, ResWord.GREATER,
            ResWord.OR, ResWord.EQUAL, ResWord.TO], ])
    def p_relation_greatnessAndEquality(p):
        if len(p) == 5:
            p[0] = Node.Comparison.Relation.GREATER
        else:
            p[0] = Node.Comparison.Relation.GREATER_OR_EQUAL

    @RULE(Var.RELATION, [[ResWord.IS, ResWord.MORE, ResWord.SMALLER,
        ResWord.OR, ResWord.EQUAL, ResWord.TO],
        [ResWord.IS, ResWord.MORE, ResWord.SMALLER, ResWord.THAN], ])
    def p_relation_smallnessAndEquality(p):
        if len(p) == 5:
            p[0] = Node.Comparison.Relation.LESSER
        else:
            p[0] = Node.Comparison.Relation.LESSER_OR_EQUAL

    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, Var.RELATION, Var.EXPRESSION],
        [Var.EXPRESSION, ResWord.NOT, Var.RELATION, Var.EXPRESSION], ])
    def p_expression_sizeComparison(p):
        if len(p) == 4:
            p[0] = Node.Comparison(p[1], p[2], p[3])
        else:
            p[0] = Node.Comparison(p[1], p[3], p[4]).reverse()

    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, ResWord.BOTH, Var.EXPRESSION], ])
    def p_expression_booleanAnd(p):
        p[0] = Node.LogicAnd(p[1], p[3])

    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, ResWord.OR, Var.EXPRESSION], ])
    def p_expression_booleanOr(p):
        p[0] = Node.LogicOr(p[1], p[3])

    @RULE(Var.EXPRESSION, [[ResWord.NOT, Var.EXPRESSION], ])
    def p_expression_booleanNot(p):
        p[0] = Node.LogicNot(p[2])

    @RULE(Var.EXPRESSION, [[Var.VARIABLE, POS.V_PRES], ])
    def p_expression_stateQuery(p):
        p[0] = Node.QueryState(p[1], p[2])

    # ----------------------------   literals and constants    ------------------------- #
    @RULE(Var.EXPRESSION, [[ResWord.NONE], ])
    def p_expression_none(p):
        p[0] = Node.NoneNode()

    @RULE(Var.EXPRESSION, [[ResWord.TRUE], ])
    def p_expression_true(p):
        p[0] = Node.Boolean(True)

    @RULE(Var.EXPRESSION, [[ResWord.FALSE], ])
    def p_expression_false(p):
        p[0] = Node.Boolean(False)

    @RULE(Var.EXPRESSION, [[UaTer.STRING], ])
    def p_expression_string(p):
        p[0] = Node.String(p[1])

    @RULE(Var.TIME_POINT, [[ResWord.THE, Var.LARGE_ORDINAL, ResWord.AND, Var.NUMBER_LITERAL], ])
    def p_time_point(p):
        parsed_hour = parseEntireNumber(p[2])
        parsed_minutes = parseEntireNumber(p[4])
        if parsed_minutes < 1:
            parsed_minutes = int(60 * parsed_minutes)
        if parsed_minutes >= 60:
            raise EsperantoSyntaxError("Illegal number of minutes entered: " + str(parsed_minutes))
        p[0] = Node.TimePoint(hour=Node.Number(parsed_hour), minute=Node.Number(parsed_minutes))

    @RULE(Var.TIME_POINT, [[ResWord.THE, Var.LARGE_ORDINAL, ResWord.TIME_INDICATION], ])
    def p_round_time_point(p):
        if p[3] != "horo":
            raise EsperantoSyntaxError(
                "wrong hour format. expected hour descriptor, then minute number.")
        p[0] = Node.TimePoint(hour=Node.Number(parseEntireNumber(p[2])))

    @RULE(Var.TIME_SPAN, [[Var.TIME_SPAN, ResWord.AND, Var.PARTIAL_TIME_SPAN], ])
    def p_time_span_kaj_time_span(p):
        p[0] = Node.TimeUnion(p[1], p[3])

    @RULE(Var.TIME_SPAN, [[Var.TIME_SPAN, ResWord.AND, Var.NUMBER_LITERAL], ])
    def p_time_fractions(p):
        parsed_fraction = parseEntireNumber(p[3])
        if parsed_fraction >= 1:
            raise EsperantoSyntaxError("Illegal time span format, recieved: " + " ".join(p[3])
                                       + " (" + str(parsed_fraction) + ") when expected fraction")
        p[0] = Node.TimeFractionAddition(p[1], Node.Number(parsed_fraction))

    @RULE(Var.TIME_SPAN, [[Var.PARTIAL_TIME_SPAN], ])
    def p_time_spans_escalation(p):
        p[0] = p[1]

    @RULE(Var.TIME_SPAN, [[Var.TIME_SPAN, Var.PARTIAL_TIME_SPAN], ])
    def p_time_spans_consecutive(p):
        p[0] = Node.TimeUnion(p[1], p[2])

    @RULE(Var.PARTIAL_TIME_SPAN, [[ResWord.TIME_INDICATION],
        [Var.NUMBER_LITERAL, ResWord.TIME_INDICATION], ])
    def p_time_span(p):
        if len(p) > 2:
            time_unit = p[2]
            amount = parseEntireNumber(p[1])
        else:
            time_unit = p[1]
            amount = 1

        days = 0
        hours = 0
        minutes = 0
        seconds = 0
        if time_unit.startswith("tago"):
            days = math.floor(amount)
            hours = 24 * (amount - math.floor(amount))
        elif time_unit.startswith("horo"):
            hours = math.floor(amount)
            minutes = 60 * (amount - math.floor(amount))
        elif time_unit.startswith("minuto"):
            minutes = math.floor(amount)
            seconds = 60 * (amount - math.floor(amount))
        else:
            seconds = amount
        p[0] = Node.TimeSpan(Node.Number(days),
            Node.Number(hours),
            Node.Number(minutes),
            Node.Number(seconds))

    # ------------------------    routine invocation and definition   --------------------- #
    @RULE(Var.ROUTINE_INVOCATION, [[POS.V_IMP, Var.ROUTINE_ARGUMENTS], ])
    def p_routineInvocation_imperativeVerbAndArguments(p):
        p[0] = Node.RoutineInvocation(p[1], p[2])

    @RULE(Var.ROUTINE_INVOCATION, [[POS.V_IMP], ])
    def p_routineInvocation_lonelyImperativeVerb(p):
        # noinspection PyTypeChecker
        p[0] = Node.RoutineInvocation(p[1], [])

    @RULE(Var.ROUTINE_ARGUMENTS, [[Var.ROUTINE_ARGUMENT],
        [Var.ROUTINE_ARGUMENTS, Var.DELIMITER,
            Var.ROUTINE_ARGUMENT], ])
    def p_routineArguments_argumentAndAdditionalExpression(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE(Var.ROUTINE_ARGUMENT, [[Var.EXPRESSION], ])
    def p_routineArgument_firstExpression(p):
        p[0] = p[1]

    @RULE(Var.PARAMETERS, [[Var.NAME],
        [Var.PARAMETERS, Var.DELIMITER, Var.NAME], ])
    def p_inputArg_NOUN(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE(Var.PARAMETERS, [[], ])
    def p_inputArg_nothing(p):
        p[0] = []

    @RULE(Var.ROUTINE_DEFINITION,
        [[POS.V_INF, Var.PARAMETERS, ResWord.THIS_WAY, Var.BLOCK, ResWord.END], ])
    def p_routineDefinition_nameAndArgs(p):
        p[0] = Node.RoutineDefinition(p[1][:-1] + "u", p[2], p[4])

    @RULE(Var.DELIMITER, [[UaTer.DELIMITER],
        [POS.PREPOSITION],
        [ResWord.TO],
        [ResWord.AND], ])
    def p_delimiter_prepositionOrComma(p):
        p[0] = p[1]

    # Error rule for syntax errors
    # noinspection PyUnusedLocal
    def p_error(p):
        symbol_stack_trace = ""
        for symbol in ast_builder.symstack[1:]:
            # noinspection PyUnresolvedReferences
            if isinstance(symbol, yacc.YaccSymbol) and isinstance(symbol.value, Node.AstNode):
                # noinspection PyUnresolvedReferences
                symbol_stack_trace += symbol.value.pretty_print()
            else:
                symbol_stack_trace += str(symbol)
            symbol_stack_trace += "\n"
        raise EsperantoSyntaxError("Syntax error in input: " + str(p) +
                                   "\nOn parse tree:\n" + symbol_stack_trace)

    global ast_builder
    ast_builder = yacc.yacc(tabmodule="my_parsetab", start=start, errorlog=yacc.NullLogger())
    return ast_builder


class Object(object):
    pass


if __name__ == "__main__":
    import compilation.esperanto_lexer as lxr
    import sys

    lxr.build()

    if "-c" in sys.argv:
        ast = build(start=Var.PROGRAM.value)
    else:
        ast = build(start=Var.STATEMENT.value)
        demo_state = Object()
        demo_state.variables = {}
        print("This is a limited AST demo.\n",
            "it can only deal with simple arithmetic and variable usage")

        while True:
            txt = input(">")
            if txt == "":
                break
            demo_state, result = ast.parse(txt).evaluate(demo_state)
            print(result)
