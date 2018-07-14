import math

import ply.yacc as yacc

import compilation.node as Node
from compilation.esp_lexer import UnalphabeticTerminal as UaTer, PartOfSpeech as POS, \
    ReservedWord as ResWord, tokens
from library.atomic_types import *


class EsperantoSyntaxError(Exception):
    pass


class Var(Enum):
    PROGRAM = 'Vprogram'
    BLOCK = 'Vblock'
    STATEMENT = 'Vstatement'
    IF_STATEMENT = 'Vif_statement'
    WHILE_LOOP = 'Vwhile_loop'
    DELAYED_STATEMENT = 'Vdelayed_statement'
    DELIMITER = 'Vdelimiter'
    SCHEDULED_STATEMENT = 'Vscheduled_statement'
    REPEATING_STATEMENT = 'Vrepeating_statement'
    RETURN_STATEMENT = 'Vreturn_statement'
    ASSIGN_STATEMENT = 'Vassign_statement'
    EXPRESSION = 'Vexpression'
    TIME_SPAN = 'Vtime_span'
    TIME_POINT = 'Vtime_point'
    NAME = 'Vname'
    PARTIAL_NAME = 'Vpartial_name'
    ADJECTIVE = 'Vadjective'
    TERM = 'Vterm'
    FACTOR = 'Vfactor'
    FUNCTION_CALL = 'Vfunction_call'
    VERBAL_NUMBER = 'Vnumber_literal'
    HOUR_NUMERATOR = 'Vhour_numerator'
    PARTIAL_TIME_SPAN = 'VpartTime_span'
    FUNCTION_ARGUMENTS = 'Vfunction_arguments'
    FUNCTION_ARGUMENT = 'Vfunction_argument'
    INPUT_ARGUMENTS = 'Vinput_arguments'
    FUNCTION_DEFINITION = 'Vfunc_definition'
    RELATION = 'Vrelation'


def RULE(product, rule_list):
    def set_rule(fun):
        doc = product.value + " : "
        for rule in rule_list:
            doc = doc + ' '.join([item.value for item in rule])
            doc = doc + "\n | "

        fun.__doc__ = doc[:-2]
        return fun

    return set_rule


def get_digit(number, digit):
    return number // 10 ** digit % 10


def build(start=None):
    # noinspection PyUnusedLocal
    allTokenTypes = tokens  # just so the import will be considered meaningful.

    @RULE(Var.PROGRAM, [[Var.BLOCK]])
    def p_program_block(p):
        p[0] = p[1]

    @RULE(Var.BLOCK, [[Var.STATEMENT, UaTer.PERIOD],
                      [Var.BLOCK, Var.STATEMENT, UaTer.PERIOD]])
    def p_block_separatedStatements(p):
        if len(p) == 3:
            p[0] = Node.Program(None, p[1])
        else:
            p[0] = Node.Program(p[1], p[2])

    @RULE(Var.STATEMENT, [[Var.EXPRESSION],
                          [Var.FUNCTION_DEFINITION],
                          [Var.ASSIGN_STATEMENT],
                          [Var.RETURN_STATEMENT],
                          [Var.IF_STATEMENT],
                          [Var.WHILE_LOOP],
                          [Var.DELAYED_STATEMENT],
                          [Var.SCHEDULED_STATEMENT],
                          [Var.REPEATING_STATEMENT]])
    def p_statement_anyKind(p):
        p[0] = p[1]

    # ------------------------     statement definitions     --------------------------#

    @RULE(Var.IF_STATEMENT, [[ResWord.IF, Var.EXPRESSION, ResWord.THEN, Var.BLOCK, ResWord.END],
                             [ResWord.IF, Var.EXPRESSION, ResWord.THEN, Var.BLOCK,
                              ResWord.ELSE, Var.BLOCK, ResWord.END]])
    def p_ifStatement_ifCondition(p):
        if len(p) == 6:
            p[0] = Node.ConditionalStatement(p[2], p[4], None)
        else:
            p[0] = Node.ConditionalStatement(p[2], p[4], p[6])

    @RULE(Var.WHILE_LOOP, [[ResWord.DURING, Var.EXPRESSION, ResWord.THEN, Var.BLOCK, ResWord.END]])
    def p_whileLoop_loopBlock(p):
        p[0] = Node.LoopStatement(p[2], p[4])

    @RULE(Var.DELAYED_STATEMENT, [[Var.STATEMENT, ResWord.AFTER, Var.TIME_SPAN]])
    def p_delayedStatement_delayAndAction(p):
        p[0] = Node.DelayedStatement(p[1], p[3])

    @RULE(Var.SCHEDULED_STATEMENT, [[Var.STATEMENT, ResWord.AT, Var.TIME_POINT]])
    def p_scheduledStatement_timeAndAction(p):
        p[0] = Node.ScheduledStatement(p[1], p[3])

    @RULE(Var.REPEATING_STATEMENT, [[Var.STATEMENT, ResWord.EVERY, Var.TIME_SPAN]])
    def p_repeatingStatement_repetitionAndAction(p):
        p[0] = Node.RepeatedStatement(p[1], p[3])

    @RULE(Var.ASSIGN_STATEMENT, [[Var.NAME, UaTer.ASSIGN, Var.EXPRESSION]])
    def p_assignStatement_assign(p):
        p[0] = Node.VariableAssignment(p[1], p[3])

    @RULE(Var.RETURN_STATEMENT, [[ResWord.RETURN, Var.EXPRESSION]])
    def p_returnStatement_return(p):
        p[0] = Node.ReturnValue(p[2])

    # ---------------------       variable name definitions     ----------------------------#

    @RULE(Var.NAME, [[Var.NAME, ResWord.OF, Var.NAME]])
    def p_name_dereference(p):
        p[0] = Node.Dereference(p[1], p[3])

    @RULE(Var.NAME, [[POS.NOUN],
                     [Var.PARTIAL_NAME, POS.NOUN]])
    def p_name_partialNameAndNoun(p):
        p[0] = Node.VariableName(p[1] + (p[2] if len(p) == 3 else ""))

    @RULE(Var.PARTIAL_NAME, [[Var.PARTIAL_NAME, Var.ADJECTIVE],
                             [Var.ADJECTIVE]])
    def p_partialName_partialNameAndAdjective(p):
        if len(p) == 3:
            p[0] = p[1] + p[2] + " "
        else:
            p[0] = p[1] + " "

    @RULE(Var.ADJECTIVE, [[ResWord.GREATER],
                          [ResWord.SMALLER],
                          [POS.ADJECTIVE],
                          [POS.NUMERATOR]])
    def p_adjective_normalAdjectiveOrWeaklyReservedWord(p):
        p[0] = str(p[1])

    @RULE(Var.PARTIAL_NAME, [[ResWord.THE]])
    def p_partialName_la(p):
        p[0] = ""

    # -------------------------   mathematical calculations   -----------------------------#
    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, UaTer.PLUS, Var.TERM],
                           [Var.EXPRESSION, ResWord.MORE, Var.TERM]])
    def p_expression_plus(p):
        p[0] = Node.Add(p[1], p[3])

    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, UaTer.MINUS, Var.TERM],
                           [Var.EXPRESSION, ResWord.LESS, Var.TERM]])
    def p_expression_minus(p):
        p[0] = Node.Subtract(p[1], p[3])

    @RULE(Var.TERM, [[Var.TERM, UaTer.TIMES, Var.FACTOR],
                     [Var.TERM, ResWord.TIMES, Var.FACTOR]])
    def p_term_multiply(p):
        p[0] = Node.Multiply(p[1], p[3])

    @RULE(Var.TERM, [[Var.TERM, UaTer.DIVIDE, Var.FACTOR],
                     [Var.TERM, ResWord.PARTS, Var.FACTOR]])
    def p_term_div(p):
        p[0] = Node.Divide(p[1], p[3])

    @RULE(Var.FACTOR, [[UaTer.MINUS, Var.FACTOR],
                       [ResWord.LESS, Var.FACTOR]])
    def p_factor_unaryMinus(p):
        p[0] = Node.Subtract(Node.Number(0), p[2])

    @RULE(Var.EXPRESSION, [[Var.TERM],
                           [Var.TIME_POINT],
                           [Var.TIME_SPAN],
                           [Var.FUNCTION_CALL]])
    def p_expression_term(p):
        p[0] = p[1]

    @RULE(Var.TERM, [[Var.FACTOR]])
    def p_term_factor(p):
        p[0] = p[1]

    @RULE(Var.FACTOR, [[UaTer.NUMBER],
                       [Var.VERBAL_NUMBER]])
    def p_factor_num(p):
        p[0] = Node.Number(p[1])

    @RULE(Var.VERBAL_NUMBER, [[ResWord.VERBAL_DIGIT],
                              [Var.VERBAL_NUMBER, ResWord.VERBAL_DIGIT]])
    def p_verbal_number(p):
        if len(p) == 2:
            p[0] = p[1]
        else:
            for digit in range(min(int(math.log10(p[1])), int(math.log10(p[2]))) + 1):
                if get_digit(p[1], digit) != 0 and get_digit(p[2], digit) != 0:
                    raise EsperantoSyntaxError("illegal verbal number combination: "
                                               + str(p[1]) + " and " + str(p[2]))
            p[0] = p[1] + p[2]

    @RULE(Var.FACTOR, [[Var.NAME]])
    def p_factor_noun(p):
        p[0] = p[1]

    @RULE(Var.FACTOR, [[UaTer.L_PAREN, Var.EXPRESSION, UaTer.R_PAREN]])
    def p_factor_expr(p):
        p[0] = p[2]

    # ----------------------------   boolean calculations    ------------------------- #
    @RULE(Var.RELATION, [[UaTer.ASSIGN, ResWord.EQUAL, ResWord.TO]])
    def p_relation_equal(p):
        p[0] = Node.Comparison.Relation.EQUAL

    @RULE(Var.RELATION, [[UaTer.ASSIGN, ResWord.MORE, ResWord.GREATER, ResWord.THAN],
                         [UaTer.ASSIGN, ResWord.MORE, ResWord.GREATER,
                          ResWord.OR, ResWord.EQUAL, ResWord.TO]])
    def p_relation_greatnessAndEquality(p):
        if len(p) == 5:
            p[0] = Node.Comparison.Relation.GREATER
        else:
            p[0] = Node.Comparison.Relation.GREATER_OR_EQUAL

    @RULE(Var.RELATION, [[UaTer.ASSIGN, ResWord.MORE, ResWord.SMALLER,
                          ResWord.OR, ResWord.EQUAL, ResWord.TO],
                         [UaTer.ASSIGN, ResWord.MORE, ResWord.SMALLER, ResWord.THAN]])
    def p_relation_smallnessAndEquality(p):
        if len(p) == 5:
            p[0] = Node.Comparison.Relation.LESSER
        else:
            p[0] = Node.Comparison.Relation.LESSER_OR_EQUAL

    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, Var.RELATION, Var.EXPRESSION],
                           [Var.EXPRESSION, ResWord.NOT, Var.RELATION, Var.EXPRESSION]])
    def p_expression_sizeComparison(p):
        if len(p) == 4:
            p[0] = Node.Comparison(p[1], p[2], p[3])
        else:
            p[0] = Node.Comparison(p[1], p[3], p[4]).reverse()

    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, ResWord.BOTH, Var.EXPRESSION]])
    def p_expression_booleanAnd(p):
        p[0] = Node.LogicAnd(p[1], p[3])

    @RULE(Var.EXPRESSION, [[Var.EXPRESSION, ResWord.OR, Var.EXPRESSION]])
    def p_expression_booleanOr(p):
        p[0] = Node.LogicOr(p[1], p[3])

    @RULE(Var.EXPRESSION, [[ResWord.NOT, Var.EXPRESSION]])
    def p_expression_booleanNot(p):
        p[0] = Node.LogicNot(p[2])

    # ----------------------------   expression constants    ------------------------- #
    @RULE(Var.EXPRESSION, [[ResWord.NONE]])
    def p_factor_none(p):
        p[0] = Node.NoneNode()

    @RULE(Var.EXPRESSION, [[ResWord.TRUE]])
    def p_expression_true(p):
        p[0] = Node.Boolean(True)

    @RULE(Var.EXPRESSION, [[ResWord.FALSE]])
    def p_expression_false(p):
        p[0] = Node.Boolean(False)

    # -------------------------    constructors for special types  --------------------- #
    @RULE(Var.TIME_POINT, [[Var.HOUR_NUMERATOR, ResWord.AND, Var.VERBAL_NUMBER]])
    def p_time_point(p):
        p[0] = p[1]
        if p[3] < 1:
            p[3] = int(60 * p[3])
        if p[3] >= 60:
            raise EsperantoSyntaxError("Illegal number of minutes entered: " + str(p[3]))
        p[0] = Node.TimePoint(hour=p[1], minute=Node.Number(p[3]))

    @RULE(Var.TIME_POINT, [[Var.HOUR_NUMERATOR, ResWord.TIME_INDICATION]])
    def p_round_time_point(p):
        if len(p) > 2 and p[2] != "horo":
            raise EsperantoSyntaxError(
                "wrong hour format. expected hour descriptor, then minute number.")
        p[0] = Node.TimePoint(hour=p[1])

    @RULE(Var.HOUR_NUMERATOR, [[ResWord.THE, POS.NUMERATOR],
                               [ResWord.THE, Var.VERBAL_NUMBER, POS.NUMERATOR]])
    def p_hour_numerator(p):
        p[0] = p[2]
        if len(p) > 3:
            p[0] += p[3]
        if p[0] > 24:
            raise EsperantoSyntaxError(
                "Illegal hour entered: " + str(p[0]) + ". we use a 24h system")
        p[0] = Node.Number(p[0])

    @RULE(Var.TIME_SPAN, [[Var.TIME_SPAN, ResWord.AND, Var.PARTIAL_TIME_SPAN]])
    def p_time_span_kaj_time_span(p):
        p[0] = Node.TimeUnion(p[1], p[3])

    @RULE(Var.TIME_SPAN, [[Var.TIME_SPAN, ResWord.AND, Var.VERBAL_NUMBER]])
    def p_time_fractions(p):
        if p[3] >= 1:
            raise EsperantoSyntaxError("Illegal time span format, recieved: "
                                       + str(p[3]) + " when expected fraction")
        p[0] = Node.TimeFractionAddition(p[1], Node.Number(p[3]))

    @RULE(Var.TIME_SPAN, [[Var.PARTIAL_TIME_SPAN]])
    def p_time_spans_escalation(p):
        p[0] = p[1]

    @RULE(Var.TIME_SPAN, [[Var.TIME_SPAN, Var.PARTIAL_TIME_SPAN]])
    def p_time_spans_consecutive(p):
        p[0] = Node.TimeUnion(p[1], p[2])

    @RULE(Var.PARTIAL_TIME_SPAN, [[ResWord.TIME_INDICATION],
                                  [Var.VERBAL_NUMBER, ResWord.TIME_INDICATION]])
    def p_time_span(p):
        if len(p) > 2:
            time_unit = p[2]
            amount = p[1]
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

    # ------------------------    function invocation and definition   --------------------- #
    @RULE(Var.FUNCTION_CALL, [[POS.V_IMP, Var.FUNCTION_ARGUMENTS]])
    def p_function_call(p):
        p[0] = Node.FunctionInvocation(p[1], p[2])

    @RULE(Var.FUNCTION_ARGUMENTS, [[Var.FUNCTION_ARGUMENT],
                                   [Var.FUNCTION_ARGUMENTS, Var.DELIMITER, Var.FUNCTION_ARGUMENT]])
    def p_function_arguments(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE(Var.FUNCTION_ARGUMENT, [[Var.EXPRESSION]])
    def p_first_function_argument(p):
        p[0] = p[1]

    @RULE(Var.INPUT_ARGUMENTS, [[Var.NAME],
                                [Var.INPUT_ARGUMENTS, Var.DELIMITER, Var.NAME]])
    def p_inputArg_NOUN(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE(Var.INPUT_ARGUMENTS, [[]])
    def p_inputArg_nothing(p):
        p[0] = []

    @RULE(Var.FUNCTION_DEFINITION,
          [[POS.V_INF, Var.INPUT_ARGUMENTS, ResWord.THIS_WAY, Var.BLOCK, ResWord.END]])
    def p_funcDef_nameAndArgs(p):
        p[0] = Node.FunctionDefinition(p[1][:-1] + "u", p[2], p[4])

    @RULE(Var.DELIMITER, [[UaTer.DELIMITER],
                          [POS.PREPOSITION],
                          [ResWord.TO],
                          [ResWord.AND]])
    def p_delimiter_prepositionOrComma(p):
        p[0] = p[1]

    # Error rule for syntax errors
    # noinspection PyUnusedLocal
    def p_error(p):
        raise EsperantoSyntaxError("Syntax error in input: " + str(p))

    ast_builder = yacc.yacc(tabmodule="my_parsetab", start=start, errorlog=yacc.NullLogger())
    return ast_builder


class Object(object):
    pass


if __name__ == "__main__":
    import compilation.esp_lexer as lxr
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
