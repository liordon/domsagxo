import math

import ply.yacc as yacc

import compilation.node as Node
from compilation.esp_lexer import UnalphabeticTerminal as UaTer, PartOfSpeech as POS, \
    ReservedWord as ResWord, tokens
from library.atomic_types import *


class EsperantoSyntaxError(Exception):
    pass


def RULE(product, rule_list):
    def rulePartToString(rule_part):
        if isinstance(rule_part, Enum):
            return rule_part.value
        else:
            return rule_part

    def set_rule(fun):
        doc = product + " : "
        for rule in rule_list:
            doc = doc + ' '.join([rulePartToString(item) for item in rule])
            doc = doc + "\n | "

        fun.__doc__ = doc[:-2]
        return fun

    return set_rule


def get_digit(number, digit):
    return number // 10 ** digit % 10


def build(start=None):
    allTokenTypes = tokens  # just so the import will be considered meaningful.

    @RULE('program', [['statement', UaTer.PERIOD],
                      ['program', 'statement', UaTer.PERIOD]])
    def p_program_separatedStatements(p):
        if len(p) == 3:
            p[0] = Node.Program(None, p[1])
        else:
            p[0] = Node.Program(p[1], p[2])

    # ------------------------     statement definitions     --------------------------#
    @RULE('statement', [[ResWord.IF, 'expression', ResWord.THEN, 'program', ResWord.END],
                        [ResWord.IF, 'expression', ResWord.THEN, 'program',
                         ResWord.ELSE, 'program', ResWord.END]])
    def p_statement_ifCondition(p):
        if len(p) == 6:
            p[0] = Node.ConditionalStatement(p[2], p[4], None)
        else:
            p[0] = Node.ConditionalStatement(p[2], p[4], p[6])

    @RULE('statement', [[ResWord.DURING, 'expression', ResWord.THEN, 'program', ResWord.END]])
    def p_statement_whileLoop(p):
        p[0] = Node.LoopStatement(p[2], p[4])

    @RULE('statement', [['statement', ResWord.AFTER, 'timeSpan']])
    def p_statement_delayedAction(p):
        p[0] = Node.DelayedStatement(p[1], p[3])

    @RULE('statement', [['statement', ResWord.AT, 'timePoint']])
    def p_statement_scheduledAction(p):
        p[0] = Node.ScheduledStatement(p[1], p[3])

    @RULE('statement', [['statement', ResWord.EVERY, 'timeSpan']])
    def p_statement_repeatedAction(p):
        p[0] = Node.RepeatedStatement(p[1], p[3])

    @RULE('statement', [['expression']])
    def p_statement_expr(p):
        p[0] = p[1]

    @RULE('statement', [['name ', UaTer.ASSIGN, 'expression']])
    def p_statement_assign(p):
        p[0] = Node.VariableAssignment(p[1], p[3])

    @RULE('statement', [[ResWord.RETURN, 'expression']])
    def p_statement_return(p):
        p[0] = Node.ReturnValue(p[2])

    # ---------------------       variable name definitions     ----------------------------#
    @RULE('name', [[POS.NOUN],
                   ['partialName', POS.NOUN]])
    def p_name_partialNameAndNoun(p):
        p[0] = Node.VariableName(p[1] + (p[2] if len(p) == 3 else ""))

    @RULE('partialName', [['partialName', 'adjective'],
                          ['adjective']])
    def p_partialName_partialNameAndAdjective(p):
        if len(p) == 3:
            p[0] = p[1] + p[2] + " "
        else:
            p[0] = p[1] + " "

    @RULE('adjective', [[ResWord.GREATER],
                        [ResWord.SMALLER],
                        [POS.ADJECTIVE],
                        [POS.NUMERATOR]])
    def p_adjective_normalAdjectiveOrWeaklyReservedWord(p):
        p[0] = str(p[1])

    @RULE('partialName', [[ResWord.THE]])
    def p_partialName_la(p):
        p[0] = ""

    # -------------------------   mathematical calculations   -----------------------------#
    @RULE('expression', [['expression', UaTer.PLUS, 'term'],
                         ['expression', ResWord.MORE, 'term']])
    def p_expression_plus(p):
        p[0] = Node.Add(p[1], p[3])

    @RULE('expression', [['expression', UaTer.MINUS, 'term'],
                         ['expression', ResWord.LESS, 'term']])
    def p_expression_minus(p):
        p[0] = Node.Subtract(p[1], p[3])

    @RULE('term', [['term', UaTer.TIMES, 'factor'],
                   ['term', ResWord.TIMES, 'factor']])
    def p_term_mult(p):
        p[0] = Node.Multiply(p[1], p[3])

    @RULE('term', [['term', UaTer.DIVIDE, 'factor'],
                   ['term', ResWord.PARTS, 'factor']])
    def p_term_div(p):
        p[0] = Node.Divide(p[1], p[3])

    @RULE('factor', [[UaTer.MINUS, 'factor'],
                     [ResWord.LESS, 'factor']])
    def p_factor_unaryMinus(p):
        p[0] = Node.Subtract(Node.Number(0), p[2])

    @RULE('expression', [['term'],
                         ['timePoint'],
                         ['timeSpan'],
                         ['functionCall']])
    def p_expression_term(p):
        p[0] = p[1]

    @RULE('term', [['factor']])
    def p_term_factor(p):
        p[0] = p[1]

    @RULE('factor', [[UaTer.NUMBER],
                     ['verbalNumber']])
    def p_factor_num(p):
        p[0] = Node.Number(p[1])

    @RULE('verbalNumber', [[ResWord.VERBAL_DIGIT],
                           ['verbalNumber', ResWord.VERBAL_DIGIT]])
    def p_verbal_number(p):
        if len(p) == 2:
            p[0] = p[1]
        else:
            for digit in range(min(int(math.log10(p[1])), int(math.log10(p[2]))) + 1):
                if get_digit(p[1], digit) != 0 and get_digit(p[2], digit) != 0:
                    raise EsperantoSyntaxError("illegal verbal number combination: "
                                               + str(p[1]) + " and " + str(p[2]))
            p[0] = p[1] + p[2]

    @RULE('factor', [['name']])
    def p_factor_noun(p):
        p[0] = p[1]

    @RULE('factor', [[UaTer.L_PAREN, 'expression', UaTer.R_PAREN]])
    def p_factor_expr(p):
        p[0] = p[2]

    # ----------------------------   boolean calculations    ------------------------- #
    @RULE('relation', [[UaTer.ASSIGN, ResWord.EQUAL, ResWord.TO]])
    def p_relation_equal(p):
        p[0] = Node.Comparison.Relation.EQUAL

    @RULE('relation', [[UaTer.ASSIGN, ResWord.MORE, ResWord.GREATER, ResWord.THAN],
                       [UaTer.ASSIGN, ResWord.MORE, ResWord.GREATER,
                        ResWord.OR, ResWord.EQUAL, ResWord.TO]])
    def p_relation_greatnessAndEquality(p):
        if len(p) == 5:
            p[0] = Node.Comparison.Relation.GREATER
        else:
            p[0] = Node.Comparison.Relation.GREATER_OR_EQUAL

    @RULE('relation', [[UaTer.ASSIGN, ResWord.MORE, ResWord.SMALLER,
                        ResWord.OR, ResWord.EQUAL, ResWord.TO],
                       [UaTer.ASSIGN, ResWord.MORE, ResWord.SMALLER, ResWord.THAN]])
    def p_relation_smallnessAndEquality(p):
        if len(p) == 5:
            p[0] = Node.Comparison.Relation.LESSER
        else:
            p[0] = Node.Comparison.Relation.LESSER_OR_EQUAL

    @RULE('expression', [['expression', 'relation', 'expression'],
                         ['expression', ResWord.NOT, 'relation', 'expression']])
    def p_expression_sizeComparison(p):
        if len(p) == 4:
            p[0] = Node.Comparison(p[1], p[2], p[3])
        else:
            p[0] = Node.Comparison(p[1], p[3], p[4]).reverse()

    # ----------------------------   expression constants    ------------------------- #
    @RULE('expression', [[ResWord.NONE]])
    def p_factor_none(p):
        p[0] = Node.NoneNode()

    @RULE('expression', [[ResWord.TRUE]])
    def p_expression_true(p):
        p[0] = Node.Boolean(True)

    @RULE('expression', [[ResWord.FALSE]])
    def p_expression_false(p):
        p[0] = Node.Boolean(False)

    # -------------------------    constructors for special types  --------------------- #
    @RULE('timePoint', [['hourNumerator', ResWord.AND, 'verbalNumber']])
    def p_time_point(p):
        p[0] = p[1]
        if p[3] < 1:
            p[3] = int(60 * p[3])
        if p[3] >= 60:
            raise EsperantoSyntaxError("Illegal number of minutes entered: " + str(p[3]))
        p[0] = Node.TimePoint(hour=p[1], minute=Node.Number(p[3]))

    @RULE('timePoint', [['hourNumerator', ResWord.TIME_INDICATION]])
    def p_round_time_point(p):
        if len(p) > 2 and p[2] != "horo":
            raise EsperantoSyntaxError(
                "wrong hour format. expected hour descriptor, then minute number.")
        p[0] = Node.TimePoint(hour=p[1])

    @RULE('hourNumerator', [[ResWord.THE, POS.NUMERATOR],
                            [ResWord.THE, 'verbalNumber', POS.NUMERATOR]])
    def p_hour_numerator(p):
        p[0] = p[2]
        if len(p) > 3:
            p[0] += p[3]
        if p[0] > 24:
            raise EsperantoSyntaxError(
                "Illegal hour entered: " + str(p[0]) + ". we use a 24h system")
        p[0] = Node.Number(p[0])

    @RULE('timeSpan', [['timeSpan', ResWord.AND, 'partTimeSpan']])
    def p_time_span_kaj_time_span(p):
        p[0] = Node.TimeUnion(p[1], p[3])

    @RULE('timeSpan', [['timeSpan', ResWord.AND, 'verbalNumber']])
    def p_time_fractions(p):
        if p[3] >= 1:
            raise EsperantoSyntaxError("Illegal time span format, recieved: "
                                       + str(p[3]) + " when expected fraction")
        p[0] = Node.TimeFractionAddition(p[1], Node.Number(p[3]))

    @RULE('timeSpan', [['partTimeSpan']])
    def p_time_spans_escalation(p):
        p[0] = p[1]

    @RULE('timeSpan', [['timeSpan', 'partTimeSpan']])
    def p_time_spans_consequtive(p):
        p[0] = Node.TimeUnion(p[1], p[2])

    @RULE('partTimeSpan', [[ResWord.TIME_INDICATION],
                           ['verbalNumber', ResWord.TIME_INDICATION]])
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
    @RULE('functionCall', [[POS.V_IMP, 'functionArgs']])
    def p_function_call(p):
        p[0] = Node.FunctionInvocation(p[1], p[2])

    @RULE('functionArgs', [['functionArg'],
                           ['functionArgs', ResWord.AND, 'functionArg'],
                           ['functionArgs', UaTer.DELIM, 'functionArg']])
    def p_function_arguments(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE('functionArg', [['expression']])
    def p_first_function_argument(p):
        p[0] = p[1]

    @RULE('defFuncName', [[POS.V_INF]])
    def p_define_function_name(p):
        p[0] = p[1][:-1] + "u"

    @RULE('inputArgs', [['name'],
                        ['inputArgs', ResWord.AND, 'name'],
                        ['inputArgs', UaTer.DELIM, 'name']])
    def p_inputArg_NOUN(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE('inputArgs', [[]])
    def p_inputArg_nothing(p):
        p[0] = []

    @RULE('funcDef', [['defFuncName', 'inputArgs', ResWord.THIS_WAY, 'program', ResWord.END]])
    def p_funcDef_nameAndArgs(p):
        p[0] = Node.FunctionDefinition(p[1], p[2], p[4])

    # Error rule for syntax errors
    def p_error(p):
        raise EsperantoSyntaxError("Syntax error in input: " + str(p))

    ast_builder = yacc.yacc(tabmodule="my_parsetab", start=start, errorlog=yacc.NullLogger())
    return ast_builder


if __name__ == "__main__":
    import compilation.esp_lexer as lxr
    class Object(object):
        pass

    lxr.build()
    ast = build(start="statement")
    demo_state = Object()
    demo_state.variables={}
    print("this is a limited AST demo, it can only deal with simple arithmetic and variable usage")

    while True:
        txt = input(">")
        if txt == "":
            break
        demo_state, result = ast.parse(txt).evaluate(demo_state)
        print(result)
