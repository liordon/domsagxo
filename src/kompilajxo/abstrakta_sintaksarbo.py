import math

import ply.yacc as yacc

from biblioteko.estra_komponantoj import Domsagxo
from kompilajxo.leksisto import UnalphabeticTerminal as UaTer
from kompilajxo.leksisto import PartOfSpeech as POS
from kompilajxo.leksisto import ReservedWord as ResWord
from kompilajxo.leksisto import tokens
from biblioteko.atomaj_tipoj import *
import kompilajxo.nodo as Node


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

    # precedence = (
    #     ('left', 'KAJ', 'DELIM'),
    #     ('left', 'PLUS', 'MINUS'),
    # )

    @RULE('program', [['statement', UaTer.PERIOD],
                      ['program', 'statement', UaTer.PERIOD]])
    def p_program_separatedStatements(p):
        if len(p) == 3:
            p[0] = Node.Program(None, p[1])
        else:
            p[0] = Node.Program(p[1], p[2])

    # @RULE('statement : ' + ResWord.IF + ' expression ' + UaTer.COLON + ' statement')
    # def p_statement_ifCondition(p):
    #     if p[2]:
    #

    @RULE('statement', [['expression']])
    def p_statement_expr(p):
        p[0] = p[1]

    @RULE('statement', [['name ', UaTer.ASSIGN, 'expression']])
    def p_statement_assign(p):
        p[0] = Node.VariableAssignment(p[1], p[3])

    @RULE('statement', [[ResWord.REVENU, 'expression']])
    def p_statement_return(p):
        p[0] = Node.ReturnValue(p[2])

    @RULE('name', [[POS.NOUN],
                   ['partialName', POS.NOUN]])
    def p_name_partialNameAndNoun(p):
        p[0] = Node.VariableName(p[1] + (p[2] if len(p) == 3 else ""))

    @RULE('partialName', [['partialName', POS.ADJECTIVE],
                          [POS.ADJECTIVE]])
    def p_partialName_partialNameAndAdjective(p):
        if len(p) == 3:
            p[0] = p[1] + p[2] + " "
        else:
            p[0] = p[1] + " "

    @RULE('partialName', [[ResWord.LA]])
    def p_partialName_la(p):
        p[0] = ""

    @RULE('expression', [['expression', UaTer.PLUS, 'term'],
                         ['expression', UaTer.MINUS, 'term']])
    def p_expression_plus_minus(p):
        p[0] = Node.MathOp(p[1], p[2], p[3])

    @RULE('expression', [['term'],
                         ['timePoint'],
                         ['timeSpan'],
                         ['functionCall']])
    def p_expression_term(p):
        p[0] = p[1]

    @RULE('term', [['term', UaTer.TIMES, 'factor'],
                   ['term', UaTer.DIVIDE, 'factor']])
    def p_term_mult_div(p):
        p[0] = Node.MathOp(p[1], p[2], p[3])

    @RULE('term', [['factor']])
    def p_term_factor(p):
        p[0] = p[1]

    @RULE('factor', [[UaTer.NUMBER],
                     ['verbalNumber'],
                     [UaTer.MINUS, 'factor']])
    def p_factor_num(p):
        if len(p) == 2:
            p[0] = Node.Number(p[1])
        else:
            p[0] = Node.MathOp(Node.Number(0), p[1], p[2])

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

    @RULE('factor', [[ResWord.NENIO]])
    def p_factor_none(p):
        p[0] = Node.NoneNode()

    @RULE('factor', [[UaTer.L_PAREN, 'expression', UaTer.R_PAREN]])
    def p_factor_expr(p):
        p[0] = p[2]

    @RULE('expression', [[ResWord.TRUE]])
    def p_expression_true(p):
        p[0] = Node.Boolean(True)

    @RULE('expression', [[ResWord.FALSE]])
    def p_expression_false(p):
        p[0] = Node.Boolean(False)

    @RULE('timePoint', [['hourNumerator', ResWord.KAJ, 'verbalNumber']])
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
            raise EsperantoSyntaxError("wrong hour format. expected hour descriptor, then minute number.")
        p[0] = Node.TimePoint(hour=p[1])

    @RULE('hourNumerator', [[ResWord.LA, POS.NUMERATOR],
                            [ResWord.LA, 'verbalNumber', POS.NUMERATOR]])
    def p_hour_numerator(p):
        p[0] = p[2]
        if len(p) > 3:
            p[0] += p[3]
        if p[0] > 24:
            raise EsperantoSyntaxError("Illegal hour entered: " + str(p[0]) + ". we use a 24h system")
        p[0] = Node.Number(p[0])

    @RULE('timeSpan', [['timeSpan', ResWord.KAJ, 'partTimeSpan']])
    def p_time_span_kaj_time_span(p):
        p[0] = Node.TimeUnion(p[1], p[3])

    @RULE('timeSpan', [['timeSpan', ResWord.KAJ, 'verbalNumber']])
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

    @RULE('functionCall', [[POS.V_IMP, 'functionArgs']])
    def p_function_call(p):
        p[0] = Node.FunctionInvocation(p[1], p[2])

    @RULE('functionArgs', [['functionArg'],
                           ['functionArgs', ResWord.KAJ, 'functionArg'],
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
        p[0] = p[1][:-1]+"u"

    @RULE('inputArgs', [['name'],
                        ['inputArgs', ResWord.KAJ, 'name'],
                        ['inputArgs', UaTer.DELIM, 'name']])
    def p_inputArg_NOUN(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE('inputArgs', [[]])
    def p_inputArg_nothing(p):
        p[0] = []

    @RULE('funcDef', [['defFuncName', 'inputArgs', ResWord.TIEL, 'program', ResWord.FINU]])
    def p_funcDef_nameAndArgs(p):
        p[0] = Node.FunctionDefinition(p[1], p[2], p[4])

    # Error rule for syntax errors
    def p_error(p):
        raise EsperantoSyntaxError("Syntax error in input: " + str(p))

    ast_builder = yacc.yacc(tabmodule="my_parsetab", start=start, errorlog=yacc.NullLogger())
    return ast_builder


if __name__ == "__main__":
    import kompilajxo.leksisto as lxr

    lxr.build()
    ast = build(start="statement")

    while True:
        txt = input(">")
        if txt == "":
            break
        print(ast.parse(txt))
