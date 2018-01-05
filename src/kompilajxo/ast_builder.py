from enum import Enum

import math

import ply.yacc as yacc
from kompilajxo.lexer_builder import UnalphabeticTerminal as UaTer
from kompilajxo.lexer_builder import PartOfSpeech as POS
from kompilajxo.lexer_builder import ReservedWord as ResWord
from kompilajxo.lexer_builder import tokens
import biblioteko.atomic_types as Atypes
import biblioteko.predefined_functions as Pfuncs


class EsperantoSyntaxError(Exception):
    pass


def RULE(*rule_list):
    def rulePartToString(rule_part):
        if isinstance(rule_part, Enum):
            return rule_part.value + ' '
        else:
            return rule_part

    def set_rule(fun):
        fun.__doc__ = "".join([rulePartToString(item) for item in rule_list])
        return fun

    return set_rule


def get_digit(number, digit):
    return number // 10**digit %10


def build(start=None):
    allTokenTypes = tokens # just so the import will be considered meaningful.
    variable_table = {}

    @RULE('''program : statement ''', UaTer.PERIOD, '''
                    | program statement ''', UaTer.PERIOD)
    def p_program_separatedStatements(p):
        pass

    # @RULE('statement : ' + ResWord.IF + ' expression ' + UaTer.COLON + ' statement')
    # def p_statement_ifCondition(p):
    #     if p[2]:
    #

    @RULE('statement : expression')
    def p_statement_expr(p):
        p[0] = None

    @RULE('statement : name ', UaTer.ASSIGN, ' expression')
    def p_statement_assign(p):
        variable_table[p[1]] = p[3]

    @RULE('''name : ''', POS.NOUN, '''
                | partialName ''', POS.NOUN)
    def p_name_partialNameAndNoun(p):
        p[0] = p[1] + (p[2] if len(p) == 3 else "")
        # p[0] = p[1] + p[2]

    @RULE('''partialName : partialName ''', POS.ADJECTIVE, '''
                        | ''', POS.ADJECTIVE)
    def p_partialName_partialNameAndAdjective(p):
        if len(p) == 3:
            p[0] = p[1] + p[2] + " "
        else:
            p[0] = p[1] + " "
        # p[0] = p[1] + (p[2] + " " if len(p) == 3 else "")

    @RULE('partialName : ', ResWord.LA)
    def p_partialName_la(p):
        p[0] = ""

    @RULE('expression : expression ', UaTer.PLUS, ' term')
    def p_expression_plus(p):
        p[0] = p[1] + p[3]

    @RULE('''expression : expression ''', UaTer.MINUS, ''' term''')
    def p_expression_minus(p):
        p[0] = p[1] - p[3]

    @RULE('''expression : term
                        | timePoint
                        | timeSpan''')
    def p_expression_term(p):
        p[0] = p[1]

    @RULE('term : term ', UaTer.TIMES, ' factor')
    def p_term_times(p):
        p[0] = p[1] * p[3]

    @RULE('term : term ', UaTer.DIVIDE, ' factor')
    def p_term_div(p):
        p[0] = p[1] / p[3]

    @RULE('term : factor')
    def p_term_factor(p):
        p[0] = p[1]

    @RULE('factor : ', UaTer.NUMBER, '''
                | verbalNumber
                | ''', UaTer.MINUS, '''factor''')
    def p_factor_num(p):
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = -p[2]

    @RULE('verbalNumber : ', ResWord.VERBAL_DIGIT, '''
                    |  verbalNumber ''', ResWord.VERBAL_DIGIT)
    def p_verbal_number(p):
        if len(p) == 2:
            p[0] = p[1]
        else:
            for digit in range(min(int(math.log10(p[1])), int(math.log10(p[2]))) + 1):
                if get_digit(p[1], digit) != 0 and get_digit(p[2], digit) != 0:
                    raise EsperantoSyntaxError("illegal verbal number combination: "
                                               + str(p[1]) + " and " + str(p[2]))
            p[0] = p[1] + p[2]

    @RULE('''factor : name''')
    def p_factor_noun(p):
        if p[1] in variable_table.keys():
            p[0] = variable_table[p[1]]
        else:
            p[0] = p[1]

    @RULE('factor : ', UaTer.L_PAREN, ' expression ', UaTer.R_PAREN)
    def p_factor_expr(p):
        p[0] = p[2]

    @RULE('expression : ', ResWord.TRUE)
    def p_expression_true(p):
        p[0] = True

    @RULE('expression : ', ResWord.FALSE)
    def p_expression_false(p):
        p[0] = False

    @RULE('''timePoint : hourNumerator ''', ResWord.KAJ, '''verbalNumber''')
    def p_time_point(p):
        p[0] = p[1]
        if p[3] < 1:
            p[3] *= 60
        if p[3] >= 60:
            raise EsperantoSyntaxError("Illegal number of minutes entered: " + str(p[3]))
        p[0] = Atypes.TimePoint(p[3], p[1])

    @RULE('''timePoint :  hourNumerator ''', ResWord.TIME_INDICATION)
    def p_round_time_point(p):
        if len(p) > 2 and p[2] != "horo":
            raise EsperantoSyntaxError("wrong hour format. expected hour descriptor, then minute number.")
        p[0] = Atypes.TimePoint(0, p[1])

    @RULE('''hourNumerator : ''', ResWord.LA, POS.NUMERATOR, '''
                    | ''', ResWord.LA, '''verbalNumber ''',  POS.NUMERATOR)
    def p_hour_numerator(p):
        p[0] = p[2]
        if len(p) > 3:
            p[0] += p[3]
        if p[0] > 24:
            raise EsperantoSyntaxError("Illegal hour entered: " + str(p[0]) + ". we use a 24h system")

    @RULE('''timeSpan : timeSpan ''', ResWord.KAJ, '''timeSpan
                   | timeSpan ''', UaTer.DELIM, '''timeSpan''')
    def p_time_spans(p):
        p[0] = Atypes.TimeSpan.unite(p[1], p[3])

    @RULE('''timeSpan : timeSpan ''', ResWord.KAJ, '''verbalNumber''')
    def p_time_fractions(p):
        if p[3] >= 1:
            raise EsperantoSyntaxError("Illegal time span format, recieved: " + str(p[3]) + " when expected fraction")
        p[0] = p[1].addFraction(p[3])

    @RULE('''timeSpan : ''', ResWord.TIME_INDICATION, '''
                    | verbalNumber ''', ResWord.TIME_INDICATION)
    def p_time_span(p):
        if len(p) > 2:
            time_unit = p[2]
            amount = p[1]
        else:
            time_unit = p[1]
            amount = 1

        if time_unit.startswith("horo"):
            p[0] = Atypes.TimeSpan(hours=math.floor(amount), minutes=60*(amount - math.floor(amount)))
        elif time_unit.startswith("minuto"):
            p[0] = Atypes.TimeSpan(minutes=math.floor(amount), seconds=60*(amount - math.floor(amount)))
        else:
            p[0] = Atypes.TimeSpan(seconds=amount)

    @RULE('''functionCall : ''', POS.V_IMP, '''functionArgs''')
    def p_function_call(p):
        p[0] = Pfuncs.method_dict[p[1]](p[2])

    @RULE('''functionArgs : expression
                    | functionArgs ''', ResWord.KAJ, '''expression
                    | functionArgs ''', UaTer.DELIM, '''expression''')
    def p_function_arguments(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    # Error rule for syntax errors
    def p_error(p):
        raise EsperantoSyntaxError("Syntax error in input: " + str(p))

    ast_builder = yacc.yacc(tabmodule="my_parsetab", start=start, errorlog=yacc.NullLogger())
    ast_builder.variable_table = variable_table
    return ast_builder


if __name__ == "__main__":
    import kompilajxo.lexer_builder as lxr
    lxr.build()
    ast = build(start="statement")

    while True:
        txt = input(">")
        if txt == "":
            break
        print(ast.parse(txt))
