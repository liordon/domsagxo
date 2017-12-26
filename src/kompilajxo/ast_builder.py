import ply.yacc as yacc
from kompilajxo.lexer_builder import UnalphabeticTerminal as UaTer
from kompilajxo.lexer_builder import PartOfSpeech as POS
from kompilajxo.lexer_builder import ReservedWord as ResWord
from kompilajxo.lexer_builder import tokens


class SyntaxError(Exception):
    pass


def RULE(ruleString):
    def set_rule(fun):
        fun.__doc__ = ruleString
        return fun

    return set_rule


def build(start=None):
    allTokenTypes = tokens # just so the import will be considered meaningful.
    variable_table = {}

    @RULE('''program : statement ''' + UaTer.PERIOD.value + '''
                    | program statement ''' + UaTer.PERIOD.value)
    def p_program_separatedStatements(p):
        pass

    # @RULE('statement : ' + ResWord.IF.value + ' expression ' + UaTer.COLON.value + ' statement')
    # def p_statement_ifCondition(p):
    #     if p[2]:
    #

    @RULE('statement : expression')
    def p_statement_expr(p):
        p[0] = None

    @RULE('statement : name ' + UaTer.ASSIGN.value + ' expression')
    def p_statement_assign(p):
        variable_table[p[1]] = p[3]

    @RULE('''name : ''' + POS.NOUN.value + '''
                | partialName ''' + POS.NOUN.value)
    def p_name_partialNameAndNoun(p):
        p[0] = p[1] + (p[2] if len(p) == 3 else "")
        # p[0] = p[1] + p[2]

    @RULE('''partialName : partialName ''' + POS.ADJECTIVE.value + '''
                        | ''' + POS.ADJECTIVE.value)
    def p_partialName_partialNameAndAdjective(p):
        if len(p) == 3:
            p[0] = p[1] + p[2] + " "
        else:
            p[0] = p[1] + " "
        # p[0] = p[1] + (p[2] + " " if len(p) == 3 else "")

    @RULE('partialName : ' + ResWord.LA.value)
    def p_partialName_la(p):
        p[0] = ""

    @RULE('expression : expression ' + UaTer.PLUS.value + ' term')
    def p_expression_plus(p):
        p[0] = p[1] + p[3]

    @RULE('''expression : expression ''' + UaTer.MINUS.value + ''' term
                      | ''' + UaTer.MINUS.value + ''' term''')
    def p_expression_minus(p):
        if (len(p) == 4):
            p[0] = p[1] - p[3]
        elif (len(p) == 3):
            p[0] = -p[2]

    @RULE('expression : term')
    def p_expression_term(p):
        p[0] = p[1]

    @RULE('term : term ' + UaTer.TIMES.value + ' factor')
    def p_term_times(p):
        p[0] = p[1] * p[3]

    @RULE('term : term ' + UaTer.DIVIDE.value + ' factor')
    def p_term_div(p):
        p[0] = p[1] / p[3]

    @RULE('term : factor')
    def p_term_factor(p):
        p[0] = p[1]

    @RULE('factor : ' + UaTer.NUMBER.value)
    def p_factor_num(p):
        p[0] = p[1]

    @RULE('factor : ' + POS.NOUN.value)
    def p_factor_noun(p):
        p[0] = variable_table[p[1]]

    @RULE('factor : ' + UaTer.L_PAREN.value + ' expression ' + UaTer.R_PAREN.value)
    def p_factor_expr(p):
        p[0] = p[2]

    @RULE('expression : ' + ResWord.TRUE.value)
    def p_expression_true(p):
        p[0] = True

    @RULE('expression : ' + ResWord.FALSE.value)
    def p_expression_false(p):
        p[0] = False

    # Error rule for syntax errors
    def p_error(p):
        raise SyntaxError("Syntax error in input: " + str(p))

    astBuilder = yacc.yacc(tabmodule="my_parsetab", start=start)
    astBuilder.variable_table = variable_table
    return astBuilder


