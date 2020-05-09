import math

import ply.yacc as yacc

from domsagxo.compilation import node
from domsagxo.compilation.definitions import EsperantoSyntaxError, EsperantoLocatedSyntaxError, GrammarVariable
from domsagxo.compilation.esperanto_lexer import UnalphabeticTerminal as UaTer, PartOfSpeech as PoS, \
    ReservedWord as ResWord, tokens


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
    "nul": 0,
    "unu": 1,
    "du": 2,
    "tri": 3,
    "kvar": 4,
    "kvin": 5,
    "ses": 6,
    "sep": 7,
    "ok": 8,
    "naŭ": 9,
    "naux": 9,
    "": 1
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
    all_token_types = tokens  # just so the import will be considered meaningful.

    @RULE(GrammarVariable.PROGRAM, [[GrammarVariable.BLOCK], ])
    def p_program_block(p):
        p[0] = p[1]

    @RULE(GrammarVariable.BLOCK, [[GrammarVariable.STATEMENT],
        [GrammarVariable.BLOCK, ResWord.AND_THEN, GrammarVariable.STATEMENT],
        [GrammarVariable.BLOCK, ResWord.SIMULTANEOUSLY, GrammarVariable.STATEMENT], ])
    def p_block_separatedStatements(p):
        if len(p) == 2:
            p[0] = node.Program(None, p[1])
        else:
            p[0] = node.Program(p[1], p[3])

    @RULE(GrammarVariable.STATEMENT, [[GrammarVariable.ROUTINE_DEFINITION],
        [GrammarVariable.ROUTINE_INVOCATION],
        [GrammarVariable.ASSIGN_STATEMENT],
        [GrammarVariable.RETURN_STATEMENT],
        [GrammarVariable.IF_STATEMENT],
        [GrammarVariable.WHILE_LOOP],
        [GrammarVariable.DELAYED_STATEMENT],
        [GrammarVariable.SCHEDULED_STATEMENT],
        [GrammarVariable.ONCE_STATEMENT],
        [GrammarVariable.WHENEVER_STATEMENT],
        [GrammarVariable.REPEATING_STATEMENT], ])
    def p_statement_anyKind(p):
        p[0] = p[1]

    # ------------------------     statement definitions     --------------------------#

    @RULE(GrammarVariable.IF_STATEMENT,
        [[ResWord.IF, GrammarVariable.EXPRESSION, ResWord.THEN, GrammarVariable.BLOCK, ResWord.END],
            [ResWord.IF, GrammarVariable.EXPRESSION, ResWord.THEN, GrammarVariable.BLOCK,
                ResWord.ELSE, GrammarVariable.BLOCK, ResWord.END], ])
    def p_ifStatement_ifCondition(p):
        if len(p) == 6:
            p[0] = node.ConditionalStatement(p[2], p[4])
        else:
            p[0] = node.ConditionalStatement(p[2], p[4], p[6])

    @RULE(GrammarVariable.WHILE_LOOP,
        [[ResWord.DURING, GrammarVariable.EXPRESSION, ResWord.THEN, GrammarVariable.BLOCK, ResWord.END], ])
    def p_whileLoop_loopBlock(p):
        p[0] = node.LoopStatement(p[2], p[4])

    @RULE(GrammarVariable.DELAYED_STATEMENT, [[GrammarVariable.STATEMENT, ResWord.AT, GrammarVariable.TIME_SPAN], ])
    def p_delayedStatement_delayAndAction(p):
        p[0] = node.DelayedStatement(p[1], p[3])

    @RULE(GrammarVariable.SCHEDULED_STATEMENT, [[GrammarVariable.STATEMENT, ResWord.AT, GrammarVariable.TIME_POINT], ])
    def p_scheduledStatement_timeAndAction(p):
        p[0] = node.ScheduledStatement(p[1], p[3])

    @RULE(GrammarVariable.REPEATING_STATEMENT,
        [[GrammarVariable.STATEMENT, ResWord.EVERY, GrammarVariable.TIME_SPAN], ])
    def p_repeatingStatement_repetitionAndAction(p):
        p[0] = node.RepeatedStatement(p[1], p[3])

    @RULE(GrammarVariable.ONCE_STATEMENT, [[GrammarVariable.STATEMENT, ResWord.ONCE, GrammarVariable.EXPRESSION], ])
    def p_onceStatement_actionAndTrigger(p):
        p[0] = node.OnceStatement(p[1], p[3])

    @RULE(GrammarVariable.WHENEVER_STATEMENT,
        [[GrammarVariable.STATEMENT, ResWord.WHENEVER, GrammarVariable.EXPRESSION], ])
    def p_wheneverStatement_actionAndTrigger(p):
        p[0] = node.WheneverStatement(p[1], p[3])

    @RULE(GrammarVariable.ASSIGN_STATEMENT,
        [[ResWord.PUT, GrammarVariable.EXPRESSION, ResWord.TO, GrammarVariable.VARIABLE], ])
    def p_assignStatement_verbalAssign(p):
        p[0] = node.VariableAssignment(p[4], p[2])

    @RULE(GrammarVariable.ASSIGN_STATEMENT, [[GrammarVariable.VARIABLE, UaTer.ASSIGN, GrammarVariable.EXPRESSION], ])
    def p_assignStatement_signedAssign(p):
        p[0] = node.VariableAssignment(p[1], p[3])

    @RULE(GrammarVariable.RETURN_STATEMENT, [[ResWord.RETURN], ])
    def p_returnStatement_return(p):
        p[0] = node.ReturnValue(None)

    @RULE(GrammarVariable.RETURN_STATEMENT, [[ResWord.RETURN, GrammarVariable.EXPRESSION], ])
    def p_returnStatement_returnValue(p):
        p[0] = node.ReturnValue(p[2])

    # ---------------------       variable name definitions     ----------------------------#

    @RULE(GrammarVariable.VARIABLE, [[GrammarVariable.NAME, ResWord.OF, GrammarVariable.VARIABLE], ])
    def p_variable_dereference(p):
        p[0] = node.Dereference(p[1], p[3])

    @RULE(GrammarVariable.VARIABLE, [[GrammarVariable.PARTIAL_NAME, ResWord.OF, GrammarVariable.VARIABLE], ])
    def p_variable_arrayAccessViaVariable(p):
        p[0] = node.ArrayAccess(p[1], p[3])

    @RULE(GrammarVariable.LARGE_ORDINAL, [[GrammarVariable.NUMBER_LITERAL, PoS.ORDINAL],
        [PoS.ORDINAL], ])
    def p_largeOrdinal_NumberAndLargeOrdinal(p):
        if len(p) > 2:
            p[0] = p[1] + [p[2][:-1]]
        else:
            p[0] = [p[1][:-1]]

    @RULE(GrammarVariable.VARIABLE, [[GrammarVariable.LARGE_ORDINAL, ResWord.OF, GrammarVariable.VARIABLE], ])
    def p_variable_arrayAccessViaOrdinal(p):
        p[0] = node.ArrayAccess(node.Number(parseEntireNumber(p[1])), p[3])

    @RULE(GrammarVariable.VARIABLE, [[GrammarVariable.NAME], ])
    def p_variable_name(p):
        p[0] = p[1]

    @RULE(GrammarVariable.VARIABLE, [[ResWord.THE, GrammarVariable.VARIABLE], ])
    def p_variable_definiteArticle(p):
        p[0] = p[2]

    @RULE(GrammarVariable.NAME, [[ResWord.IT],
        [PoS.NOUN],
        [GrammarVariable.PARTIAL_NAME, PoS.NOUN], ])
    def p_name_partialNameAndNoun(p):
        if len(p) == 3:
            p[0] = node.VariableName(p[2], p[1])
        else:
            p[0] = node.VariableName(p[1], node.NoneNode())

    @RULE(GrammarVariable.PARTIAL_NAME, [[GrammarVariable.PARTIAL_NAME, GrammarVariable.ADJECTIVE],
        [GrammarVariable.ADJECTIVE], ])
    def p_partialName_partialNameAndAdjective(p):
        if len(p) == 3:
            p[0] = node.Description(p[2], p[1])
        else:
            p[0] = node.Description(p[1])

    @RULE(GrammarVariable.ADJECTIVE, [[ResWord.GREATER],
        [ResWord.SMALLER],
        [PoS.ADJECTIVE], ])
    def p_adjective_normalAdjectiveOrReclaimedWeaklyReservedWord(p):
        p[0] = str(p[1])

    @RULE(GrammarVariable.ADJECTIVE, [[GrammarVariable.LARGE_ORDINAL], ])
    def p_adjective_reclaimedOrdinal(p):
        p[0] = " ".join(p[1]) + "a"

    # -------------------------   mathematical calculations   -----------------------------#
    @RULE(GrammarVariable.EXPRESSION, [[GrammarVariable.EXPRESSION, UaTer.PLUS, GrammarVariable.TERM],
        [GrammarVariable.EXPRESSION, ResWord.MORE, GrammarVariable.TERM], ])
    def p_expression_plus(p):
        p[0] = node.Add(p[1], p[3])

    @RULE(GrammarVariable.EXPRESSION, [[GrammarVariable.EXPRESSION, UaTer.MINUS, GrammarVariable.TERM],
        [GrammarVariable.EXPRESSION, ResWord.LESS, GrammarVariable.TERM], ])
    def p_expression_minus(p):
        p[0] = node.Subtract(p[1], p[3])

    @RULE(GrammarVariable.TERM, [[GrammarVariable.TERM, UaTer.TIMES, GrammarVariable.FACTOR],
        [GrammarVariable.TERM, ResWord.TIMES, GrammarVariable.FACTOR], ])
    def p_term_multiply(p):
        p[0] = node.Multiply(p[1], p[3])

    @RULE(GrammarVariable.TERM, [[GrammarVariable.TERM, UaTer.DIVIDE, GrammarVariable.FACTOR],
        [GrammarVariable.TERM, ResWord.PARTS, GrammarVariable.FACTOR], ])
    def p_term_div(p):
        p[0] = node.Divide(p[1], p[3])

    @RULE(GrammarVariable.FACTOR, [[UaTer.MINUS, GrammarVariable.FACTOR],
        [ResWord.LESS, GrammarVariable.FACTOR], ])
    def p_factor_unaryMinus(p):
        p[0] = node.Subtract(node.Number(0), p[2])

    @RULE(GrammarVariable.EXPRESSION, [[GrammarVariable.TERM],
        [GrammarVariable.TIME_POINT],
        [GrammarVariable.TIME_SPAN], ])
    def p_expression_term(p):
        p[0] = p[1]

    @RULE(GrammarVariable.TERM, [[GrammarVariable.FACTOR], ])
    def p_term_factor(p):
        p[0] = p[1]

    @RULE(GrammarVariable.FACTOR, [[UaTer.NUMBER], ])
    def p_factor_number(p):
        p[0] = node.Number(p[1])

    @RULE(GrammarVariable.FACTOR, [[GrammarVariable.NUMBER_LITERAL], ])
    def p_factor_numberLiteral(p):
        p[0] = node.Number(parseEntireNumber(p[1]))

    @RULE(GrammarVariable.NUMBER_LITERAL, [[ResWord.VERBAL_DIGIT],
        [GrammarVariable.NUMBER_LITERAL, ResWord.VERBAL_DIGIT], ])
    def p_verbal_number(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[2]]

    @RULE(GrammarVariable.FACTOR, [[GrammarVariable.VARIABLE], ])
    def p_factor_noun(p):
        p[0] = p[1]

    @RULE(GrammarVariable.FACTOR, [[UaTer.L_PAREN, GrammarVariable.EXPRESSION, UaTer.R_PAREN], ])
    def p_factor_expr(p):
        p[0] = node.Parentheses(p[2])

    # ----------------------------   boolean calculations    ------------------------- #
    @RULE(GrammarVariable.RELATION, [[ResWord.IS, ResWord.EQUAL, ResWord.TO], ])
    def p_relation_equal(p):
        p[0] = node.Comparison.Relation.EQUAL

    @RULE(GrammarVariable.RELATION, [[ResWord.IS, ResWord.MORE, ResWord.GREATER, ResWord.THAN],
        [UaTer.GREATER_THAN], ])
    def p_relation_greatness(p):
        p[0] = node.Comparison.Relation.GREATER

    @RULE(GrammarVariable.RELATION, [[ResWord.IS, ResWord.MORE, ResWord.GREATER,
        ResWord.OR, ResWord.EQUAL, ResWord.TO],
        [UaTer.GREATER_EQUAL], ])
    def p_relation_greatnessOrEquality(p):
        p[0] = node.Comparison.Relation.GREATER_OR_EQUAL

    @RULE(GrammarVariable.RELATION, [[ResWord.IS, ResWord.MORE, ResWord.SMALLER, ResWord.THAN],
        [UaTer.LESSER_THAN], ])
    def p_relation_smallness(p):
        p[0] = node.Comparison.Relation.LESSER

    @RULE(GrammarVariable.RELATION, [[ResWord.IS, ResWord.MORE, ResWord.SMALLER,
        ResWord.OR, ResWord.EQUAL, ResWord.TO],
        [UaTer.LESSER_EQUAL], ])
    def p_relation_smallnessOrEquality(p):
        p[0] = node.Comparison.Relation.LESSER_OR_EQUAL

    @RULE(GrammarVariable.EXPRESSION,
        [[GrammarVariable.EXPRESSION, GrammarVariable.RELATION, GrammarVariable.EXPRESSION],
            [GrammarVariable.EXPRESSION, ResWord.NOT, GrammarVariable.RELATION, GrammarVariable.EXPRESSION], ])
    def p_expression_sizeComparison(p):
        if len(p) == 4:
            p[0] = node.Comparison(p[1], p[2], p[3])
        else:
            p[0] = node.Comparison(p[1], p[3], p[4]).reverse()

    @RULE(GrammarVariable.EXPRESSION, [[GrammarVariable.EXPRESSION, ResWord.BOTH, GrammarVariable.EXPRESSION], ])
    def p_expression_booleanAnd(p):
        p[0] = node.LogicAnd(p[1], p[3])

    @RULE(GrammarVariable.EXPRESSION, [[GrammarVariable.EXPRESSION, ResWord.OR, GrammarVariable.EXPRESSION], ])
    def p_expression_booleanOr(p):
        p[0] = node.LogicOr(p[1], p[3])

    @RULE(GrammarVariable.EXPRESSION, [[ResWord.NOT, GrammarVariable.EXPRESSION], ])
    def p_expression_booleanNot(p):
        p[0] = node.LogicNot(p[2])

    @RULE(GrammarVariable.EXPRESSION, [[GrammarVariable.VARIABLE, PoS.V_PRES], ])
    def p_expression_stateQuery(p):
        p[0] = node.QueryState(p[1], p[2])

    # ----------------------------   literals and constants    ------------------------- #
    @RULE(GrammarVariable.EXPRESSION, [[ResWord.NONE], ])
    def p_expression_none(p):
        p[0] = node.NoneNode()

    @RULE(GrammarVariable.EXPRESSION, [[ResWord.TRUE], ])
    def p_expression_true(p):
        p[0] = node.Boolean(True)

    @RULE(GrammarVariable.EXPRESSION, [[ResWord.FALSE], ])
    def p_expression_false(p):
        p[0] = node.Boolean(False)

    @RULE(GrammarVariable.EXPRESSION, [[UaTer.STRING], ])
    def p_expression_string(p):
        p[0] = node.String(p[1])

    @RULE(GrammarVariable.TIME_POINT,
        [[ResWord.THE, GrammarVariable.LARGE_ORDINAL, ResWord.AND, GrammarVariable.NUMBER_LITERAL], ])
    def p_time_point(p):
        parsed_hour = parseEntireNumber(p[2])
        parsed_minutes = parseEntireNumber(p[4])
        if parsed_minutes < 1:
            parsed_minutes = int(60 * parsed_minutes)
        if parsed_minutes >= 60:
            raise EsperantoSyntaxError("Illegal number of minutes entered: " + str(parsed_minutes))
        p[0] = node.TimePoint(hour=node.Number(parsed_hour), minute=node.Number(parsed_minutes))

    @RULE(GrammarVariable.TIME_POINT, [[ResWord.THE, GrammarVariable.LARGE_ORDINAL, ResWord.TIME_INDICATION], ])
    def p_round_time_point(p):
        if p[3] != "horo":
            raise EsperantoSyntaxError(
                "wrong hour format. expected hour descriptor, then minute number.")
        p[0] = node.TimePoint(hour=node.Number(parseEntireNumber(p[2])))

    @RULE(GrammarVariable.TIME_SPAN, [[GrammarVariable.TIME_SPAN, ResWord.AND, GrammarVariable.PARTIAL_TIME_SPAN], ])
    def p_time_span_kaj_time_span(p):
        p[0] = node.TimeUnion(p[1], p[3])

    @RULE(GrammarVariable.TIME_SPAN, [[GrammarVariable.TIME_SPAN, ResWord.AND, GrammarVariable.NUMBER_LITERAL], ])
    def p_time_fractions(p):
        parsed_fraction = parseEntireNumber(p[3])
        if parsed_fraction >= 1:
            raise EsperantoSyntaxError("Illegal time span format, recieved: " + " ".join(p[3])
                                       + " (" + str(parsed_fraction) + ") when expected fraction")
        p[0] = node.TimeFractionAddition(p[1], node.Number(parsed_fraction))

    @RULE(GrammarVariable.TIME_SPAN, [[GrammarVariable.PARTIAL_TIME_SPAN], ])
    def p_time_spans_escalation(p):
        p[0] = p[1]

    @RULE(GrammarVariable.TIME_SPAN, [[GrammarVariable.TIME_SPAN, GrammarVariable.PARTIAL_TIME_SPAN], ])
    def p_time_spans_consecutive(p):
        p[0] = node.TimeUnion(p[1], p[2])

    @RULE(GrammarVariable.PARTIAL_TIME_SPAN, [[ResWord.TIME_INDICATION],
        [GrammarVariable.NUMBER_LITERAL, ResWord.TIME_INDICATION], ])
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
        p[0] = node.TimeSpan(node.Number(days),
            node.Number(hours),
            node.Number(minutes),
            node.Number(seconds))

    # ------------------------    routine invocation and definition   --------------------- #
    @RULE(GrammarVariable.ROUTINE_INVOCATION, [[PoS.V_IMP, GrammarVariable.ROUTINE_ARGUMENTS], ])
    def p_routineInvocation_imperativeVerbAndArguments(p):
        p[0] = node.RoutineInvocation(p[1], p[2])

    @RULE(GrammarVariable.ROUTINE_INVOCATION, [[PoS.V_IMP], ])
    def p_routineInvocation_lonelyImperativeVerb(p):
        # noinspection PyTypeChecker
        p[0] = node.RoutineInvocation(p[1], [])

    @RULE(GrammarVariable.ROUTINE_ARGUMENTS, [[GrammarVariable.ROUTINE_ARGUMENT],
        [GrammarVariable.ROUTINE_ARGUMENTS, GrammarVariable.DELIMITER,
            GrammarVariable.ROUTINE_ARGUMENT], ])
    def p_routineArguments_argumentAndAdditionalExpression(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE(GrammarVariable.ROUTINE_ARGUMENT, [[GrammarVariable.EXPRESSION], ])
    def p_routineArgument_firstExpression(p):
        p[0] = p[1]

    @RULE(GrammarVariable.PARAMETERS, [[GrammarVariable.NAME],
        [GrammarVariable.PARAMETERS, GrammarVariable.DELIMITER, GrammarVariable.NAME], ])
    def p_inputArg_NOUN(p):
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    @RULE(GrammarVariable.PARAMETERS, [[], ])
    def p_inputArg_nothing(p):
        p[0] = []

    @RULE(GrammarVariable.ROUTINE_DEFINITION,
        [[PoS.V_INF, GrammarVariable.PARAMETERS, ResWord.THIS_WAY, GrammarVariable.BLOCK, ResWord.END], ])
    def p_routineDefinition_nameAndArgs(p):
        p[0] = node.RoutineDefinition(p[1][:-1] + "u", p[2], p[4])

    @RULE(GrammarVariable.DELIMITER, [[UaTer.DELIMITER],
        [PoS.PREPOSITION],
        [ResWord.TO],
        [ResWord.AND], ])
    def p_delimiter_prepositionOrComma(p):
        p[0] = p[1]

    # Error rule for syntax errors
    # noinspection PyUnusedLocal
    def p_error(p):
        symbol_stack_trace = ""
        shifted_tokens = 0
        for symbol in ast_builder.symstack[1:]:
            # noinspection PyUnresolvedReferences
            if isinstance(symbol, yacc.YaccSymbol) and isinstance(symbol.value, node.AstNode):
                # noinspection PyUnresolvedReferences
                symbol_stack_trace += symbol.value.pretty_print()
                shifted_tokens += symbol.value.total_number_of_tokens()
            elif isinstance(symbol, yacc.YaccSymbol) and isinstance(symbol.value, list):
                shifted_tokens += sum([node.total_number_of_tokens() for node in symbol.value])
                shifted_tokens += 0 if len(symbol.value) == 0 else len(symbol.value) - 1
                symbol_stack_trace += str(symbol) + str(symbol.value)
            else:
                symbol_stack_trace += str(symbol)
                shifted_tokens += 1
            symbol_stack_trace += "\n"
        raise EsperantoLocatedSyntaxError(shifted_tokens,
            "Syntax error in input: " + str(p) + "\nOn parse tree:\n" + symbol_stack_trace)

    ast_builder = yacc.yacc(tabmodule="my_parsetab", start=start, errorlog=yacc.NullLogger())
    return ast_builder


class Object(object):
    pass


if __name__ == "__main__":
    import domsagxo.compilation.esperanto_lexer as lxr
    import sys

    lxr.build()

    if "-c" in sys.argv:
        ast = build(start=GrammarVariable.PROGRAM.value)
    else:
        ast = build(start=GrammarVariable.STATEMENT.value)
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
