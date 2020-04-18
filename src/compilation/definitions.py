from enum import Enum


class UnalphabeticTerminal(Enum):
    PLUS = 'TPLUS'
    MINUS = 'TMINUS'
    DELIMITER = 'TDELIM'
    COLON = 'TCOLON'
    TIMES = 'TTIMES'
    DIVIDE = 'TDIVIDE'
    PERIOD = 'TPERIOD'
    ASSIGN = 'TASSIGN'
    NUMBER = 'TNUMBER'
    STRING = 'TSTRING'
    L_PAREN = 'TLPAREN'
    R_PAREN = 'TRPAREN'
    COMMENT = 'TCOMMENT'
    GREATER_THAN = 'TGT'
    GREATER_EQUAL = 'TGE'
    LESSER_THAN = 'TLT'
    LESSER_EQUAL = 'TLE'
    EXACTLY_EQUAL = 'TEQ'
    NOT_EQUAL = 'TNEQ'


class ReservedWord(Enum):
    AFTER = 'Tpost'
    AND = 'Tkaj'
    AND_THEN = 'Tposte'
    AT = 'Tje'
    BOTH = 'Tambaux'
    DURING = 'Tdum'
    ELSE = 'Talie'
    END = 'Tfinu'
    EQUAL = 'Tegala'
    EVERY = 'Tcxiu'
    FALSE = 'Tmalvero'
    FOR = 'Tpor'
    GREATER = 'Tgranda'
    IF = 'Tse'
    IN = 'Ten'
    IS = 'Testas'
    IT = 'Tgxi'
    LESS = 'Tmalpli'
    MORE = 'Tpli'
    NONE = 'Tnenio'
    NOT = 'Tne'
    OF = 'Tde'
    ONCE = 'Tunufoje'
    OR = 'Taux'
    PARTS = 'Tpartoj'
    PUT = 'Tasignu'
    RETURN = 'Trevenu'
    SIMULTANEOUSLY = 'Tsamtempe'
    SMALLER = 'Tmalgranda'
    THAN = 'Tol'
    THE = 'Tla'
    THEN = 'Ttiam'
    THIS_WAY = 'Tsignifas'
    TIME_INDICATION = 'Vtime_unit'
    TIMES = 'Tfojoj'
    TO = 'Tal'
    TRUE = 'Tvero'
    VERBAL_DIGIT = 'Vdigit_literal'
    WHENEVER = 'Tcxiufoje'
    WORD = 'Tvorto'


class PartOfSpeech(Enum):
    ADJECTIVE = 'Padjective'
    ADVERB = 'Padverb'
    NOUN = 'Pnoun'
    ORDINAL = 'Pordinal'
    OTHER = 'Pother'
    PREPOSITION = 'Ppreposition'
    V_INF = 'Pinfinitive_verb'
    V_PRES = 'Ppresent_verb'
    V_IMP = 'Pimperative_verb'


reserved_words = {
    'al': ReservedWord.TO.value,
    'alie': ReservedWord.ELSE.value,
    'ambaŭ': ReservedWord.BOTH.value,
    'ambaux': ReservedWord.BOTH.value,
    'asignu': ReservedWord.PUT.value,
    'aŭ': ReservedWord.OR.value,
    'aux': ReservedWord.OR.value,
    'ĉiu': ReservedWord.EVERY.value,
    'ĉiufoje': ReservedWord.WHENEVER.value,
    'cxiufoje': ReservedWord.WHENEVER.value,
    'cxiu': ReservedWord.EVERY.value,
    'dum': ReservedWord.DURING.value,
    'de': ReservedWord.OF.value,
    'egala': ReservedWord.EQUAL.value,
    'estas': ReservedWord.IS.value,
    'finu': ReservedWord.END.value,
    'fojoj': ReservedWord.TIMES.value,
    'granda': ReservedWord.GREATER.value,
    'gxi': ReservedWord.IT.value,
    'gxin': ReservedWord.IT.value,
    'ĝi': ReservedWord.IT.value,
    'ĝin': ReservedWord.IT.value,
    'je': ReservedWord.AT.value,
    'kaj': ReservedWord.AND.value,
    'krampo': UnalphabeticTerminal.L_PAREN.value,
    'la': ReservedWord.THE.value,
    'malgranda': ReservedWord.SMALLER.value,
    'malkrampo': UnalphabeticTerminal.R_PAREN.value,
    'malpli': ReservedWord.LESS.value,
    'malvero': ReservedWord.FALSE.value,
    'ne': ReservedWord.NOT.value,
    'nenio': ReservedWord.NONE.value,
    'ol': ReservedWord.THAN.value,
    'partoj': ReservedWord.PARTS.value,
    'pli': ReservedWord.MORE.value,
    'por': ReservedWord.FOR.value,
    'post': ReservedWord.AFTER.value,
    'poste': ReservedWord.AND_THEN.value,
    'revenu': ReservedWord.RETURN.value,
    'se': ReservedWord.IF.value,
    'samtempe': ReservedWord.SIMULTANEOUSLY.value,
    'signifas': ReservedWord.THIS_WAY.value,
    'tiam': ReservedWord.THEN.value,
    'unufoje': ReservedWord.ONCE.value,
    'vero': ReservedWord.TRUE.value,
}

prepositions = {
    'al',
    'anstataŭ',
    'anstataux',
    'antaŭ',
    'antaux',
    'apud',
    'ĉe',
    'cxe',
    'ĉirkaŭ',
    'cxirkaux',
    'de',
    'dum',
    'ekde',
    'ekster',
    'eksteren',
    'el',
    'en',
    'ĝis',
    'gxis',
    'inter',
    'kiel',
    'kontraŭ',
    'kontraux',
    'krom',
    'kun',
    'laŭ',
    'laux',
    'mala',
    'malgraŭ',
    'malgraux',
    'malkiel',
    'malsupren',
    'ol',
    'per',
    'plus',
    'po',
    'por',
    'post',
    'preter',
    'pri',
    'pro',
    'sed',
    'sekva',
    'sen',
    'sub',
    'suben',
    'super',
    'supren',
    'sur',
    'tra',
    'trans'
}


class EsperantoSyntaxError(Exception):
    def __init__(self, error_message):
        self.message = error_message


class EsperantoLocatedSyntaxError(EsperantoSyntaxError):
    def __init__(self, index: int, description: str):
        self.index = index
        super(EsperantoSyntaxError, self).__init__(description)


class GrammarVariable(Enum):
    PROGRAM = 'V_program'
    BLOCK = 'V_block'
    STATEMENT = 'V_statement'
    IF_STATEMENT = 'V_if'
    WHILE_LOOP = 'V_while_loop'
    DELAYED_STATEMENT = 'V_delayed_statement'
    DELIMITER = 'V_separator'
    SCHEDULED_STATEMENT = 'V_scheduled_statement'
    REPEATING_STATEMENT = 'V_repeating_statement'
    RETURN_STATEMENT = 'V_return'
    ASSIGN_STATEMENT = 'V_assign'
    EXPRESSION = 'V_expression'
    TIME_SPAN = 'V_time_span'
    TIME_POINT = 'V_time_point'
    NAME = 'V_name'
    VARIABLE = 'V_variable'
    PARTIAL_NAME = 'V_partial_name'
    ADJECTIVE = 'V_adjective'
    TERM = 'V_term'
    FACTOR = 'V_factor'
    ROUTINE_INVOCATION = 'V_routine_invocation'
    NUMBER_LITERAL = 'V_number_literal'
    HOUR_ORDINAL = 'V_hour_ordinal'
    PARTIAL_TIME_SPAN = 'V_partial_time_span'
    ROUTINE_ARGUMENTS = 'V_arguments'
    ROUTINE_ARGUMENT = 'V_single_argument'
    PARAMETERS = 'V_parameters'
    ROUTINE_DEFINITION = 'V_routine_definition'
    RELATION = 'V_relation'
    LARGE_ORDINAL = 'V_large_ordinal'
    ONCE_STATEMENT = 'V_once_statement'
    WHENEVER_STATEMENT = 'V_whenever_statement'