import re
from enum import Enum

import ply.lex as lex


class SemanticError(Exception):
    pass


def idList(token_enum):
    return [elm.value for elm in list(token_enum)]


class UnalphabeticTerminal(Enum):
    PLUS = 'TPLUS'
    MINUS = 'TMINUS'
    DELIM = 'TDELIM'
    COLON = 'TCOLON'
    TIMES = 'TTIMES'
    DIVIDE = 'TDIVIDE'
    PERIOD = 'TPERIOD'
    ASSIGN = 'TASSIGN'
    NUMBER = 'TNUMBER'
    L_PAREN = 'TLPAREN'
    R_PAREN = 'TRPAREN'
    COMMENT = 'TCOMMENT'


class ReservedWord(Enum):
    AFTER = 'Tpost'
    AND = 'Tkaj'
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
    LESS = 'Tmalpli'
    MORE = 'Tpli'
    NONE = 'Tnenio'
    NOT = 'Tne'
    OF = 'Tde'
    OR = 'Taux'
    PARTS = 'Tpartoj'
    RETURN = 'Trevenu'
    SMALLER = 'Tmalgranda'
    THAN = 'Tol'
    THE = 'Tla'
    THEN = 'Ttiam'
    THIS_WAY = 'Ttiel'
    TIME_INDICATION = 'VtimeUnit'
    TIMES = 'Tfojoj'
    TO = 'Tal'
    TRUE = 'Tvero'
    VERBAL_DIGIT = 'Tparolcifero'
    WORD = 'Tvorto'


class PartOfSpeech(Enum):
    ADJECTIVE = 'Padjective'
    ADVERB = 'Padverb'
    NOUN = 'Pnoun'
    NUMERATOR = "Pnumerator"
    OTHER = "Pother"
    PREPOSITION = "Ppreposition"
    V_INF = 'Pv_inf'
    V_PRES = 'Pv_pres'
    V_IMP = 'Pv_imp'


reserved_words = {
    "al"       : ReservedWord.TO.value,
    "alie"     : ReservedWord.ELSE.value,
    "ambaux"   : ReservedWord.BOTH.value,
    "aux"      : ReservedWord.OR.value,
    "cxiu"     : ReservedWord.EVERY.value,
    "dum"      : ReservedWord.DURING.value,
    "de"       : ReservedWord.OF.value,
    "egala"    : ReservedWord.EQUAL.value,
    "estas"    : UnalphabeticTerminal.ASSIGN.value,
    "finu"     : ReservedWord.END.value,
    "fojoj"    : ReservedWord.TIMES.value,
    "granda"   : ReservedWord.GREATER.value,
    "je"       : ReservedWord.AT.value,
    "kaj"      : ReservedWord.AND.value,
    "la"       : ReservedWord.THE.value,
    "malgranda": ReservedWord.SMALLER.value,
    "malpli"   : ReservedWord.LESS.value,
    "malvero"  : ReservedWord.FALSE.value,
    "ne"       : ReservedWord.NOT.value,
    "nenio"    : ReservedWord.NONE.value,
    "ol"       : ReservedWord.THAN.value,
    "partoj"   : ReservedWord.PARTS.value,
    "pli"      : ReservedWord.MORE.value,
    "por"      : ReservedWord.FOR.value,
    "post"     : ReservedWord.AFTER.value,
    "revenu"   : ReservedWord.RETURN.value,
    "se"       : ReservedWord.IF.value,
    "signifas" : ReservedWord.THIS_WAY.value,
    "tiam"     : ReservedWord.THEN.value,
    "vero"     : ReservedWord.TRUE.value,
}

prepositions = {
    "al",
    "anstataux",
    "antaux",
    "apud",
    "cxe",
    "cxirkaux",
    "de",
    "dum",
    "ekde",
    "ekster",
    "eksteren",
    "el",
    "en",
    "gxis",
    "inter",
    "kiel",
    "kontraux",
    "krom",
    "kun",
    "laux",
    "mala",
    "malgraux",
    "malkiel",
    "malsupren",
    "ol",
    "per",
    "plus",
    "po",
    "por",
    "post",
    "preter",
    "pri",
    "pro",
    "sed",
    "sekva",
    "sen",
    "sub",
    "suben",
    "super",
    "supren",
    "sur",
    "tra",
    "trans"
}

tokens = [] + idList(ReservedWord) \
         + idList(PartOfSpeech) \
         + idList(UnalphabeticTerminal)

# Regular expression rules for simple tokens
t_TPLUS = r'\+'
t_TMINUS = r'-'
t_TCOLON = r':'
t_TTIMES = r'\*'
t_TDIVIDE = r'/'
t_TASSIGN = r'='
t_TLPAREN = r'\('
t_TRPAREN = r'\)'
t_TPERIOD = r'\.'
t_TDELIM = r','
t_ignore_TCOMMENT = r'\#.*'


def t_TNUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


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
    "naux": 9,
    ""    : 1
}

digitRe = re.compile(
    "(nul|(unu)?|du|tri|kvar|kvin|ses|sep|ok|naux)(dek|cent|ono?)?")

timeUnitRe = re.compile("(jaro|monato|semajno|tago|horo|minuto|sekundo)j?")


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


def t_TWORD(t):
    r'[a-z]+'
    if reserved_words.keys().__contains__(t.value):
        t.type = reserved_words[t.value]
    elif digitRe.fullmatch(t.value):
        t.type = ReservedWord.VERBAL_DIGIT.value
        t.value = parseDigit(t.value)
    elif timeUnitRe.fullmatch(t.value):
        t.type = ReservedWord.TIME_INDICATION.value
    elif t.value in prepositions:
        t.type = UnalphabeticTerminal.DELIM.value
    else:
        t.value = re.sub(r'n$', "", t.value)
        if re.search(r'((o)|(oj))$', t.value):
            t.type = PartOfSpeech.NOUN.value
        elif re.search(r'((a)|(aj))$', t.value):
            if digitRe.fullmatch(t.value[0:-1]) and t.value[-1] == 'a':
                t.type = PartOfSpeech.NUMERATOR.value
                t.value = parseDigit(t.value[:-1])
            else:
                t.type = PartOfSpeech.ADJECTIVE.value
        elif re.search(r'i$', t.value):
            t.type = PartOfSpeech.V_INF.value
        elif re.search(r'u$', t.value):
            t.type = PartOfSpeech.V_IMP.value
        elif re.search(r'as$', t.value):
            t.type = PartOfSpeech.V_PRES.value
    return t


# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# Error handling rule
def t_error(t):
    raise SemanticError("Illegal character '%s'" % t.value[0])


def build():
    return lex.lex()


if __name__ == "__main__":
    lexer = build()

    while True:
        lexer.input(input("kio vi diras?"))
        for token in lexer:
            print(token)
            if token.value == "finu":
                exit(0)
