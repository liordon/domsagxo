import re
from enum import Enum

import ply.lex as lex


class SemanticError(Exception):
    pass


def idList(token_enum):
    return [elm.value for elm in list(token_enum)]


class UnalphabeticTerminal(Enum):
    PLUS = 'PLUS'
    MINUS = 'MINUS'
    DELIM = 'DELIM'
    COLON = 'COLON'
    TIMES = 'TIMES'
    DIVIDE = 'DIVIDE'
    PERIOD = 'PERIOD'
    ASSIGN = 'ASSIGN'
    NUMBER = 'NUMBER'
    L_PAREN = 'LPAREN'
    R_PAREN = 'RPAREN'
    COMMENT = 'COMMENT'


class ReservedWord(Enum):
    AFTER = 'POST'
    AND = 'KAJ'
    AT = 'JE'
    DURING = 'DUM'
    ELSE = 'ALIE'
    END = 'FINU'
    EQUAL = 'EGALA'
    EVERY = 'CXIU'
    FALSE = 'FALSE'
    FOR = 'FOR'
    GREATER = 'GRANDA'
    IF = 'IF'
    IN = 'EN'
    MORE = 'PLI'
    NONE = 'NENIO'
    NOT = 'NE'
    OF = 'DE'
    OR = 'OR'
    RETURN = 'REVENU'
    SMALLER = 'MALGRANDA'
    THAN = 'OL'
    THE = 'LA'
    THEN = 'TIAM'
    THIS_WAY = 'TIEL'
    TIME_INDICATION = 'TIME_INDICATION'
    TO = 'AL'
    TRUE = 'TRUE'
    VERBAL_DIGIT = 'VERBAL_DIGIT'
    WORD = 'WORD'


class PartOfSpeech(Enum):
    ADJECTIVE = 'ADJECTIVE'
    ADVERB = 'ADVERB'
    NOUN = 'NOUN'
    NUMERATOR = "NUMERATOR"
    OTHER = "OTHER"
    PREPOSITION = "PREPOSITION"
    V_INF = 'VINF'
    V_PRES = 'VPRES'
    V_IMP = 'VIMP'


reserved_words = {
    "se"       : ReservedWord.IF.value,
    "je"       : ReservedWord.AT.value,
    "al"       : ReservedWord.TO.value,
    "aux"      : ReservedWord.OR.value,
    "ne"       : ReservedWord.NOT.value,
    "la"       : ReservedWord.THE.value,
    "por"      : ReservedWord.FOR.value,
    "kaj"      : ReservedWord.AND.value,
    "finu"     : ReservedWord.END.value,
    "ol"       : ReservedWord.THAN.value,
    "pli"      : ReservedWord.MORE.value,
    "tiam"     : ReservedWord.THEN.value,
    "alie"     : ReservedWord.ELSE.value,
    "vero"     : ReservedWord.TRUE.value,
    "nenio"    : ReservedWord.NONE.value,
    "post"     : ReservedWord.AFTER.value,
    "cxiu"     : ReservedWord.EVERY.value,
    "egala"    : ReservedWord.EQUAL.value,
    "malvero"  : ReservedWord.FALSE.value,
    "dum"      : ReservedWord.DURING.value,
    "revenu"   : ReservedWord.RETURN.value,
    "granda"   : ReservedWord.GREATER.value,
    "malgranda": ReservedWord.SMALLER.value,
    "tiel"     : ReservedWord.THIS_WAY.value,
    "estas"    : UnalphabeticTerminal.ASSIGN.value,
}

prepositions = [
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
]

tokens = [] + idList(ReservedWord) \
         + idList(PartOfSpeech) \
         + idList(UnalphabeticTerminal)

# Regular expression rules for simple tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_COLON = r':'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_ASSIGN = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PERIOD = r'\.'
t_DELIM = r','
t_ignore_COMMENT = r'\#.*'


def t_NUMBER(t):
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


def t_WORD(t):
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
