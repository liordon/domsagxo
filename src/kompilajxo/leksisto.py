from enum import Enum

import ply.lex as lex
import re


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
    DE = 'DE'
    EN = 'EN'
    LA = 'LA'
    IF = 'IF'
    KAJ = 'KAJ'
    FOR = 'FOR'
    WORD = 'WORD'
    TIEL = 'TIEL'
    TIAM = 'TIAM'
    ALIE = 'ALIE'
    FINU = 'FINU'
    TRUE = 'TRUE'
    FALSE = 'FALSE'
    NENIO = 'NENIO'
    REVENU = 'REVENU'
    VERBAL_DIGIT = 'VERBAL_DIGIT'
    TIME_INDICATION = 'TIME_INDICATION'


class PartOfSpeech(Enum):
    NOUN = 'NOUN'
    ADJECTIVE = 'ADJECTIVE'
    ADVERB = 'ADVERB'
    V_INF = 'VINF'
    V_PRES = 'VPRES'
    V_IMP = 'VIMP'
    PREPOSITION = "PREPOSITION"
    NUMERATOR = "NUMERATOR"
    OTHER = "OTHER"


reserved_words = {
    "se": ReservedWord.IF.value,
    "la": ReservedWord.LA.value,
    "por": ReservedWord.FOR.value,
    "kaj": ReservedWord.KAJ.value,
    "finu": ReservedWord.FINU.value,
    "tiel": ReservedWord.TIEL.value,
    "tiam": ReservedWord.TIAM.value,
    "alie": ReservedWord.ALIE.value,
    "vero": ReservedWord.TRUE.value,
    "estas": UnalphabeticTerminal.ASSIGN.value,
    "nenio": ReservedWord.NENIO.value,
    "revenu": ReservedWord.REVENU.value,
    "malvero": ReservedWord.FALSE.value}

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
    "nul": 0,
    "unu": 1,
    "du": 2,
    "tri": 3,
    "kvar": 4,
    "kvin": 5,
    "ses": 6,
    "sep": 7,
    "ok": 8,
    "naux": 9,
    "": 1
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
    # t.lexer.skip(1) # this line is unreachable because I raised an exception instead of skipping it.


def build():
    return lex.lex()


if __name__ == "__main__":
    lexer = build()
    lexer.input(input())

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
