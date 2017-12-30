from enum import Enum

import ply.lex as lex
import re


class SemanticError(Exception):
    pass


# List of token names.   This is always required
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
    VERBAL_DIGIT = 'VERBAL_DIGIT'


class ReservedWord(Enum):
    DE = 'DE'
    EN = 'EN'
    LA = 'LA'
    IF = 'IF'
    KAJ = 'KAJ'
    FOR = 'FOR'
    WORD = 'WORD'
    TRUE = 'TRUE'
    HORO = 'HORO'
    FALSE = 'FALSE'
    MINUTOJ = 'MINUTOJ'


class PartOfSpeech(Enum):
    NOUN = 'NOUN'
    ADJECTIVE = 'ADJECTIVE'
    ADVERB = 'ADVERB'
    V_INF = 'V_INF'
    V_PRES = 'V_PRES'
    V_IMP = 'V_IMP'
    ACCUSATIVE = "ACCUSATIVE"
    NUMERATOR = "NUMERATOR"
    OTHER = "OTHER"


reserved_words = {
    "de": ReservedWord.DE.value,
    "en": ReservedWord.EN.value,
    "el": UnalphabeticTerminal.DELIM.value,
    "al": UnalphabeticTerminal.DELIM.value,
    "la": ReservedWord.LA.value,
    "por": ReservedWord.FOR.value,
    "kun": UnalphabeticTerminal.DELIM.value,
    "sur": UnalphabeticTerminal.DELIM.value,
    "kaj": ReservedWord.KAJ.value,
    "vero": ReservedWord.TRUE.value,
    "horo": ReservedWord.HORO.value,
    "inter": UnalphabeticTerminal.DELIM.value,
    "exter": UnalphabeticTerminal.DELIM.value,
    "trans": UnalphabeticTerminal.DELIM.value,
    "estas": UnalphabeticTerminal.ASSIGN.value,
    "minutoj": ReservedWord.MINUTOJ.value,
    "malvero": ReservedWord.FALSE.value}

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
t_ignore_COMMENT = r'\#.*'


# A regular expression rule with some action code
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


def parseDigit(name):
    mult = 1
    if name[-3:] == "dek":
        name = name[:-3]
        mult = 10
    elif name[-4:] == "cent":
        name = name[:-4]
        mult = 100
    return digitNames[name] * mult


def t_WORD(t):
    r'[a-z]+'
    digitRe = re.compile(
        "(nul|(unu)?|du|tri|kvar|kvin|ses|sep|ok|naux)(dek|cent)?")
    if reserved_words.keys().__contains__(t.value):
        t.type = reserved_words[t.value]
    elif digitRe.fullmatch(t.value):
        t.type = UnalphabeticTerminal.VERBAL_DIGIT.value
        t.value = parseDigit(t.value)
    else:
        t.value = re.sub(r'n$', "", t.value)
        if re.search(r'((o)|(oj))$', t.value):
            t.type = PartOfSpeech.NOUN.value
        elif re.search(r'((a)|(aj))$', t.value):
            if digitRe.fullmatch(t.value[0:-1]) and t.value[-1] == 'a':
                t.type = PartOfSpeech.NUMERATOR.value
            else:
                t.type = PartOfSpeech.ADJECTIVE.value
        # elif re.search(r'i$', t.value):
        #     t.type = PartOfSpeech.V_INF.value
        elif re.search(r'u$', t.value):
            t.type = PartOfSpeech.V_IMP.value
        elif re.search(r'as$', t.value):
            t.type = PartOfSpeech.V_PRES.value
    return t


t_ADVERB = r'[a-z]+e'
# 'ADJECTIVE'
t_V_INF = r'[a-z]+i'
t_V_PRES = r'[a-z]+as'
t_V_IMP = r'[a-z]+u'

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
