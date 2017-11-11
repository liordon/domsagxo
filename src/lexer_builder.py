from enum import Enum

import ply.lex as lex
import re


class SemanticError(Exception):
    pass


# List of token names.   This is always required
def idList(tokenEnum):
    return [elm.value for elm in list(tokenEnum)]


class UnalphabeticTerminal(Enum):
    DELIM = 'DELIM'
    ASSIGN = 'ASSIGN'
    NUMBER = 'NUMBER'
    PLUS = 'PLUS'
    MINUS = 'MINUS'
    TIMES = 'TIMES'
    DIVIDE = 'DIVIDE'
    L_PAREN = 'LPAREN'
    R_PAREN = 'RPAREN'
    PERIOD = 'PERIOD'
    COMMENT = 'COMMENT'


class ReservedWord(Enum):
    WORD = 'WORD'
    FOR = 'FOR'
    DE = 'DE'
    EN = 'EN'
    LA = 'LA'
    TRUE = 'TRUE'
    FALSE = 'FALSE'


class PartOfSpeech(Enum):
    NOUN = 'NOUN'
    ADJECTIVE = 'ADJECTIVE'
    ADVERB = 'ADVERB'
    V_INF = 'V_INF'
    V_PRES = 'V_PRES'
    V_IMP = 'V_IMP'
    ACCUSATIVE = "ACCUSATIVE"
    PLURAL = "PLURAL"
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
    "kaj": UnalphabeticTerminal.DELIM.value,
    "vero": ReservedWord.TRUE.value,
    "inter": UnalphabeticTerminal.DELIM.value,
    "exter": UnalphabeticTerminal.DELIM.value,
    "trans": UnalphabeticTerminal.DELIM.value,
    "estas": UnalphabeticTerminal.ASSIGN.value,
    "malvero": ReservedWord.FALSE.value}

tokens = [ ] + idList(ReservedWord) \
         + idList(PartOfSpeech) \
         + idList(UnalphabeticTerminal)

# Regular expression rules for simple tokens
t_PLUS = r'\+'
t_MINUS = r'-'
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


def t_WORD(t):
    r'[a-z]+'
    # t.value = re.sub(r'n?\s+',"_",t.value)
    if reserved_words.keys().__contains__(t.value):
        t.type = reserved_words[t.value]
    else:
        t.value = re.sub(r'n$', "", t.value)
        if re.search(r'((o)|(oj))$', t.value):
            t.type = PartOfSpeech.NOUN.value
        elif re.search(r'((a)|(aj))$', t.value):
            t.type = PartOfSpeech.ADJECTIVE.value
        elif re.search(r'i$', t.value):
            t.type = PartOfSpeech.V_INF.value
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


def t_foo_newline(t):
    r'\n'
    t.lexer.lineno += 1


# Error handling rule
def t_error(t):
    raise SemanticError("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


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
