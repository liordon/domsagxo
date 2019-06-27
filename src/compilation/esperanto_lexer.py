import re

import ply.lex as lex

from compilation.definitions import *


class SemanticError(Exception):
    pass


def idList(token_enum):
    return [elm.value for elm in list(token_enum)]


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
    r"""\d+"""
    t.value = int(t.value)
    return t


digitRe = re.compile(
    r"((nul|unu)|((du|tri|kvar|kvin|ses|sep|ok|naux|naŭ)(dek|cent|ono)?)|(dek|cent|mil))\b")

timeUnitRe = re.compile(r"(jaro|monato|semajno|tago|horo|minuto|sekundo)j?\b")


def t_string(t):
    r"""('.*?')|(".*?")|(\bmaldekstra\scitilo\b.*?\bdekstra\scitilo\b)|(
    \bcitilo\b.*?\bmalcitilo\b)"""
    if t.value.startswith("maldekstra citilo"):
        t.value = t.value[18:-15]
    if t.value.startswith("citilo"):
        t.value = t.value[7:-10]
    if t.value.startswith(("'", '"')):
        t.value = t.value[1:-1]
    t.type = UnalphabeticTerminal.STRING.value
    return t


def t_TWORD(t):
    r"""[a-zĉĝĥĵŝŭA-ZĈĜĤĴŜŬ]+"""
    if reserved_words.keys().__contains__(t.value):
        determine_type_and_value_of_reserved_words(t)
    elif digitRe.fullmatch(t.value):
        t.type = ReservedWord.VERBAL_DIGIT.value
    elif timeUnitRe.fullmatch(t.value):
        t.type = ReservedWord.TIME_INDICATION.value
    elif t.value in prepositions:
        t.type = PartOfSpeech.PREPOSITION.value
    else:
        if re.search(r'(oj?n?)$', t.value):
            t.value = re.sub(r'n$', "", t.value)
            t.type = PartOfSpeech.NOUN.value
        elif re.search(r'(aj?n?)$', t.value):
            t.value = re.sub(r'n$', "", t.value)
            if digitRe.fullmatch(t.value[0:-1]) and t.value[-1] == 'a':
                t.type = PartOfSpeech.ORDINAL.value
            else:
                t.type = PartOfSpeech.ADJECTIVE.value
        elif re.search(r'i$', t.value):
            t.type = PartOfSpeech.V_INF.value
        elif re.search(r'u$', t.value):
            t.type = PartOfSpeech.V_IMP.value
        elif re.search(r'as$', t.value):
            t.type = PartOfSpeech.V_PRES.value
    return t


def determine_type_and_value_of_reserved_words(t):
    t.type = reserved_words[t.value]
    if t.type == ReservedWord.IT.value:
        t.value = ReservedWord.IT.value[1:]


# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


def t_newline(t):
    r"""\n+"""
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
