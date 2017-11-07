import ply.lex as lex
import re

# List of token names.   This is always required
reserved_words = {"por" : 'POR',
                  "kun" : 'DELIM',
                  "sur" : 'DELIM',
                  "kaj" : 'DELIM',
                  "de" : 'DE',
                  "en" : 'EN',
                  "el" : 'DELIM',
                  "al" : 'DELIM',
                  "inter" : 'DELIM',
                  "exter" : 'DELIM',
                  "trans" : 'DELIM',
                  "estas" : 'ASSIGN'}

tokens = [
   'NUMBER', 'WORD', 'COMMENT',
   'NOUN', 'ADJECTIVE', 'ADVERB',
   'V_INF', 'V_PRES', 'V_IMP',
   'ACCUSATIVE', 'PLURAL', 'OTHER', #????
   'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
   'LPAREN', 'RPAREN', 'PERIOD'
] + list(set(reserved_words.values()))

# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_ASSIGN  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_PERIOD  = r'\.'
t_ignore_COMMENT = r'\#.*'

# t_DELIM   = r'((sur)|(de)|(kun)|(kaj)|(en)|(el)|(al)|(inter)|(trans))\s+'

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
        if re.search(r'(o)|(oj)$', t.value):
            t.type = 'NOUN'
        elif re.search(r'(a)|(aj)$', t.value):
            t.type = 'ADJECTIVE'
        elif re.search(r'as$', t.value):
            t.type = 'V_INF'
    return t


t_ADVERB = r'[a-z]+e'
    # 'ADJECTIVE'
t_V_INF = r'[a-z]+i'
t_V_PRES= r'[a-z]+as'
t_V_IMP = r'[a-z]+u'

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'


# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

def build():
    return lex.lex()

if __name__=="__main__":
    lexer = build()
    lexer.input(input())

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
