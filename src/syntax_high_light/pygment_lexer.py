# coding=utf8
from pygments import highlight
from pygments.formatters.latex import LatexFormatter
from pygments.lexer import RegexLexer
from pygments.token import *

from compilation.esp_lexer import reserved_words, prepositions, digitRe, timeUnitRe
from syntax_high_light.pygment_style import DomsagxoStyle


def regexFromWordList(words):
    return "(" + "|".join(words) + r')\b'


eo_lowercase = r'[a-z]|ĉ|ĝ|ĥ|ĵ|ŝ|ŭ'
eo_uppercase = r'[A-Z]|Ĉ|Ĝ|Ĥ|Ĵ|Ŝ|Ŭ'
eo_letters = r'(%s|%s)' % (eo_lowercase, eo_uppercase)


class DomsagxoLexer(RegexLexer):
    """All your lexer code goes here!"""
    name = "Domsagxo Keyword Lexer"
    aliases = ['domsagxo']
    filenames = ['*.domsa']
    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'\|\s*\|', Comment),
            (digitRe.pattern, Number),
            (r'maldekstra citilo', Keyword, 'string'),
            (timeUnitRe.pattern, Generic.TypeIndicator),
            (r'\bkaj\b|,|\.', Generic.Separator),
            (regexFromWordList(prepositions), Generic.Separator),
            (regexFromWordList(reserved_words), Keyword),
            (eo_letters + r'+oj?n?\b', Name),
            (eo_letters + r'+a\b', Name),
            (eo_letters + r'+u\b', Name.Function),
            (eo_letters + r'+i\b', Name.Function),
        ],
        'string' : [
            ('dekstra citilo',Keyword, '#pop'),
            ('((' + eo_letters + ')+)', String),
            (r'\s+', Whitespace),
        ]
    }

class EngluentoLexer(RegexLexer):
    english_keywords = [ 'like','so','means',
            'if','while','then','true','false','greater','equal','not','return','end',
            'at','is','of','than' ]
    english_prepositions = ['between','to' ]
    name = "Engluento Keyword Lexer"
    aliases = ['engluento']
    filenames = ['*.englu']
    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (r'"', Keyword, 'string'),
            (r'((hour)|(minute)|(second))s?', Generic.TypeIndicator),
            (r'\band\b|,|\.', Generic.Separator),
            (regexFromWordList(english_prepositions), Generic.Separator),
            (regexFromWordList(english_keywords), Keyword),
            (eo_letters + r'+-?((ation)|(ate)|(ify)|(ing))\b', Name.Function),
        ],
        'string' : [
            ('"',Keyword, '#pop'),
            ('(\w+)', String),
            (r'\s+', Whitespace),
        ]
    }


if __name__ == "__main__":
    print("started")
    lexer = DomsagxoLexer()
    print("created lexer")
    code = """ŝambaluli en la nokton signifas dudek horoj estas egala al la kato en domsaĝo finu"""
    formatter = LatexFormatter(style=DomsagxoStyle)
    formatter.full = True
    # print(
    with open('domsa.tex', 'w', encoding='utf8') as outFile:
        highlight(code, lexer, formatter,
                  outFile)
        # )
    # )
    # for token in lexer.get_tokens_unprocessed(code):
    #     print(token)
    # so_called_regex = re.compile(r"(" + eo_letters + ")+" + gobble_spaces)
    # # print(so_called_regex.pattern)
    # while 1:
    #     exp = input(">")
    #     print(so_called_regex.match(exp) is not None)
