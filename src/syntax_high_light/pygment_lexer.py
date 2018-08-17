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


class domsaLex(RegexLexer):
    """All your lexer code goes here!"""
    name = "Domsagxo Keyword Lexer"
    aliases = ['domsaLex']
    filenames = ['*.domsa']
    tokens = {
        'root': [
            (r'\s+', Whitespace),
            (digitRe.pattern, Number),
            (r'maldekstra citilo (' + eo_letters + '|\s)* dekstra citilo', String),
            (timeUnitRe.pattern, Generic.TypeIndicator),
            (r'\bkaj\b|,', Generic.Separator),
            (regexFromWordList(prepositions), Generic.Separator),
            (regexFromWordList(reserved_words), Keyword),
            (eo_letters + r'+oj?n?\b', Name),
            (eo_letters + r'+a\b', Name),
            (eo_letters + r'+u\b', Name.Function),
            (eo_letters + r'+i\b', Name.Function),
        ]
    }


if __name__ == "__main__":
    print("started")
    lexer = domsaLex()
    # lexer = IdentifierLexer()
    # lexer = CompleteLexer()
    # lexer = SimplestLexer({
    #     re.compile(r'[a-z]+o'): Other.Noun,
    #     re.compile(r'[a-z]+a'): Other.Adjective,
    #     re.compile(r'[a-z]+i'): Other.Infinity,
    #     re.compile(r'[a-z]+u'): Other.Imperative,
    # })
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
