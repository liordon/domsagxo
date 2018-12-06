# coding=utf8
import syntax_high_light.engluento as engluento
from pygments import highlight
from pygments.formatters.latex import LatexFormatter
from pygments.lexer import RegexLexer
from pygments.token import *
from syntax_high_light.pygment_style import DomsagxoStyle

from compilation.esp_lexer import reserved_words, prepositions, digitRe, timeUnitRe


def regexFromWordList(words):
    return r"(?i)(\b(" + "|".join(words) + r")\b)"


alphabet = r'[^\W\d_]'


class DomsagxoLexer(RegexLexer):
    """All your lexer code goes here!"""
    name = "Domsagxo Keyword Lexer"
    aliases = ['domsagxo']
    filenames = ['*.domsa']
    tokens = {
        'root'  : [
            (r'\s+', Whitespace),
            (r'#.*\n', Comment),
            (r'\bla\b', Comment),
            (r'\.|:|,', Comment),
            (digitRe.pattern, Number),
            (r'maldekstra citilo', Keyword, 'string'),
            (timeUnitRe.pattern, Generic.TypeIndicator),
            (r'\bkaj\b|poste|samtempe', Generic.Separator),
            (regexFromWordList(reserved_words), Keyword),
            (regexFromWordList(prepositions), Generic.Separator),
            (alphabet + r'+oj?n?\b', Name),
            (alphabet + r'+aj?n?\b', Name),
            (alphabet + r'+u\b', Name.Function),
            (alphabet + r'+i\b', Name.Function),
            (alphabet + r'+as\b', Name.Function),
        ],
        'string': [
            ('dekstra citilo', Keyword, '#pop'),
            ('((' + alphabet + ')+)', String),
            (r'\s+', Whitespace),
        ]
    }


class EngluentoLexer(RegexLexer):
    name = "Engluento Keyword Lexer"
    aliases = ['engluento']
    filenames = ['*.englu']
    tokens = {
        'root'  : [
            (alphabet + r'+-?((ation)|(ate)|(ify)|(ing))\b', Name.Function),
            (regexFromWordList(
                engluento.fractions + engluento.digits + engluento.teens +
                engluento.decades + engluento.largeAmounts), Number),
            (regexFromWordList(engluento.nouns), Name),
            (regexFromWordList(engluento.adjectives), Name),
            (r'\s+', Whitespace),
            (r'\b\'s\b', Operator),
            (r'"', Keyword, 'string'),
            (r'#.*\n', Comment),
            (r'(?i)\b(a|(the))\b', Comment),
            (r'\d+', Number),
            (r'(week|day|hour|minute|second)s?', Generic.TypeIndicator),
            (r'\band\b|,|\.|:', Generic.Separator),
            (regexFromWordList(engluento.prepositions), Generic.Separator),
            (regexFromWordList(engluento.keywords), Keyword),
            (regexFromWordList(engluento.verbs), Name.Function),
            (r'[\+\-\=\*\\/]', Operator),
        ],
        'string': [
            ('"', Keyword, '#pop'),
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
    # so_called_regex = re.compile(r"(" + alphabet + ")+" + gobble_spaces)
    # # print(so_called_regex.pattern)
    # while 1:
    #     exp = input(">")
    #     print(so_called_regex.match(exp) is not None)
