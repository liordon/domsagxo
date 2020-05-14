import pytest
from pygments.token import *

from domsagxo.syntax_high_light import pygment_lexer as lxr


class SyntaxLexerProvided(object):
    @pytest.fixture
    def lexer(self):
        return lxr.DomsagxoLexer()

    @staticmethod
    def assertNoErrorTokensInList(token_list):
        for token_type, content in token_list:
            assert token_type != Token.Error

    @staticmethod
    def assertAllTokensOfSameType(token_list, desired_type):
        for token_type, content in token_list:
            if token_type == Whitespace:
                continue
            assert token_type == desired_type, "token " + content + " had the wrong type"

    @staticmethod
    def assertTokensTypeSequence(token_list, types_list):
        current_type = 0
        for token_type, content in token_list:
            if Whitespace == token_type:
                continue
            assert types_list[current_type] == token_type
            current_type += 1


class TestLiterals(SyntaxLexerProvided):

    def test_numbers(self, lexer):
        tokens = lexer.get_tokens("naux dudek ses tricent unu")
        self.assertAllTokensOfSameType(tokens, Number)

    def test_strings(self, lexer):
        tokens = lexer.get_tokens("maldekstra citilo mi nomigxas lioro dekstra citilo")
        self.assertTokensTypeSequence(tokens, [Keyword] + [String] * 3 + [Keyword])

    def test_timePoints_timeSpans(self, lexer):
        tokens = lexer.get_tokens("horo minutoj jaro sekundoj")
        self.assertAllTokensOfSameType(tokens, Generic.TypeIndicator)


class TestRegularWords(SyntaxLexerProvided):

    def test_functions(self, lexer):
        tokens = lexer.get_tokens("vini vidi vicxi sxambaluli lernu programu sxambalulu")
        self.assertAllTokensOfSameType(tokens, Name.Function)

    def test_adjectives(self, lexer):
        tokens = lexer.get_tokens("blua varma longa")
        self.assertAllTokensOfSameType(tokens, Name)

    def test_nouns(self, lexer):
        tokens = lexer.get_tokens("nomo domo homo pomo")
        self.assertAllTokensOfSameType(tokens, Name)


class TestReserveWords(SyntaxLexerProvided):

    def test_separators(self, lexer):
        tokens = lexer.get_tokens("gxis kaj ekster trans,")
        self.assertAllTokensOfSameType(tokens, Generic.Separator)

    def test_incorrectlyHighlightedWordsFromThesis(self, lexer):
        tokens = lexer.get_tokens("Du")
        self.assertAllTokensOfSameType(tokens, Literal.Number)
        tokens = lexer.get_tokens("dekstra citilo")
        self.assertAllTokensOfSameType(tokens, Token.Keyword)
        tokens = lexer.get_tokens("maldekstra krampo dekstra krampo")
        self.assertAllTokensOfSameType(tokens, Token.Keyword)
        tokens = lexer.get_tokens("tagoj")
        self.assertAllTokensOfSameType(tokens, Generic.TypeIndicator)
