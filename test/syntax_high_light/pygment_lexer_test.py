import pytest
import syntax_high_light.pygment_lexer as lxr
from pygments.token import *


class LexerProvided(object):
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
            assert token_type == desired_type


class TestLiterals(LexerProvided):

    def test_numbers(self, lexer):
        tokens = lexer.get_tokens("naux dudek ses tricent unu")
        self.assertAllTokensOfSameType(tokens, Number)

    def test_strings(self, lexer):
        tokens = lexer.get_tokens("maldekstra citilo mi nomigxas lioro dekstra citilo")
        self.assertAllTokensOfSameType(tokens, String)

    def test_timePoints_timeSpans(self, lexer):
        tokens = lexer.get_tokens("horo minutoj jaro sekundoj")
        self.assertAllTokensOfSameType(tokens, Generic.TypeIndicator)


class TestRegularWords(LexerProvided):

    def test_functions(self, lexer):
        tokens = lexer.get_tokens("vini vidi vicxi sxambaluli lernu programu sxambalulu")
        self.assertAllTokensOfSameType(tokens, Name.Function)

    def test_adjectives(self, lexer):
        tokens = lexer.get_tokens("blua varma longa")
        self.assertAllTokensOfSameType(tokens, Name)

    def test_nouns(self, lexer):
        tokens = lexer.get_tokens("nomo domo homo pomo")
        self.assertAllTokensOfSameType(tokens, Name)


class TestReserveWords(LexerProvided):

    def test_separators(self, lexer):
        tokens = lexer.get_tokens("gxis kaj ekster trans,")
        self.assertAllTokensOfSameType(tokens, Generic.Separator)
