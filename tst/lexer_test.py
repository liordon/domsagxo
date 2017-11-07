import pytest
import src.lexer_builder as lxr


class TestLexer(object):


    @pytest.fixture
    def lexer(self):
        return lxr.build()

    def getTokenList(self, lexer):
        res = []
        tok = lexer.token()
        while tok:
            res += [tok]
            tok = lexer.token()
        return res

    def test_adjectivesAreNotCategorizedAsWords(self, lexer):
        lexer.input("granda")
        token = lexer.token()
        assert token.type == 'ADJECTIVE'
        assert token.value == "granda"


    def test_theWordEstasIsEquivalentToEqualsSign(self, lexer):
        estasAssignment = "pagxoj estas 3"
        equalsAssignment = "pagxoj = 3"
        lexer.input(estasAssignment)
        estasTokens = self.getTokenList(lexer)
        lexer.input(equalsAssignment)
        equalsTokens = self.getTokenList(lexer)

        for i in range(len(estasTokens)):
            assert estasTokens[i].type == equalsTokens[i].type

    def test_accusativeNounsAreEvaluatedWithoutTheAccusativeCase(self, lexer):
        lexer.input("muson musojn")
        assert lexer.token().value == 'muso'
        assert lexer.token().value == 'musoj'

    def test_kajIsADelimiterAndNotAnAdjective(self, lexer):
        lexer.input("hundo kaj hundino")
        tokens = self.getTokenList(lexer)
        assert 3 == len(tokens)
        assert "DELIM" == tokens[1].type

    def test_kunIsADelimAndNotAnAccusativeImperativeVerb(self, lexer):
        lexer.input("kato ludas kun hundo")
        tokens = self.getTokenList(lexer)
        assert 4 == len(tokens)
        assert 'DELIM' == tokens[2].type

    def test_numericalTokensHaveIntValuesInsteadOfStrings(self, lexer):
        lexer.input("42")
        assert 42 == lexer.token().value