import pytest
import src.lexer_builder as lxr
from src.lexer_builder import PartOfSpeech, UnalphabeticTerminal, ReservedWord


class TestLexer(object):

    @pytest.fixture
    def lexer(self):
        return lxr.build()

    @staticmethod
    def assertPartOfSpeech(token, partOfSpeech):
        assert partOfSpeech.value == token.type

    @staticmethod
    def getTokenList(lexer):
        res = []
        tok = lexer.token()
        while tok:
            res += [tok]
            tok = lexer.token()
        return res

    def test_adjectivesAreNotCategorizedAsWords(self, lexer):
        lexer.input("granda")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.ADJECTIVE)
        assert token.value == "granda"

    def test_ImperativeVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presu")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.V_IMP)
        assert token.value == "presu"

    def test_PresentVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presas")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.V_PRES)
        assert token.value == "presas"

    def test_InfinitiveVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presi")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.V_INF)
        assert token.value == "presi"

    def test_theWordEstasIsEquivalentToEqualsSign(self, lexer):
        estasAssignment = "estas"
        equalsAssignment = "="
        lexer.input(estasAssignment)
        self.assertPartOfSpeech(lexer.token(), UnalphabeticTerminal.ASSIGN)
        lexer.input(equalsAssignment)
        self.assertPartOfSpeech(lexer.token(), UnalphabeticTerminal.ASSIGN)

    def test_accusativeNounsAreEvaluatedWithoutTheAccusativeCase(self, lexer):
        lexer.input("muson musojn")
        assert lexer.token().value == 'muso'
        assert lexer.token().value == 'musoj'

    def test_kajIsADelimiterAndNotAnAdjective(self, lexer):
        lexer.input("kaj")
        self.assertPartOfSpeech(lexer.token(), UnalphabeticTerminal.DELIM)

    def test_kunIsADelimAndNotAnAccusativeImperativeVerb(self, lexer):
        lexer.input("kun")
        self.assertPartOfSpeech(lexer.token(), UnalphabeticTerminal.DELIM)

    def test_numericalTokensHaveIntValuesInsteadOfStrings(self, lexer):
        lexer.input("42")
        assert 42 == lexer.token().value

    def test_reservedWordTrue(self, lexer):
        lexer.input("vero")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TRUE)

    def test_reservedWordFalse(self, lexer):
        lexer.input("malvero")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.FALSE)

    def test_reservedWordLa(self, lexer):
        lexer.input("la")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.LA)


class TestMultipleTokenSequences(object):
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

    def test_canParseSeveralTokensTogether(self, lexer):
        lexer.input("kato ludas hun hundo")
        assert 4 == len(self.getTokenList(lexer))

    def test_canParseSimpleAdditionExpression(self, lexer):
        lexer.input("1+1")
        tokens = self.getTokenList(lexer)
        print(tokens)