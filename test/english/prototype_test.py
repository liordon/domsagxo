import pytest

from compilation.definitions import PartOfSpeech
from english.prototype import Prototype
from test_utils.providers import PartOfSpeechVerifier


class EnglishLexerProvided(PartOfSpeechVerifier):
    @pytest.fixture
    def lexer(self):
        return Prototype()


class TestEnglishParserPrototype(EnglishLexerProvided):

    def test_canIdentifyCatAsNoun(self, lexer):
        lexer.input("cat")
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyBigAsAdjective(self, lexer):
        lexer.input("big")
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.ADJECTIVE, lexer)

    def test_canIdentifyCatHatAsTwoConsecutiveNouns(self, lexer):
        lexer.input("cat hat")
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyBigManAsConsecutiveAdjectiveAndANoun(self, lexer):
        lexer.input("big man")
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.ADJECTIVE, lexer)
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyFirstAsANumerator(self, lexer):
        lexer.input("first")
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.ORDINAL, lexer)

    def test_canIdentifyGoAsAnImperativeVerb(self, lexer):
        lexer.input("go")
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.V_IMP, lexer)
