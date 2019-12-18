import pytest

from compilation.definitions import PartOfSpeech, ReservedWord
from englishPrototype.english_lexer import EnglishLexer, Token
from test_utils.providers import PartOfSpeechVerifier


class TestBasicTokenConversionFromNlpToPos(object):

    def test_DtIsConvertedToThe(self):
        assert Token.NlpTagSimplifier["DT"] == ReservedWord.THE.value

    def test_InIsConvertedToPreposition(self):
        assert Token.NlpTagSimplifier["IN"] == PartOfSpeech.PREPOSITION.value

    def test_JjAndJjsAreConvertedToAdjective(self):
        assert Token.NlpTagSimplifier["JJ"] == PartOfSpeech.ADJECTIVE.value
        assert Token.NlpTagSimplifier["JJS"] == PartOfSpeech.ADJECTIVE.value

    def test_NnAndNnsAreConvertedToNoun(self):
        assert Token.NlpTagSimplifier["NN"] == PartOfSpeech.NOUN.value
        assert Token.NlpTagSimplifier["NNS"] == PartOfSpeech.NOUN.value

    def test_VbIsConvertedToImperativeVerb(self):
        assert Token.NlpTagSimplifier["VB"] == PartOfSpeech.V_IMP.value

    def test_VbzIsConvertedToPresentVerb(self):
        assert Token.NlpTagSimplifier["VBZ"] == PartOfSpeech.V_PRES.value


class EnglishLexerProvided(PartOfSpeechVerifier):
    @pytest.fixture
    def lexer(self):
        return EnglishLexer()


class TestEnglishParserPrototype(EnglishLexerProvided):

    def test_canIdentifyCatAsNoun(self, lexer):
        lexer.input("cat")
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyCatsAsNounAndDisregardPluralism(self, lexer):
        lexer.input("cats")
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


def extract_all_tokens(lexer):
    return [t for t in lexer]


class TestEnglishLexerPrototypeOnCommands(EnglishLexerProvided):

    def test_turningOnTheLights(self, lexer):
        lexer.input("turn on the lights")

        token_type_list = [t.type for t in (extract_all_tokens(lexer))]
        # assert PartOfSpeech.V_IMP.value in token_type_list
        assert ReservedWord.THE.value in token_type_list
        assert PartOfSpeech.NOUN.value in token_type_list
