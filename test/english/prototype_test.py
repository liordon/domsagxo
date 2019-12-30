import pytest

from englishPrototype.english_lexer import *
from test_utils.providers import PartOfSpeechVerifier


class TestBasicTokenConversionFromNlpToPos(object):

    def test_DtIsConvertedToThe(self):
        assert convert_tag_to_part_of_speech("DT") == ReservedWord.THE.value

    def test_InIsConvertedToPreposition(self):
        assert convert_tag_to_part_of_speech("IN") == PartOfSpeech.PREPOSITION.value

    def test_JjAndJjsAreConvertedToAdjective(self):
        assert convert_tag_to_part_of_speech("JJ") == PartOfSpeech.ADJECTIVE.value
        assert convert_tag_to_part_of_speech("JJS") == PartOfSpeech.ADJECTIVE.value

    def test_NnAndNnsAreConvertedToNoun(self):
        assert convert_tag_to_part_of_speech("NN") == PartOfSpeech.NOUN.value
        assert convert_tag_to_part_of_speech("NNS") == PartOfSpeech.NOUN.value

    def test_VbIsConvertedToImperativeVerb(self):
        assert convert_tag_to_part_of_speech("VB") == PartOfSpeech.V_IMP.value


class EnglishLexerProvided(PartOfSpeechVerifier):
    @pytest.fixture
    def lexer(self):
        return WordnetProtoLexer()


class TestEnglishParserPrototype(EnglishLexerProvided):

    def test_canIdentifyCatAsNoun(self, lexer):
        lexer.input("cat")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyCatsAsNounAndDisregardPluralism(self, lexer):
        lexer.input("cats")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyBigAsAdjective(self, lexer):
        lexer.input("big")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(PartOfSpeech.ADJECTIVE, lexer)

    def test_canIdentifyCatHatAsTwoConsecutiveNouns(self, lexer):
        lexer.input("cat hat")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyBigManAsConsecutiveAdjectiveAndANoun(self, lexer):
        lexer.input("big man")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(PartOfSpeech.ADJECTIVE, lexer)
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyGoAsAnImperativeVerb(self, lexer):
        lexer.input("go")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(PartOfSpeech.V_IMP, lexer)


def extract_all_tokens(lexer):
    return [t for t in lexer]


class TestEnglishLexerPrototypeOnCommands(EnglishLexerProvided):

    def test_turningOnTheLights(self, lexer):
        lexer.input("turn on the lights")

        token_type_list = [t.types.keys() for t in (extract_all_tokens(lexer))]
        assert PartOfSpeech.V_IMP.value in token_type_list[0]
        assert PartOfSpeech.NOUN.value in token_type_list[-1]
