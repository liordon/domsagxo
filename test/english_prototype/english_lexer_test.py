import pytest

from english_prototype.english_lexer import *
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


class TestEnglishKeywordsRecognition(EnglishLexerProvided):
    def test_canIdentifyKeywordAssign(self, lexer):
        lexer.input("assign")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.PUT, lexer)

    def test_canIdentifyKeywordTo(self, lexer):
        lexer.input("to")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TO, lexer)

    def test_canIdentifyKeywordThe(self, lexer):
        lexer.input("the")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.THE, lexer)


class TestUnalphabeticTerminalRecognition(EnglishLexerProvided):
    def test_canIdentifyPlusSignAndWord(self, lexer):
        lexer.input("+")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.PLUS, lexer)

        lexer.input("plus")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.PLUS, lexer)

    def test_canIdentifyMinusSignAndWord(self, lexer):
        lexer.input("-")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.MINUS, lexer)

        lexer.input("minus")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.MINUS, lexer)

    def test_canIdentifyMultiplySignAndWord(self, lexer):
        lexer.input("*")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.TIMES, lexer)

        lexer.input("times")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.TIMES, lexer)

    def test_canIdentifyDivisionSignAndWord(self, lexer):
        lexer.input("/")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.DIVIDE, lexer)

        lexer.input("parts")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.DIVIDE, lexer)

    def test_words_rightLeftParenthesis_areReservedForMath(self, lexer):
        lexer.input("(")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.L_PAREN, lexer)

        lexer.input(")")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.R_PAREN, lexer)

    def test_timeUnitsAreRecognizedTimeIndications(self, lexer):
        lexer.input("year")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("month")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("week")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("day")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("hour")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("minute")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("second")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)

    def test_pluralTimeUnitsAreRecognizedTimeIndications(self, lexer):
        lexer.input("years")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("months")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("weeks")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("days")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("hours")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("minutes")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("seconds")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)


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

    def test_canIdentifyRelationalOperators(self, lexer):
        lexer.input("is greater than")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.IS, lexer)
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.GREATER, lexer)
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.THAN, lexer)

        lexer.input("is lesser or equal to")
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.IS, lexer)
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.SMALLER, lexer)
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.OR, lexer)
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.EQUAL, lexer)
        self.assertOnePossiblePartOfSpeechForNextTokenOfLexer(ReservedWord.TO, lexer)


def extract_all_tokens(lexer):
    return [t for t in lexer]


class TestEnglishLexerFindingPossibleTags(EnglishLexerProvided):

    def test_turningOnTheLights(self, lexer):
        lexer.input("activate the lights")

        token_type_list = [t.tags.keys() for t in (extract_all_tokens(lexer))]
        assert PartOfSpeech.V_IMP.value in token_type_list[0]
        assert ReservedWord.THE.value in token_type_list[1]
        assert PartOfSpeech.NOUN.value in token_type_list[2]

    def test_assigningValueToVariable(self, lexer):
        lexer.input("assign 3 to dog")

        token_type_list = [t.tags.keys() for t in (extract_all_tokens(lexer))]
        assert ReservedWord.PUT.value in token_type_list[0]
        assert UnalphabeticTerminal.NUMBER.value in token_type_list[1]
        assert ReservedWord.TO.value in token_type_list[2]
        assert PartOfSpeech.NOUN.value in token_type_list[3]

    def test_lockTheFrontDoor(self, lexer):
        lexer.input("lock the front door")

        token_type_list = [t.tags.keys() for t in (extract_all_tokens(lexer))]
        assert PartOfSpeech.V_IMP.value in token_type_list[0]
        assert ReservedWord.THE.value in token_type_list[1]
        assert PartOfSpeech.ADJECTIVE.value in token_type_list[2]
        assert PartOfSpeech.NOUN.value in token_type_list[3]


