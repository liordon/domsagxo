import pytest

from english_prototype.english_lexer import *
from test_utils.providers import PartOfSpeechVerifier, BeamTokensProvided


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


class TestBeamTokenPrettyPrint(BeamTokensProvided):
    def test_aSingleBeamTokenIsPrintedWithItsPossibleTagOnTheFirstLine(self, kite_noun_token):
        token_format = kite_noun_token.pretty_format()
        format_lines = token_format.splitlines()
        assert format_lines[0] == kite_noun_token.tags.__repr__()

    def test_aSingleBeamTokenIsPrintedWithItsValuesInTheLastLine(self, kite_noun_token):
        token_format = kite_noun_token.pretty_format()
        format_lines = token_format.splitlines()
        assert kite_noun_token.value in format_lines[-1]

    def test_allLinesOfPrettyPrintedTokenAreOfSameLength(self, kite_noun_token):
        token_format = kite_noun_token.pretty_format()
        format_lines = token_format.splitlines()
        print(format_lines)
        assert len(format_lines[0]) == len(format_lines[1])

    def test_tokenValueLineIsPaddedWithSpacesOnBothSides(self, kite_noun_token):
        token_format = kite_noun_token.pretty_format()
        format_lines = token_format.splitlines()
        expected_number_of_pads = len(format_lines[0]) - len(kite_noun_token.value)

        assert format_lines[1].startswith(' ' * int(expected_number_of_pads / 2))
        assert format_lines[1].endswith(' ' * int(expected_number_of_pads / 2))

    def test_tokenWithMultipleTagsHasOneLineForEachTagAndOneForValue(self, love_noun_or_verb_token):
        token_format = love_noun_or_verb_token.pretty_format()
        format_lines = token_format.splitlines()
        assert len(format_lines) == len(love_noun_or_verb_token.tags) + 1

    def test_tokenWithMultipleTagsHasAllLinesWithSameLength(self, love_noun_or_verb_token):
        token_format = love_noun_or_verb_token.pretty_format()
        format_lines = token_format.splitlines()
        for line in format_lines:
            assert len(line) == len(format_lines[0])

    def test_listOfMultipleBeamTokensIsPrintedWithAsMuchLinesAsTheMostPossibleTagsPlusOne(self, kite_noun_token,
            love_noun_or_verb_token):
        token_list = [kite_noun_token, love_noun_or_verb_token]
        list_format = BeamToken.list_pretty_format(token_list)
        assert len(list_format.splitlines()) == len(love_noun_or_verb_token.tags) + 1


class TestBeamTree(BeamTokensProvided):
    @pytest.fixture
    def double_love_tree(self, love_noun_or_verb_token):
        return BeamTree([love_noun_or_verb_token] * 2)

    def test_GivenSingleTagTheTreeRootValueAndTagAndProbabilityAreEqualToTag(self, kite_noun_token):
        assert BeamTree([kite_noun_token]).token.value == kite_noun_token.value
        assert BeamTree([kite_noun_token]).token.tag == "noun"
        assert BeamTree([kite_noun_token]).probability == 1

    def test_GivenMultipleTagsTheTreeRootValueIsEmptyStringWithProbability1(self, love_noun_or_verb_token):
        assert BeamTree([love_noun_or_verb_token]).token.value == ""
        assert BeamTree([love_noun_or_verb_token]).probability == 1

    def test_beamTreeOfSingleTokenHasSizeEqualToTokenTagsPlusOneForRoot(self, kite_noun_token, love_noun_or_verb_token):
        assert BeamTree([kite_noun_token]).size() == 1
        assert BeamTree([love_noun_or_verb_token]).size() == 3

    def test_beamTreeOf2PossibleTagsEachTimeHasAsManyNodesAsBinaryTree(self, love_noun_or_verb_token):
        # number of nodes is 2^(lvl+1)-1 => 2^3-1 => 7
        assert BeamTree([love_noun_or_verb_token] * 2).size() == 7

    def test_beamTreeHasSubTreeForEachBeamTokenInterpretationWithItsProbability(self, love_noun_or_verb_token):
        subtrees = BeamTree([love_noun_or_verb_token]).get_children()
        noun_location = 0 if subtrees[0].token.tag == "noun" else 1
        assert len(subtrees) == 2
        assert subtrees[noun_location].probability == love_noun_or_verb_token.tags["noun"]
        assert subtrees[1 - noun_location].probability == love_noun_or_verb_token.tags["verb"]

    def test_pruningWholeTreeReturnsNone(self, kite_noun_token):
        assert BeamTree([kite_noun_token] * 2).prune(["noun"]) is None

    def test_canBePrunedGivenTagListSubtractingAsManyNodesAsNeeded(self, double_love_tree):
        assert double_love_tree.prune([None, "noun"]).size() == 4

    def test_gettingNextOfNoneReturnsAPossibleParsingForEachWord(self, double_love_tree):
        next_of_none = double_love_tree.get_next_interpretation(None)
        assert len(next_of_none) == 3
        assert next_of_none[0] == Token('', None)
        assert next_of_none[1] == Token("love", "noun")
        assert next_of_none[2] == Token("love", "noun")

    def test_gettingNextOfFirstInterpretationChangesOnlyTheLastWord(self, double_love_tree):
        next_of_none = double_love_tree.get_next_interpretation(None)
        next_of_first = double_love_tree.get_next_interpretation(next_of_none)
        assert len(next_of_first) == 3
        assert next_of_first[0] == Token('', None)
        assert next_of_first[1] == Token("love", "noun")
        assert next_of_first[2] == Token("love", "verb")

    def test_gettingNextOfLastInterpretationReturnsNone(self, double_love_tree):
        last_interpretation = [
            Token('', None),
            Token("love", "verb"),
            Token("love", "verb"),
        ]
        next_of_last = double_love_tree.get_next_interpretation(last_interpretation)
        assert next_of_last is None

    def test_providingUnrelatedInterpretationProducesKeyError(self, double_love_tree):
        fake_interpretation = [Token("love", "nounjunctive")]
        with pytest.raises(KeyError):
            double_love_tree.get_next_interpretation(fake_interpretation)

    def test_whenLastWordOptionsAreExhaustedNextInterpretationChangesPreviousWord(self, double_love_tree):
        last_interpretation = [
            Token('', None),
            Token("love", "noun"),
            Token("love", "verb"),
        ]
        next_of_first = double_love_tree.get_next_interpretation(last_interpretation)
        assert len(next_of_first) == 3
        assert next_of_first[0] == Token('', None)
        assert next_of_first[1] == Token("love", "verb")
        assert next_of_first[2] == Token("love", "noun")
