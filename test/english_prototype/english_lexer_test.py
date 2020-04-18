from english_prototype.english_lexer import *
from test_utils.providers import EnglishLexerProvided


class TestBasicTokenConversionFromNlpToPos(object):

    def test_DtIsConvertedToThe(self):
        assert convert_tag_to_part_of_speech("DT") == ReservedWord.THE

    def test_InIsConvertedToPreposition(self):
        assert convert_tag_to_part_of_speech("IN") == PartOfSpeech.PREPOSITION

    def test_JjAndJjsAreConvertedToAdjective(self):
        assert convert_tag_to_part_of_speech("JJ") == PartOfSpeech.ADJECTIVE
        assert convert_tag_to_part_of_speech("JJS") == PartOfSpeech.ADJECTIVE

    def test_NnAndNnsAreConvertedToNoun(self):
        assert convert_tag_to_part_of_speech("NN") == PartOfSpeech.NOUN
        assert convert_tag_to_part_of_speech("NNS") == PartOfSpeech.NOUN

    def test_VbIsConvertedToImperativeVerb(self):
        assert convert_tag_to_part_of_speech("VB") == PartOfSpeech.V_IMP


class TestEnglishKeywordsRecognition(EnglishLexerProvided):
    def test_canIdentifyKeywordsForAssignment(self, lexer):
        lexer.input("assign")
        self.assert_possible_next_token(ReservedWord.PUT, lexer)
        lexer.input("to")
        self.assert_possible_next_token(ReservedWord.TO, lexer)

    def test_canIdentifyKeywordThe(self, lexer):
        lexer.input("the")
        self.assert_possible_next_token(ReservedWord.THE, lexer)

    def test_canIdentifyKeywordMeans(self, lexer):
        lexer.input("means")
        self.assert_possible_next_token(ReservedWord.THIS_WAY, lexer)

    def test_canIdentifyKeywordReturn(self, lexer):
        lexer.input("return")
        self.assert_possible_next_token(ReservedWord.RETURN, lexer)

    def test_canIdentifyKeywordEnd(self, lexer):
        lexer.input("End")
        self.assert_possible_next_token(ReservedWord.END, lexer)

    def test_canIdentifyKeywordsSeparatingCommands(self, lexer):
        lexer.input("Afterwards")
        self.assert_possible_next_token(ReservedWord.AND_THEN, lexer)
        lexer.input("Simultaneously")
        self.assert_possible_next_token(ReservedWord.SIMULTANEOUSLY, lexer)

    def test_canIdentifyKeywordAnd(self, lexer):
        lexer.input("And")
        self.assert_possible_next_token(ReservedWord.AND, lexer)

    def test_canIdentifyKeywordOf(self, lexer):
        lexer.input("Of")
        self.assert_possible_next_token(ReservedWord.OF, lexer)

    def test_canIdentifyKeywordsForIfStatement(self, lexer):
        lexer.input("If")
        self.assert_possible_next_token(ReservedWord.IF, lexer)
        lexer.input("Then")
        self.assert_possible_next_token(ReservedWord.THEN, lexer)
        lexer.input("Else")
        self.assert_possible_next_token(ReservedWord.ELSE, lexer)

    def test_canIdentifyNotKeyword(self, lexer):
        lexer.input("Not")
        self.assert_possible_next_token(ReservedWord.NOT, lexer)

    def test_canIdentifyKeywordsForTimedCommands(self, lexer):
        lexer.input("at")
        self.assert_possible_next_token(ReservedWord.AT, lexer)
        lexer.input("every")
        self.assert_possible_next_token(ReservedWord.EVERY, lexer)
        lexer.input("once")
        self.assert_possible_next_token(ReservedWord.ONCE, lexer)
        lexer.input("whenever")
        self.assert_possible_next_token(ReservedWord.WHENEVER, lexer)


class TestUnalphabeticTerminalRecognition(EnglishLexerProvided):
    def test_canIdentifyPlusSignAndWord(self, lexer):
        lexer.input("+")
        self.assert_possible_next_token(UnalphabeticTerminal.PLUS, lexer)

        lexer.input("plus")
        self.assert_possible_next_token(UnalphabeticTerminal.PLUS, lexer)

    def test_canIdentifyMinusSignAndWord(self, lexer):
        lexer.input("-")
        self.assert_possible_next_token(UnalphabeticTerminal.MINUS, lexer)

        lexer.input("minus")
        self.assert_possible_next_token(UnalphabeticTerminal.MINUS, lexer)

    def test_canIdentifyMultiplySignAndWord(self, lexer):
        lexer.input("*")
        self.assert_possible_next_token(UnalphabeticTerminal.TIMES, lexer)

        lexer.input("times")
        self.assert_possible_next_token(UnalphabeticTerminal.TIMES, lexer)

    def test_canIdentifyDivisionSignAndWord(self, lexer):
        lexer.input("/")
        self.assert_possible_next_token(UnalphabeticTerminal.DIVIDE, lexer)

        lexer.input("parts")
        self.assert_possible_next_token(UnalphabeticTerminal.DIVIDE, lexer)

    def test_words_rightLeftParenthesis_areReservedForMath(self, lexer):
        lexer.input("(")
        self.assert_possible_next_token(UnalphabeticTerminal.L_PAREN, lexer)

        lexer.input(")")
        self.assert_possible_next_token(UnalphabeticTerminal.R_PAREN, lexer)

    def test_canIdentifySizeRelations(self, lexer):
        lexer.input("<")
        self.assert_possible_next_token(UnalphabeticTerminal.LESSER_THAN, lexer)

        lexer.input(">")
        self.assert_possible_next_token(UnalphabeticTerminal.GREATER_THAN, lexer)

        lexer.input("<=")
        self.assert_possible_next_token(UnalphabeticTerminal.LESSER_EQUAL, lexer)

        lexer.input(">=")
        self.assert_possible_next_token(UnalphabeticTerminal.GREATER_EQUAL, lexer)

        lexer.input("≤")
        self.assert_possible_next_token(UnalphabeticTerminal.LESSER_EQUAL, lexer)

        lexer.input("≥")
        self.assert_possible_next_token(UnalphabeticTerminal.GREATER_EQUAL, lexer)

    def test_timeUnitsAreRecognizedTimeIndications(self, lexer):
        lexer.input("year")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("month")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("week")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("day")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("hour")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("minute")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("second")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)

    def test_pluralTimeUnitsAreRecognizedTimeIndications(self, lexer):
        lexer.input("years")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("months")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("weeks")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("days")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("hours")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("minutes")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("seconds")
        self.assert_possible_next_token(ReservedWord.TIME_INDICATION, lexer)

    def test_canRecognizePunctuationMarks(self, lexer):
        lexer.input(',')
        self.assert_possible_next_token(UnalphabeticTerminal.COLON, lexer)

    def test_irrelevantTokensAreNotKeptInLexer(self, lexer):
        lexer.input('.')
        assert not lexer.has_next()
        lexer.input(':')
        assert not lexer.has_next()


class TestEnglishParserPrototype(EnglishLexerProvided):

    def test_canIdentifyCatAsNoun(self, lexer):
        lexer.input("cat")
        self.assert_possible_next_token(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyCatsAsNounAndDisregardPluralism(self, lexer):
        lexer.input("cats")
        self.assert_possible_next_token(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyBigAsAdjective(self, lexer):
        lexer.input("big")
        self.assert_possible_next_token(PartOfSpeech.ADJECTIVE, lexer)

    def test_canIdentifyCatHatAsTwoConsecutiveNouns(self, lexer):
        lexer.input("cat hat")
        self.assert_possible_next_token(PartOfSpeech.NOUN, lexer)
        self.assert_possible_next_token(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyBigManAsConsecutiveAdjectiveAndANoun(self, lexer):
        lexer.input("big man")
        self.assert_possible_next_token(PartOfSpeech.ADJECTIVE, lexer)
        self.assert_possible_next_token(PartOfSpeech.NOUN, lexer)

    def test_canIdentifyGoAsAnImperativeVerb(self, lexer):
        lexer.input("go")
        self.assert_possible_next_token(PartOfSpeech.V_IMP, lexer)

    def test_canIdentifyRelationalOperators(self, lexer):
        lexer.input("is greater than")
        self.assert_possible_next_token(ReservedWord.IS, lexer)
        self.assert_possible_next_token(ReservedWord.GREATER, lexer)
        self.assert_possible_next_token(ReservedWord.THAN, lexer)

        lexer.input("is lesser or equal to")
        self.assert_possible_next_token(ReservedWord.IS, lexer)
        self.assert_possible_next_token(ReservedWord.SMALLER, lexer)
        self.assert_possible_next_token(ReservedWord.OR, lexer)
        self.assert_possible_next_token(ReservedWord.EQUAL, lexer)
        self.assert_possible_next_token(ReservedWord.TO, lexer)


class TestMultipleWordTokens(EnglishLexerProvided):
    def test_toDoSomethingIsParsedAsSingleInfinitiveVerb(self, lexer):
        infinitive_verb = "to love"
        lexer.input(infinitive_verb)
        token = lexer.token()
        assert PartOfSpeech.V_INF in token.tags.keys()
        assert token.value == infinitive_verb


def extract_all_tokens(lexer):
    return [t for t in lexer]


class TestEnglishLexerFindingCorrectTags(EnglishLexerProvided):

    def test_turningOnTheLights(self, lexer):
        lexer.input("activate the lights")

        token_type_list = [t.tags.keys() for t in (extract_all_tokens(lexer))]
        assert PartOfSpeech.V_IMP in token_type_list[0]
        assert ReservedWord.THE in token_type_list[1]
        assert PartOfSpeech.NOUN in token_type_list[2]

    def test_assigningValueToVariable(self, lexer):
        lexer.input("assign 3 to dog")

        token_type_list = [t.tags.keys() for t in (extract_all_tokens(lexer))]
        assert ReservedWord.PUT in token_type_list[0]
        assert UnalphabeticTerminal.NUMBER in token_type_list[1]
        assert ReservedWord.TO in token_type_list[2]
        assert PartOfSpeech.NOUN in token_type_list[3]

    def test_lockTheFrontDoor(self, lexer):
        lexer.input("lock the front door")

        token_type_list = [t.tags.keys() for t in (extract_all_tokens(lexer))]
        assert PartOfSpeech.V_IMP in token_type_list[0]
        assert ReservedWord.THE in token_type_list[1]
        assert PartOfSpeech.ADJECTIVE in token_type_list[2]
        assert PartOfSpeech.NOUN in token_type_list[3]
