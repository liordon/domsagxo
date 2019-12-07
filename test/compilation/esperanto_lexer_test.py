import pytest

from compilation.definitions import PartOfSpeech, UnalphabeticTerminal, ReservedWord
from test_utils.providers import EsperantoLexerProvided


class TestPartsOfSpeech(EsperantoLexerProvided):

    def test_aTerminatedWordsAreAdjectives(self, lexer):
        lexer.input("blanka")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(PartOfSpeech.ADJECTIVE, token)
        assert token.value == "blanka"

    def test_numberWithAdjectiveEndingIsOrdinal(self, lexer):
        lexer.input("unua")
        token = lexer.token()
        assert PartOfSpeech.ORDINAL.value == token.type

    def test_uTerminatedWordsAreImperativeVerbs(self, lexer):
        lexer.input("presu")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(PartOfSpeech.V_IMP, token)
        assert token.value == "presu"

    def test_asTerminatedWordsArePresentVerbs(self, lexer):
        lexer.input("presas")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(PartOfSpeech.V_PRES, token)
        assert token.value == "presas"

    def test_iTerminatedWordsAreInfinitiveVerbs(self, lexer):
        lexer.input("presi")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(PartOfSpeech.V_INF, token)
        assert token.value == "presi"

    def test_accusativeNounsAreEvaluatedWithoutTheAccusativeCase(self, lexer):
        lexer.input("muson musojn")
        assert lexer.token().value == 'muso'
        assert lexer.token().value == 'musoj'

    def test_accusativeAdjectivesAreEvaluatedWithoutTheAccusativeCase(self, lexer):
        lexer.input("belan belajn")
        assert lexer.token().value == 'bela'
        assert lexer.token().value == 'belaj'

    def test_doesNotAcceptAccusativeInAbnormalPlaces(self, lexer):
        self.assertTerminalNotMatchingPartOfSpeech(lexer, "sxambalulin", PartOfSpeech.V_INF)
        self.assertTerminalNotMatchingPartOfSpeech(lexer, "sxambalulasn", PartOfSpeech.V_PRES)
        self.assertTerminalNotMatchingPartOfSpeech(lexer, "sxambalulun", PartOfSpeech.V_IMP)
        self.assertTerminalNotMatchingPartOfSpeech(lexer, "unun", UnalphabeticTerminal.NUMBER)

    def assertTerminalNotMatchingPartOfSpeech(self, lexer, terminal, part_of_speech):
        lexer.input(terminal)
        with pytest.raises(Exception):
            assert lexer.token().type != part_of_speech.value


class TestReservedWords(EsperantoLexerProvided):

    def test_theWordAsignuIsRecognizedForAssignment(self, lexer):
        lexer.input("asignu")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.PUT, lexer)

    def test_theWordEstasIsEquivalentToEqualsSign(self, lexer):
        lexer.input("=")
        self.assertPartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.ASSIGN, lexer)

    def test_kajIsAReservedWordAndNotAnAdjective(self, lexer):
        lexer.input("kaj")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.AND, lexer)

    def test_kunIsAPrepositionAndNotAnAccusativeImperativeVerb(self, lexer):
        lexer.input("kun")
        self.assertPartOfSpeechForNextTokenOfLexer(PartOfSpeech.PREPOSITION, lexer)

    def test_reservedWordLa(self, lexer):
        lexer.input("la")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.THE, lexer)

    def test_numericalTokensHaveIntValuesInsteadOfStrings(self, lexer):
        lexer.input("42")
        assert 42 == lexer.token().value

    def test_cantParseIllegalDigitCombination(self, lexer):
        lexer.input("dudu")
        assert ReservedWord.VERBAL_DIGIT.value != lexer.token().type

    def test_booleanReservedWords_TrueFalse(self, lexer):
        lexer.input("vero")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TRUE, lexer)
        lexer.input("malvero")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.FALSE, lexer)

    def test_words_moreLessTimesParts_areReservedForMath(self, lexer):
        lexer.input("pli")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.MORE, lexer)
        lexer.input("malpli")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.LESS, lexer)
        lexer.input("fojoj")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TIMES, lexer)
        lexer.input("partoj")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.PARTS, lexer)

    def test_words_rightLeftParenthesis_areReservedForMath(self, lexer):
        lexer.input("(")
        self.assertPartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.L_PAREN, lexer)
        lexer.input(")")
        self.assertPartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.R_PAREN, lexer)
        lexer.input("krampo")
        self.assertPartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.L_PAREN, lexer)
        lexer.input("malkrampo")
        self.assertPartOfSpeechForNextTokenOfLexer(UnalphabeticTerminal.R_PAREN, lexer)

    def test_timeUnitsAreRecognizedTimeIndicationsAndNotNouns(self, lexer):
        lexer.input("jaro")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("monato")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("semajno")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("tago")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("horo")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("minutoj")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("sekundoj")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TIME_INDICATION, lexer)

    def test_theWords_IfThenAndElse_areReservedForConditionalStatements(self, lexer):
        lexer.input("se")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.IF, lexer)
        lexer.input("tiam")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.THEN, lexer)
        lexer.input("alie")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.ELSE, lexer)

    def test_theWord_While_isReservedForLoopStatements(self, lexer):
        lexer.input("dum")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.DURING, lexer)

    def test_theWords_ThenToMoreGreatSmallOrNot_areReservedForComparisons(self, lexer):
        lexer.input("ne")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.NOT, lexer)
        lexer.input("al")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.TO, lexer)
        lexer.input("ol")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.THAN, lexer)
        lexer.input("pli")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.MORE, lexer)
        lexer.input("aux")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.OR, lexer)
        lexer.input("egala")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.EQUAL, lexer)
        lexer.input("granda")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.GREATER, lexer)
        lexer.input("malgranda")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.SMALLER, lexer)

    def test_theWord_Both_isReservedForLogicOperations(self, lexer):
        lexer.input("ambaux")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.BOTH, lexer)

    def test_theWords_AtAfterEvery_areReservedForScheduling(self, lexer):
        lexer.input("je")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.AT, lexer)
        lexer.input("post")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.AFTER, lexer)
        lexer.input("cxiu")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.EVERY, lexer)

    def test_theWords_AndThenAtTheSameTime_areReservedForChainingCommands(self, lexer):
        lexer.input("poste")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.AND_THEN, lexer)
        lexer.input("samtempe")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.SIMULTANEOUSLY, lexer)

    def test_theWord_it_hasItsOwnType(self, lexer):
        lexer.input("gxi")
        self.assertPartOfSpeechForNextTokenOfLexer(ReservedWord.IT, lexer)

    def test_theWord_it_resultsInNominativeTokenEvenWhenAccusative(self, lexer):
        lexer.input("gxin")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(ReservedWord.IT, token)
        assert token.value == "gxi"

    def test_theWord_it_losesEsperantoLetterWhenLexed(self, lexer):
        lexer.input("ĝi")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(ReservedWord.IT, token)
        assert token.value == "gxi"

    def test_theWord_it_losesEsperantoLetterAndAccusativeCaseWhenLexed(self, lexer):
        lexer.input("ĝin")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(ReservedWord.IT, token)
        assert token.value == "gxi"


class TestStrings(EsperantoLexerProvided):
    def test_theWords_leftQuotationRightQuotation_createAStringToken(self, lexer):
        lexer.input("maldekstra citilo dekstra citilo")
        token = lexer.token()
        assert UnalphabeticTerminal.STRING.value == token.type
        assert "" == token.value

    def test_quotationMarks_createAStringToken(self, lexer):
        lexer.input("''")
        token = lexer.token()
        assert UnalphabeticTerminal.STRING.value == token.type
        assert "" == token.value

    def test_doubleQuotes_createAStringToken(self, lexer):
        lexer.input('""')
        token = lexer.token()
        assert UnalphabeticTerminal.STRING.value == token.type
        assert "" == token.value

    def test_stringContentIsNotParsedLexicallyAsCode(self, lexer):
        lexer.input("'kato'")
        token = lexer.token()
        assert "kato" == token.value
        assert PartOfSpeech.NOUN.value != token.type
        assert UnalphabeticTerminal.STRING.value == token.type

    def test_canDifferentiateTwoStringsInSameSpeech(self, lexer):
        lexer.input("maldekstra citilo dekstra citilo maldekstra citilo dekstra citilo")
        assert UnalphabeticTerminal.STRING.value == lexer.token().type
        assert UnalphabeticTerminal.STRING.value == lexer.token().type
        lexer.input("''''")

    def test_literalQuotationMarksDoNotLeaveLeadingOrTrailingSpaces(self, lexer):
        lexer.input("maldekstra citilo kato dekstra citilo")
        token = lexer.token()
        assert token.value == "kato"

    def test_canUseShortQuotationKeywords(self, lexer):
        lexer.input("citilo kato malcitilo")
        token = lexer.token()
        assert token.value == "kato"

    def test_cannotMixCitationTypesWhenDefiningAString(self, lexer):
        lexer.input("maldekstra citilo '")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type
        lexer.input("maldekstra citilo \"")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type
        # unmatched ' or " are not even legal in this grammar, so they have to be caught.
        # this is still considered valid as far as I am concerned, but I don't want to
        # expect it as a part of the test.
        from compilation.esperanto_lexer import SemanticError
        try:
            lexer.input("' dekstra citilo")
            assert UnalphabeticTerminal.STRING.value != lexer.token().type
        except SemanticError:
            pass
        try:
            lexer.input("\" dekstra citilo")
            assert UnalphabeticTerminal.STRING.value != lexer.token().type
        except SemanticError:
            pass

    def test_literalQuotationMarksCannotHavePrefixesOrSuffixes(self, lexer):
        lexer.input("maldekstra citiloj dekstra citilo")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type
        lexer.input("maldekstra citilo dekstra citiloj")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type
        lexer.input("maldekstra citilo maldekstra citilo")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type
        lexer.input("malmaldekstra citilo dekstra citilo")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type


class TestMultipleTokenSequences(EsperantoLexerProvided):

    @staticmethod
    def getTokenList(lexer):
        res = []
        tok = lexer.token()
        while tok:
            res += [tok]
            tok = lexer.token()
        return res

    def test_canParseSeveralTokensTogether(self, lexer):
        lexer.input("kato ludas kun hundo")
        assert 4 == len(self.getTokenList(lexer))

    def test_canParseSimpleAdditionExpression(self, lexer):
        lexer.input("1+1")
        assert 3 == len(self.getTokenList(lexer))
