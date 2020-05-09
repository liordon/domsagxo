import pytest

from domsagxo.compilation.definitions import PartOfSpeech, UnalphabeticTerminal, ReservedWord
from domsagxo_test.test_utils.providers import EsperantoLexerProvided


class TestPartsOfSpeech(EsperantoLexerProvided):

    def test_aTerminatedWordsAreAdjectives(self, lexer):
        lexer.input("blanka")
        token = lexer.token()
        self.assert_part_of_speech_for_token(PartOfSpeech.ADJECTIVE, token)
        assert token.value == "blanka"

    def test_numberWithAdjectiveEndingIsOrdinal(self, lexer):
        lexer.input("unua")
        token = lexer.token()
        self.assert_part_of_speech_for_token(PartOfSpeech.ORDINAL, token)

    def test_uTerminatedWordsAreImperativeVerbs(self, lexer):
        lexer.input("presu")
        token = lexer.token()
        self.assert_part_of_speech_for_token(PartOfSpeech.V_IMP, token)
        assert token.value == "presu"

    def test_asTerminatedWordsArePresentVerbs(self, lexer):
        lexer.input("presas")
        token = lexer.token()
        self.assert_part_of_speech_for_token(PartOfSpeech.V_PRES, token)
        assert token.value == "presas"

    def test_iTerminatedWordsAreInfinitiveVerbs(self, lexer):
        lexer.input("presi")
        token = lexer.token()
        self.assert_part_of_speech_for_token(PartOfSpeech.V_INF, token)
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
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.PUT, lexer)
        lexer.input("Asignu")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.PUT, lexer)

    def test_theWordEstasIsEquivalentToEqualsSign(self, lexer):
        lexer.input("=")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.ASSIGN, lexer)

    def test_kajIsAReservedWordAndNotAnAdjective(self, lexer):
        lexer.input("kaj")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.AND, lexer)
        lexer.input("Kaj")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.AND, lexer)

    def test_kunIsAPrepositionAndNotAnAccusativeImperativeVerb(self, lexer):
        lexer.input("kun")
        self.assert_part_of_speech_for_next_token_of_lexer(PartOfSpeech.PREPOSITION, lexer)

    def test_reservedWordLa(self, lexer):
        lexer.input("la")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.THE, lexer)
        lexer.input("La")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.THE, lexer)

    def test_numericalTokensHaveIntValuesInsteadOfStrings(self, lexer):
        lexer.input("42")
        assert 42 == lexer.token().value

    def test_cantParseIllegalDigitCombination(self, lexer):
        lexer.input("dudu")
        assert ReservedWord.VERBAL_DIGIT.value != lexer.token().type

    def test_booleanReservedWords_TrueFalse(self, lexer):
        lexer.input("vero")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TRUE, lexer)
        lexer.input("malvero")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.FALSE, lexer)
        lexer.input("Vero")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TRUE, lexer)
        lexer.input("Malvero")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.FALSE, lexer)

    def test_words_moreLessTimesParts_areReservedForMath(self, lexer):
        lexer.input("pli")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.MORE, lexer)
        lexer.input("malpli")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.LESS, lexer)
        lexer.input("fojoj")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIMES, lexer)
        lexer.input("partoj")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.PARTS, lexer)
        lexer.input("Pli")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.MORE, lexer)
        lexer.input("Malpli")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.LESS, lexer)
        lexer.input("Fojoj")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIMES, lexer)
        lexer.input("Partoj")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.PARTS, lexer)

    def test_words_rightLeftParenthesis_areReservedForMath(self, lexer):
        lexer.input("(")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.L_PAREN, lexer)
        lexer.input(")")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.R_PAREN, lexer)
        lexer.input("krampo")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.L_PAREN, lexer)
        lexer.input("malkrampo")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.R_PAREN, lexer)
        lexer.input("Krampo")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.L_PAREN, lexer)
        lexer.input("Malkrampo")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.R_PAREN, lexer)

    def test_timeUnitsAreRecognizedTimeIndicationsAndNotNouns(self, lexer):
        lexer.input("jaro")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("monato")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("semajno")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("tago")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("horo")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("minutoj")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIME_INDICATION, lexer)
        lexer.input("sekundoj")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TIME_INDICATION, lexer)

    def test_theWords_IfThenAndElse_areReservedForConditionalStatements(self, lexer):
        lexer.input("se")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.IF, lexer)
        lexer.input("tiam")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.THEN, lexer)
        lexer.input("alie")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.ELSE, lexer)
        lexer.input("Se")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.IF, lexer)
        lexer.input("Tiam")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.THEN, lexer)
        lexer.input("Alie")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.ELSE, lexer)

    def test_theWord_While_isReservedForLoopStatements(self, lexer):
        lexer.input("dum")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.DURING, lexer)
        lexer.input("Dum")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.DURING, lexer)

    def test_theWords_ThenToMoreGreatSmallOrNot_areReservedForComparisons(self, lexer):
        lexer.input("ne")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.NOT, lexer)
        lexer.input("al")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.TO, lexer)
        lexer.input("ol")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.THAN, lexer)
        lexer.input("pli")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.MORE, lexer)
        lexer.input("aux")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.OR, lexer)
        lexer.input("egala")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.EQUAL, lexer)
        lexer.input("granda")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.GREATER, lexer)
        lexer.input("malgranda")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.SMALLER, lexer)

    def test_theWord_Both_isReservedForLogicOperations(self, lexer):
        lexer.input("ambaux")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.BOTH, lexer)

    def test_theWords_AtAfterEvery_areReservedForScheduling(self, lexer):
        lexer.input("je")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.AT, lexer)
        lexer.input("post")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.AFTER, lexer)
        lexer.input("cxiu")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.EVERY, lexer)

    def test_theWords_AndThenAtTheSameTime_areReservedForChainingCommands(self, lexer):
        lexer.input("poste")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.AND_THEN, lexer)
        lexer.input("samtempe")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.SIMULTANEOUSLY, lexer)

    def test_theWord_it_hasItsOwnType(self, lexer):
        lexer.input("gxi")
        self.assert_part_of_speech_for_next_token_of_lexer(ReservedWord.IT, lexer)

    def test_theWord_it_resultsInNominativeTokenEvenWhenAccusative(self, lexer):
        lexer.input("gxin")
        token = lexer.token()
        self.assert_part_of_speech_for_token(ReservedWord.IT, token)
        assert token.value == "gxi"

    def test_theWord_it_losesEsperantoLetterWhenLexed(self, lexer):
        lexer.input("ĝi")
        token = lexer.token()
        self.assert_part_of_speech_for_token(ReservedWord.IT, token)
        assert token.value == "gxi"

    def test_theWord_it_losesEsperantoLetterAndAccusativeCaseWhenLexed(self, lexer):
        lexer.input("ĝin")
        token = lexer.token()
        self.assert_part_of_speech_for_token(ReservedWord.IT, token)
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
        from domsagxo.compilation.esperanto_lexer import SemanticError
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

    def test_canIdentifyStringAfterImperativeVerb(self, lexer):
        lexer.input("anoncu citilo sxambalulo malcitilo")
        _ = lexer.token()
        assert lexer.token().type == UnalphabeticTerminal.STRING.value

    def test_canIdentifyStringSpanningMultipleLines(self, lexer):
        lexer.input("""citilo sxambalulo
         kahxolo malcitilo""")
        assert lexer.token().type == UnalphabeticTerminal.STRING.value


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


class TestUnalphabeticSyntacticSugar(EsperantoLexerProvided):
    def test_relationOperatorsAreRecognizedByLexer(self, lexer):
        lexer.input(">")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.GREATER_THAN, lexer)

        lexer.input("<")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.LESSER_THAN, lexer)

        lexer.input(">=")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.GREATER_EQUAL, lexer)

        lexer.input("<=")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.LESSER_EQUAL, lexer)

        lexer.input("≥")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.GREATER_EQUAL, lexer)

        lexer.input("≤")
        self.assert_part_of_speech_for_next_token_of_lexer(UnalphabeticTerminal.LESSER_EQUAL, lexer)
