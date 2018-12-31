from compilation.esp_lexer import PartOfSpeech, UnalphabeticTerminal, ReservedWord
from test_utils.providers import LexerProvided


class TestPartsOfSpeech(LexerProvided):

    def test_adjectivesAreNotCategorizedAsWords(self, lexer):
        lexer.input("blanka")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(token, PartOfSpeech.ADJECTIVE)
        assert token.value == "blanka"

    def test_numberWithAdjectiveEndingIsNumerator(self, lexer):
        lexer.input("unua")
        token = lexer.token()
        assert PartOfSpeech.NUMERATOR.value == token.type

    def test_ImperativeVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presu")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(token, PartOfSpeech.V_IMP)
        assert token.value == "presu"

    def test_PresentVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presas")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(token, PartOfSpeech.V_PRES)
        assert token.value == "presas"

    def test_InfinitiveVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presi")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(token, PartOfSpeech.V_INF)
        assert token.value == "presi"

    def test_accusativeNounsAreEvaluatedWithoutTheAccusativeCase(self, lexer):
        lexer.input("muson musojn")
        assert lexer.token().value == 'muso'
        assert lexer.token().value == 'musoj'


class TestReservedWords(LexerProvided):

    def test_theWordAsignuIsRecognizedForAssignment(self, lexer):
        lexer.input("asignu")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.PUT)

    def test_theWordEstasIsEquivalentToEqualsSign(self, lexer):
        lexer.input("=")
        self.assertPartOfSpeechForNextToken(lexer, UnalphabeticTerminal.ASSIGN)

    def test_kajIsAReservedWordAndNotAnAdjective(self, lexer):
        lexer.input("kaj")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.AND)

    def test_kunIsAPrepositionAndNotAnAccusativeImperativeVerb(self, lexer):
        lexer.input("kun")
        self.assertPartOfSpeechForNextToken(lexer, PartOfSpeech.PREPOSITION)

    def test_reservedWordLa(self, lexer):
        lexer.input("la")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.THE)

    def test_numericalTokensHaveIntValuesInsteadOfStrings(self, lexer):
        lexer.input("42")
        assert 42 == lexer.token().value

    def test_cantParseIllegalDigitCombination(self, lexer):
        lexer.input("dudu")
        assert ReservedWord.VERBAL_DIGIT.value != lexer.token().type

    def test_booleanReservedWords_TrueFalse(self, lexer):
        lexer.input("vero")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TRUE)
        lexer.input("malvero")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.FALSE)

    def test_words_moreLessTimesParts_areReservedForMath(self, lexer):
        lexer.input("pli")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.MORE)
        lexer.input("malpli")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.LESS)
        lexer.input("fojoj")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TIMES)
        lexer.input("partoj")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.PARTS)

    def test_timeUnitsAreRecognizedTimeIndicationsAndNotNouns(self, lexer):
        lexer.input("jaro")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TIME_INDICATION)
        lexer.input("monato")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TIME_INDICATION)
        lexer.input("semajno")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TIME_INDICATION)
        lexer.input("tago")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TIME_INDICATION)
        lexer.input("horo")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TIME_INDICATION)
        lexer.input("minutoj")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TIME_INDICATION)
        lexer.input("sekundoj")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TIME_INDICATION)

    def test_theWords_IfThenAndElse_areReservedForConditionalStatements(self, lexer):
        lexer.input("se")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.IF)
        lexer.input("tiam")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.THEN)
        lexer.input("alie")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.ELSE)

    def test_theWord_While_isReservedForLoopStatements(self, lexer):
        lexer.input("dum")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.DURING)

    def test_theWords_ThenToMoreGreatSmallOrNot_areReservedForComparisons(self, lexer):
        lexer.input("ne")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.NOT)
        lexer.input("al")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TO)
        lexer.input("ol")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.THAN)
        lexer.input("pli")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.MORE)
        lexer.input("aux")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.OR)
        lexer.input("egala")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.EQUAL)
        lexer.input("granda")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.GREATER)
        lexer.input("malgranda")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.SMALLER)

    def test_theWord_Both_isReservedForLogicOperations(self, lexer):
        lexer.input("ambaux")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.BOTH)

    def test_theWords_AtAfterEvery_areReservedForScheduling(self, lexer):
        lexer.input("je")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.AT)
        lexer.input("post")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.AFTER)
        lexer.input("cxiu")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.EVERY)

    def test_theWords_AndThenAtTheSameTime_areReservedForChainingCommands(self, lexer):
        lexer.input("poste")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.AND_THEN)
        lexer.input("samtempe")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.SIMULTANEOUSLY)


class TestStrings(LexerProvided):
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
        assert "kato" == token.value

    def test_literalQuotationMarksCannotHavePrefixesOrSuffixes(self, lexer):
        lexer.input("maldekstra citiloj dekstra citilo")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type
        lexer.input("maldekstra citilo dekstra citiloj")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type
        lexer.input("maldekstra citilo maldekstra citilo")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type
        lexer.input("malmaldekstra citilo dekstra citilo")
        assert UnalphabeticTerminal.STRING.value != lexer.token().type


class TestMultipleTokenSequences(LexerProvided):

    @staticmethod
    def getTokenList(lexer):
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
        assert 3 == len(self.getTokenList(lexer))
