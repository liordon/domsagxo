import pytest

import compilation.esp_lexer as lxr
from compilation.esp_lexer import PartOfSpeech, UnalphabeticTerminal, ReservedWord


class LexerProvided(object):
    @pytest.fixture
    def lexer(self):
        return lxr.build()

    @staticmethod
    def assertPartOfSpeechForGivenToken(token, partOfSpeech):
        assert partOfSpeech.value == token.type

    @staticmethod
    def assertPartOfSpeechForNextToken(lexer, partOfSpeech):
        assert partOfSpeech.value == lexer.token().type


class TestPartsOfSpeech(LexerProvided):

    def test_adjectivesAreNotCategorizedAsWords(self, lexer):
        lexer.input("blanka")
        token = lexer.token()
        self.assertPartOfSpeechForGivenToken(token, PartOfSpeech.ADJECTIVE)
        assert token.value == "blanka"

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

    def test_theWordEstasIsEquivalentToEqualsSign(self, lexer):
        lexer.input("estas")
        self.assertPartOfSpeechForNextToken(lexer, UnalphabeticTerminal.ASSIGN)
        lexer.input("=")
        self.assertPartOfSpeechForNextToken(lexer, UnalphabeticTerminal.ASSIGN)

    def test_kajIsAReservedWordAndNotAnAdjective(self, lexer):
        lexer.input("kaj")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.AND)

    def test_kunIsADelimAndNotAnAccusativeImperativeVerb(self, lexer):
        lexer.input("kun")
        self.assertPartOfSpeechForNextToken(lexer, UnalphabeticTerminal.DELIM)

    def test_reservedWordLa(self, lexer):
        lexer.input("la")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.THE)

    def test_numericalTokensHaveIntValuesInsteadOfStrings(self, lexer):
        lexer.input("42")
        assert 42 == lexer.token().value

    def test_booleanReservedWords_TrueFalse(self, lexer):
        lexer.input("vero")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.TRUE)
        lexer.input("malvero")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.FALSE)

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

    def test_theWords_AtAfterEvery_areReservedForScheduling(self, lexer):
        lexer.input("je")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.AT)
        lexer.input("post")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.AFTER)
        lexer.input("cxiu")
        self.assertPartOfSpeechForNextToken(lexer, ReservedWord.EVERY)


class TestMultipleTokenSequences(LexerProvided):

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
        assert 3 == len(self.getTokenList(lexer))


class TestVerbalNumbers(LexerProvided):
    @staticmethod
    def assertVerbalNumberValue(numerical_value, token):
        assert ReservedWord.VERBAL_DIGIT.value == token.type
        assert numerical_value == token.value

    def test_canParseDigit0(self, lexer):
        lexer.input("nul")
        self.assertVerbalNumberValue(0, lexer.token())

    def test_canParseDigit1(self, lexer):
        lexer.input("unu")
        self.assertVerbalNumberValue(1, lexer.token())

    def test_canParseDigit2(self, lexer):
        lexer.input("du")
        self.assertVerbalNumberValue(2, lexer.token())

    def test_canParseDigit3(self, lexer):
        lexer.input("tri")
        self.assertVerbalNumberValue(3, lexer.token())

    def test_canParseDigit4(self, lexer):
        lexer.input("kvar")
        self.assertVerbalNumberValue(4, lexer.token())

    def test_canParseDigit5(self, lexer):
        lexer.input("kvin")
        self.assertVerbalNumberValue(5, lexer.token())

    def test_canParseDigit6(self, lexer):
        lexer.input("ses")
        self.assertVerbalNumberValue(6, lexer.token())

    def test_canParseDigit7(self, lexer):
        lexer.input("sep")
        self.assertVerbalNumberValue(7, lexer.token())

    def test_canParseDigit8(self, lexer):
        lexer.input("ok")
        self.assertVerbalNumberValue(8, lexer.token())

    def test_canParseDigit9(self, lexer):
        lexer.input("naux")
        self.assertVerbalNumberValue(9, lexer.token())

    def test_canParseNumber10(self, lexer):
        lexer.input("dek")
        self.assertVerbalNumberValue(10, lexer.token())

    def test_canParseNumber20(self, lexer):
        lexer.input("dudek")
        self.assertVerbalNumberValue(20, lexer.token())

    def test_canParseNumber100(self, lexer):
        lexer.input("cent")
        self.assertVerbalNumberValue(100, lexer.token())

    def test_canParseNumber248(self, lexer):
        lexer.input("ducent kvardek ok")
        self.assertVerbalNumberValue(200, lexer.token())
        self.assertVerbalNumberValue(40, lexer.token())
        self.assertVerbalNumberValue(8, lexer.token())

    def test_cantParseIllegalDigitCombination(self, lexer):
        lexer.input("dudu")
        assert ReservedWord.VERBAL_DIGIT.value != lexer.token().type

    def test_canParseFractionHalf(self, lexer):
        lexer.input("duono")
        self.assertVerbalNumberValue(1 / 2, lexer.token())

    def test_canParseFractionQuarter(self, lexer):
        lexer.input("kvarono")
        self.assertVerbalNumberValue(1 / 4, lexer.token())

    def test_numberWithAdjectiveEndingIsNumerator(self, lexer):
        lexer.input("unua")
        token = lexer.token()
        assert PartOfSpeech.NUMERATOR.value == token.type
        assert 1 == token.value
