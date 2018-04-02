import pytest

import kompilajxo.leksisto as lxr
from kompilajxo.leksisto import PartOfSpeech, UnalphabeticTerminal, ReservedWord


class LexerProvided(object):
    @pytest.fixture
    def lexer(self):
        return lxr.build()


class TestPartsOfSpeech(LexerProvided):

    @staticmethod
    def assertPartOfSpeech(token, partOfSpeech):
        assert partOfSpeech.value == token.type

    def test_adjectivesAreNotCategorizedAsWords(self, lexer):
        lexer.input("blanka")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.ADJECTIVE)
        assert token.value == "blanka"

    def test_ImperativeVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presu")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.V_IMP)
        assert token.value == "presu"

    def test_PresentVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presas")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.V_PRES)
        assert token.value == "presas"

    def test_InfinitiveVerbsAreNotCategorizedAsWords(self, lexer):
        lexer.input("presi")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.V_INF)
        assert token.value == "presi"

    def test_accusativeNounsAreEvaluatedWithoutTheAccusativeCase(self, lexer):
        lexer.input("muson musojn")
        assert lexer.token().value == 'muso'
        assert lexer.token().value == 'musoj'


class TestReservedWords(LexerProvided):

    @staticmethod
    def assertPartOfSpeech(token, partOfSpeech):
        assert partOfSpeech.value == token.type

    def test_theWordEstasIsEquivalentToEqualsSign(self, lexer):
        estasAssignment = "estas"
        equalsAssignment = "="
        lexer.input(estasAssignment)
        self.assertPartOfSpeech(lexer.token(), UnalphabeticTerminal.ASSIGN)
        lexer.input(equalsAssignment)
        self.assertPartOfSpeech(lexer.token(), UnalphabeticTerminal.ASSIGN)

    def test_kajIsAReservedWordAndNotAnAdjective(self, lexer):
        lexer.input("kaj")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.KAJ)

    def test_kunIsADelimAndNotAnAccusativeImperativeVerb(self, lexer):
        lexer.input("kun")
        self.assertPartOfSpeech(lexer.token(), UnalphabeticTerminal.DELIM)

    def test_numericalTokensHaveIntValuesInsteadOfStrings(self, lexer):
        lexer.input("42")
        assert 42 == lexer.token().value

    def test_reservedWordTrue(self, lexer):
        lexer.input("vero")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TRUE)

    def test_reservedWordFalse(self, lexer):
        lexer.input("malvero")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.FALSE)

    def test_reservedWordLa(self, lexer):
        lexer.input("la")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.LA)

    def test_YearIsTimeIndicationAndNotNoun(self, lexer):
        lexer.input("jaro")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TIME_INDICATION)

    def test_MonthIsTimeIndicationAndNotNoun(self, lexer):
        lexer.input("monato")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TIME_INDICATION)

    def test_WeekIsTimeIndicationAndNotNoun(self, lexer):
        lexer.input("semajno")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TIME_INDICATION)

    def test_DayIsTimeIndicationAndNotNoun(self, lexer):
        lexer.input("tago")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TIME_INDICATION)

    def test_HourIsTimeIndicationAndNotNoun(self, lexer):
        lexer.input("horo")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TIME_INDICATION)

    def test_MinutesAreTimeIndicationAndNotNoun(self, lexer):
        lexer.input("minutoj")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TIME_INDICATION)

    def test_secondsAreTimeIndicationAndNotNoun(self, lexer):
        lexer.input("sekundoj")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TIME_INDICATION)

    def test_theWordsIfThenAndElseAreReservedForConditionalStatements(self, lexer):
        lexer.input("se")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.IF)
        lexer.input("tiam")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.TIAM)
        lexer.input("alie")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.ALIE)

    def test_theWordWhileIsReservedForLoopStatements(self, lexer):
        lexer.input("dum")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.DUM)

    def test_theWords_ThenToMoreGreatSmallOrNot_AreReservedForComparisons(self, lexer):
        lexer.input("ne")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.NE)
        lexer.input("al")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.AL)
        lexer.input("ol")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.OL)
        lexer.input("pli")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.PLI)
        lexer.input("aux")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.OR)
        lexer.input("egala")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.EGALA)
        lexer.input("granda")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.GRANDA)
        lexer.input("malgranda")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.MALGRANDA)


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
        tokens = self.getTokenList(lexer)
        print(tokens)


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
