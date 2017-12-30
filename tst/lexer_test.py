import pytest
import kompilajxo.lexer_builder as lxr
from kompilajxo.lexer_builder import PartOfSpeech, UnalphabeticTerminal, ReservedWord


class TestPartsOfSpeech(object):

    @pytest.fixture
    def lexer(self):
        return lxr.build()

    @staticmethod
    def assertPartOfSpeech(token, partOfSpeech):
        assert partOfSpeech.value == token.type

    def test_adjectivesAreNotCategorizedAsWords(self, lexer):
        lexer.input("granda")
        token = lexer.token()
        self.assertPartOfSpeech(token, PartOfSpeech.ADJECTIVE)
        assert token.value == "granda"

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

    # def test_InfinitiveVerbsAreNotCategorizedAsWords(self, lexer):
    #     lexer.input("presi")
    #     token = lexer.token()
    #     self.assertPartOfSpeech(token, PartOfSpeech.V_INF)
    #     assert token.value == "presi"

    def test_accusativeNounsAreEvaluatedWithoutTheAccusativeCase(self, lexer):
        lexer.input("muson musojn")
        assert lexer.token().value == 'muso'
        assert lexer.token().value == 'musoj'


class TestReservedWords(object):

    @pytest.fixture
    def lexer(self):
        return lxr.build()

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

    def test_reservedWordHoro(self, lexer):
        lexer.input("horo")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.HORO)

    def test_reservedWordMinutoj(self, lexer):
        lexer.input("minutoj")
        self.assertPartOfSpeech(lexer.token(), ReservedWord.MINUTOJ)


class TestMultipleTokenSequences(object):
    @pytest.fixture
    def lexer(self):
        return lxr.build()

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


class TestVerbalNumbers(object):
    @pytest.fixture
    def lexer(self):
        return lxr.build()

    @staticmethod
    def assertVerbalNumberValue(numericalValue, token):
        assert UnalphabeticTerminal.VERBAL_DIGIT.value == token.type
        assert numericalValue == token.value

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
        assert UnalphabeticTerminal.VERBAL_DIGIT.value != lexer.token().type

    def test_numberWithAdjectiveEndingIsNumerator(self, lexer):
        lexer.input("unua")
        assert PartOfSpeech.NUMERATOR.value == lexer.token().type
