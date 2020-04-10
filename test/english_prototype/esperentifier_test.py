import pytest
from english_prototype.esperantifier import *
from compilation.definitions import PartOfSpeech, ReservedWord, UnalphabeticTerminal
from test_utils.providers import StatementLevelAstProvided, evaluate_and_return_state_variables, \
    MockSmartHomeStateVariablesProvided


class TestWordEsperantification(object):

    @pytest.fixture
    def word(self):
        return "cat"

    def test_experantifyingANounAppendsOsoAtTheEnd(self, word):
        assert esperantify_word(word, PartOfSpeech.NOUN) == word + "oso"

    def test_experantifyingAnAdjectiveAppendsOsaAtTheEnd(self, word):
        assert esperantify_word(word, PartOfSpeech.ADJECTIVE) == word + "osa"

    def test_experantifyingAnInfinitiveVerbAppendsOsiAtTheEnd(self, word):
        assert esperantify_word(word, PartOfSpeech.V_INF) == word + "osi"

    def test_experantifyingAPresentVerbAppendsOsasAtTheEnd(self, word):
        assert esperantify_word(word, PartOfSpeech.V_PRES) == word + "osas"

    def test_experantifyingAnImperativeVerbAppendsOsuAtTheEnd(self, word):
        assert esperantify_word(word, PartOfSpeech.V_IMP) == word + "osu"

    def test_esperantifyingAKeywordTypeReturnsTheKeywordRegardlessOfOriginalValue(self, word):
        for reserved_word in ReservedWord:
            assert esperantify_word(word, reserved_word) == reserved_word.value[1:]

    def test_esperantifyingANumberTypeReturnsTheNumberIntact(self, word):
        for number in range(50):
            assert esperantify_word(str(number), UnalphabeticTerminal.NUMBER) == str(number)

    def test_esperantifyingAnUnalphabeticTerminalTypeReturnsTheTerminalRegardlessOfOriginalValue(self, word):
        assert esperantify_word(word, UnalphabeticTerminal.R_PAREN) == ")"
        assert esperantify_word(word, UnalphabeticTerminal.L_PAREN) == "("
        assert esperantify_word(word, UnalphabeticTerminal.PLUS) == "+"
        assert esperantify_word(word, UnalphabeticTerminal.MINUS) == "-"
        assert esperantify_word(word, UnalphabeticTerminal.TIMES) == "*"
        assert esperantify_word(word, UnalphabeticTerminal.DIVIDE) == "/"


def esperantify_tuples(sentence_tuples):
    return " ".join([esperantify_word(word, pos) for word, pos in sentence_tuples])


class TestEsperantificationOnDomsagxoParserGivenPerfectPreparsing(StatementLevelAstProvided,
    MockSmartHomeStateVariablesProvided):
    def test_canAssignToVariable(self, ast, state):
        sentence_tuples = [
            ("assign", ReservedWord.PUT),
            ("7", UnalphabeticTerminal.NUMBER),
            ("to", ReservedWord.TO),
            ("small", PartOfSpeech.ADJECTIVE),
            ("cat", PartOfSpeech.NOUN)]
        new_state = evaluate_and_return_state_variables(ast,
            esperantify_tuples(sentence_tuples), state)
        assert 7 == new_state[esperantify_tuples(sentence_tuples[-2:])]

    def test_canUseIfConditions(self, ast, state):
        sentence_tuples = [
            ("if", ReservedWord.IF),
            ("false", ReservedWord.FALSE),
            ("then", ReservedWord.THEN),
            ("assign", ReservedWord.PUT),
            ("7", UnalphabeticTerminal.NUMBER),
            ("to", ReservedWord.TO),
            ("cat", PartOfSpeech.NOUN),
            ("end", ReservedWord.END),
        ]
        assert esperantify_word("cat", PartOfSpeech.NOUN) not in evaluate_and_return_state_variables(ast,
            esperantify_tuples(sentence_tuples), state)

    def test_canUseIfElseStatements(self, ast, state):
        sentence_tuples = [
            ("if", ReservedWord.IF),
            ("false", ReservedWord.FALSE),
            ("then", ReservedWord.THEN),
            ("assign", ReservedWord.PUT),
            ("7", UnalphabeticTerminal.NUMBER),
            ("to", ReservedWord.TO),
            ("cat", PartOfSpeech.NOUN),
            ("else", ReservedWord.ELSE),
            ("assign", ReservedWord.PUT),
            ("9", UnalphabeticTerminal.NUMBER),
            ("to", ReservedWord.TO),
            ("cat", PartOfSpeech.NOUN),
            ("end", ReservedWord.END),
        ]
        new_state = evaluate_and_return_state_variables(ast,
            esperantify_tuples(sentence_tuples), state)
        assert 9 == new_state[esperantify_word("cat", PartOfSpeech.NOUN)]

    @pytest.mark.timeout(5)
    def test_canDefineWhileLoopThatEvaluatesFiveTimes(self, ast, state):
        sentence_tuples = [
            ("while", ReservedWord.DURING),
            ("cat", PartOfSpeech.NOUN),
            ("is", ReservedWord.IS),
            ("more", ReservedWord.MORE),
            ("grater", ReservedWord.GREATER),
            ("than", ReservedWord.THAN),
            ("0", UnalphabeticTerminal.NUMBER),
            ("then", ReservedWord.THEN),
            ("assign", ReservedWord.PUT),
            ("cat", PartOfSpeech.NOUN),
            ("minus", UnalphabeticTerminal.MINUS),
            ("1", UnalphabeticTerminal.NUMBER),
            ("to", ReservedWord.TO),
            ("cat", PartOfSpeech.NOUN),
            ("end", ReservedWord.END),
        ]
        state.variables[esperantify_word("cat", PartOfSpeech.NOUN)] = 5
        new_state = evaluate_and_return_state_variables(ast,
            esperantify_tuples(sentence_tuples), state)
        assert 0 == new_state[esperantify_word("cat", PartOfSpeech.NOUN)]

    def test_canTurnVariablesIntoOrdinalsViaChangeFromNounToAdjective(self, ast, state):
        sentence_tuples = [
            ("assign", ReservedWord.PUT),
            ("index", PartOfSpeech.ADJECTIVE),
            ("of", ReservedWord.OF),
            ("light", PartOfSpeech.ADJECTIVE),
            ("bulb", PartOfSpeech.NOUN),
            ("to", ReservedWord.TO),
            ("cat", PartOfSpeech.NOUN),
        ]
        state.variables[esperantify_word("index", PartOfSpeech.NOUN)] = 2
        state.variables[
            esperantify_tuples([
                ("light", PartOfSpeech.ADJECTIVE),
                ("bulb", PartOfSpeech.NOUN),
            ])
        ] = [1, 2, 3]
        new_state = evaluate_and_return_state_variables(ast,
            esperantify_tuples(sentence_tuples), state)
        assert 2 == new_state[esperantify_word("cat", PartOfSpeech.NOUN)]

    def test_canInvokeRoutineWithArguments(self, ast, state):
        # noinspection PyUnusedLocal
        def mock_routine(arg):
            pass

        sentence_tuples = [
            ("fubar", PartOfSpeech.V_IMP),
            ("42", UnalphabeticTerminal.NUMBER),
        ]
        state.method_dict[esperantify_word("fubar", PartOfSpeech.V_IMP)] = mock_routine
        # should execute without exception
        evaluate_and_return_state_variables(ast,
            esperantify_tuples(sentence_tuples), state)
