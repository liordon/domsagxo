import pytest

from domsagxo.english_prototype.data_structures import BeamToken
from domsagxo.english_prototype.esperantifier import *
from domsagxo.english_prototype.esperantifier import esperantify_tuples
from test.test_utils.providers import StatementLevelAstProvided, evaluate_and_return_state_variables, \
    MockSmartHomeStateVariablesProvided, BeamTokensProvided


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

    def test_esperantifying2WordInfinitiveVerb(self):
        esperantified = esperantify_word("to constantify", PartOfSpeech.V_INF)
        assert "to" not in esperantified


class TestEsperantificationOnDomsagxoParserGivenPerfectPreparsing(StatementLevelAstProvided,
    MockSmartHomeStateVariablesProvided):
    def test_canAssignToVariable(self, ast, state):
        sentence_tuples = [
            ("assign", ReservedWord.PUT),
            ("7", UnalphabeticTerminal.NUMBER),
            ("to", ReservedWord.TO),
            ("small", PartOfSpeech.ADJECTIVE),
            ("cat", PartOfSpeech.NOUN),
        ]
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


class TestIntegrationOfEsperantifierAndBeamTree(BeamTokensProvided, StatementLevelAstProvided,
    MockSmartHomeStateVariablesProvided):

    @pytest.fixture
    def esperantifier(self, state, ast):
        return Esperantifier(state, ast)

    def test_returnsNoneForInvalidSentence(self, esperantifier, kite_noun_token):
        interpretations = esperantifier.try_interpreting([kite_noun_token])
        assert interpretations is None

    def test_returnsBeamTreeOfSize1For1ValidInterpretation(self, esperantifier):
        sentence_tokens = [
            BeamToken("assign", {ReservedWord.PUT: 1}),
            BeamToken("7", {UnalphabeticTerminal.NUMBER: 1}),
            BeamToken("to", {ReservedWord.TO: 1}),
            BeamToken("small", {PartOfSpeech.ADJECTIVE: 1}),
            BeamToken("cat", {PartOfSpeech.NOUN: 1}),
        ]

        interpretations = esperantifier.try_interpreting(sentence_tokens)
        assert isinstance(interpretations, BeamTree)
        assert interpretations.number_of_leaves() == 1

    def test_whenLastTokenHasInvalidInterpretationItIsRemoved(self, esperantifier):
        sentence_tokens = [
            BeamToken("assign", {ReservedWord.PUT: 1}),
            BeamToken("7", {UnalphabeticTerminal.NUMBER: 1}),
            BeamToken("to", {ReservedWord.TO: 1}),
            BeamToken("small", {PartOfSpeech.ADJECTIVE: 1}),
            BeamToken("cat", {PartOfSpeech.V_IMP: 0.5, PartOfSpeech.NOUN: 0.5}),
        ]

        interpretations = esperantifier.try_interpreting(sentence_tokens)
        remaining_interpretation = interpretations.get_next_interpretation()
        assert isinstance(interpretations, BeamTree)
        assert interpretations.number_of_leaves() == 1
        assert len(remaining_interpretation) == len(sentence_tokens)
        assert remaining_interpretation[4].tag == PartOfSpeech.NOUN

    def test_whenMidwayNodeHasInvalidInterpretationsItIsPruned(self, esperantifier):
        sentence_tokens = [
            BeamToken("assign", {ReservedWord.PUT: 1}),
            BeamToken("7", {UnalphabeticTerminal.NUMBER: 1}),
            BeamToken("to", {ReservedWord.TO: 1, PartOfSpeech.V_IMP: 0.5}),
            BeamToken("small", {PartOfSpeech.ADJECTIVE: 1}),
            BeamToken("cat", {PartOfSpeech.NOUN: 0.5}),
        ]

        interpretations = esperantifier.try_interpreting(sentence_tokens)
        remaining_interpretation = interpretations.get_next_interpretation()
        assert isinstance(interpretations, BeamTree)
        assert interpretations.number_of_leaves() == 1
        assert len(remaining_interpretation) == len(sentence_tokens)
        assert remaining_interpretation[2].tag == ReservedWord.TO

    def test_whenWrongInterpretationIsAfter1RuleHasBeenFoldedPruningActionDoesNotMiss(self, esperantifier):
        sentence_tokens = [
            BeamToken("assign", {ReservedWord.PUT: 1}),
            BeamToken("40", {UnalphabeticTerminal.NUMBER: 1}),
            BeamToken("+", {UnalphabeticTerminal.PLUS: 1}),
            BeamToken("1", {UnalphabeticTerminal.NUMBER: 1}),
            BeamToken("+", {UnalphabeticTerminal.PLUS: 1}),
            BeamToken("1", {UnalphabeticTerminal.NUMBER: 1}),
            BeamToken("to", {ReservedWord.TO: 1, PartOfSpeech.V_IMP: 0.5}),
            BeamToken("small", {PartOfSpeech.ADJECTIVE: 1}),
            BeamToken("cat", {PartOfSpeech.NOUN: 0.5}),
        ]

        interpretations = esperantifier.try_interpreting(sentence_tokens)
        remaining_interpretation = interpretations.get_next_interpretation()
        assert isinstance(interpretations, BeamTree)
        assert interpretations.number_of_leaves() == 1
        assert len(remaining_interpretation) == len(sentence_tokens)
        assert remaining_interpretation[6].tag == ReservedWord.TO

    def test_pruningOneBranchDoesNotAffectOtherSubtrees(self, esperantifier):
        """The bug that this checks: pruning [PUT, ADJECTIVE, TO] has pruned TO
        from the [PUT,NOUN] branch as well, resulting in:
        └- BeamTree- root (None: 1)
          └- BeamTreeNode- assign (ReservedWord.PUT: 1)
          ├- BeamTreeNode- love (PartOfSpeech.ADJECTIVE: 1) -- real mistake
          │ ├- BeamTreeNode- to (ReservedWord.TO: 1)
          │ │  └- BeamTreeNode- small (PartOfSpeech.ADJECTIVE: 1)
          │	│     └- BeamTreeNode- cat (PartOfSpeech.NOUN: 0.5)
          │	└- BeamTreeNode- to (PartOfSpeech.V_IMP: 0.5)
          │    └- BeamTreeNode- small (PartOfSpeech.ADJECTIVE: 1)
          │       └- BeamTreeNode- cat (PartOfSpeech.NOUN: 0.5)
          └- BeamTreeNode- love (PartOfSpeech.NOUN: 1)
            ├- BeamTreeNode- to (ReservedWord.TO: 1) -- pruned unnecessarily
            │  └- BeamTreeNode- small (PartOfSpeech.ADJECTIVE: 1)
            │    └- BeamTreeNode- cat (PartOfSpeech.NOUN: 0.5)
            └- BeamTreeNode- to (PartOfSpeech.V_IMP: 0.5)
              └- BeamTreeNode- small (PartOfSpeech.ADJECTIVE: 1)
                └- BeamTreeNode- cat (PartOfSpeech.NOUN: 0.5)"""
        sentence_tokens = [
            BeamToken("assign", {ReservedWord.PUT: 1}),
            BeamToken("love", {PartOfSpeech.ADJECTIVE: 1, PartOfSpeech.NOUN: 1}),
            BeamToken("to", {ReservedWord.TO: 1, PartOfSpeech.V_IMP: 0.5}),
            BeamToken("small", {PartOfSpeech.ADJECTIVE: 1}),
            BeamToken("cat", {PartOfSpeech.NOUN: 0.5}),
        ]

        interpretations = esperantifier.try_interpreting(sentence_tokens)
        remaining_interpretation = interpretations.get_next_interpretation()
        assert isinstance(interpretations, BeamTree)
        assert interpretations.number_of_leaves() == 1
        assert len(remaining_interpretation) == len(sentence_tokens)
        assert remaining_interpretation[2].tag == ReservedWord.TO
