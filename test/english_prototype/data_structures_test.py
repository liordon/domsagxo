from concurrent.futures import thread
from time import sleep

import pytest

from compilation.definitions import PartOfSpeech
from english_prototype.data_structures import BeamToken, BeamTree, Token
from test_utils.providers import BeamTokensProvided


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
        return BeamTree.from_tokens_list([love_noun_or_verb_token] * 2)

    @pytest.fixture
    def empty_tree(self):
        return BeamTree([])

    def test_GivenMultipleTagsTheTreeRootTagIsNoneWithProbability1(self, love_noun_or_verb_token):
        single_love_tree = BeamTree.from_tokens_list([love_noun_or_verb_token])
        assert single_love_tree.token.tag is None
        assert single_love_tree.probability == 1

    def test_beamTreeHasSizeEqualToTokenTagsPlusOneForRoot(self, kite_noun_token, love_noun_or_verb_token):
        assert BeamTree.from_tokens_list([kite_noun_token]).tree_size() == 2
        assert BeamTree.from_tokens_list([love_noun_or_verb_token]).tree_size() == 3

    def test_beamTreeOf2PossibleTagsEachTimeHasAsManyNodesAsBinaryTree(self, love_noun_or_verb_token):
        # number of nodes is 2^(lvl+1)-1 => 2^3-1 => 7
        assert BeamTree.from_tokens_list([love_noun_or_verb_token] * 2).tree_size() == 7

    def test_beamTreeHasSubTreeForEachBeamTokenInterpretationWithItsProbability(self, love_noun_or_verb_token):
        subtrees = BeamTree.from_tokens_list([love_noun_or_verb_token]).get_children()
        noun_location = 0 if subtrees[0].token.tag == PartOfSpeech.NOUN else 1
        assert len(subtrees) == 2
        assert subtrees[noun_location].probability == love_noun_or_verb_token.tags[PartOfSpeech.NOUN]
        assert subtrees[1 - noun_location].probability == love_noun_or_verb_token.tags[PartOfSpeech.V_IMP]

    def test_canReturnTreeDepthIfQueried(self, double_love_tree):
        assert double_love_tree.tree_depth() == 3

    def test_pruningDoesNotAffectTreeDepthIfQueried(self, double_love_tree):
        assert double_love_tree.prune([PartOfSpeech.NOUN]).tree_depth() == 3

    def test_loneRootHasDepthOf1(self, double_love_tree):
        assert BeamTree([]).tree_depth() == 1

    def test_canVerifyTreeIntegrity(self, double_love_tree):
        assert double_love_tree.verify_integrity()

    def test_integrityVerificationFailsWhenTreeHasDanglingBranch(self, double_love_tree):
        double_love_tree.children[0].children = []
        assert not double_love_tree.verify_integrity()

    def test_pruningWholeTreeReturnsRoot(self, kite_noun_token):
        assert BeamTree.from_tokens_list([kite_noun_token] * 2) \
                   .prune([PartOfSpeech.NOUN]).tree_size() == 1

    def test_canQueryNumberOfLeavesForAmountOfPossibleCombinations(self, double_love_tree):
        assert double_love_tree.number_of_leaves() == 4

    def test_canBePrunedGivenTagListSubtractingAsManyNodesAsNeeded(self, double_love_tree):
        pruned_tree = double_love_tree.prune([PartOfSpeech.NOUN])
        assert pruned_tree.tree_size() == 4
        assert pruned_tree.number_of_leaves() == 2

    def test_pruningOneSubtreeDoesNotAffectTheOtherSubtree(self, double_love_tree):
        pruned_tree = double_love_tree.prune([PartOfSpeech.NOUN, PartOfSpeech.NOUN])
        assert pruned_tree.tree_size() == 6
        assert pruned_tree.number_of_leaves() == 3

    def test_whenAllChildrenArePrunedSubtreeItselfIsPruned(self, double_love_tree):
        pruned_tree = double_love_tree.prune([PartOfSpeech.NOUN, PartOfSpeech.NOUN])
        pruned_tree = pruned_tree.prune([PartOfSpeech.NOUN, PartOfSpeech.V_IMP])
        assert pruned_tree.tree_size() == 4
        assert pruned_tree.number_of_leaves() == 2

    def test_whenDeepBranchIsPrunedParallelBranchesAreUnaffected(self, kite_noun_token, love_noun_or_verb_token):
        love_tree = BeamTree.from_tokens_list([love_noun_or_verb_token] + [kite_noun_token] * 2)
        pruned_tree = love_tree.prune([PartOfSpeech.NOUN] * 3)
        assert pruned_tree.tree_size() == 4
        assert pruned_tree.number_of_leaves() == 1

    def test_gettingInterpretationOfEmptyTreeYieldsEmptyList(self):
        empty_tree = BeamTree([])
        next_of_none = empty_tree.get_next_interpretation()
        assert next_of_none == []

    def test_gettingNextOfNoneReturnsAPossibleParsingForEachWord(self, double_love_tree):
        next_of_none = double_love_tree.get_next_interpretation(None)
        assert len(next_of_none) == 2
        assert next_of_none[0] == Token("love", PartOfSpeech.NOUN)
        assert next_of_none[1] == Token("love", PartOfSpeech.NOUN)

    def test_gettingNextOfFirstInterpretationChangesOnlyTheLastWord(self, double_love_tree):
        next_of_none = double_love_tree.get_next_interpretation(None)
        next_of_first = double_love_tree.get_next_interpretation(next_of_none)
        assert len(next_of_first) == 2
        assert next_of_first[0] == Token("love", PartOfSpeech.NOUN)
        assert next_of_first[1] == Token("love", PartOfSpeech.V_IMP)

    def test_gettingNextOfLastInterpretationReturnsNone(self, double_love_tree):
        last_interpretation = [
            Token("love", PartOfSpeech.V_IMP),
            Token("love", PartOfSpeech.V_IMP),
        ]
        next_of_last = double_love_tree.get_next_interpretation(last_interpretation)
        assert next_of_last is None

    def test_whenLastWordOptionsAreExhaustedNextInterpretationChangesPreviousWord(self, double_love_tree):
        last_interpretation = [
            Token("love", PartOfSpeech.NOUN),
            Token("love", PartOfSpeech.V_IMP),
        ]
        next_of_first = double_love_tree.get_next_interpretation(last_interpretation)
        assert len(next_of_first) == 2
        assert next_of_first[0] == Token("love", PartOfSpeech.V_IMP)
        assert next_of_first[1] == Token("love", PartOfSpeech.NOUN)

    def test_whenProvidingIncompleteInterpretationNextInterpretationReturnedIsComplete(self, double_love_tree):
        last_interpretation = [
            Token("love", PartOfSpeech.V_IMP),
        ]
        next_of_first = double_love_tree.get_next_interpretation(last_interpretation)
        assert len(next_of_first) == 2
        assert next_of_first[0] == Token("love", PartOfSpeech.V_IMP)
        assert next_of_first[1] == Token("love", PartOfSpeech.NOUN)

    def test_canQueryInterpretationAndReceiveLongestMatchingSubsequence(self, double_love_tree):
        sub_interpretation = double_love_tree.longest_legal_sub_interpretation([
            Token("love", PartOfSpeech.NOUN),
            Token("love", PartOfSpeech.PREPOSITION),
            Token("love", PartOfSpeech.OTHER),
        ])
        assert len(sub_interpretation) == 1
        assert sub_interpretation[0] == Token("love", PartOfSpeech.NOUN)

    def test_emptyInterpretationIsValidEvenForEmptyTree(self, empty_tree):
        sub_interpretation = empty_tree.longest_legal_sub_interpretation([])
        assert sub_interpretation == []

    def test_completelyValidInterpretationIsReturnedFully(self, double_love_tree):
        interpretation = [
            Token("love", PartOfSpeech.NOUN),
            Token("love", PartOfSpeech.NOUN),
        ]
        sub_interpretation = double_love_tree.longest_legal_sub_interpretation(interpretation)
        assert sub_interpretation == interpretation

    @pytest.mark.timeout(timeout=1)
    def test_canRapidlyConstructHugeBeamTrees(self, love_noun_or_verb_token):
        BeamTree.from_tokens_list([love_noun_or_verb_token] * 20)
