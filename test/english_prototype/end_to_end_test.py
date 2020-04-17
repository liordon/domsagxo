import pytest

from english_prototype import example_programs
from english_prototype.esperantifier import Esperantifier
from library.management_components import Domsagxo
from test_utils.providers import EnglishLexerProvided
import compilation.abstract_syntax_tree as ast


class TestEndToEndExamplePrograms(EnglishLexerProvided):
    @pytest.fixture
    def esperantifier(self):
        return Esperantifier(Domsagxo(), ast.build())

    def test_muFunction_constant(self, lexer, esperantifier):
        lexer.input(example_programs.mu_constant)
        tokenized_program = [t for t in lexer]
        legal_interpretations_tree = esperantifier.try_interpreting(tokenized_program)
        assert legal_interpretations_tree.number_of_leaves() > 0


    # def test_program_sillyNameExample(self, lexer, esperantifier):
    #     lexer.input(example_programs.silly_name_generator)
    #
    # def test_program_sequencedAnnouncements(self, lexer):
    #     lexer.input(example_programs.sequenced_announcements)
    #
    # def test_program_triggeredAnnouncements(self, lexer):
    #     lexer.input(example_programs.triggered_announcements)