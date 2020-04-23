import pytest

from demo.example_programs import *
from english_prototype.esperantifier import Esperantifier
from library.management_components import Domsagxo
from test_utils.providers import EnglishLexerProvided
import compilation.abstract_syntax_tree as ast
import compilation.esperanto_lexer as eo_lxr
import english_prototype.english_lexer as en_lxr


class TestEndToEndExamplePrograms(EnglishLexerProvided):
    @pytest.fixture
    def esperantifier(self):
        return Esperantifier(Domsagxo(), ast.build())

    def test_countUnrecognizedTokensInExamplePrograms(self, lexer):
        problematic_tokens = []
        for program in EngluentoPrograms:
            lexer.input(program.value)
            for token in lexer:
                if len(token.tags.keys()) == 0 and token.value not in problematic_tokens:
                    problematic_tokens += [token.value]
        print(len(problematic_tokens))
        print(problematic_tokens)
        assert len(problematic_tokens) < 6

    def test_checkIfCorrectTaggingExistsForAllExamples(self):
        domsagxo_lexer = eo_lxr.build()
        engluento_lexer = en_lxr.WordnetProtoLexer()

        pair_programs = [
            (DomsagxoPrograms.mu_constant, EngluentoPrograms.mu_constant),
            (DomsagxoPrograms.silly_name_generator, EngluentoPrograms.silly_name_generator),
        ]
        for domsa_prog, englu_prog in pair_programs:
            domsagxo_lexer.input(domsa_prog.value)
            domsagxo_tokens = [t for t in domsagxo_lexer]
            engluento_lexer.input(englu_prog.value)
            engluento_tokens = [t for t in engluento_lexer]

            print(domsa_prog.name)
            print((len(domsagxo_tokens), len(engluento_tokens)))
            for eo_token, en_token in zip(domsagxo_tokens, engluento_tokens):
                print(str(eo_token.value) + "->" + str(en_token.value) + str(en_token.tags))
                assert eo_token.type in [k.value for k in en_token.tags.keys()]

    def test_muFunction_constant(self, lexer, esperantifier):
        lexer.input(EngluentoPrograms.mu_constant.value)
        tokenized_program = [t for t in lexer]
        legal_interpretations_tree = esperantifier.try_interpreting(tokenized_program)
        assert legal_interpretations_tree.number_of_leaves() > 0

    # def test_program_sillyNameExample(self, lexer, esperantifier):
    #     lexer.input(EngluentoPrograms.silly_name_generator.value)
    #     tokenized_program = [t for t in lexer]
    #     legal_interpretations_tree = esperantifier.try_interpreting(tokenized_program)
    #     assert legal_interpretations_tree.number_of_leaves() > 0

    # def test_program_sequencedAnnouncements(self, lexer):
    #     lexer.input(example_programs.sequenced_announcements)
    #
    # def test_program_triggeredAnnouncements(self, lexer):
    #     lexer.input(example_programs.triggered_announcements)
