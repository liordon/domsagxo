import pytest

from demo.example_programs import *
from english_prototype.esperantifier import Esperantifier
from library.management_components import Domsagxo
from test_utils.providers import WordNetEnglishLexerProvided
import compilation.abstract_syntax_tree as ast
import compilation.esperanto_lexer as eo_lxr
import english_prototype.english_lexer as en_lxr

working_functions_list = [
    EngluentoPrograms.mu_constant,
    EngluentoPrograms.mu_successor,
    EngluentoPrograms.mu_projection,
    EngluentoPrograms.silly_name_generator,
]


class TestEndToEndExamplePrograms(WordNetEnglishLexerProvided):
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
        # engluento_lexer = en_lxr.WordnetProtoLexer()
        engluento_lexer = en_lxr.NltkProtoLexer()

        pair_programs = [
            (DomsagxoPrograms.mu_constant, EngluentoPrograms.mu_constant),
            (DomsagxoPrograms.mu_successor, EngluentoPrograms.mu_successor),
            (DomsagxoPrograms.mu_projection, EngluentoPrograms.mu_projection),
            # (DomsagxoPrograms.silly_name_generator, EngluentoPrograms.silly_name_generator),
            (DomsagxoPrograms.prime_seeking_routine, EngluentoPrograms.prime_seeking_routine),
        ]
        matches = 0
        total_tokens = 0
        for domsa_prog, englu_prog in pair_programs:
            domsagxo_lexer.input(domsa_prog.value)
            domsagxo_tokens = [t for t in domsagxo_lexer]
            engluento_lexer.input(englu_prog.value)
            engluento_tokens = [t for t in engluento_lexer]
            if engluento_tokens[0].value == "to":
                engluento_tokens = engluento_tokens[1:]
            total_tokens += len(domsagxo_tokens)

            # print(domsa_prog.name)
            # print((len(domsagxo_tokens), len(engluento_tokens)))
            for eo_token, en_token in zip(domsagxo_tokens, engluento_tokens):
                # print(str(eo_token.value) + "->" + str(en_token.value) + str(en_token.tags))
                # matches += 0 if eo_token.type in [k.value for k in en_token.tags.keys()] else 1
                matches += 1 if en_token.tag is not None and eo_token.type == en_token.tag.value else 0
        print(matches)
        print(total_tokens)

    @pytest.mark.parametrize("function", working_functions_list)
    def test_exampleFunctions(self, lexer, esperantifier, function):
        lexer.input(function.value)
        tokenized_program = [t for t in lexer]
        legal_interpretations_tree = esperantifier.try_interpreting(tokenized_program)
        assert legal_interpretations_tree.number_of_leaves() > 0
