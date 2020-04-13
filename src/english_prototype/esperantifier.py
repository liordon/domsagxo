from compilation.abstract_syntax_tree import EsperantoSyntaxError, EsperantoLocatedSyntaxError
from compilation.definitions import PartOfSpeech, ReservedWord, UnalphabeticTerminal
from english_prototype.data_structures import BeamTree

_part_of_speech_suffix = {
    PartOfSpeech.NOUN: "oso",
    PartOfSpeech.ADJECTIVE: "osa",
    PartOfSpeech.V_INF: "osi",
    PartOfSpeech.V_IMP: "osu",
    PartOfSpeech.V_PRES: "osas",
}

_unalphabetic_terminals = {
    UnalphabeticTerminal.R_PAREN: ")",
    UnalphabeticTerminal.L_PAREN: "(",
    UnalphabeticTerminal.PLUS: "+",
    UnalphabeticTerminal.MINUS: "-",
    UnalphabeticTerminal.TIMES: "*",
    UnalphabeticTerminal.DIVIDE: "/",
}


def esperantify_word(word: str, part_of_speech):
    if part_of_speech == UnalphabeticTerminal.NUMBER:
        return word
    if part_of_speech in ReservedWord:
        return part_of_speech.value[1:]
    if part_of_speech in UnalphabeticTerminal:
        return _unalphabetic_terminals[part_of_speech]
    return word + _part_of_speech_suffix[part_of_speech]


def esperantify_tuples(sentence_tuples):
    return " ".join([esperantify_word(word, pos) for word, pos in sentence_tuples])


def esperantify_tokens(sentence_tokens):
    return " ".join([esperantify_word(token.value, token.tag) for token in sentence_tokens])


class Esperantifier(object):
    def __init__(self, eo_smart_home, abstract_syntax_tree):
        self._smart_home = eo_smart_home
        self._ast = abstract_syntax_tree

    def try_interpreting(self, statement: list):
        interpretations_tree = BeamTree.from_tokens_list(statement)
        interpretation = interpretations_tree.get_next_interpretation()
        while interpretation is not None:
            try:
                self._ast.parse(esperantify_tokens(interpretation))
            except EsperantoLocatedSyntaxError as e:
                interpretations_tree = interpretations_tree.prune([t.tag for t in interpretation[:e.index-1]])
                interpretation = interpretation[:e.index - 2]
                if interpretations_tree.tree_size() == 1:
                    return None
            except EsperantoSyntaxError as e:
                print(e.message)
                return None
            print(interpretation)
            interpretation = interpretations_tree.get_next_interpretation(interpretation)
        return interpretations_tree
