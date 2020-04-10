import re

import nltk
from nltk.corpus import wordnet

from compilation.definitions import PartOfSpeech, ReservedWord, UnalphabeticTerminal
from english_prototype.english_keyword_converter import identify_potential_keywords
from print_utils.pretty_prints import leaf_form, branch_form


def insensitive_starts_with(txt, insensitive_prefix):
    return bool(re.match(insensitive_prefix, txt, re.I))


def convert_tag_to_part_of_speech(tag):
    if insensitive_starts_with(tag, 'a') or insensitive_starts_with(tag, 'j') or insensitive_starts_with(tag,
            's'):  # adjective or satellite
        return PartOfSpeech.ADJECTIVE.value
    elif insensitive_starts_with(tag, 'd'):
        return ReservedWord.THE.value
    elif insensitive_starts_with(tag, 'i'):
        return PartOfSpeech.PREPOSITION.value
    elif insensitive_starts_with(tag, 'n'):
        return PartOfSpeech.NOUN.value
    elif insensitive_starts_with(tag, 'r'):
        return PartOfSpeech.ADVERB.value
    elif insensitive_starts_with(tag, 'v'):
        return PartOfSpeech.V_IMP.value
    else:
        return ''


class Token(object):
    def __init__(self, token_tuple):
        (word, part_of_speech) = token_tuple
        self.value = word
        self.type = part_of_speech


class NltkProtoLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, txt):
        self._stack += [(txt, convert_tag_to_part_of_speech(val)) for (txt, val) in
            nltk.pos_tag(nltk.word_tokenize(txt))[::-1]]

    def has_next(self):
        return len(self._stack) > 0


def collect_probabilities_from_list(possible_vals):
    types = {}
    for val in possible_vals:
        types[val] = types.get(val, 0) + 1
    for val in types.keys():
        types[val] /= len(possible_vals)
    return types


def convert_word_to_stochastic_token_tuple(w):
    if re.match(r"\d+", w):
        return w, {UnalphabeticTerminal.NUMBER.value: 1.}
    # if re.match(r"[a-zĉĝĥĵŝŭA-ZĈĜĤĴŜŬ]+", w):
    else:
        return (w, collect_probabilities_from_list(
            [convert_tag_to_part_of_speech(syn.pos()) for syn in wordnet.synsets(w)]
            + identify_potential_keywords(w)))


class BeamToken(object):
    def __init__(self, token_tuple):
        (str, possible_vals) = token_tuple
        self.value = str
        self.types = possible_vals

    def token(self):
        return Token(self._stack.pop())

    def __iter__(self):
        while self.has_next():
            yield self.token()

    def pretty_format(self):
        token_representation_lines = self.create_tag_string_list() + [self.value]
        longest_line = max(len(line) for line in token_representation_lines)
        return '\n'.join(line.center(longest_line) for line in token_representation_lines)

    def create_tag_string_list(self):
        return [convert_item_to_dict_representation(t) for t in self.types.items()]

    @staticmethod
    def list_pretty_format(token_list: list):
        formatted_token_list = [token.pretty_format() for token in token_list]
        maximum_lines = max(len(formatted_token.splitlines()) for formatted_token in formatted_token_list)
        formatted_tokens_with_equal_lines = [
            (' ' * len(formatted_token.splitlines()[0]) + '\n') * (
                    maximum_lines - len(formatted_token.splitlines())) + formatted_token for
            formatted_token in formatted_token_list]
        output = [' + '.join(
            [formatted_token.splitlines()[line_number] for formatted_token in formatted_tokens_with_equal_lines]) for
            line_number in range(len(formatted_tokens_with_equal_lines[0].splitlines()))]
        return '\n'.join(output)

    def break_into_types(self):
        return [BeamToken((self.value, {tag: self.types[tag]})) for tag in self.types]


class BeamTree(object):
    def __init__(self, beam_tokens):
        if len(beam_tokens[0].types) == 1:
            self.value = beam_tokens[0].value
            self.tag = [*beam_tokens[0].types][0]
            self.probability = beam_tokens[0].types[self.tag]
            beam_tokens = beam_tokens[1:]
        else:
            self.value = ""
            self.tag = None
            self.probability = 1
        if len(beam_tokens) == 0:
            self.children = []
        else:
            other_tokens = [] if len(beam_tokens) < 1 else beam_tokens[1:]
            self.children = [
                BeamTree([tag] + other_tokens) for tag in beam_tokens[0].break_into_types()
            ]

    def size(self):
        return 1 + sum([child.size() for child in self.children])

    def get_children(self):
        return self.children

    def pretty_print(self, indent="", last_child=True):
        res = indent + leaf_form(last_child) + str(type(self).__name__) + "- " \
              + self.value + " (" + str(self.tag) + ": " + str(self.probability) + ")"
        for i in range(len(self.children)):
            child = self.children[i]
            newIndent = indent + branch_form(last_child)
            res += "\n" + child.pretty_print(newIndent, i == len(self.children) - 1)

        return res

    def prune(self, tag_list):
        if len(tag_list) == 1 and tag_list[0] == self.tag:
            return None
        else:
            for i in self.children:
                if i.tag == tag_list[1] and i.prune(tag_list[1:]) is None:
                    self.children.remove(i)
                    break
            return self


class WordnetProtoLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, text):
        self._stack += [convert_word_to_stochastic_token_tuple(w) for w in text.split(" ")][::-1]

    def has_next(self):
        return len(self._stack) > 0

    def token(self):
        return BeamToken(self._stack.pop())

    def __iter__(self):
        while self.has_next():
            yield self.token()


def convert_item_to_dict_representation(dict_item: tuple):
    return '{' + dict_item.__repr__().replace(',', ':')[1:-1] + '}'
