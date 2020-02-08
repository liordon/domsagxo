import re

import nltk
from nltk.corpus import wordnet

from compilation.definitions import PartOfSpeech, ReservedWord, UnalphabeticTerminal
from english_prototype.english_keyword_converter import identify_potential_keywords


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
