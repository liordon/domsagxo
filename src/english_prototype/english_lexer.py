import re

import nltk
from nltk.corpus import wordnet

from compilation.definitions import PartOfSpeech, ReservedWord, UnalphabeticTerminal
from english_prototype.english_keyword_converter import identify_potential_keywords
from english_prototype.data_structures import BeamToken


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


class NltkProtoLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, txt):
        self._stack += [(txt, convert_tag_to_part_of_speech(val)) for (txt, val) in
            nltk.pos_tag(nltk.word_tokenize(txt))[::-1]]

    def has_next(self):
        return len(self._stack) > 0


def collect_probabilities_from_list(possible_vals):
    tags = {}
    for val in possible_vals:
        tags[val] = tags.get(val, 0) + 1
    for val in tags.keys():
        tags[val] /= len(possible_vals)
    return tags


def convert_word_to_stochastic_token_tuple(w):
    if re.match(r"\d+", w):
        return w, {UnalphabeticTerminal.NUMBER.value: 1.}
    # if re.match(r"[a-zĉĝĥĵŝŭA-ZĈĜĤĴŜŬ]+", w):
    else:
        return (w, collect_probabilities_from_list(
            [convert_tag_to_part_of_speech(syn.pos()) for syn in wordnet.synsets(w)]
            + identify_potential_keywords(w)))


class WordnetProtoLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, text):
        self._stack += [convert_word_to_stochastic_token_tuple(w) for w in text.split(" ")][::-1]

    def has_next(self):
        return len(self._stack) > 0

    def token(self):
        return BeamToken.from_tuple(self._stack.pop())

    def __iter__(self):
        while self.has_next():
            yield self.token()


