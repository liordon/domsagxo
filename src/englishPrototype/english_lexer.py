import nltk
import re
from nltk.corpus import wordnet

from compilation.definitions import PartOfSpeech, ReservedWord


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


def collect_probabilities_from_list(possible_vals):
    types = {}
    for val in possible_vals:
        types[val] = types.get(val, 0) + 1
    for val in types.keys():
        types[val] /= len(possible_vals)
    return types


class BeamToken(object):
    def __init__(self, token_tuple):
        (str, possible_vals) = token_tuple
        self.value = str
        self.types = possible_vals


class NltkProtoLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, txt):
        self._stack += [(txt, convert_tag_to_part_of_speech(val)) for (txt, val) in
                        nltk.pos_tag(nltk.word_tokenize(txt))[::-1]]

    def has_next(self):
        return len(self._stack) > 0

    def token(self):
        return Token(self._stack.pop())

    def __iter__(self):
        while self.has_next():
            yield self.token()


class WordnetProtoLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, str):
        self._stack += [(w, collect_probabilities_from_list(
            [convert_tag_to_part_of_speech(syn.pos()) for syn in wordnet.synsets(w)]
        )) for w in str.split(" ")][::-1]

    def has_next(self):
        return len(self._stack) > 0

    def token(self):
        return BeamToken(self._stack.pop())

    def __iter__(self):
        while self.has_next():
            yield self.token()
