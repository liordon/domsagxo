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
        return PartOfSpeech.ADJECTIVE
    elif insensitive_starts_with(tag, 'd'):
        return ReservedWord.THE
    elif insensitive_starts_with(tag, 'i'):
        return PartOfSpeech.PREPOSITION
    elif insensitive_starts_with(tag, 'n'):
        return PartOfSpeech.NOUN
    # elif insensitive_starts_with(tag, 'r'):
    #     return PartOfSpeech.ADVERB # not using adverbs
    elif insensitive_starts_with(tag, 'v'):
        return PartOfSpeech.V_IMP
    else:
        return None


class NltkProtoLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, txt):
        self._stack += [(txt, convert_tag_to_part_of_speech(val)) for (txt, val) in
            nltk.pos_tag(nltk.word_tokenize(txt))[::-1]]

    def has_next(self):
        return len(self._stack) > 0


def collect_probabilities_from_list(possible_vals):
    filtered_values = [v for v in possible_vals if v is not None]
    tags = {}
    for val in filtered_values:
        tags[val] = tags.get(val, 0) + 1
    for val in tags.keys():
        tags[val] /= len(filtered_values)
    return tags


def convert_word_to_stochastic_token_tuple(w):
    if re.match(r"\d+", w):
        return w, {UnalphabeticTerminal.NUMBER: 1.}
    # if re.match(r"[a-zĉĝĥĵŝŭA-ZĈĜĤĴŜŬ]+", w):
    else:
        return (w, collect_probabilities_from_list(
            [convert_tag_to_part_of_speech(syn.pos()) for syn in wordnet.synsets(w)]
            + identify_potential_keywords(w.lower())))


class WordnetProtoLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, text):
        split_words = self.split_input(text)
        i = 0
        while i < len(split_words):
            current_word = split_words[i]
            if i == 0 and len(split_words) >= 2 and current_word.lower() == "to":
                self._stack.insert(0, (" ".join(split_words[i:i + 2]), {PartOfSpeech.V_INF: 1}))
                i += 1
            elif current_word == '"':
                i += 1
                while '"' not in split_words[i]:
                    current_word += ' ' + split_words[i]
                    i += 1
                current_word += ' "'
                self._stack.insert(0, (current_word, {UnalphabeticTerminal.STRING: 1}))
            else:
                token_tuple = convert_word_to_stochastic_token_tuple(current_word)
                if current_word.endswith("th") and len(token_tuple[1].keys()) == 0:
                    token_tuple[1][PartOfSpeech.ADJECTIVE] = 1
                if UnalphabeticTerminal.COMMENT not in token_tuple[1] \
                        or token_tuple[1][UnalphabeticTerminal.COMMENT] < 1:
                    self._stack.insert(0, token_tuple)
            i += 1

    def split_input(self, text):
        # return [w for w in re.split(r"\s+", text) if w != '']

        return [w for w in re.split(r"\b|\s+", text) if not re.fullmatch(r"\s*", w)]

    def has_next(self):
        return len(self._stack) > 0

    def token(self):
        return BeamToken.from_tuple(self._stack.pop())

    def __iter__(self):
        while self.has_next():
            yield self.token()
