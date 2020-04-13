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
    def __init__(self, word, part_of_speech):
        self.value = word
        self.tag = part_of_speech

    def __eq__(self, other):
        if not isinstance(other, Token):
            return False
        else:
            return self.value == other.value \
                   and self.tag == other.tag

    def __repr__(self):
        return self.__class__.__name__ + '(' + self.value + ',' + self.tag.__repr__() + ')'

    @staticmethod
    def from_tuple(token_tuple):
        (word, part_of_speech) = token_tuple
        return Token(word, part_of_speech)


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


class BeamToken(object):
    def __init__(self, str, possible_vals):
        self.value = str
        self.tags = possible_vals

    def __repr__(self):
        return self.__class__.__name__ + '(' + self.value + ',' + self.tags.__repr__() + ')'

    def pretty_format(self):
        token_representation_lines = self.create_tag_string_list() + [self.value]
        longest_line = max(len(line) for line in token_representation_lines)
        return '\n'.join(line.center(longest_line) for line in token_representation_lines)

    def create_tag_string_list(self):
        return [convert_item_to_dict_representation(t) for t in self.tags.items()]

    def break_into_tags(self):
        return [BeamToken(self.value, {tag: self.tags[tag]}) for tag in self.tags]

    @staticmethod
    def from_tuple(token_tuple):
        (str, possible_vals) = token_tuple
        return BeamToken(str, possible_vals)

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


class BeamTree(object):
    def __init__(self, token: Token, probability: float, children: list):
        self.token = token
        self.probability = probability
        self.children = children

    def tree_size(self):
        return 1 + sum([child.tree_size() for child in self.children])

    def number_of_leaves(self):
        return 1 if len(self.children) == 0 \
            else sum([child.number_of_leaves() for child in self.children])

    def get_children(self):
        return self.children

    def pretty_print(self, indent="", last_child=True):
        res = indent + leaf_form(last_child) + str(type(self).__name__) + "- " \
              + self.token.value + " (" + str(self.token.tag) + ": " + str(self.probability) + ")"
        for i in range(len(self.children)):
            child = self.children[i]
            newIndent = indent + branch_form(last_child)
            res += "\n" + child.pretty_print(newIndent, i == len(self.children) - 1)

        return res

    def prune(self, tag_list):
        if len(tag_list) == 1 and tag_list[0] == self.token.tag:
            return None
        else:
            for i in self.children:
                if i.token.tag == tag_list[1] and i.prune(tag_list[1:]) is None:
                    self.children.remove(i)
                    break
            return self

    def get_next_interpretation(self, complete_interpretation=None):
        current_token = self.token

        if complete_interpretation is None or complete_interpretation == [] \
                or complete_interpretation == [self.token]:
            return [current_token] + ([] if len(self.children) == 0 else self.children[0].get_next_interpretation())

        if complete_interpretation[0] != self.token:
            raise KeyError(str(complete_interpretation[0]) + " not a valid interpretation.")

        next_interpretations = None if len(complete_interpretation) < 2 \
            else complete_interpretation[1:]

        if len(self.children) == 0:  # no children
            return [current_token]
        else:
            matching_child_index = self.find_child_matching(
                next_interpretations[0])  # referred to by current implementation
            child_interpretation = self.children[matching_child_index].get_next_interpretation(next_interpretations)
            if (len(next_interpretations) == 1 and self._are_children_leaves()) \
                    or child_interpretation is None:  # if we exhausted this child
                matching_child_index += 1  # roll over to next child
                if (matching_child_index >= len(self.children)):
                    return None  # ran out of children
                child_interpretation = self.children[matching_child_index].get_next_interpretation()

            # at this point, we have either stayed with the original child or rolled over to
            # an existing child.
            return [current_token] + child_interpretation

    def _are_children_leaves(self):
        return len(self.children) + 1 == self.tree_size()

    def find_child_matching(self, search_token):
        for i in range(len(self.children)):
            if self.children[i].token == search_token:
                return i
        raise KeyError("unable to find " + str(search_token))

    @staticmethod
    def from_tokens_list(beam_tokens):
        subtrees = BeamTree.__forest_from_tokens_list(beam_tokens)
        return BeamTree(Token("", None), 1, subtrees)

    @staticmethod
    def __forest_from_tokens_list(beam_tokens):
        children = [] if len(beam_tokens) < 2 \
            else BeamTree.__forest_from_tokens_list(beam_tokens[1:])
        return [BeamTree.__branch_from_beam_token(beam_token, children)
            for beam_token in beam_tokens[0].break_into_tags()]

    @staticmethod
    def __branch_from_beam_token(beam_token, children):
        token = Token(beam_token.value, [*beam_token.tags][0])
        probability = beam_token.tags[token.tag]
        return BeamTree(token, probability, children)


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


def convert_item_to_dict_representation(dict_item: tuple):
    return '{' + dict_item.__repr__().replace(',', ':')[1:-1] + '}'
