from domsagxo.print_utils.pretty_prints import leaf_form, branch_form


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


class BeamTreeNode(object):
    def __init__(self, token: Token, probability: float, children: list):
        self.token = token
        self.probability = probability
        self.children = children
        self.children_copied = False

    def tree_size(self):
        return 1 + sum([child.tree_size() for child in self.children])

    def tree_depth(self):
        return 1 + (0 if len(self.children) == 0 else self.children[0].tree_depth())

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
        if tag_list[0] != self.token.tag:
            return self
        if len(tag_list) == 1 and tag_list[0] == self.token.tag:
            return None
        else:
            new_children = [child.prune(tag_list[1:]) for child in self.children]
            new_children = [child for child in new_children if child is not None]
            if len(new_children) == 0:
                return None
            return BeamTreeNode(self.token, self.probability, new_children)

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
            matching_child_index = self._find_index_of_child_matching_token(
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

    def longest_legal_sub_interpretation(self, interpretation):
        if interpretation[0] != self.token:
            raise KeyError()
        if len(interpretation) > 1:
            matching_child = self._find_index_of_child_matching_token(interpretation[1])
            if matching_child is None:
                return interpretation[:1]
            else:
                return interpretation[:1] + \
                       self.children[matching_child]. \
                           longest_legal_sub_interpretation(interpretation[1:])
        elif len(interpretation) == 1:
            return interpretation

    def __remove_child(self, i):
        """Used to be self.children.remove(i) but that doesn't work anymore
        due to using Copy On Write"""
        child_index = self._find_index_of_child_matching_token(i.token)
        self.children.pop(child_index)

    def _are_children_leaves(self):
        return len(self.children) + 1 == self.tree_size()

    def _find_index_of_child_matching_token(self, search_token):
        for i in range(len(self.children)):
            if self.children[i].token == search_token:
                return i
        return None

    def verify_integrity(self):
        if len(self.children) == 0:
            return True
        children_depths = [child.tree_depth() for child in self.children]
        if len(set(children_depths)) > 1:
            return False
        return False not in [child.verify_integrity() for child in self.children]


class BeamTree(BeamTreeNode):
    ROOT_TOKEN = Token("root", None)

    def __init__(self, subtrees: list):
        super(BeamTree, self).__init__(BeamTree.ROOT_TOKEN, 1, subtrees)

    def prune(self, tag_list):
        prune_result = super(BeamTree, self).prune([None] + tag_list)
        if prune_result is None:
            self.children = []
        else:
            self.children = prune_result.children
        return self

    def get_next_interpretation(self, complete_interpretation=None):
        padded_interpretation = None if complete_interpretation is None \
            else ([self.token] + complete_interpretation)
        next_interpretation = super(BeamTree, self).get_next_interpretation(padded_interpretation)
        return None if next_interpretation is None else next_interpretation[1:]

    def longest_legal_sub_interpretation(self, interpretation):
        return super(BeamTree, self).longest_legal_sub_interpretation([BeamTree.ROOT_TOKEN] + interpretation)[1:]

    @staticmethod
    def from_tokens_list(beam_tokens):
        subtrees = BeamTree.__forest_from_tokens_list(beam_tokens)
        return BeamTree(subtrees)

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
        return BeamTreeNode(token, probability, children)


def convert_item_to_dict_representation(dict_item: tuple):
    return '{' + dict_item.__repr__().replace(',', ':')[1:-1] + '}'


def calculate_total_combinations(beam_tokens: list):
    combinations = 1
    for t in beam_tokens:
        length = len(t.tags.keys())
        combinations *= 1 if length <= 0 else length
    return combinations
