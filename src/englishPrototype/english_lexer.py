import nltk

from compilation.definitions import PartOfSpeech, ReservedWord


class Token(object):
    NlpTagSimplifier = {
        "DT" : ReservedWord.THE.value,
        "IN" : PartOfSpeech.PREPOSITION.value,
        "JJ" : PartOfSpeech.ADJECTIVE.value,
        "JJS": PartOfSpeech.ADJECTIVE.value,
        "NN" : PartOfSpeech.NOUN.value,
        "NNS": PartOfSpeech.NOUN.value,
        "RB" : PartOfSpeech.ORDINAL.value,
        "VB" : PartOfSpeech.V_IMP.value,
        "VBZ": PartOfSpeech.V_PRES.value,
    }

    def __init__(self, token_tuple):
        (str, val) = token_tuple
        print("str: {} val: {}".format(str, val))  # TODO: delete after tests are done
        self.type = self.NlpTagSimplifier[val]
        self.value = str


class EnglishLexer(object):

    def __init__(self):
        self._stack = []

    def input(self, str):
        self._stack += nltk.pos_tag(nltk.word_tokenize(str))[::-1]

    def has_next(self):
        return len(self._stack) > 0

    def token(self):
        return Token(self._stack.pop())

    def __iter__(self):
        while self.has_next():
            yield self.token()
