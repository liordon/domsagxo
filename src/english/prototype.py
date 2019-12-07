import nltk

from compilation.definitions import PartOfSpeech


class Token(object):

    def __init__(self, token_tuple):
        (str, val) = token_tuple
        print("str: {} val: {}".format(str,val))
        if val == "NN":
            self.type = PartOfSpeech.NOUN.value
        elif val == "JJ":
            self.type = PartOfSpeech.ADJECTIVE.value
        elif val == "RB":
            self.type = PartOfSpeech.ORDINAL.value
        elif val == "VB":
            self.type = PartOfSpeech.V_IMP.value

        self.value = str


class Prototype(object):

    def __init__(self):
        self._stack = []

    def input(self, str):
        self._stack += nltk.pos_tag(nltk.word_tokenize(str))[::-1]

    def token(self):
        return Token(self._stack.pop())
