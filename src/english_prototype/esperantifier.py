from compilation.definitions import PartOfSpeech, ReservedWord, UnalphabeticTerminal

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
