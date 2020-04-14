from compilation.definitions import ReservedWord, UnalphabeticTerminal

english_keywords_dictionary = {
    "assign": [ReservedWord.PUT],
    "day": [ReservedWord.TIME_INDICATION],
    "days": [ReservedWord.TIME_INDICATION],
    "equal": [ReservedWord.EQUAL],
    "greater": [ReservedWord.GREATER],
    "hour": [ReservedWord.TIME_INDICATION],
    "hours": [ReservedWord.TIME_INDICATION],
    "is": [ReservedWord.IS],
    "than": [ReservedWord.THAN],
    "the": [ReservedWord.THE],
    "times": [UnalphabeticTerminal.TIMES],
    "to": [ReservedWord.TO],
    "lesser": [ReservedWord.SMALLER],
    "minus": [UnalphabeticTerminal.MINUS],
    "minute": [ReservedWord.TIME_INDICATION],
    "minutes": [ReservedWord.TIME_INDICATION],
    "month": [ReservedWord.TIME_INDICATION],
    "months": [ReservedWord.TIME_INDICATION],
    "or": [ReservedWord.OR],
    "parts": [UnalphabeticTerminal.DIVIDE],
    "plus": [UnalphabeticTerminal.PLUS],
    "second": [ReservedWord.TIME_INDICATION],
    "seconds": [ReservedWord.TIME_INDICATION],
    "week": [ReservedWord.TIME_INDICATION],
    "weeks": [ReservedWord.TIME_INDICATION],
    "year": [ReservedWord.TIME_INDICATION],
    "years": [ReservedWord.TIME_INDICATION],
    "+": [UnalphabeticTerminal.PLUS],
    "-": [UnalphabeticTerminal.MINUS],
    "*": [UnalphabeticTerminal.TIMES],
    "/": [UnalphabeticTerminal.DIVIDE],
    "(": [UnalphabeticTerminal.L_PAREN],
    ")": [UnalphabeticTerminal.R_PAREN],
}


def identify_potential_keywords(word):
    return english_keywords_dictionary.get(word, [])
