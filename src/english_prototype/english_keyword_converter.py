from compilation.definitions import ReservedWord, UnalphabeticTerminal

english_keywords_dictionary = {
    "assign": [ReservedWord.PUT.value],
    "day": [ReservedWord.TIME_INDICATION.value],
    "days": [ReservedWord.TIME_INDICATION.value],
    "equal": [ReservedWord.EQUAL.value],
    "greater": [ReservedWord.GREATER.value],
    "hour": [ReservedWord.TIME_INDICATION.value],
    "hours": [ReservedWord.TIME_INDICATION.value],
    "is": [ReservedWord.IS.value],
    "than": [ReservedWord.THAN.value],
    "the": [ReservedWord.THE.value],
    "times": [UnalphabeticTerminal.TIMES.value],
    "to": [ReservedWord.TO.value],
    "lesser": [ReservedWord.SMALLER.value],
    "minus": [UnalphabeticTerminal.MINUS.value],
    "minute": [ReservedWord.TIME_INDICATION.value],
    "minutes": [ReservedWord.TIME_INDICATION.value],
    "month": [ReservedWord.TIME_INDICATION.value],
    "months": [ReservedWord.TIME_INDICATION.value],
    "or": [ReservedWord.OR.value],
    "parts": [UnalphabeticTerminal.DIVIDE.value],
    "plus": [UnalphabeticTerminal.PLUS.value],
    "second": [ReservedWord.TIME_INDICATION.value],
    "seconds": [ReservedWord.TIME_INDICATION.value],
    "week": [ReservedWord.TIME_INDICATION.value],
    "weeks": [ReservedWord.TIME_INDICATION.value],
    "year": [ReservedWord.TIME_INDICATION.value],
    "years": [ReservedWord.TIME_INDICATION.value],
    "+": [UnalphabeticTerminal.PLUS.value],
    "-": [UnalphabeticTerminal.MINUS.value],
    "*": [UnalphabeticTerminal.TIMES.value],
    "/": [UnalphabeticTerminal.DIVIDE.value],
    "(": [UnalphabeticTerminal.L_PAREN.value],
    ")": [UnalphabeticTerminal.R_PAREN.value],
}


def identify_potential_keywords(word):
    return english_keywords_dictionary.get(word, [])
