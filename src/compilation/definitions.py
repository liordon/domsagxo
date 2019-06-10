from enum import Enum


class UnalphabeticTerminal(Enum):
    PLUS = 'TPLUS'
    MINUS = 'TMINUS'
    DELIMITER = 'TDELIM'
    COLON = 'TCOLON'
    TIMES = 'TTIMES'
    DIVIDE = 'TDIVIDE'
    PERIOD = 'TPERIOD'
    ASSIGN = 'TASSIGN'
    NUMBER = 'TNUMBER'
    STRING = 'TSTRING'
    L_PAREN = 'TLPAREN'
    R_PAREN = 'TRPAREN'
    COMMENT = 'TCOMMENT'


class ReservedWord(Enum):
    AFTER = 'Tpost'
    AND = 'Tkaj'
    AND_THEN = 'Tposte'
    AT = 'Tje'
    BOTH = 'Tambaux'
    DURING = 'Tdum'
    ELSE = 'Talie'
    END = 'Tfinu'
    EQUAL = 'Tegala'
    EVERY = 'Tcxiu'
    FALSE = 'Tmalvero'
    FOR = 'Tpor'
    GREATER = 'Tgranda'
    IF = 'Tse'
    IN = 'Ten'
    IS = 'Testas'
    LESS = 'Tmalpli'
    MORE = 'Tpli'
    NONE = 'Tnenio'
    NOT = 'Tne'
    OF = 'Tde'
    ONCE = 'Tunufoje'
    OR = 'Taux'
    PARTS = 'Tpartoj'
    PUT = 'Tasignu'
    RETURN = 'Trevenu'
    SIMULTANEOUSLY = 'Tsamtempe'
    SMALLER = 'Tmalgranda'
    THAN = 'Tol'
    THE = 'Tla'
    THEN = 'Ttiam'
    THIS_WAY = 'Tsignifas'
    TIME_INDICATION = 'Vtime_unit'
    TIMES = 'Tfojoj'
    TO = 'Tal'
    TRUE = 'Tvero'
    VERBAL_DIGIT = 'Vdigit_literal'
    WHENEVER = 'Tcxiufoje'
    WORD = 'Tvorto'


class PartOfSpeech(Enum):
    ADJECTIVE = 'Padjective'
    ADVERB = 'Padverb'
    NOUN = 'Pnoun'
    ORDINAL = "Pordinal"
    OTHER = "Pother"
    PREPOSITION = "Ppreposition"
    V_INF = 'Pinfinitive_verb'
    V_PRES = 'Ppresent_verb'
    V_IMP = 'Pimperative_verb'


reserved_words = {
    "al"       : ReservedWord.TO.value,
    "alie"     : ReservedWord.ELSE.value,
    "ambaŭ"    : ReservedWord.BOTH.value,
    "ambaux"   : ReservedWord.BOTH.value,
    "asignu"   : ReservedWord.PUT.value,
    "aŭ"       : ReservedWord.OR.value,
    "aux"      : ReservedWord.OR.value,
    "ĉiu"      : ReservedWord.EVERY.value,
    "ĉiufoje"  : ReservedWord.WHENEVER.value,
    "cxiufoje" : ReservedWord.WHENEVER.value,
    "cxiu"     : ReservedWord.EVERY.value,
    "dum"      : ReservedWord.DURING.value,
    "de"       : ReservedWord.OF.value,
    "egala"    : ReservedWord.EQUAL.value,
    "estas"    : ReservedWord.IS.value,
    "finu"     : ReservedWord.END.value,
    "fojoj"    : ReservedWord.TIMES.value,
    "granda"   : ReservedWord.GREATER.value,
    "je"       : ReservedWord.AT.value,
    "kaj"      : ReservedWord.AND.value,
    "krampo"   : UnalphabeticTerminal.L_PAREN.value,
    "la"       : ReservedWord.THE.value,
    "malgranda": ReservedWord.SMALLER.value,
    "malkrampo": UnalphabeticTerminal.R_PAREN.value,
    "malpli"   : ReservedWord.LESS.value,
    "malvero"  : ReservedWord.FALSE.value,
    "ne"       : ReservedWord.NOT.value,
    "nenio"    : ReservedWord.NONE.value,
    "ol"       : ReservedWord.THAN.value,
    "partoj"   : ReservedWord.PARTS.value,
    "pli"      : ReservedWord.MORE.value,
    "por"      : ReservedWord.FOR.value,
    "post"     : ReservedWord.AFTER.value,
    "poste"    : ReservedWord.AND_THEN.value,
    "revenu"   : ReservedWord.RETURN.value,
    "se"       : ReservedWord.IF.value,
    "samtempe" : ReservedWord.SIMULTANEOUSLY.value,
    "signifas" : ReservedWord.THIS_WAY.value,
    "tiam"     : ReservedWord.THEN.value,
    "unufoje"  : ReservedWord.ONCE.value,
    "vero"     : ReservedWord.TRUE.value,
}

prepositions = {
    "al",
    "anstataŭ",
    "anstataux",
    "antaŭ",
    "antaux",
    "apud",
    "ĉe",
    "cxe",
    "ĉirkaŭ",
    "cxirkaux",
    "de",
    "dum",
    "ekde",
    "ekster",
    "eksteren",
    "el",
    "en",
    "ĝis",
    "gxis",
    "inter",
    "kiel",
    "kontraŭ",
    "kontraux",
    "krom",
    "kun",
    "laŭ",
    "laux",
    "mala",
    "malgraŭ",
    "malgraux",
    "malkiel",
    "malsupren",
    "ol",
    "per",
    "plus",
    "po",
    "por",
    "post",
    "preter",
    "pri",
    "pro",
    "sed",
    "sekva",
    "sen",
    "sub",
    "suben",
    "super",
    "supren",
    "sur",
    "tra",
    "trans"
}
