from setuptools import setup

setup(
    name="domsagxo",
    entry_points="""
        [pygments.lexers]
        domsagxoLexer = pygment_lexer:KeywordLexer
        [pygments.styles]
        domsagxoStyle = pygment_style:DomsagxoStyle
    """

    # entry_points="""
    #     [pygments.lexers]
    #     domsagxoLexer = syntax_high_light:pygment_lexer
    #     [pygments.styles]
    #     domsagxoStyle = syntax_high_light:pygment_style
    # """
)
