from setuptools import setup, find_packages

setup(
    name="domsagxo_syntax_highlighter",
    packages=find_packages(),

    entry_points="""
         [pygments.lexers]
         domsagxo = domsagxo.syntax_high_light.pygment_lexer:DomsagxoLexer
         engluento = domsagxo.syntax_high_light.pygment_lexer:EngluentoLexer
         talon = domsagxo.syntax_high_light.pygment_lexer:TalonLexer
         [pygments.styles]
         domsagxoStyle = domsagxo.syntax_high_light.pygment_style:DomsagxoStyle
     """
)
