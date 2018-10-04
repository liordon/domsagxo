from setuptools import setup, find_packages

setup(
    name="domsagxo",
    packages=find_packages(),

     entry_points="""
         [pygments.lexers]
         domsagxo = syntax_high_light.pygment_lexer:DomsagxoLexer
         engluento = syntax_high_light.pygment_lexer:EngluentoLexer
         [pygments.styles]
         domsagxoStyle = syntax_high_light.pygment_style:DomsagxoStyle
     """
)
