from setuptools import setup, find_packages

setup(
    name="domsagxo",
    packages=find_packages(),

     entry_points="""
         [pygments.lexers]
         domsagxoLexer = syntax_high_light.pygment_lexer:KeywordLexer
         [pygments.styles]
         domsagxoStyle = syntax_high_light.pygment_style:DomsagxoStyle
     """
)
