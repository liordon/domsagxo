from setuptools import setup, find_packages

setup(
    name="domsagxo",
    packages=find_packages(),

     entry_points="""
         [pygments.lexers]
         domsaLex = syntax_high_light.pygment_lexer:domsaLex
         [pygments.styles]
         domsagxoStyle = syntax_high_light.pygment_style:DomsagxoStyle
     """
)
