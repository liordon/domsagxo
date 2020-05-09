from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Generic


class DomsagxoStyle(Style):
    default_style = ""
    styles = {
        Comment: 'italic #888',
        Keyword: 'bold #2C693F',
        Name: '#f00',
        Name.Function: '#800000',
        Name.Class: 'bold #0f0',
        String: 'bg:#eee #111',
        Generic.Separator: '#000000'
    }
