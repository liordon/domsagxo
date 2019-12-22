alpha_alt = 'air bat cap drum each fine gust harp sit jury crunch look made near odd pit quench red sun trap urge vest whale plex yank zip'.split()

f_keys = ['f'+str(i) for i in range(1, 13)]
# arrows are separated because 'up' has a high false positive rate
arrows = ['left', 'right', 'up', 'down']
simple_keys = [
    'tab', 'escape', 'enter', 'space',
    'home', 'pageup', 'pagedown', 'end',
]
alternate_keys = ['delete', 'forward delete',]

symbols = [ 'back tick', 'comma', 'dot', 'period', 'semi', 'semicolon',
    'quote', 'L square', 'left square', 'square', 'R square', 'right square',
    'forward slash', 'slash', 'backslash', 'minus', 'dash', 'equals',
]

modifiers = [
    'command', 'control', 'shift',
    'alt', 'option',
]
