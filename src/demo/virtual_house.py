from appJar import gui

import compilation.abstract_syntax_tree as ast_bld
from library.management_components import Domsagxo
import compilation.esp_lexer as lxr

lxr.build()
smart_home = Domsagxo()
ast = ast_bld.build(start=ast_bld.Var.PROGRAM.value)


def press(button):
    global app, smart_home, ast
    if button == "Cancel":
        app.stop()
    else:
        speech = app.getTextArea("Speech")
        print("parsing command:" + speech + "\n")
        smart_home, value = ast.parse(speech).evaluate(smart_home)


app = gui("virtuala domo")
app.addLabel("title", "Welcome to Domsagxo", colspan=3)
app.setLabelBg("title", "red")

app.startLabelFrame("Enirejo", 1, 2, 1)
app.addLabel("l2", "row=1\ncolumn=1\ncolspan=1")
app.stopLabelFrame()

app.startLabelFrame("koridoro", 1, 0, 2)
app.addLabel("l4", "row=1\ncolumn=0\ncolspan=2")
app.stopLabelFrame()

app.startLabelFrame("oficejo", 2, 2, 1, 1)
app.addLabel("l6", "row=1\ncolumn=2\ncolspan=1\nrowspan=1")
app.stopLabelFrame()
app.startLabelFrame("necesejo", 2)
app.addLabel("l7", "row=2\ncolumn=0")
app.stopLabelFrame()
app.startLabelFrame("dormcxambro", 2, 1)
app.addLabel("l8", "row=2\ncolumn=1")
app.stopLabelFrame()

app.addLabel("l9", "speech:")
app.addTextArea("Speech", row=3, column=1, colspan=2)

app.addButtons(["Submit", "Cancel"], press, row=5, colspan=3)
app.go()
