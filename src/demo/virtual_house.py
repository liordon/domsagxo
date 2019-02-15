import math
import os
from enum import Enum

from appJar import gui

import compilation.abstract_syntax_tree as ast_bld
import compilation.esperanto_lexer as lxr
from library.atomic_types import Appliance, ApplianceTypes, Color
from library.management_components import Domsagxo


def announce_in_multimedia(*args):
    for output in args:
        print(output)
        os.system('espeak -v eo+f3 "' + str(output) + '"')


lxr.build()
ast = ast_bld.build(start=ast_bld.Var.PROGRAM.value)
smart_home = Domsagxo()
smart_home.start_scheduler()

smart_home.method_dict['anoncu'] = announce_in_multimedia


def all_true(argument_list):
    for arg in argument_list:
        if not arg:
            return False
    return True


def is_prime(number):
    if number <= 1:
        return False
    return all_true([number % i for i in range(2, int(math.sqrt(number)) + 1)])


def press(button_or_key):
    global app, smart_home, ast
    if button_or_key == "Cancel" or button_or_key == "<Escape>":
        smart_home.stop_scheduler()
        app.stop()
    else:
        speech = app.getTextArea("Speech")
        print("parsing command:" + speech + "\n")
        smart_home, value = ast.parse(speech).evaluate(smart_home)
        app.clearTextArea("Speech")


def create_light_bulb(name, row=0, col=0):
    bulb = Appliance(ApplianceTypes.LIGHT, name)
    smart_home.addAppliance(bulb)
    app.addLabel(name, row=row, column=col)

    def update_gui():
        bulb_color = bulb.properties["koloro"]
        if not bulb.isTurnedOn():
            app.setLabelBg(name, "black")
        elif bulb_color == Color.WHITE.value:
            app.setLabelBg(name, "white")
        elif bulb_color == Color.RED.value:
            app.setLabelBg(name, "red")
        elif bulb_color == Color.BLUE.value:
            app.setLabelBg(name, "blue")
        elif bulb_color == Color.YELLOW.value:
            app.setLabelBg(name, "yellow")
        elif bulb_color == Color.GREEN.value:
            app.setLabelBg(name, "green")
        elif bulb_color == Color.ORANGE.value:
            app.setLabelBg(name, "orange")
        else:
            app.setLabelBg(name, "gray")

    app.registerEvent(update_gui)
    return bulb


class DemoType(Enum):
    HOUSE = "house"
    CORRIDOR = "corridor"


demo = DemoType.CORRIDOR

with gui("virtuala domo") as app:
    app.addLabel("title", "Welcome to Domsagxo", colspan=3)
    app.setLabelBg("title", "green")

    if demo == DemoType.HOUSE:
        with app.labelFrame("Enirejo", 1, 2, 1):
            create_light_bulb("enirlumo")
        with app.labelFrame("Koridoro", 1, 0, 2):
            create_light_bulb("koridora lumo")
        with app.labelFrame("Oficejo", 2, 2, 1, 1):
            create_light_bulb("laborlampo")
        with app.labelFrame("Necesejo", 2):
            create_light_bulb("necesa lumo")
        with app.labelFrame("dormcxambro", 2, 1):
            create_light_bulb("dormlumo")
    else:
        with app.labelFrame("Koridoro", colspan=2):
            light_bulbs = []
            app.setPollTime(1000)
            for i in range(100):
                light_bulbs += [create_light_bulb(str(i + 1), row=int(i / 10), col=(i) % 10)]
            smart_home.variables["ampoloj"] = light_bulbs
        with app.labelFrame("salono", row=2, colspan=2):
            smart_home.variables["sxambalulo"] = create_light_bulb("sxambalulo")


        def turn_light_on_if_prime(number):
            if is_prime(number):
                smart_home.variables['sxambalulo'].turnOn()
            else:
                smart_home.variables['sxambalulo'].turnOff()


        smart_home.method_dict['cxuprimu'] = turn_light_on_if_prime

    app.addLabel("l9", "speech:")
    app.addTextArea("Speech", row=3, column=1, colspan=2)

    app.addButtons(["Submit", "Cancel"], press, row=5, colspan=3)
    app.enableEnter(press)
    app.bindKey("<Escape>", press)
