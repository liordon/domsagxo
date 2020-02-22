import math
import os
from enum import Enum

from appJar import gui

import compilation.abstract_syntax_tree as ast_bld
import compilation.esperanto_lexer as lxr
from demo.gui_wrappers import GuiBulb
from library.atomic_types import Appliance, ApplianceTypes
from library.management_components import Domsagxo, Horaro
from library.predefined_values import ApplianceProperties
from test_utils.mocks import MockClock


class HouseType(Enum):
    HOUSE = "house"
    CORRIDOR = "corridor"


class ClockType(Enum):
    REAL = "real"
    SIMULATIVE = "simulative"


def announce_in_multimedia(*args):
    for output in args:
        print(output)
        app.setLabel("Reply", output)
        # os.system('espeak -v eo+m3 "' + str(output) + '"')


house_type = HouseType.HOUSE
clock_type = ClockType.REAL


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


def create_door(name, row=0, col=0):
    door = Appliance(ApplianceTypes.SWITCH, name)
    smart_home.addAppliance(door)
    app.addLabel(name, row=row, column=col)

    def update_gui():
        if door.isTurnedOn():
            is_on_character = "□"
        else:
            is_on_character = "■"
        app.setLabel(name, is_on_character + " " + name)

    app.registerEvent(update_gui)
    return door


def create_boiler(name, row=0, col=0):
    boiler = Appliance(ApplianceTypes.BOILER, name)
    smart_home.addAppliance(boiler)
    app.addLabel(name, row=row, column=col)
    app.addMeter(name, row, col)

    def update_gui():
        desired_temperature = boiler.properties[ApplianceProperties.DESIRED_TEMPERATURE.value]
        current_temperature = boiler.properties[ApplianceProperties.CURRENT_TEMPERATURE.value]
        if boiler.isTurnedOn():
            current_temperature += (desired_temperature - current_temperature) / 20
            is_on_character = "☺"
        else:
            current_temperature -= current_temperature / 20
            is_on_character = "☻"
        boiler.properties[ApplianceProperties.CURRENT_TEMPERATURE.value] = current_temperature
        app.setMeter(name, current_temperature, " " + is_on_character + " " + name)

    app.registerEvent(update_gui)
    return boiler


def create_air_conditioner(name, row=0, col=0):
    air_conditioner = Appliance(ApplianceTypes.BOILER, name)
    smart_home.addAppliance(air_conditioner)
    app.addLabel(name, row=row, column=col)
    app.addMeter(name, row, col)
    air_conditioner.properties[ApplianceProperties.DESIRED_TEMPERATURE.value] = 23
    air_conditioner.properties[ApplianceProperties.CURRENT_TEMPERATURE.value] = 50

    def update_gui():
        desired_temperature = air_conditioner.properties[
            ApplianceProperties.DESIRED_TEMPERATURE.value]
        current_temperature = air_conditioner.properties[
            ApplianceProperties.CURRENT_TEMPERATURE.value]
        if air_conditioner.isTurnedOn():
            current_temperature += (desired_temperature - current_temperature) / 20
            is_on_character = "☺"
        else:
            current_temperature -= current_temperature / 20
            is_on_character = "☻"
        air_conditioner.properties[
            ApplianceProperties.CURRENT_TEMPERATURE.value] = current_temperature
        app.setMeter(name, current_temperature * (100 / desired_temperature),
            " " + is_on_character + " " + name)

    app.registerEvent(update_gui)
    return air_conditioner


def create_speaker(name, row=0, col=0):
    speaker = Appliance(ApplianceTypes.SWITCH, name)
    smart_home.addAppliance(speaker)
    app.addLabel(name, row=row, column=col)

    def update_gui():
        if speaker.isTurnedOn():
            is_on_character = "♫♪"
        else:
            is_on_character = "  "
        app.setLabel(name, name + is_on_character)

    app.registerEvent(update_gui)
    return speaker


def create_efficient_bulb(name, row=0, col=0):
    bulb = Appliance(ApplianceTypes.LIGHT, name)
    smart_home.addAppliance(bulb)
    app.addLabel(name, row=row, column=col)
    bulb_wrapper = GuiBulb(bulb)

    def update_gui():
        if bulb_wrapper.has_changed():
            app.setLabelBg(*bulb_wrapper.get_gui_settings())
            bulb_wrapper.refresh()

    app.registerEvent(update_gui)
    return bulb


def create_clock(name, row=0, col=0):
    clock_scheduler = smart_home.scheduler
    app.addLabel(name, row=row, column=col)

    def update_gui():
        now = clock_scheduler.getDate()
        app.setLabel(name, f"{now.hour:02d}:{now.minute:02d}:{now.second:02d}")

    app.registerEvent(update_gui)


lxr.build()
ast = ast_bld.build(start=ast_bld.GrammarVariable.PROGRAM.value)
if clock_type == ClockType.REAL:
    smart_home = Domsagxo()
    smart_home.start_scheduler()
else:
    simulative_time = MockClock()
    scheduler = Horaro(time_function=simulative_time.get_current_time,
        delay_function=simulative_time.increase_time)
    smart_home = Domsagxo(scheduler)
smart_home.method_dict["haltu"] = smart_home.scheduler.runSetTime
smart_home.method_dict['anoncu'] = announce_in_multimedia
smart_home.variables['saluto'] = "saluton mondo!"
smart_home.variables['mia nomo'] = "Lioro!"

with gui("virtuala domo", showIcon=False) as app:
    app.addLabel("title", "Welcome to Domsagxo", colspan=2)
    app.setLabelBg("title", "green")
    create_clock("horaro", 0, 2)

    if house_type == HouseType.HOUSE:
        with app.labelFrame("Enirejo", 1, 2, 1):
            create_door("antauxa pordo", 0, 0)
            create_efficient_bulb("enirlumo", 1, 0)
        with app.labelFrame("Koridoro", 1, 0, 2):
            create_efficient_bulb("koridora lumo")
            create_speaker("parolilo", 0, 1)
        with app.labelFrame("Oficejo", 2, 2, 1, 1):
            create_efficient_bulb("laborlampo")
            create_air_conditioner("ofica klimatizilo", 1, 0)
        with app.labelFrame("Necesejo", 2):
            create_efficient_bulb("necesa lumo")
            create_boiler("doma kaldrono", 1, 0)
        with app.labelFrame("dormcxambro", 2, 1):
            create_efficient_bulb("dormlumo")
    else:
        with app.labelFrame("Koridoro", colspan=2):
            light_bulbs = []
            for i in range(100):
                light_bulbs += [
                    create_efficient_bulb(f"{i + 1:03d}", row=int(i / 10), col=(i % 10))]
            smart_home.variables["ampoloj"] = light_bulbs
        with app.labelFrame("salono", row=2, colspan=2):
            smart_home.variables["sxambalulo"] = create_efficient_bulb("sxambalulo")


        def turn_light_on_if_prime(number):
            if is_prime(number):
                smart_home.variables['sxambalulo'].turnOn()
            else:
                smart_home.variables['sxambalulo'].turnOff()


        smart_home.method_dict['cxuprimu'] = turn_light_on_if_prime

    app.addLabel("l9", "speech:")
    app.addTextArea("Speech", row=3, column=1, colspan=2)

    app.addLabel("20", "reply:")
    app.addLabel("Reply", "Speech", row=4, column=1, colspan=2)

    app.addButtons(["Submit", "Cancel"], press, row=5, colspan=3)
    app.enableEnter(press)
    app.bindKey("<Escape>", press)
    app.setIcon("../../resources/logo.gif")
