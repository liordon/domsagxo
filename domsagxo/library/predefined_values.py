from enum import Enum


class ApplianceTypes(Enum):
    # MOST BASIC - TURN ON OR OFF
    SWITCH = "sxalto"
    # A DIMMER WITH A RANGE OF VALUES
    KNOB = "agordo"
    # LIKE A KNOB BUT ALSO WITH COLOR
    LIGHT = "lumo"
    # A BOILER THAT HEATS WATER
    BOILER = "kaldrono"
    # A SENSOR FOR TEMPERATURE AND HUMIDITY
    THERMOSTAT = "termostato"
    # A SENSOR FOR VISUAL FOOTAGE
    CAMERA = "fotilo"


class ApplianceProperties(Enum):
    BRIGHTNESS = "brilo"
    COLOR = "koloro"
    CHANNEL = "kanalo"
    TEMPERATURE = "temperaturo"
    DESIRED_TEMPERATURE = "volita temperaturo"
    CURRENT_TEMPERATURE = "nuna temperaturo"


class ApplianceQueries(Enum):
    IS_ON = "sxaltas"


class Color(Enum):
    WHITE = "blanko"
    RED = "rugxo"
    GREEN = "verdo"
    BLACK = "nigro"
    BLUE = "bluo"
    YELLOW = "flavo"
    ORANGE = "orangxo"


class PossibleRandomType(Enum):
    NUMBER = "nombro"
    TIME_POINT = "horo"
    TIME_SPAN = "tempo"


all_categories = [Color, ApplianceProperties, ApplianceTypes, PossibleRandomType]
