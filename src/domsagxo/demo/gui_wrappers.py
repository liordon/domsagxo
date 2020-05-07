from domsagxo.library.predefined_values import Color


class GuiBulb(object):

    def __init__(self, light_bulb):
        self.inner_bulb = light_bulb
        self.last_setting = None

    def has_changed(self):
        return self.last_setting != self.get_gui_settings()

    def get_gui_settings(self):
        bulb_color = self.inner_bulb.properties["koloro"]
        gui_color = "grey"
        if not self.inner_bulb.isTurnedOn():
            gui_color = "black"
        elif bulb_color == Color.WHITE.value:
            gui_color = "white"
        elif bulb_color == Color.RED.value:
            gui_color = "red"
        elif bulb_color == Color.BLUE.value:
            gui_color = "blue"
        elif bulb_color == Color.YELLOW.value:
            gui_color = "yellow"
        elif bulb_color == Color.GREEN.value:
            gui_color = "green"
        elif bulb_color == Color.ORANGE.value:
            gui_color = "orange"
        return self.inner_bulb.name, gui_color

    def refresh(self):
        self.last_setting = self.get_gui_settings()
