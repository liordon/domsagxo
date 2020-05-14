class Bunch:
    def __init__(self, **keywords):
        self.__dict__.update(keywords)


class MockHouse(object):
    def __init__(self):
        self.variables = {}
        self.method_dict = {}

    def set_return_value(self, value):
        self.variables['gxi'] = value

    def get_return_value(self):
        return self.variables['gxi']

    def has_return_value(self):
        return 'gxi' in self.variables


class MockClock(object):
    def __init__(self):
        self.current_time = int(0)

    def increase_time(self, amount):
        self.current_time += int(amount)

    def get_current_time(self):
        return self.current_time
