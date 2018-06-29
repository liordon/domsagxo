class Bunch:
    def __init__(self, **keywords):
        self.__dict__.update(keywords)


class MockClock(object):
    def __init__(self):
        self.current_time = int(0)

    def increase_time(self, amount):
        self.current_time += int(amount)

    def get_current_time(self):
        return self.current_time
