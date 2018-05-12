class MockClock(object):
    def __init__(self):
        self.current_time = 0

    def increase_time(self, amount):
        self.current_time += amount

    def get_current_time(self):
        return self.current_time
