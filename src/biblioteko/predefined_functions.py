from random import randrange

def generateRandom(argList):
    if len(argList) > 1:
        return randrange(argList[1],argList[2])
    else:
        return randrange(100)

method_dict = {"hazardu":generateRandom}