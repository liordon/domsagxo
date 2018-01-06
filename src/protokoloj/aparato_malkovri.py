import os

rooms = ["mia chambro", "la necesejo"]

def outputMethod(output):
    print(output)
    os.system('espeak -v eo+f3 "'+output+'"')


outputMethod("mi trovis novan aparaton! mi nomigxis gin gxon doux, "
      "cxu vi volas ke mi sxaltos gxi?")
answer = input("")
if (answer == "jes" ):
    outputMethod("mi sxaltas gxin, en kiu cxambro gxi estas?")

    answer = input("")
    while (answer not in rooms):
        outputMethod("pardonu min... mi ne sciis tio cxambro, bonvolu provu denove")
        answer = input("")

    room = answer
    outputMethod("gxon doux estas en " + room + ", cxu vi volas sxangxi gxian nomon?")
    answer = input("")

    if answer.startswith("jes"):
        answer = answer[5:]
        if answer.startswith("nomigxu gxin"):
            answer = answer[12:]
        while (len(answer) == 0):
            outputMethod("kiel vi volas nomigxi gxin?")
            answer = input("")
        name = answer

        outputMethod("kompreneble, " + name + " estas en " + room)