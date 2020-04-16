
mu_constant = """
To constantify numbers means:
    Return 0
End.
"""

mu_successor = """
To successor a number means:
    return number+1.
End.
"""

mu_projection = """
To project numbers and i means:
    return the first of the numbers.
End.
"""

mu_composition = """
To compose routines and numbers means:
    Arguments is a new list
    and then assign 1 to i
    and then while i ≤ the length of routines do:
        calculation is the ith of routines
        and then calculate numbers
        and then arguments = arguments + it
        and then i += 1.
    End.
    And then Subcompose the arguments
    and then return it.
End.
"""

mu_recursion = """
To recurse first, second, other and numbers means:
    If the other is greater than 0 then:
        Assign other - 1 to other
        And then recurse first, second, other, and numbers.
        And then second-ate other, radio's channel and numbers.
        And then assign other + 1 to other.
    Else:
        first-ate of numbers.
        Assign the light-bulb's brightness to the radio's channel.
    End.
End
"""

mu_minimization = """
To minimize a function and numbers means:
    assign 0 to Light-bulb's brightness
    and then function-ate the
        light-bulb's brightness and numbers
    and then while the radio channel is not equal to 0 then:
        Assign light-bulb's brightness +1 to light-bulb's brightness
        and then function-ate the light-bulb brightness and numbers.
    End.
End.
"""

silly_name_generator = """
To sillyname means:
    Announce "what is your lucky number?"
    and then listen for lucky number
    and then announce "what is your favourite color?"
    and then listen for favourite color
    and then assign favourite color + lucky number to silly name
    and then announce "your silly name is"
    and then announce silly name.
End.
"""

sequenced_announcements = """Announce "hello" after 6 seconds
                    and then announce "bye" after 4 seconds"""

triggered_announcements = """
Announce "turned on" every time the light activates
and then announce "open".
"""

nightify_routine = """
To nightify means:
    Lock the front door
    and then close the window blinds
    and then activate the lights
End.
"""

water_heat_alert = """
Assign red to the color of the office lamp
    whenever the current temperature of the boiler is greater than forty.
"""

pretend_to_be_home = """
To pretend means:
    Randomize time between 0700 and 0800
    and then assign it into first time
    and then Randomize time between 2100 and 2200
    and then assign it into second time
    And then activate the television at the first time
    And then Turn off the television at the second time
End.
"""

discipline_child = """
to ground a child means
    lock the door of the child's room
    and then announce "you are grounded.
        Now think about your bad behaviour"
end.
"""

morning_routine = """
To morningify means
    report the weather
    and then report the traffic to Haifa
end.
"""

randomize_light = """
to random light means
    randomize number up to the quantity of lights
    and then assign it to light number
    and then randomize color
    and then assign it to light color
    and then randomize number between 70 and 100
    and then assign it to light brightness
    and then change the color of the light numberth light to light color 
        at the same time change the brightness of
            the light numberth light to light brightness
end
"""

party_routine = """
to party means
    change the color of the lights to red
    and then play my favourite songs
    and then random light every second
        at the same time whenever the front door opens
            announce "a new guest arrived"
end
"""

sunrise = """
to sunrise means
    change the color of the bedroom lamp to blue
    and then for brightness index between 1 and 100 do
        assign brightness index to the brightness of
            the bedroom lamp in 5 seconds
    end
end
"""

