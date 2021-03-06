from enum import Enum


class DomsagxoPrograms(Enum):
    """ The notation here is mostly verbal, though sometimes I'll use the
    shorter notation to stay parallel to the english writing. These programs
    are used in tests that expect 1:1 correlation between the english and
    esperanto version of each script."""

    mu_constant = """Konstanti nombroj signifas Revenu 0 Finu"""

    mu_successor = """posteuli nombro signifas
                revenu nombro + 1 finu"""

    mu_projection = """projekci nombroj kaj indekso signifas
            revenu la indeksa de la nombroj finu"""

    silly_name_generator = """
                sensencnomi signifas
                    Anoncu citilo kio estas via
                        bonsxanca nombro malcitilo
                    poste atentu bonsxanca nombro
                    poste anoncu citilo kio estas via
                        sxatata koloro malcitilo
                    poste atentu sxatata koloro
                    poste asignu sxatata koloro +
                        bonsxanca nombro al sensenca nomo
                    poste anoncu citilo via sensenca nomo
                        estas malcitilo
                    poste anoncu sensenca nomo
                finu """

    square_function = """
Kvadrati nombron signifas
    Asignu nombro * nombro al
        la temperaturo de la forno
Finu
"""

    prime_seeking_routine = """
Cxuprimi nombron signifas
   Asignu 2 al unuo
   poste kvadratu unuo
   poste dum la temperaturo de la forno ne ≥ nombro tiam
     Asignu unuo al duo
     poste dum unuo * duo ne ≥ nombro tiam
       Se unuo * duo estas
           egala al nombro tiam
         Malsxaltu la ampolon
         poste revenu
       Finu
       Poste asignu duo + 1 al duo
     Finu
     Poste asignu unuo + 1 al unuo
     poste kvadratu unuo
   Finu
   Poste sxaltu la ampolon
Finu"""

    search_primes_forever = """
Asignu du al indekso
poste dum vero tiam
   cxuprimu indekso
   poste se la ampolo sxaltas tiam
     Anoncu indekso
   Finu
   Poste asignu indekso pli unu al indekso
Finu
"""


class EngluentoPrograms(Enum):
    mu_constant = """
    To constantify numbers means:
        Return 0
    End.
    """

    mu_successor = """
    To successor a number means:
        return number + 1.
    End.
    """

    mu_projection = """
    To project numbers and index means:
        return the indexth of the numbers.
    End.
    """

    mu_composition = """
    To compose routines and numbers means:
        Arguments is a new list
        afterwards assign 1 to i
        afterwards while i ≤ the length of routines do:
            calculation is the ith of routines
            afterwards calculate numbers
            afterwards assign arguments + it to arguments
            afterwards assign i + 1 to i.
        End.
        afterwards Subcompose the arguments
        afterwards return it.
    End.
    """

    mu_recursion = """
    To recurse first, second, other and numbers means:
        If the other is greater than 0 then:
            Assign other - 1 to other
            afterwards recurse first, second, other, and numbers.
            afterwards second-ate other, radio's channel and numbers.
            afterwards assign other + 1 to other.
        Else:
            first-ate of numbers.
            Assign the light-bulb's brightness to the radio's channel.
        End.
    End
    """

    mu_minimization = """
    To minimize a function and numbers means:
        assign 0 to Light-bulb's brightness
        afterwards function-ate the
            light-bulb's brightness and numbers
        afterwards while the radio channel is not equal to 0 then:
            Assign light-bulb's brightness +1 to light-bulb's brightness
            afterwards function-ate the light-bulb brightness and numbers.
        End.
    End.
    """

    silly_name_generator = """
    To sillyname means:
        Announce "what is your lucky number?"
        afterwards listen lucky number
        afterwards announce "what is your favourite color?"
        afterwards listen favourite color
        afterwards assign favourite color + lucky number to silly name
        afterwards announce "your silly name is"
        afterwards announce silly name.
    End.
    """

    sequenced_announcements = """Announce "hello" after 6 seconds
                        afterwards announce "bye" after 4 seconds"""

    triggered_announcements = """
    Announce "turned on" every time the light activates
    afterwards announce "open".
    """

    nightify_routine = """
    To nightify means:
        Lock the front door
        afterwards close the window blinds
        afterwards activate the lights
    End.
    """

    water_heat_alert = """
    Assign red to the color of the office lamp
        whenever the current temperature of the boiler is greater than forty.
    """

    pretend_to_be_home = """
    To pretend means:
        Randomize time between 0700 and 0800
        afterwards assign it to first time
        afterwards Randomize time between 2100 and 2200
        afterwards assign it to second time
        afterwards activate the television at the first time
        afterwards Turn off the television at the second time
    End.
    """

    discipline_child = """
    to ground a child means
        lock the door of the child's room
        afterwards announce "you are grounded.
            Now think about your bad behaviour"
    end.
    """

    morning_routine = """
    To morningify means
        report the weather
        afterwards report the traffic to Haifa
    end.
    """

    randomize_light = """
    to random light means
        randomize number up to the quantity of lights
        afterwards assign it to light number
        afterwards randomize color
        afterwards assign it to light color
        afterwards randomize number between 70 and 100
        afterwards assign it to light brightness
        afterwards change the color of the light numberth light to light color
            at the same time change the brightness of
                the light numberth light to light brightness
    end
    """

    party_routine = """
    to party means
        change the color of the lights to red
        afterwards play my favourite songs
        afterwards random light every second
            at the same time whenever the front door opens
                announce "a new guest arrived"
    end
    """

    sunrise = """
    to sunrise means
        change the color of the bedroom lamp to blue
        afterwards assign 1 to brightness-index
        afterwards while brightness-index is lesser
                or equal to 100 then
            assign brightness index to the brightness of
                the bedroom lamp in 5 seconds
            afterwards assign brightness-index + 1
                to brightness-index
        end
    end
    """

    square_function = """
To square a number means:
	Assign number * number to
		the temperature of the oven.
End.
"""

    prime_seeking_routine = """
To check a number means:
	Assign 2 to first.
	afterwards square first.
	afterwards While the temperature of the oven not ≥ number then:
		Assign first to second.
		afterwards While first * second not ≥ number then:
			If first * second is
					equal to number then:
				deactivate the light.
				afterwards Return.
			End.
			afterwards Assign second + 1 to second.
		End.
		afterwards Assign first + 1 to first.
		afterwards Square first.
	End.
	afterwards activate the light.
End.
"""

    search_primes_forever = """
Assign two to index.
While true then
	Check index.
	If the light-bulb is on then
		Announce index.
	End.
	Add 1 to index.
End."""
