import math

import pytest

import library.atomic_types as atypes
import library.management_components as mgmt_cmp
from test_utils.providers import FunctionDefinitionLevelAstProvided, evaluate_and_return_state, \
    ProvidedSmartHomeWithLightBulb


def all_true(argument_list):
    for arg in argument_list:
        if not arg:
            return False
    return True


def is_prime(number):
    if number <= 1:
        return False
    return all_true([number % i for i in range(2, int(math.sqrt(number)) + 1)])


class TestDefinitionAndActivationOfRoutines(FunctionDefinitionLevelAstProvided,
    ProvidedSmartHomeWithLightBulb):

    def test_canDefiningSimpleReturningRoutineAddsItToMethodDict(self, ast):
        new_state = evaluate_and_return_state(ast,
            '''sxambaluli signifas revenu finu''')
        assert 1 == len(new_state.method_dict)
        assert 'sxambalulu' in new_state.method_dict.keys()

    def test_canDefiningRoutineReturningValueAddsItToMethodDict(self, ast):
        new_state = evaluate_and_return_state(ast,
            '''sxambaluli signifas revenu nul finu''')
        assert 1 == len(new_state.method_dict)
        assert 'sxambalulu' in new_state.method_dict.keys()

    def test_returnValueIsSavedInVariableItAfterFunctionCall(self, ast):
        new_state = evaluate_and_return_state(ast,
            '''sxambaluli signifas revenu nul finu''')
        new_state.method_dict['sxambalulu']()
        assert new_state.variables['gxi'] == 0

    def test_cannotPassMoreArgumentsThanPlannedToUserDefinedFunction(self, ast):
        new_state = evaluate_and_return_state(
            ast, '''memori sxambalulo signifas revenu finu''')
        with pytest.raises(TypeError):
            new_state.method_dict['memoru'](1, 2)

    def test_functionArgumentsDoNotMigrateBetweenFunctions(self, ast):
        new_state = evaluate_and_return_state(
            ast, '''forgesi sxambalulo signifas revenu finu''')
        new_state = evaluate_and_return_state(
            ast, '''memori signifas asignu sxambalulo al kato finu''', new_state)
        new_state.method_dict['forgesu'](100)
        # forgesi now knows a variable called sxambalulo
        with pytest.raises(NameError):
            new_state.method_dict['memoru']()
            # memori doesn't know about sxambalulo

    def test_routinesCanChangeApplianceState(self, ast, smart_home):
        new_state = evaluate_and_return_state(
            ast, '''sxangxi signifas asignu kvardek al brilo de sxambalulo finu''', smart_home)
        new_state.method_dict['sxangxu']()
        assert 40 == new_state.variables["sxambalulo"].properties["brilo"]

    def test_routinesCanAccessAppliancesDefinedAfterThemselves(self, ast):
        initial_state = mgmt_cmp.Domsagxo()
        new_state = evaluate_and_return_state(
            ast, '''sxangxi signifas asignu kvardek al brilo de sxambalulo finu''', initial_state)
        new_state.addAppliance(
            appliance=atypes.Appliance(atypes.ApplianceTypes.LIGHT, "sxambalulo"))
        new_state.method_dict['sxangxu']()
        assert 40 == new_state.variables["sxambalulo"].properties["brilo"]

    def test_routinesDoNotOverwriteEachOthersArguments(self, ast):
        new_state = evaluate_and_return_state(
            ast, '''
            rekursi sxambalulo signifas
                se sxambalulo estas egala al nul tiam
                    revenu
                alie
                    rekursu sxambalulo malpli unu
                    poste asignu sxambalulo fojoj dek al brilo de sxambalula de sxambaluloj
                    poste revenu
                finu
            finu''')
        new_state.variables["sxambaluloj"] = [
            atypes.Appliance(
                atypes.ApplianceTypes.LIGHT, "unuo"),
            atypes.Appliance(
                atypes.ApplianceTypes.LIGHT, "duo"),
            atypes.Appliance(
                atypes.ApplianceTypes.LIGHT, "trio"),
        ]
        new_state.method_dict['rekursu'](3)
        for i in range(len(new_state.variables["sxambaluloj"])):
            assert 10 * (i + 1) == new_state.variables["sxambaluloj"][i].properties["brilo"]

    def test_canDefineMuConstantFunction(self, ast, smart_home):
        """the Mu-recursive constant function has a predefined constant n which it always returns.
        The function is simply: f(x) = n."""
        new_state = evaluate_and_return_state(
            ast, '''konstanti hundo, kato kaj muso signifas
                asignu sep al brilo de sxambalulo finu''', smart_home)
        new_state.method_dict['konstantu'](10, 809, 341)
        assert 7 == new_state.variables['sxambalulo'].properties["brilo"]

    def test_canDefineMuSuccessorFunction(self, ast, smart_home):
        """the Mu-recursive successor function recieves an argument and returns it's successor.
        Basically, it's just f(x) = x+1."""
        new_state = evaluate_and_return_state(
            ast, '''posteuli nombro signifas 
                asignu nombro pli unu al brilo de sxambalulo finu''', smart_home)
        new_state.method_dict['posteulu'](41)
        assert 42 == new_state.variables['sxambalulo'].properties["brilo"]

    def test_canDefineTheMuProjectionFunction(self, ast, smart_home):
        """the Mu-recursive projection function (also called the identity function)
        is a function that receives k inputs and returns the ith input without change.
        In this specific example, we accept 2 inputs and return the first one."""
        new_state = evaluate_and_return_state(
            ast, '''elekti hundo kaj kato signifas 
            asignu hundo al brilo de sxambalulo finu''', smart_home)
        new_state.method_dict['elektu'](31, 42)
        assert 31 == new_state.variables['sxambalulo'].properties["brilo"]

    def test_canDefineFunctionToTellIfANumberIsPrime(self, ast, smart_home):
        """yogi was sassy and said I could not tell the prime numbers apart even if I tried.
        So let's check, since I bet I can do that with several simple functions."""
        new_state = evaluate_and_return_state(
            ast, '''cxuprimi nombro signifas
                asignu du al unua indekso 
                poste dum unua indekso fojoj unua indekso 
                        ne estas pli granda ol nombro tiam
                    asignu unua indekso al dua indekso
                    poste dum unua indekso fojoj dua indekso 
                            ne estas pli granda ol nombro tiam
                        se unua indekso fojoj dua indekso estas egala al nombro tiam
                            malsxaltu sxambalulon
                            poste revenu
                        finu
                        poste asignu dua indekso pli unu al dua indekso
                    finu
                    poste asignu unua indekso pli unu al unua indekso
                finu
                poste sxaltu sxambalulon
                finu''', smart_home)

        for i in range(2, 100):
            new_state.method_dict['cxuprimu'](i)
            assert is_prime(i) == new_state.variables['sxambalulo'].isTurnedOn()

    def test_assumingICanIdentifyAPrimeICanFindAllPrimesUpTo100(self, ast, smart_home):
        """For the sake of test independence I will program the prime chekcer in python.
        also, the printing part itself will feed a list and the list will be verified."""
        self.prime_list = []

        def add_prime_to_list(additional_prime):
            self.prime_list += [additional_prime]

        new_state = evaluate_and_return_state(
            ast, '''primumi signifas
                asignu du al indekso
                poste dum indekso ne estas pli granda ol cent tiam
                    cxuprimu indekso
                    poste se sxambalulo sxaltas tiam
                        sxaltu indeksa de sxambaluloj
                        poste presu indekso
                    alie
                        malsxaltu indeksa de sxambaluloj
                    finu
                    poste asignu indekso pli unu al indekso
                finu
                finu''', smart_home)

        def turn_light_on_if_prime(number):
            if is_prime(number):
                new_state.variables['sxambalulo'].turnOn()
            else:
                new_state.variables['sxambalulo'].turnOff()

        new_state.variables['sxambalulo'] = \
            atypes.Appliance(atypes.ApplianceTypes.LIGHT, 'sxambalulo')
        new_state.variables['sxambaluloj'] = []
        for i in range(100):
            new_state.variables['sxambaluloj'] += \
                [atypes.Appliance(atypes.ApplianceTypes.LIGHT, i)]
        new_state.method_dict['cxuprimu'] = turn_light_on_if_prime
        new_state.method_dict['presu'] = add_prime_to_list
        new_state.method_dict['primumu']()

        assert 25 == len(self.prime_list)
        for i in range(2, 100):
            assert is_prime(i) == new_state.variables['sxambaluloj'][i - 1].isTurnedOn()
            assert is_prime(i) == (i in self.prime_list)
