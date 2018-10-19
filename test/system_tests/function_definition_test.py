import math

import pytest

import compilation.abstract_syntax_tree as ast_bld
import compilation.esp_lexer as lxr
import library.management_components as esk

lxr.build()


class ProvidedAstUpToFunctionDefinitionLevel(object):
    @staticmethod
    def evaluate_and_return_state(ast, statement, initial_state=None):
        if initial_state is None:
            initial_state = esk.Domsagxo()  # so as not to put a mutable default
        ast_parse = ast.parse(statement)
        state, nothing = ast_parse.evaluate(initial_state)
        return state

    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.Var.FUNCTION_DEFINITION.value)


def all_true(argument_list):
    for arg in argument_list:
        if not arg:
            return False
    return True


def is_prime(number):
    if number <= 1:
        return False
    return all_true([number % i for i in range(2, int(math.sqrt(number)) + 1)])


class TestDefinitionAndActivationOfFunctions(ProvidedAstUpToFunctionDefinitionLevel):

    def test_canDefineNoneReturningFunction(self, ast):
        number_of_predefined_functions = len(esk.Domsagxo().method_dict)
        new_state = self.evaluate_and_return_state(ast,
                                                   '''sxambaluli signifas revenu nenio. finu''')
        assert number_of_predefined_functions + 1 == len(new_state.method_dict)
        assert 'sxambalulu' in new_state.method_dict.keys()
        assert new_state.method_dict['sxambalulu']() is None

    def test_canDefineUnitFunctionReturningItsArgument(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''diri sxambalulo signifas revenu sxambalulo. finu''')
        assert 1 == new_state.method_dict['diru'](1)

    def test_cannotPassMoreArgumentsThanPlannedToUserDefinedFunction(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''memori sxambalulo signifas revenu sxambalulo. finu''')
        with pytest.raises(TypeError):
            new_state.method_dict['memoru'](1, 2)

    def test_functionArgumentsDoNotMigrateBetweenFunctions(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''forgesi sxambalulo signifas revenu nenio. finu''')
        new_state = self.evaluate_and_return_state(
            ast, '''memori signifas revenu sxambalulo. finu''', new_state)
        assert new_state.method_dict['forgesu'](100) is None
        with pytest.raises(NameError):
            assert new_state.method_dict['memoru']()

    def test_functionsDoNotOverwriteEachOthersArguments(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''
            rekursi sxambalulo signifas
                se sxambalulo estas egala al nul tiam
                    revenu nul.
                alie
                    rekurso estas rekursu sxambalulo malpli unu.
                    revenu sxambalulo pli rekurso.
                finu.
            finu''')
        assert 6 == new_state.method_dict['rekursu'](3)

    def test_canDefineMuConstantFunction(self, ast):
        """the Mu-recursive constant function has a predefined constant n which it always returns.
        The function is simply: f(x) = n."""
        new_state = self.evaluate_and_return_state(
            ast, '''forgesi sxambalulo, hundo kaj kato signifas revenu sep. finu''')
        assert 7 == new_state.method_dict['forgesu'](10, 809, 341)

    def test_canDefineMuSuccessorFunction(self, ast):
        """the Mu-recursive successor function recieves an argument and returns it's successor.
        Basically, it's just f(x) = x+1."""
        new_state = self.evaluate_and_return_state(
            ast, '''diri sxambalulo signifas revenu sxambalulo pli unu. finu''')
        assert 42 == new_state.method_dict['diru'](41)

    def test_canDefineTheMuProjectionFunction(self, ast):
        """the Mu-recursive projection function (also called the identity function)
        is a function that receives k inputs and returns the ith input without change.
        In this specific example, we accept 2 inputs and return the first one."""
        new_state = self.evaluate_and_return_state(
            ast, '''elekti hundo kaj kato signifas revenu hundo. finu''')
        assert 1 == new_state.method_dict['elektu'](1, 2)

    def test_canDefineMuCompositionOperator(self, ast):
        """the Mu-recursive composition operator receives an m-ary function (h)
        and m k-ary functions (g_1, ..., g_m).
        The result of the composition is a k-ary function (f) such thath:
        f(x_1, ..., x_k) = h(g_1(x_1, ..., x_k), ..., g_m(x_1, ..., x_k))"""
        new_state = self.evaluate_and_return_state(
            ast, '''trienigi hundo kaj kato kaj muso signifas hundo fojoj kato fojoj muso. finu''')
        new_state = self.evaluate_and_return_state(
            ast, '''hundi unuo kaj duo signifas revenu unuo fojoj duo. finu''', new_state)
        new_state = self.evaluate_and_return_state(
            ast, '''kati unuo kaj duo signifas revenu unuo pli duo. finu''', new_state)
        new_state = self.evaluate_and_return_state(
            ast, '''musi unuo kaj duo signifas revenu unuo malpli duo. finu''', new_state)
        new_state = self.evaluate_and_return_state(
            ast, '''sxambaluli oro kaj argxento signifas
            hundo estas hundu oro kaj cent.
            kato estas katu argxento kaj kvin.
            muso estas musu argxento kaj oro. 
            revenu trienigu hundo, kato kaj muso. finu''', new_state)
        assert (2 * 100) * (5 + 5) * (5 - 2) == new_state.method_dict['sxambalulu'](2, 5)

    def test_canDefineMuPrimitiveRecursionOperator(self, ast):
        """the Mu-recursive primitive recursion operator receives a k-ary function (g)
        and a k+2-ary function (h).
        The result of the recursion is a k+1-ary function (f) following this specification:
        f(0, x_1, ..., x_k) = g(x_1, ..., x_k)
        f(y+1, x_1, ..., x_k) = h(y, f(y, x_1, ..., x_k), x_1, ..., x_k)
        I am not intuitively sure why anyone would want to have such a function, but even though
        it seems senseless, it's mathematical power is evident.
        """
        new_state = self.evaluate_and_return_state(
            ast, '''duenigi unuo kaj duo signifas revenu unuo fojoj duo. finu''')
        new_state = self.evaluate_and_return_state(
            ast, '''kvarenigi unuo, duo, trio kaj kvaro signifas
            revenu kvaro fojoj unuo fojoj unuo pli trio fojoj unuo pli duo. finu''', new_state)
        new_state = self.evaluate_and_return_state(
            ast, '''trienigi oro, argxento kaj kupro signifas
                    se oro estas egala al nul tiam
                        duenigu argxento kaj kupro.
                    finu.
                    rekursajxo estas trienigu oro malpli unu, argxento kaj kupro.
                    revenu kvarenigu oro malpli unu, rekursajxo, argxento kaj kupro. finu''',
            new_state)

        two_input_function_result = new_state.method_dict['duenigu'](2, 5)
        four_input_function_result = \
            new_state.method_dict['kvarenigu'](0, two_input_function_result, 2, 5)

        assert two_input_function_result == new_state.method_dict['trienigu'](0, 2, 5)
        assert four_input_function_result == new_state.method_dict['trienigu'](1, 2, 5)

    def test_canDefineMuMinimisationOperator(self, ast):
        """the Mu-recursive minimisation operator receives a k+1-ary function (g).
        The result of the recursion is a k+1-ary function (f) following this specification:
        f(z, x_1, ..., x_k) = z <-> g(z, x_1, ..., x_k) = 0 and
                                    g(i, x_1, ..., x_k) > 0 forall i in [0, z-1]
        (recall that these functions apply to natural numbers and return natural numbers)
        Intuitively, minimisation seeks—beginning the search from 0 and proceeding upwards—the
        smallest
        argument that causes the function to return zero; if there is no such argument, the search
        never terminates.

        in this case, I'll implement the function unuenigu that is the parabole: -x^2 + 2x +3
        which returns zero when x is 3
        """
        new_state = self.evaluate_and_return_state(
            ast, '''unuenigi unuo signifas revenu malpli unuo fojoj unuo pli du fojoj
            unuo pli tri. finu''')
        new_state = self.evaluate_and_return_state(
            ast, '''minimumigi signifas
                    nombro estas nul.
                    rezulto estas unuenigu nombro.
                    dum rezulto ne estas egala al nul tiam
                        nombro estas nombro pli unu.
                        rezulto estas unuenigu nombro.
                    finu.
                    revenu nombro.
                    finu''', new_state)

        for i in range(0, 3):
            assert 0 < new_state.method_dict['unuenigu'](i)
        assert 0 == new_state.method_dict['unuenigu'](3)
        assert 3 == new_state.method_dict['minimumigu']()

    def test_canDefineFunctionToTellIfANumberIsPrime(self, ast):
        """yogi was sassy and said I could not tell the prime numbers apart even if I tried.
        So let's check, since I bet I can do that with several simple functions."""
        new_state = self.evaluate_and_return_state(
            ast, '''cxuprimi nombro signifas
                unua indekso estas du.
                dum unua indekso fojoj unua indekso ne estas pli granda ol nombro tiam
                    dua indekso estas unua indekso.
                    dum unua indekso fojoj dua indekso ne estas pli granda ol nombro tiam
                        se unua indekso fojoj dua indekso estas egala al nombro tiam
                            revenu malvero.
                        finu.
                        dua indekso estas dua indekso pli unu.
                    finu.
                    unua indekso estas unua indekso pli unu.
                finu.
                revenu vero.
                finu''')

        for i in range(2, 100):
            assert is_prime(i) == new_state.method_dict['cxuprimu'](i)

    def test_assumingICanDifferentiateAPrimeICanFindAllPrimesUpTo100(self, ast):
        """For the sake of test independence I will program the prime chekcer in python.
        also, the printing part itself will feed a list and the list will be verified."""
        self.prime_list = []

        def add_prime_to_list(additional_prime):
            self.prime_list += [additional_prime]

        new_state = self.evaluate_and_return_state(
            ast, '''primumi signifas
                indekso estas du.
                dum indekso ne estas pli granda ol cent tiam
                    se cxuprimu indekso tiam
                        presu indekso.
                    finu.
                    indekso estas indekso pli unu.
                finu.
                finu''')
        new_state.method_dict['cxuprimu'] = is_prime
        new_state.method_dict['presu'] = add_prime_to_list
        new_state.method_dict['primumu']()

        assert 25 == len(self.prime_list)
        for prime in self.prime_list:
            square_root = math.sqrt(prime)
            assert int(square_root) != square_root
