import biblioteko.atomaj_tipoj as tipo
import biblioteko.estra_komponantoj as esk
import kompilajxo.leksisto as lxr
import kompilajxo.abstrakta_sintaksarbo as ast_bld
import kompilajxo.nodo as Node
import pytest

lxr.build()


class ProvidedAstUpToProgramLevel(object):
    @staticmethod
    def evaluate_and_return_state(ast, statement, initial_state=None):
        if initial_state is None:
            initial_state = esk.Domsagxo()  # so as not to put a mutable default
        ast_parse = ast.parse(statement)
        state, nothing = ast_parse.evaluate(initial_state)
        return state

    @pytest.fixture
    def ast(self):
        return ast_bld.build(start="funcDef")


class TestDefinitionAndActivationOfFunctions(ProvidedAstUpToProgramLevel):

    def test_canDefineNoneReturningFunction(self, ast):
        num_predef_funcs = len(esk.Domsagxo().method_dict)
        new_state = self.evaluate_and_return_state(ast, '''sxambaluli tiel revenu nenio. finu''')
        assert num_predef_funcs + 1 == len(new_state.method_dict)
        assert 'sxambalulu' in new_state.method_dict.keys()
        assert new_state.method_dict['sxambalulu']([]) is None

    def test_canDefineUnitFunctionReturningItsArgument(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''diri sxambalulo tiel revenu sxambalulo. finu''')
        assert 1 == new_state.method_dict['diru']([1])

    def test_cannotPassMoreArgumentsThanPlannedToUserDefinedFunction(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''memori sxambalulo tiel revenu sxambalulo. finu''')
        with pytest.raises(TypeError):
            new_state.method_dict['memoru']([1, 2])

    def test_functionArgumentsDoNotMigrateBetweenFunctions(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''forgesi sxambalulo tiel revenu nenio. finu''')
        new_state = self.evaluate_and_return_state(
            ast, '''memori tiel revenu sxambalulo. finu''', new_state)
        assert new_state.method_dict['forgesu']([100]) is None
        assert "sxambalulo" == new_state.method_dict['memoru']([])

    def test_functionsDoNotOverwriteEachOthersArguments(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''
            rekursi sxambalulo tiel
                se sxambalulo estas nul tiam
                    revenu nul.
                alie
                    rekurso estas rekursu sxambalulo-1.
                    revenu sxambalulo + rekurso.
                finu.
            finu''')
        assert 6 == new_state.method_dict['rekursu']([3])

    def test_canDefineMuConstantFunction(self, ast):
        """the Mu-recursive constant function has a predefined constant n which it always returns.
        The function is simply: f(x) = n."""
        new_state = self.evaluate_and_return_state(
            ast, '''forgesi sxambalulo, hundo kaj kato tiel revenu sep. finu''')
        assert 7 == new_state.method_dict['forgesu']([10, 809, 341])

    def test_canDefineMuSuccessorFunction(self, ast):
        """the Mu-recursive successor function recieves an argument and returns it's successor.
        Basically, it's just f(x) = x+1."""
        new_state = self.evaluate_and_return_state(
            ast, '''diri sxambalulo tiel revenu sxambalulo+1. finu''')
        assert 42 == new_state.method_dict['diru']([41])

    def test_canDefineTheMuProjectionFunction(self, ast):
        """the Mu-recursive projection function (also called the identity function)
        is a function that receives k inputs and returns the ith input without change.
        In this specific example, we accept 2 inputs and return the first one."""
        new_state = self.evaluate_and_return_state(
            ast, '''elekti hundo kaj kato tiel revenu hundo. finu''')
        assert 1 == new_state.method_dict['elektu']([1, 2])

    def test_canDefineMuCompositionOperator(self, ast):
        """the Mu-recursive composition operator receives an m-ary function (h)
        and m k-ary functions (g_1, ..., g_m).
        The result of the composition is a k-ary function (f) such thath:
        f(x_1, ..., x_k) = h(g_1(x_1, ..., x_k), ..., g_m(x_1, ..., x_k))"""
        new_state = self.evaluate_and_return_state(
            ast, '''trienigi hundo kaj kato kaj muso tiel hundo*kato*muso. finu''')
        new_state = self.evaluate_and_return_state(
            ast, '''hundi unuo kaj duo tiel revenu unuo*duo. finu''', new_state)
        new_state = self.evaluate_and_return_state(
            ast, '''kati unuo kaj duo tiel revenu unuo+duo. finu''', new_state)
        new_state = self.evaluate_and_return_state(
            ast, '''musi unuo kaj duo tiel revenu unuo-duo. finu''', new_state)
        new_state = self.evaluate_and_return_state(
            ast, '''sxambaluli oro kaj argxento tiel
            hundo estas hundu oro kaj cent.
            kato estas katu argxento kaj kvin.
            muso estas musu argxento kaj oro. 
            revenu trienigu hundo, kato kaj muso. finu''', new_state)
        assert (2 * 100) * (5 + 5) * (5 - 2) == new_state.method_dict['sxambalulu']([2, 5])

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
            ast, '''duenigi unuo kaj duo tiel revenu unuo*duo. finu''')
        new_state = self.evaluate_and_return_state(
            ast, '''kvarenigi unuo, duo, trio kaj kvaro tiel
            revenu kvaro*unuo*unuo + trio*unuo + duo. finu''', new_state)
        new_state = self.evaluate_and_return_state(
            ast, '''trienigi oro, argxento kaj kupro tiel
                    se oro estas nul tiam
                        duenigu argxento kaj kupro.
                    finu.
                    rekursajxo estas trienigu oro-1, argxento kaj kupro.
                    revenu kvarenigu oro-1, rekursajxo, argxento kaj kupro. finu''', new_state)

        two_input_function_result = new_state.method_dict['duenigu']([2, 5])
        four_input_function_result = \
            new_state.method_dict['kvarenigu']([0, two_input_function_result, 2, 5])

        assert two_input_function_result == new_state.method_dict['trienigu']([0, 2, 5])
        assert four_input_function_result == new_state.method_dict['trienigu']([1, 2, 5])
