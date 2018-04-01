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

    def test_canDefineConstantFunctionReturning1(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''forgesi sxambalulo, hundo kaj kato tiel revenu sep. finu''')
        assert 7 == new_state.method_dict['forgesu']([10, 809, 341])

    def test_canDefineProjectionFunctionThatReceives2argumentsButReturnsTheFirst(self, ast):
        new_state = self.evaluate_and_return_state(
            ast, '''elekti hundo kaj kato tiel revenu hundo. finu''')
        assert 1 == new_state.method_dict['elektu']([1, 2])

    def test_canComposeA3InputFunctionOn3FunctionsWith2InputsAndReceiveA2InputFunction(self, ast):
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
        assert (2*100)*(5+5)*(5-2) == new_state.method_dict['sxambalulu']([2, 5])
