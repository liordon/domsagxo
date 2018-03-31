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
            ast, '''sxambaluli sxambalulo tiel revenu sxambalulo. finu''')
        assert 1 == new_state.method_dict['sxambalulu']([1])

    def test_cannotPassMoreArgumentsThanPlannedToUserDefinedFunction(self, ast):
        with pytest.raises(TypeError):
            new_state = self.evaluate_and_return_state(
                ast, '''sxambaluli sxambalulo tiel revenu sxambalulo. finu''')
            assert 1 == new_state.method_dict['sxambalulu']([1,2])
