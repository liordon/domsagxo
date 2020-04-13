import datetime

import pytest

import compilation.abstract_syntax_tree as ast_bld
import compilation.esperanto_lexer as eo_lxr
from compilation.definitions import PartOfSpeech
from english_prototype.english_lexer import BeamToken
from library import management_components as mgmt_cmp, atomic_types as atypes
from library.management_components import Horaro, Domsagxo
from test_utils import mocks
from test_utils.mocks import MockClock


class PartOfSpeechVerifier(object):
    @staticmethod
    def assertPartOfSpeechForGivenToken(partOfSpeech, token):
        assert partOfSpeech.value == token.type

    @staticmethod
    def assertPartOfSpeechForNextTokenOfLexer(partOfSpeech, lexer):
        assert partOfSpeech.value == lexer.token().type

    @staticmethod
    def assertOnePossiblePartOfSpeechForNextTokenOfLexer(partOfSpeech, lexer):
        assert partOfSpeech.value in lexer.token().tags.keys()


def evaluate_and_return_state(ast, statement, initial_state=None):
    if initial_state is None:
        initial_state = mocks.Bunch(variables={}, method_dict={})  # so as not to put a mutable default
    state, nothing = ast.parse(statement).evaluate(initial_state)
    return state


def evaluate_and_return_state_variables(ast, statement, initial_state=None):
    return evaluate_and_return_state(ast, statement, initial_state).variables


class EsperantoLexerProvided(PartOfSpeechVerifier):
    @pytest.fixture
    def lexer(self):
        return eo_lxr.build()


class PartialNameLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.GrammarVariable.PARTIAL_NAME.value)


class ExpressionLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.GrammarVariable.EXPRESSION.value)


class FunctionDefinitionLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.GrammarVariable.ROUTINE_DEFINITION.value)


class StatementLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.GrammarVariable.STATEMENT.value)


class MockSmartHomeStateVariablesProvided(object):
    @pytest.fixture
    def state(self):
        return mocks.Bunch(variables={}, method_dict={})


class TimeManagerWithSimulativeClockProvided(object):

    @pytest.fixture
    def scd(self):
        simulative_time = MockClock()
        return Horaro(time_function=simulative_time.get_current_time,
            delay_function=simulative_time.increase_time)

    @pytest.fixture
    def increaser(self):
        self.counter = 0

        def increase_counter(num=1):
            self.counter += num

        return increase_counter

    @pytest.fixture
    def one_sec(self):
        return datetime.timedelta(seconds=1)

    @pytest.fixture
    def one_min(self):
        return datetime.timedelta(minutes=1)

    @pytest.fixture
    def one_day(self):
        return datetime.timedelta(days=1)

    @pytest.fixture
    def dawn_of_time(self):
        return datetime.datetime.utcfromtimestamp(0)


class SmartHomeManagerProvided(object):
    @pytest.fixture
    def smart_home(self):
        simulative_time = MockClock()
        scheduler = Horaro(time_function=simulative_time.get_current_time,
            delay_function=simulative_time.increase_time)
        return Domsagxo(scheduler)

    @staticmethod
    def assertNumberOfNewAppliances(number, state):
        assert number == len(state.variables) - state.number_of_reserved_words

    @staticmethod
    def fastForwardBy(manager, **time):
        manager.scheduler.runSetTime(datetime.timedelta(**time))

    @staticmethod
    def fastForwardTo(manager, **time):
        current_date = manager.scheduler.getDate()
        manager.scheduler.runUntil(current_date.replace(**time))


class RealTimeSmartHomeManagerProvided_CarefulVolatile(object):

    @pytest.fixture
    def polling_interval(self):
        return 0.1

    @pytest.fixture
    def smart_home(self, polling_interval):
        scheduler = Horaro(polling_interval=polling_interval)
        return Domsagxo(scheduler)

    @staticmethod
    def assert_that_the_scheduler_is_alive(smart_home):
        assert smart_home.scheduler_runner.is_alive()
        smart_home.stop_scheduler()
        assert not smart_home.scheduler_runner.is_alive()


class BeamTokensProvided(object):
    @pytest.fixture
    def kite_noun_token(self):
        return BeamToken("kite", {PartOfSpeech.NOUN: 1})

    @pytest.fixture
    def love_noun_or_verb_token(self):
        return BeamToken("love", {PartOfSpeech.NOUN: 0.5, PartOfSpeech.V_IMP: 0.125})


class ProvidedSmartHomeWithLightBulb(object):
    @pytest.fixture
    def smart_home(self):
        smart_home = mgmt_cmp.Domsagxo()
        light_bulb = atypes.Appliance(atypes.ApplianceTypes.LIGHT, "sxambalulo")
        smart_home.addAppliance(light_bulb)
        return smart_home