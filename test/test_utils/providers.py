import datetime

import pytest

import domsagxo.compilation.abstract_syntax_tree as ast_bld
import domsagxo.compilation.definitions
import domsagxo.compilation.esperanto_lexer as eo_lxr
from domsagxo.compilation.definitions import PartOfSpeech
from domsagxo.compilation.node import AstNode
from domsagxo.english_prototype.data_structures import BeamToken
from domsagxo.english_prototype.english_lexer import WordnetProtoLexer, NltkProtoLexer
from domsagxo.library import management_components as mgmt_cmp, atomic_types as atypes
from domsagxo.library import mocks
from domsagxo.library.mocks import MockClock


class SyntaxNodesProvided(object):
    class FakeNode(AstNode):
        def __init__(self, number_of_tokens=0):
            self.num_tokens = number_of_tokens

        def total_number_of_tokens(self):
            return self.num_tokens

    @staticmethod
    def node_with_x_tokens(x):
        return SyntaxNodesProvided.FakeNode(number_of_tokens=x)

    @pytest.fixture
    def tokenless_node(self):
        return self.node_with_x_tokens(0)

    @pytest.fixture
    def single_token_node(self):
        return self.node_with_x_tokens(1)

    @pytest.fixture
    def double_token_node(self):
        return self.node_with_x_tokens(2)


class PartOfSpeechValueVerifier(object):
    @staticmethod
    def assert_part_of_speech_for_token(partOfSpeech, token):
        assert partOfSpeech.value == token.type

    @staticmethod
    def assert_part_of_speech_for_next_token_of_lexer(partOfSpeech, lexer):
        assert partOfSpeech.value == lexer.token().type


def evaluate_and_return_state(ast, statement, initial_state=None):
    if initial_state is None:
        initial_state = mocks.MockHouse()  # so as not to put a mutable default
    state, nothing = ast.parse(statement).evaluate(initial_state)
    return state


def evaluate_and_return_state_variables(ast, statement, initial_state=None):
    return evaluate_and_return_state(ast, statement, initial_state).variables


class EsperantoLexerProvided(PartOfSpeechValueVerifier):
    @pytest.fixture
    def lexer(self):
        return eo_lxr.build()


class PartialNameLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=domsagxo.compilation.definitions.GrammarVariable.PARTIAL_NAME.value)


class ExpressionLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=domsagxo.compilation.definitions.GrammarVariable.EXPRESSION.value)


class FunctionDefinitionLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=domsagxo.compilation.definitions.GrammarVariable.ROUTINE_DEFINITION.value)


class StatementLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=domsagxo.compilation.definitions.GrammarVariable.STATEMENT.value)


class TopLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build()


class MockSmartHomeStateVariablesProvided(object):
    @pytest.fixture
    def state(self):
        return mocks.Bunch(variables={}, method_dict={})


class TimeManagerWithSimulativeClockProvided(object):

    @pytest.fixture
    def scd(self):
        simulative_time = MockClock()
        return mgmt_cmp.Horaro(time_function=simulative_time.get_current_time,
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
        scheduler = mgmt_cmp.Horaro(time_function=simulative_time.get_current_time,
            delay_function=simulative_time.increase_time)
        return mgmt_cmp.Domsagxo(scheduler)

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
        scheduler = mgmt_cmp.Horaro(polling_interval=polling_interval)
        return mgmt_cmp.Domsagxo(scheduler)

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


class WordNetEnglishLexerProvided(object):
    @pytest.fixture
    def lexer(self):
        return WordnetProtoLexer()

    @staticmethod
    def assert_possible_next_token(partOfSpeech, lexer):
        assert partOfSpeech in lexer.token().tags.keys()


class NltkEnglishLexerProvided(object):
    @pytest.fixture
    def lexer(self):
        return NltkProtoLexer()

    @staticmethod
    def assert_tag_of_next_token(partOfSpeech, lexer):
        assert lexer.token().tag is partOfSpeech
