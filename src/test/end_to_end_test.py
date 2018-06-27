import datetime

import pytest

import compilation.abstract_syntax_tree as ast_bld
import compilation.esp_lexer as lxr
import library.management_components as mng_co
from test.mocks import MockClock

lxr.build()


def evaluate_and_return_state_variables(ast, statement, initial_state=None):
    if initial_state is None:
        initial_state = mng_co.Domsagxo()  # so as not to put a mutable default
    state, nothing = ast.parse(statement).evaluate(initial_state)
    return state.variables


class StatementLevelAstProvided(object):
    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.Var.STATEMENT.value)


class TestUntimedAstStatements(StatementLevelAstProvided):

    def test_anExpressionIsAlsoAStatement(self, ast):
        ast.parse("12")

    def test_capableOfNumberAssignment(self, ast):
        assert {'kato': 10} == evaluate_and_return_state_variables(ast, "kato = 10")

    def test_capableOfExpressionAssignment(self, ast):
        assert {'kato': 42} == evaluate_and_return_state_variables(ast, "kato=2+4*10")

    def test_adjectivesJoinNounsToDefineVariableNames(self, ast):
        new_state = evaluate_and_return_state_variables(ast, "nigra kato=7")
        assert {'nigra kato': 7} == new_state
        assert 'kato' not in new_state

    def test_reservedAdjectivesCanBeUsedForVariableNamesOutOfReservedContext(self, ast):
        new_state = evaluate_and_return_state_variables(ast, "malgranda kato estas 7")
        assert {'malgranda kato': 7} == new_state
        new_state = evaluate_and_return_state_variables(ast, "granda kato estas 17")
        assert {'granda kato': 17} == new_state

    def test_definiteNounsBecomeIndefinite(self, ast):
        assert {'kato': 9} == evaluate_and_return_state_variables(ast, "la kato = 9")

    def test_definiteDescribedNounAlsoBecomesIndefinite(self, ast):
        assert {"dika kato": 99} == evaluate_and_return_state_variables(ast, "la dika kato = 99")

    def test_ifStatementContentIsNotEvaluatedIfConditionIsFalse(self, ast):
        assert {} == evaluate_and_return_state_variables(
            ast, "se malvero tiam kato estas sep. finu")

    def test_ifStatementContentIsEvaluatedIfConditionIsTrue(self, ast):
        assert {'kato': 7} == evaluate_and_return_state_variables(
            ast, "se vero tiam kato estas sep. finu")

    def test_elseStatementContentIsEvaluatedIfConditionIsFalse(self, ast):
        assert {'kato': 9} == evaluate_and_return_state_variables(
            ast, "se malvero tiam kato estas sep. alie kato estas naux. finu")

    def test_canDefineSimpleWhileLoopThatDoesNotEvaluate(self, ast):
        assert {} == evaluate_and_return_state_variables(
            ast, "dum malvero tiam kato estas sep. finu")

    def test_canDefineWhileLoopThatEvaluatesOnce(self, ast):
        manager = mng_co.Domsagxo()
        manager.variables["kato"] = 1
        assert {'kato': 0} == evaluate_and_return_state_variables(
            ast, "dum kato estas pli granda ol nul tiam kato estas kato-1. finu", manager)

    def test_canDefineWhileLoopThatEvaluatesFiveTimes(self, ast):
        manager = mng_co.Domsagxo()
        manager.variables["kato"] = 5
        assert {'kato': 0} == evaluate_and_return_state_variables(
            ast, "dum kato estas pli granda ol nul tiam kato estas kato-1. finu", manager)


class TestTimedAstStatements(StatementLevelAstProvided):

    @pytest.fixture
    def fake_timed_smart_home(self):
        simulative_time = MockClock()
        scheduler = mng_co.Horaro(time_function=simulative_time.get_current_time,
                                  delay_function=simulative_time.increase_time)
        return mng_co.Domsagxo(scheduler)

    @staticmethod
    def fastForwardBy(manager, **time):
        manager.scheduler.runSetTime(datetime.timedelta(**time))

    @staticmethod
    def fastForwardTo(manager, **time):
        current_date = manager.scheduler.getDate()
        manager.scheduler.runUntil(current_date.replace(**time))

    def test_canUseDelayedActionToAddLight(self, ast, fake_timed_smart_home):
        manager, value = ast.parse("aldonu lumon post sekundo").evaluate(fake_timed_smart_home)
        assert 0 == len(manager.variables)
        self.fastForwardBy(fake_timed_smart_home, seconds=1)
        assert 1 == len(manager.variables)

    def test_canUseScheduledActionToAddLight(self, ast, fake_timed_smart_home):
        manager, value = ast.parse("aldonu lumon je la sesa horo").evaluate(fake_timed_smart_home)
        assert 0 == len(manager.variables)
        self.fastForwardBy(fake_timed_smart_home, seconds=1)
        assert 0 == len(manager.variables)
        self.fastForwardTo(fake_timed_smart_home, hour=6)
        assert 1 == len(manager.variables)

    def test_canUseRepeatedActionToAddLightTwice(self, ast, fake_timed_smart_home):
        manager, value = ast.parse("aldonu lumon cxiu minuto").evaluate(fake_timed_smart_home)
        assert 0 == len(manager.variables)
        self.fastForwardBy(fake_timed_smart_home, minutes=1)
        assert 1 == len(manager.variables)
        self.fastForwardBy(fake_timed_smart_home, minutes=1)
        assert 2 == len(manager.variables)


class TestAstPrograms(object):

    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=ast_bld.Var.PROGRAM.value)

    @pytest.fixture
    def initial_state(self):
        return mng_co.Domsagxo()

    def test_unterminatedCommandIsNotAProgram(self, ast):
        with pytest.raises(ast_bld.EsperantoSyntaxError):
            ast.parse("12")

    def test_canParseASingleCommandAsProgram(self, ast):
        ast.parse("12.")

    def test_consecutiveStatementsPropagateVariableValues(self, ast):
        new_manager = evaluate_and_return_state_variables(ast, '''kato=2+4*10.
                        hundo = kato/6.''')
        assert {'kato': 42, 'hundo': 7} == new_manager

    def test_returnStatementReturnsItsDeclaredValue(self, ast):
        value = ast.parse('''revenu ses.''').evaluate(None)[1]
        assert 6 == value

    def test_returnStopsProgramFromContinuing(self, ast, initial_state):
        new_manager, value = ast.parse('''revenu ses. kato = 10.''').evaluate(initial_state)
        assert 6 == value
        assert {} == new_manager.variables

    def test_returnStopsWhileLoopFromContinuing(self, ast, initial_state):
        manager, value = ast.parse('''
        kato estas naux.
        dum kato estas pli granda ol nul tiam
            kato estas kato-1.
            se kato estas egala al tri tiam
                revenu kato.
            finu.
        finu.
        ''').evaluate(initial_state)
        assert {'kato': 3} == manager.variables
        assert 3 == value
