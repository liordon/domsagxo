import datetime
import time

import pytest

import compilation.abstract_syntax_tree as ast_bld
import compilation.definitions
import library.management_components as mng_co
from library.atomic_types import Appliance
from library.predefined_values import ApplianceTypes, ApplianceProperties, Color
from test_utils.providers import StatementLevelAstProvided, SmartHomeManagerProvided, \
    RealTimeSmartHomeManagerProvided_CarefulVolatile, evaluate_and_return_state, evaluate_and_return_state_variables


class TestUntimedAstStatements(StatementLevelAstProvided):

    def test_capableOfNumberAssignmentIntoVariables(self, ast):
        variables = evaluate_and_return_state_variables(ast, "kato = 10")
        assert 10 == variables["kato"]

    def test_capableOfExpressionAssignmentIntoVariables(self, ast):
        state = evaluate_and_return_state_variables(ast, "kato=2+4*10")
        assert 42 == state["kato"]

    def test_adjectivesJoinNounsToDefineVariableNames(self, ast):
        new_state = evaluate_and_return_state_variables(ast, "nigra kato=7")
        assert 7 == new_state["nigra kato"]
        assert 'kato' not in new_state

    def test_variablesAreCaseInsensitiveAndSavedAsLowerCase(self, ast):
        new_state = evaluate_and_return_state_variables(ast, "Nigra Kato=7")
        assert new_state["nigra kato"] == 7

    def test_reservedAdjectivesCanBeReclaimedForVariableNamesOutOfReservedContext(self, ast):
        new_state = evaluate_and_return_state_variables(ast, "asignu 7 al malgranda kato")
        assert 7 == new_state["malgranda kato"]
        new_state = evaluate_and_return_state_variables(ast, "asignu 17 al granda kato")
        assert 17 == new_state["granda kato"]

    def test_definiteNounsBecomeIndefinite(self, ast):
        state = evaluate_and_return_state_variables(ast, "la kato = 9")
        assert 9 == state["kato"]

    def test_definiteDescribedNounAlsoBecomesIndefinite(self, ast):
        state = evaluate_and_return_state_variables(ast, "la dika kato = 99")
        assert 99 == state["dika kato"]

    def test_ifStatementContentIsNotEvaluatedIfConditionIsFalse(self, ast):
        assert "kato" not in evaluate_and_return_state_variables(
            ast, "se malvero tiam asignu sep al kato finu")

    def test_ifStatementContentIsEvaluatedIfConditionIsTrue(self, ast):
        state = \
            evaluate_and_return_state_variables(ast, "se vero tiam asignu sep al kato finu")
        assert 7 == state["kato"]

    def test_elseStatementContentIsEvaluatedIfConditionIsFalse(self, ast):
        new_state = \
            evaluate_and_return_state_variables(ast,
                "se malvero tiam asignu sep al kato "
                "alie asignu naux al kato finu")
        assert 9 == new_state["kato"]

    @pytest.mark.timeout(1)
    def test_canDefineSimpleWhileLoopThatDoesNotEvaluate(self, ast):
        assert "kato" not in evaluate_and_return_state_variables(
            ast, "dum malvero tiam asignu sep al kato finu")

    @pytest.mark.timeout(1)
    def test_canDefineWhileLoopThatEvaluatesOnce(self, ast):
        manager = mng_co.Domsagxo()
        manager.variables["kato"] = 1
        new_state = \
            evaluate_and_return_state_variables(ast,
                "dum kato estas pli granda ol nul tiam "
                "asignu kato-1 al kato finu",
                manager)
        assert 0 == new_state["kato"]

    @pytest.mark.timeout(5)
    def test_canDefineWhileLoopThatEvaluatesFiveTimes(self, ast):
        manager = mng_co.Domsagxo()
        manager.variables["kato"] = 5
        new_state = \
            evaluate_and_return_state_variables(ast,
                "dum kato estas pli granda ol nul tiam "
                "asignu kato-1 al kato finu",
                manager)
        assert 0 == new_state["kato"]

    def test_canTurnVariablesIntoOrdinalsViaChangeFromNounToAdjective(self, ast):
        manager = mng_co.Domsagxo()
        manager.variables["indekso"] = 2
        manager.variables["ampoloj"] = [1, 2, 3]
        assert 2 == evaluate_and_return_state_variables(
            ast, "asignu indeksa de ampoloj al kato", manager)['kato']

    def test_canNotInvokeNonExistentRoutine(self, ast):
        with pytest.raises(KeyError):
            evaluate_and_return_state_variables(ast, "sxamnalulu kvardek du")

    def test_canInvokeRoutineWithArguments(self, ast):
        # noinspection PyUnusedLocal
        def mock_routine(arg):
            pass

        smart_home = mng_co.Domsagxo()
        smart_home.method_dict["sxambalulu"] = mock_routine
        # should execute without exception
        evaluate_and_return_state_variables(ast, "sxambalulu kvardek du", smart_home)

    def test_canInvokeRoutineWithoutArguments(self, ast):
        def mock_routine():
            pass

        smart_home = mng_co.Domsagxo()
        smart_home.method_dict["sxambalulu"] = mock_routine
        # should execute without exception
        evaluate_and_return_state_variables(ast, "sxambalulu", smart_home)

    def test_integersArePassedToFunctionsByValueNotByReference(self, ast):
        new_state = evaluate_and_return_state(
            ast, '''referenci kato signifas
                        asignu kato pli unu al kato
                        poste revenu kato
                    finu''')
        new_state.variables["kato"] = 1
        new_state.method_dict['referencu'](3)
        assert new_state.variables['gxi'] == 4
        assert new_state.variables["kato"] == 1


class TestTimedAstStatements(StatementLevelAstProvided, SmartHomeManagerProvided):

    def test_canUseDelayedActionToAddLight(self, ast, smart_home):
        manager, value = ast.parse("aldonu lumon je unu sekundo").evaluate(smart_home)
        self.assertNumberOfNewAppliances(0, manager)
        self.fastForwardBy(smart_home, seconds=1)
        self.assertNumberOfNewAppliances(1, manager)

    def test_canUseScheduledActionToAddLight(self, ast, smart_home):
        manager, value = ast.parse("aldonu lumon je la sesa horo").evaluate(smart_home)
        self.assertNumberOfNewAppliances(0, manager)
        self.fastForwardBy(smart_home, seconds=1)
        self.assertNumberOfNewAppliances(0, manager)
        self.fastForwardTo(smart_home, hour=6)
        self.assertNumberOfNewAppliances(1, manager)

    def test_canUseRepeatedActionToAddLightTwice(self, ast, smart_home):
        manager, value = ast.parse("aldonu lumon cxiu minuto").evaluate(smart_home)
        self.assertNumberOfNewAppliances(0, manager)
        self.fastForwardBy(smart_home, minutes=1)
        self.assertNumberOfNewAppliances(1, manager)
        self.fastForwardBy(smart_home, minutes=1)
        self.assertNumberOfNewAppliances(2, manager)

    def test_delayedActionsSaveAClosureOfTheirDictationState(self, ast, smart_home):
        light_bulb = Appliance(ApplianceTypes.LIGHT, "bulbazoro")
        smart_home.variables["hundo"] = 2
        smart_home.addAppliance(light_bulb)
        smart_home, value = ast.parse("asignu hundo al brilo de bulbazoro je unu sekundo").evaluate(smart_home)
        smart_home.variables["hundo"] = 10

        self.fastForwardBy(smart_home, seconds=2)
        assert smart_home.variables["bulbazoro"].properties[ApplianceProperties.BRIGHTNESS.value] == 2

    def test_canTriggerActionOnceConditionIsMet(self, ast, smart_home):
        light_bulb = Appliance(ApplianceTypes.LIGHT, "bulbazoro")
        light_bulb2 = Appliance(ApplianceTypes.LIGHT, "cxarmandero")
        smart_home.addAppliance(light_bulb)
        smart_home.addAppliance(light_bulb2)
        smart_home, value = ast.parse("asignu kvardek du al brilo de bulbazoro unufoje la koloro de cxarmandero estas egala al rugxo").evaluate(smart_home)

        self.fastForwardBy(smart_home, seconds=2)
        assert smart_home.variables["bulbazoro"].properties[ApplianceProperties.BRIGHTNESS.value] != 42
        smart_home.variables["cxarmandero"].properties[ApplianceProperties.COLOR.value] = Color.RED.value
        self.fastForwardBy(smart_home, seconds=2)
        assert smart_home.variables["bulbazoro"].properties[ApplianceProperties.BRIGHTNESS.value] == 42

    def test_canTriggerActionWheneverConditionIsMet(self, ast, smart_home):
        light_bulb = Appliance(ApplianceTypes.LIGHT, "bulbazoro")
        light_bulb2 = Appliance(ApplianceTypes.LIGHT, "cxarmandero")
        smart_home.addAppliance(light_bulb)
        smart_home.addAppliance(light_bulb2)
        smart_home, value = ast.parse("asignu brilo de bulbazoro pli unu al brilo de bulbazoro "
                                      "cxiufoje la koloro de cxarmandero estas egala al rugxo").evaluate(smart_home)

        self.fastForwardBy(smart_home, seconds=2)
        assert smart_home.variables["bulbazoro"].properties[ApplianceProperties.BRIGHTNESS.value] == 1
        smart_home.variables["cxarmandero"].properties[ApplianceProperties.COLOR.value] = Color.RED.value
        self.fastForwardBy(smart_home, seconds=2)
        assert smart_home.variables["bulbazoro"].properties[ApplianceProperties.BRIGHTNESS.value] == 2
        smart_home.variables["cxarmandero"].properties[ApplianceProperties.COLOR.value] = Color.BLUE.value
        self.fastForwardBy(smart_home, seconds=2)
        smart_home.variables["cxarmandero"].properties[ApplianceProperties.COLOR.value] = Color.RED.value
        self.fastForwardBy(smart_home, seconds=2)
        assert smart_home.variables["bulbazoro"].properties[ApplianceProperties.BRIGHTNESS.value] == 3

    def test_canUseRepeatedActionToPropagateLampColor(self, ast, smart_home):
        light_bulbs = []
        for i in range(100):
            light_bulb = Appliance(ApplianceTypes.LIGHT, f"{i + 1:03d}")
            light_bulb.properties[ApplianceProperties.COLOR.value] = Color.WHITE.value
            smart_home.addAppliance(light_bulb)
            light_bulbs += [light_bulb]
        smart_home.variables["ampoloj"] = light_bulbs
        smart_home.variables["indekso"] = 1
        smart_home, value = ast.parse("""
            dum indekso estas pli malgranda ol cent tiam
                asignu indekso pli unu al io 
                poste asignu la koloro de indeksa de ampoloj 
                    al la koloro de ia de ampoloj
                poste asignu io al indekso
            finu cxiu sekundo""").evaluate(smart_home)
        for bulb in smart_home.variables["ampoloj"]:
            assert bulb.properties[ApplianceProperties.COLOR.value] == Color.WHITE.value

        smart_home.variables["ampoloj"][0].properties[ApplianceProperties.COLOR.value] = Color.RED.value
        self.fastForwardBy(smart_home, seconds=1)
        for bulb in smart_home.variables["ampoloj"]:
            print(bulb.name)
            assert bulb.properties[ApplianceProperties.COLOR.value] == Color.RED.value


class TestAstPrograms(object):

    @pytest.fixture
    def ast(self):
        return ast_bld.build(start=compilation.definitions.GrammarVariable.PROGRAM.value)

    @pytest.fixture
    def initial_state(self):
        return mng_co.Domsagxo()

    def test_unterminatedCommandIsNotAProgram(self, ast):
        with pytest.raises(compilation.definitions.EsperantoSyntaxError):
            ast.parse("12")

    def test_canParseASingleCommandAsProgram(self, ast):
        ast.parse("asignu sep al kato")

    def test_canExecuteTwoCommandsSequentially(self, ast):
        variables = evaluate_and_return_state_variables(
            ast, "asignu kvar al kato poste asignu kvin al hundo")
        assert 4 == variables["kato"]
        assert 5 == variables["hundo"]

    def test_canExecuteTwoCommandsInParallel(self, ast):
        variables = evaluate_and_return_state_variables(
            ast, "asignu kvin al kato samtempe asignu kvar al hundo")
        assert 5 == variables["kato"]
        assert 4 == variables["hundo"]

    def test_consecutiveStatementsPropagateVariableValues(self, ast):
        new_state = evaluate_and_return_state_variables(ast, '''kato=2+4*10
                        poste hundo = kato/6''')
        assert 42 == new_state["kato"]
        assert 7 == new_state["hundo"]

    def test_returnStopsProgramFromContinuing(self, ast, initial_state):
        new_manager, value = ast.parse('''revenu poste kato = 10''').evaluate(initial_state)
        assert "kato" not in new_manager.variables

    def test_canAssignReturnValueOfFunctionWithoutAccusativeCase(self, ast):
        variables = evaluate_and_return_state_variables(ast,
            '''sxambaluli signifas revenu nul finu
            poste sxambalulu
            poste asignu gxi al kato''')
        assert variables['kato'] == 0

    def test_canAssignReturnValueOfFunctionWithAccusativeCase(self, ast):
        variables = evaluate_and_return_state_variables(ast,
            '''sxambaluli signifas revenu nul finu
            poste sxambalulu
            poste asignu gxin al kato''')
        assert variables['kato'] == 0

    def test_returnStopsWhileLoopFromContinuing(self, ast, initial_state):
        variables = evaluate_and_return_state_variables(ast, '''
        asignu naux al kato
        poste dum kato estas pli granda ol nul tiam
            asignu kato-1 al kato
            poste se kato estas egala al tri tiam
                revenu
            finu
        finu
        ''')
        assert variables["kato"] == 3

    def test_applianceStateCanBeQueriedWithPresentVerbs(self, ast, initial_state):
        variables = evaluate_and_return_state_variables(ast, '''
        aldonu lumon
        poste se unua lumo sxaltas tiam
            asignu unu al muso
        finu
        poste sxaltu unuan lumon
        poste se unua lumo sxaltas tiam
            asignu du al kato
        finu
        poste malsxaltu unuan lumon
        poste se unua lumo sxaltas tiam
            asignu tri al hundo
        finu
        ''', initial_state)
        with pytest.raises(KeyError):
            assert variables["muso"] is None

        assert variables["kato"] == 2

        with pytest.raises(KeyError):
            assert variables["hundo"] is None


@pytest.mark.timeout(10)
class TestLargeScalePhenomena(RealTimeSmartHomeManagerProvided_CarefulVolatile):

    def test_afterSchedulerIsStartedItCanAlsoBeStopped(self, smart_home):
        smart_home.start_scheduler()
        smart_home.stop_scheduler()
        smart_home.scheduler_runner.join()

    def test_tasksCanBeAddedToSchedulerWhileItIsBusyWithOthers(self, smart_home, polling_interval):
        smart_home.start_scheduler()
        smart_home.scheduler.enter(datetime.timedelta(seconds=polling_interval * 1.5), time.sleep,
            (polling_interval * 2,))
        time.sleep(polling_interval * 5)
        self.assert_that_the_scheduler_is_alive(smart_home)

    def test_tasksCanBeSubmittedFasterThanPollingTime(self, smart_home, polling_interval):
        smart_home.start_scheduler()
        smart_home.scheduler.enter(datetime.timedelta(seconds=polling_interval * 0.01), time.sleep,
            (polling_interval * 2,))
        time.sleep(polling_interval * 0.5)
        smart_home.scheduler.enter(datetime.timedelta(seconds=polling_interval * 0.01), time.sleep,
            (polling_interval * 2,))
        self.assert_that_the_scheduler_is_alive(smart_home)
