import copy
import datetime
from enum import Enum


def leaf_form(lastChild):
    return "└- " if lastChild else "├- "


def branch_form(last_child):
    return "\t" if last_child else "│\t"


def pretty_print(whatever, indent="", last_child=True):
    if isinstance(whatever, AstNode):
        return whatever.pretty_print(indent, last_child)
    else:
        return indent + leaf_form(last_child) + str(whatever)


class AstNode(object):
    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs

    def evaluate(self, state):
        return self._method(state, *self.args, **self.kwargs)

    def _method(self, *args, **kwargs):
        return None, None

    def __repr__(self):
        arg_string = "" if len(self.args) + len(self.kwargs) == 0 else \
            "(" + ", ".join(self.list_args_and_kwargs()) + ")"

        return str(type(self).__name__) + arg_string

    def list_args_and_kwargs(self):
        return [arg.__repr__() for arg in self.args] + \
               [key + "=" + val.__repr__() for (key, val) in self.kwargs.items()]

    def pretty_print(self, indent="", last_child=True):
        res = indent + leaf_form(last_child) + str(type(self).__name__)
        for i in range(len(self.args)):
            child = self.args[i]
            newIndent = indent + branch_form(last_child)
            res += "\n" + pretty_print(child, newIndent, i == len(self.args) - 1)

        return res


class BasicNode(AstNode):
    def __init__(self, *args, **kwargs):
        super(BasicNode, self).__init__(*args, **kwargs)

    def pretty_print(self, indent="", last_child=True):
        return indent + leaf_form(last_child) + str(self)

    def __str__(self):
        return str(self.args[0])


class Number(BasicNode):
    def __init__(self, number):
        super(Number, self).__init__(number)

    def _method(self, state, number):
        return state, number


class String(BasicNode):
    def __init__(self, string):
        super(String, self).__init__(string)

    def _method(self, state, string):
        return state, string

    def __str__(self):
        return "`" + str(self.args[0]) + "'"


class NoneNode(AstNode):
    def __init__(self):
        super(NoneNode, self).__init__()

    def _method(self, state):
        return state, None

    @staticmethod
    def getContainedAdjectives():
        return []


class BinaryOp(AstNode):

    def __init__(self, arg1, arg2):
        super(BinaryOp, self).__init__(arg1, arg2)


class Add(BinaryOp):
    def _method(self, state, arg1, arg2):
        value1 = arg1.evaluate(state)[1]
        value2 = arg2.evaluate(state)[1]
        return state, value1 + value2


class Subtract(BinaryOp):
    def _method(self, state, arg1, arg2):
        value1 = arg1.evaluate(state)[1]
        value2 = arg2.evaluate(state)[1]
        return state, value1 - value2


class Multiply(BinaryOp):
    def _method(self, state, arg1, arg2):
        value1 = arg1.evaluate(state)[1]
        value2 = arg2.evaluate(state)[1]
        return state, value1 * value2


class Divide(BinaryOp):
    def _method(self, state, arg1, arg2):
        value1 = arg1.evaluate(state)[1]
        value2 = arg2.evaluate(state)[1]
        return state, value1 / value2


class LogicAnd(BinaryOp):
    def _method(self, state, arg1, arg2):
        value1 = arg1.evaluate(state)[1]
        value2 = arg2.evaluate(state)[1]
        return state, value1 and value2


class LogicOr(BinaryOp):
    def _method(self, state, arg1, arg2):
        value1 = arg1.evaluate(state)[1]
        value2 = arg2.evaluate(state)[1]
        return state, value1 or value2


class UnaryOp(AstNode):
    def __init__(self, arg):
        super(UnaryOp, self).__init__(arg)


class LogicNot(UnaryOp):
    def _method(self, state, arg):
        value = arg.evaluate(state)[1]
        return state, not value


class Boolean(BasicNode):
    def __init__(self, value):
        super(Boolean, self).__init__(value)

    def _method(self, state, value):
        return state, value


class VariableName(BasicNode):
    def __init__(self, variable_name, variable_descriptor):
        super(VariableName, self).__init__(variable_name)
        self.variable_name = " ".join(
            variable_descriptor.getContainedAdjectives() + [variable_name])

    def _method(self, state, variable_name):
        if self.variable_name in state.variables:
            return state, state.variables[self.variable_name]
        else:
            raise NameError("name " + self.variable_name + " is not defined")

    def getContainedName(self):
        return self.variable_name

    def setter(self, state, value):
        state.variables[self.variable_name] = value

    def getter(self, state):
        return state.variables[self.variable_name]

    def __str__(self):
        return '<' + self.variable_name + '>'


class Description(AstNode):
    def __init__(self, descriptor, additional_descriptor=NoneNode()):
        super(Description, self).__init__(descriptor, additional_descriptor)
        self.adjectives = additional_descriptor.getContainedAdjectives() + [descriptor]

    def _method(self, state, descriptor, additional_descriptor):
        variable_name = " ".join(self.adjectives)[:-1] + "o"
        if variable_name in state.variables:
            return state, state.variables[variable_name]
        return state, variable_name

    def getContainedAdjectives(self):
        return self.adjectives


class Dereference(AstNode):
    def __init__(self, field_name, variable_name):
        super(Dereference, self).__init__(field_name, variable_name)
        self.field_name = field_name
        self.variable_name = variable_name

    def _method(self, state, field_name, variable_name):
        state, evaluated_var = variable_name.evaluate(state)
        return state, evaluated_var.properties[field_name.getContainedName()]

    def getContainedName(self):
        return self.field_name.getContainedName()

    def _get_containing_object(self, state):
        return self.variable_name.getter(state)

    def setter(self, state, value):
        containing_object = self._get_containing_object(state)
        if self.getContainedName() not in containing_object.properties:
            raise KeyError(str(containing_object) + " has no field named `"
                           + self.getContainedName() + "'. only:\n"
                           + str(containing_object.properties))
        containing_object.properties[self.getContainedName()] = value

    def getter(self, state):
        return self._get_containing_object(state).properties[self.getContainedName()]


class ArrayAccess(AstNode):
    def __init__(self, ordinal, variable_name):
        super(ArrayAccess, self).__init__(ordinal, variable_name)
        self.ordinal = ordinal
        self.variable_name = variable_name

    def _method(self, state, ordinal, variable_name):
        state, evaluated_index = ordinal.evaluate(state)
        state, evaluated_var = variable_name.evaluate(state)
        return state, evaluated_var[evaluated_index - 1]

    def _get_containing_object(self, state):
        return self.variable_name.getter(state)

    def setter(self, state, value):
        state, evaluated_ordinal = self.ordinal.evaluate(state)
        containing_object = self._get_containing_object(state)
        containing_object[evaluated_ordinal - 1] = value

    def getter(self, state):
        state, evaluated_ordinal = self.ordinal.evaluate(state)
        return self._get_containing_object(state)[evaluated_ordinal - 1]


class VariableAssignment(AstNode):
    def __init__(self, variable_name, value):
        super(VariableAssignment, self).__init__(variable_name, value)

    def _method(self, state, variable_name, variable_value):
        state, value = variable_value.evaluate(state)
        variable_name.setter(state, value)
        return state, nextAction.GO_ON


class nextAction(Enum):
    GO_ON = "go on"
    RETURN = "return"


class Program(AstNode):
    def __init__(self, previous_commands, next_command):
        super(Program, self).__init__(previous_commands, next_command)

    def _method(self, state, previous_commands, next_command):
        if previous_commands is not None:
            state, return_value = previous_commands.evaluate(state)
            if return_value == nextAction.RETURN:
                return state, return_value
        return next_command.evaluate(state)


class TimeSpan(AstNode):
    def __init__(self, days=Number(0), hours=Number(0), minutes=Number(0), seconds=Number(0)):
        super(TimeSpan, self).__init__(days=days, hours=hours, minutes=minutes,
            seconds=seconds)

    def _method(self, state, *args, **kwargs):
        evaluated_kwargs = {}
        for key, value in kwargs.items():
            state, evaluated_kwargs[key] = value.evaluate(state)
        return state, datetime.timedelta(**evaluated_kwargs)

    def __str__(self):
        res = []
        res += self._part_of_time_span("minutes", "m")
        res += self._part_of_time_span("days", "d")
        res += self._part_of_time_span("hours", "h")
        res += self._part_of_time_span("seconds", "s")
        return " ".join(res) if len(res) != 0 else "0s"

    def _part_of_time_span(self, part, suffix):
        return [] if self.kwargs[part].args[0] == 0 else [(str(self.kwargs[part]) + suffix)]


class TimeUnion(AstNode):
    def __init__(self, span1, span2):
        super(TimeUnion, self).__init__(span1, span2)

    def _method(self, state, span1, span2):
        state, evaluated_span1 = span1.evaluate(state)
        state, evaluated_span2 = span2.evaluate(state)
        return state, evaluated_span1 + evaluated_span2


class TimeFractionAddition(AstNode):
    def __init__(self, span, fraction):
        super(TimeFractionAddition, self).__init__(span, fraction)

    def _method(self, state, span, fraction):
        state, evaluated_span = span.evaluate(state)
        state, evaluated_fraction = fraction.evaluate(state)
        return state, evaluated_span * (1 + evaluated_fraction)


class TimePoint(AstNode):
    def __init__(self, hour=Number(0), minute=Number(0)):
        super(TimePoint, self).__init__(hour=hour, minute=minute)

    def _method(self, state, *args, **kwargs):
        evaluated_kwargs = {}
        for key, value in kwargs.items():
            state, evaluated_kwargs[key] = value.evaluate(state)
        return state, datetime.time(**evaluated_kwargs)


class RoutineInvocation(AstNode):
    def __init__(self, function_name, args):
        super(RoutineInvocation, self).__init__(function_name, args)

    def _method(self, state, function_name, args):
        evaluated_args = []
        for arg in args:
            state, new_arg = arg.evaluate(state)
            evaluated_args += [new_arg]
        return state, state.method_dict[function_name](*evaluated_args)

    def pretty_print(self, indent="", last_child=True):
        res = indent + leaf_form(last_child) + str(type(self).__name__) \
              + ": " + self.args[0]
        inner_args = self.args[1]
        for i in range(len(inner_args)):
            child = inner_args[i]
            newIndent = indent + branch_form(last_child)
            res += "\n" + pretty_print(child, newIndent, i == len(inner_args) - 1)

        return res


class ReturnValue(AstNode):
    def __init__(self, return_value):
        super(ReturnValue, self).__init__(return_value)

    def _method(self, state, return_value):
        if return_value is not None:
            state, evaluated_return_value = return_value.evaluate(state)
            state.variables['gxi'] = evaluated_return_value
        return state, nextAction.RETURN


class RoutineDefinition(AstNode):
    def __init__(self, function_name, argument_names, command_subtree):
        super(RoutineDefinition, self).__init__(function_name, argument_names, command_subtree)

    @staticmethod
    def turn_ast_into_function(state, function_name, argument_names, abstract_syntax_tree):
        def subtree_function(*argument_list):
            closure = copy.copy(state)
            closure.variables = copy.copy(state.variables)
            if len(argument_list) != len(argument_names):
                raise TypeError(str(function_name) + "() expects " +
                                str(len(argument_names)) + "arguments:\n\t" +
                                str([name.getContainedName() for name in
                                     argument_names]) + "\nbut " +
                                str(len(argument_list)) + "were given.")
            for i in range(len(argument_list)):
                closure.variables[argument_names[i].getContainedName()] = argument_list[i]
            colsure, action = abstract_syntax_tree.evaluate(closure)
            if action == nextAction.RETURN and 'gxi' in closure.variables.keys():
                state.variables['gxi'] = closure.variables['gxi']
            return nextAction.GO_ON

        return subtree_function

    def _method(self, state, function_name, argument_names, command_subtree):
        state.method_dict[function_name] = self.turn_ast_into_function(
            state, function_name, argument_names, command_subtree)
        return state, nextAction.GO_ON

    def pretty_print(self, indent="", last_child=True):
        res = indent + leaf_form(last_child) + str(type(self).__name__) + ": " \
              + self.args[0] + "\n" + \
              indent + "\t" + leaf_form(False) + "Arguments:"
        inner_args = self.args[1]
        for i in range(len(inner_args)):
            res += "\n" + pretty_print(inner_args[i], indent + "\t" + branch_form(False),
                                                      i == len(inner_args) - 1)
        inner_args = self.args[2:]
        for i in range(len(inner_args)):
            child = inner_args[i]
            res += "\n" + child.pretty_print(indent + branch_form(last_child),
                                             i == len(inner_args) - 1)

        return res


class ConditionalStatement(AstNode):
    def __init__(self, condition, trueStatement, falseStatement):
        super(ConditionalStatement, self).__init__(condition, trueStatement, falseStatement)

    def _method(self, state, condition, trueStatement, falseStatement):
        state, evaluated_condition = condition.evaluate(state)
        if evaluated_condition:
            return trueStatement.evaluate(state)
        elif falseStatement is not None:
            return falseStatement.evaluate(state)
        else:
            return state, None


class ExecutionWrapper(object):
    @staticmethod
    def delayed_evaluation(state, statement):
        def evaluate(closure):
            return lambda: statement.evaluate(closure)[1]

        cloned_state = copy.copy(state)
        cloned_state.variables = copy.copy(state.variables)
        return evaluate(cloned_state)


class DelayedStatement(AstNode, ExecutionWrapper):

    def __init__(self, statement, delay):
        super(DelayedStatement, self).__init__(statement, delay)

    def _method(self, state, statement, delay):
        state, evaluated_delay = delay.evaluate(state)
        state.scheduler.enter(evaluated_delay,
            self.delayed_evaluation(state, statement))
        return state, evaluated_delay


class ScheduledStatement(AstNode, ExecutionWrapper):

    def __init__(self, statement, delay):
        super(ScheduledStatement, self).__init__(statement, delay)

    def _method(self, state, statement, delay):
        state, evaluated_time = delay.evaluate(state)
        state.scheduler.enter(evaluated_time,
            self.delayed_evaluation(state, statement))
        return state, evaluated_time


class OnceStatement(AstNode, ExecutionWrapper):

    def __init__(self, statement, trigger):
        super(OnceStatement, self).__init__(statement, trigger)

    def _method(self, state, statement, trigger):
        state.scheduler.enterAtTrigger(self.delayed_evaluation(state, trigger),
            self.delayed_evaluation(state, statement))
        return state, None


class WheneverStatement(AstNode, ExecutionWrapper):

    def __init__(self, statement, trigger):
        super(WheneverStatement, self).__init__(statement, trigger)

    def _method(self, state, statement, trigger):
        state.scheduler.repeatAtTrigger(self.delayed_evaluation(state, trigger),
            self.delayed_evaluation(state, statement))
        return state, None


class RepeatedStatement(AstNode, ExecutionWrapper):

    def __init__(self, statement, delay):
        super(RepeatedStatement, self).__init__(statement, delay)

    def _method(self, state, statement, delay):
        state, evaluated_time = delay.evaluate(state)
        state.scheduler.startAtIntervalRepeatAtInterval(
            evaluated_time, self.delayed_evaluation(state, statement))
        return state, evaluated_time


class Comparison(AstNode):
    class Relation(Enum):
        EQUAL = "EQ"
        NOT_EQUAL = "NEQ"
        GREATER = "GT"
        GREATER_OR_EQUAL = "GTE"
        LESSER = "LT"
        LESSER_OR_EQUAL = "LTE"

    def __init__(self, arg1, comparison, arg2):
        super(Comparison, self).__init__(arg1, comparison, arg2)
        self.reverser_flag = False

    def _method(self, state, arg1, comparison, arg2):
        state, evaluated_arg1 = arg1.evaluate(state)
        state, evaluated_arg2 = arg2.evaluate(state)
        if comparison == self.Relation.EQUAL:
            res = evaluated_arg1 == evaluated_arg2
        elif comparison == self.Relation.GREATER:
            res = evaluated_arg1 > evaluated_arg2
        elif comparison == self.Relation.GREATER_OR_EQUAL:
            res = evaluated_arg1 >= evaluated_arg2
        elif comparison == self.Relation.LESSER:
            res = evaluated_arg1 < evaluated_arg2
        elif comparison == self.Relation.LESSER_OR_EQUAL:
            res = evaluated_arg1 <= evaluated_arg2
        else:
            res = evaluated_arg1 != evaluated_arg2
        return state, res != self.reverser_flag

    def reverse(self):
        self.reverser_flag = not self.reverser_flag
        return self


class LoopStatement(AstNode):
    def __init__(self, condition, body):
        super(LoopStatement, self).__init__(condition, body)

    def _method(self, state, condition, body):
        state, flag = condition.evaluate(state)
        while flag:
            state, return_value = body.evaluate(state)
            if return_value == nextAction.RETURN:
                return state, return_value
            state, flag = condition.evaluate(state)

        return state, None


class QueryState(AstNode):
    def __init__(self, appliance, stateName):
        super(QueryState, self).__init__(appliance, stateName)

    def _method(self, state, appliance, stateName):
        state, evaluated_appliance = appliance.evaluate(state)
        return state, evaluated_appliance.stateQueries[stateName]
