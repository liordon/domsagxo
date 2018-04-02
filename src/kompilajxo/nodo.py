import copy
import datetime

import biblioteko.atomaj_tipoj as Types


class AstNode(object):
    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs

    def evaluate(self, state):
        return self._method(state, *self.args, **self.kwargs)

    def _method(self, *args, **kwargs):
        return None, None


class Number(AstNode):
    def __init__(self, number):
        super(Number, self).__init__(number)

    def _method(self, state, number):
        return state, number


class NoneNode(AstNode):
    def __init__(self):
        super(NoneNode, self).__init__()

    def _method(self, state):
        return state, None


class MathOp(AstNode):
    def __init__(self, op, arg1, arg2):
        super(MathOp, self).__init__(op, arg1, arg2)

    def _method(self, state, arg1, op, arg2):
        value1 = arg1.evaluate(state)[1]
        value2 = arg2.evaluate(state)[1]
        if op == '+':
            return state, value1 + value2
        elif op == '-':
            return state, value1 - value2
        elif op == '*':
            return state, value1 * value2
        elif op == '/':
            return state, value1 / value2
        elif op == '=' or op == 'estas':
            return state, value1 == value2


class Boolean(AstNode):
    def __init__(self, value):
        super(Boolean, self).__init__(value)

    def _method(self, state, value):
        return state, value


class VariableName(AstNode):
    def __init__(self, variable_name):
        super(VariableName, self).__init__(variable_name)

    def _method(self, state, variable_name):
        if variable_name not in state.variables:
            # raise NameError("name " + variable_name + " is not defined")
            return state, variable_name
        return state, state.variables[variable_name]

    def getContainedName(self):
        return self.args[0]


class VariableAssignment(AstNode):
    def __init__(self, variable_name, value):
        super(VariableAssignment, self).__init__(variable_name, value)

    def _method(self, state, variable_name, variable_value):
        state, value = variable_value.evaluate(state)
        state.variables[variable_name.getContainedName()] = value
        return state, None


class Program(AstNode):
    def __init__(self, previous_commands, next_command):
        super(Program, self).__init__(previous_commands, next_command)

    def _method(self, state, previous_commands, next_command):
        if previous_commands is not None:
            state, return_value = previous_commands.evaluate(state)
            if return_value is not None:
                return state, return_value
        return next_command.evaluate(state)


class TimeSpan(AstNode):
    def __init__(self, days=Number(0), hours=Number(0), minutes=Number(0), seconds=Number(0)):
        super(TimeSpan, self).__init__(self, days=days, hours=hours, minutes=minutes, seconds=seconds)

    def _method(self, state, *args, **kwargs):
        evaluated_kwargs = {}
        for key, value in kwargs.items():
            state, evaluated_kwargs[key] = value.evaluate(state)
        return state, Types.TimeSpan(**evaluated_kwargs)


class TimeUnion(AstNode):
    def __init__(self, span1, span2):
        super(TimeUnion, self).__init__(span1, span2)

    def _method(self, state, span1, span2):
        state, evaluated_span1 = span1.evaluate(state)
        state, evaluated_span2 = span2.evaluate(state)
        return state, Types.TimeSpan.unite(evaluated_span1, evaluated_span2)


class TimeFractionAddition(AstNode):
    def __init__(self, span, fraction):
        super(TimeFractionAddition, self).__init__(span, fraction)

    def _method(self, state, span, fraction):
        state, evaluated_span = span.evaluate(state)
        state, evaluated_fraction = fraction.evaluate(state)
        return state, evaluated_span.addFraction(evaluated_fraction)


class TimePoint(AstNode):
    def __init__(self, hour=Number(0), minute=Number(0)):
        super(TimePoint, self).__init__(hour=hour, minute=minute)

    def _method(self, state, *args, **kwargs):
        evaluated_kwargs = {}
        for key, value in kwargs.items():
            state, evaluated_kwargs[key] = value.evaluate(state)
        return state, datetime.time(**evaluated_kwargs)


class FunctionInvocation(AstNode):
    def __init__(self, function_name, args):
        super(FunctionInvocation, self).__init__(function_name, args)

    def _method(self, state, function_name, args):
        evaluated_args = []
        for arg in args:
            state, new_arg = arg.evaluate(state)
            evaluated_args += [new_arg]
        return state, state.method_dict[function_name](evaluated_args)


class ReturnValue(AstNode):
    def __init__(self, return_value):
        super(ReturnValue, self).__init__(return_value)

    def _method(self, state, return_value):
        return return_value.evaluate(state)


class FunctionDefinition(AstNode):
    def __init__(self, function_name, argument_names, command_subtree):
        super(FunctionDefinition, self).__init__(function_name, argument_names, command_subtree)

    @staticmethod
    def turn_ast_into_function(state, function_name, argument_names, abstract_syntax_tree):
        def subtree_function(argument_list):
            closure = copy.deepcopy(state)
            if len(argument_list) != len(argument_names):
                raise TypeError(str(function_name) + "() expects " +
                                str(len(argument_names)) + "arguments:\n\t" +
                                str([name.getContainedName() for name in argument_names]) + "\nbut " +
                                str(len(argument_list)) + "were given.")
            for i in range(len(argument_list)):
                closure.variables[argument_names[i].getContainedName()] = argument_list[i]
            return abstract_syntax_tree.evaluate(closure)[1]

        return subtree_function

    def _method(self, state, function_name, argument_names, command_subtree):
        state.method_dict[function_name] = self.turn_ast_into_function(
            state, function_name, argument_names, command_subtree)
        return state, None


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
