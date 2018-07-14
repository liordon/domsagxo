import sys

from compilation.abstract_syntax_tree import build, Var


def convert_name_to_token(token_name):
    name_to_token = {
        "PLUS"  : '+',
        "MINUS" : '-',
        "COLON" : ':',
        "TIMES" : '*',
        "DIVIDE": '/',
        "ASSIGN": '=',
        "LPAREN": '(',
        "RPAREN": ')',
        "PERIOD": '.',
        "DELIM" : ',',
    }
    return name_to_token[token_name] if token_name in name_to_token else token_name


def convert_raw_token_to_tex(raw_token):
    actual_token = convert_name_to_token(raw_token[1:]).replace("_", " ")
    return "\\term{" + actual_token + "}" if raw_token.startswith('T') else actual_token


if __name__ == "__main__":
    print("building grammar")
    build(start=Var.PROGRAM.value)

    parse_rules = {}

    print("parsing grammar rules")
    with open("esperanto/src/compilation/parser.out", 'r') as input_file:
        for line in input_file.readlines():
            if line.startswith("Rule "):
                line_parts = line.split()
                lhs = convert_raw_token_to_tex(line_parts[2])
                if lhs.startswith("S'"):
                    continue
                rhs = [convert_raw_token_to_tex(token) for token in line_parts[4:]]
                if lhs not in parse_rules:
                    parse_rules[lhs] = [" \\gspace ".join(rhs)]
                else:
                    parse_rules[lhs] += [" \\gspace ".join(rhs)]

    new_grammar_file = "Manuscripts/MscThesis/raw-grammar-rules.tex"
    print("writing new grammar rules to: " + new_grammar_file)
    should_print_result = "-p" in sys.argv[1:]
    with open(new_grammar_file, 'w') as output_file:
        for lhs in parse_rules:
            output_file.write("\n\\begin{IEEEeqnarray}{lCl}\n")
            output_file.write(lhs + " &=& " + parse_rules[lhs][0])
            for other_derivative in parse_rules[lhs][1:]:
                output_file.write(" \\\\\n\t&|& " + other_derivative + " \\nonumber")
            output_file.write("\n\\end{IEEEeqnarray}\n")

    if should_print_result:
        with open(new_grammar_file, 'r') as output_file:
            for line in output_file.readlines():
                print(line, end='', flush=False)
        sys.stdout.flush()
