import sys


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
    return ("\\texttt{" if raw_token.startswith('T') else "\\textit{") + convert_name_to_token(
        raw_token[1:]) + "}"


parse_rules = {}

with open("esperanto/src/compilation/parser.out", 'r') as input_file:
    for line in input_file.readlines():
        if line.startswith("Rule "):
            line_parts = line.split()
            lhs = convert_raw_token_to_tex(line_parts[2])
            rhs = [convert_raw_token_to_tex(token) for token in line_parts[4:]]
            if lhs not in parse_rules:
                parse_rules[lhs] = [" ".join(rhs)]
            else:
                parse_rules[lhs] += [" ".join(rhs)]

should_print_result = "-p" in sys.argv[1:]
with open("Manuscripts/MscThesis/M_RawParseRules.tex", 'w') as output_file:
    for lhs in parse_rules:
        output_file.write("\n")
        output_file.write(lhs + " = " + parse_rules[lhs][0] + "\\\\\n")
        for other_derivative in parse_rules[lhs][1:]:
            output_file.write("\\-\\hspace{2cm}\\textbar\\-\\hspace{0.5cm}" + other_derivative + "\\\\\n")

if should_print_result:
    with open("Manuscripts/MscThesis/M_RawParseRules.tex", 'r') as output_file:
        for line in output_file.readlines():
            print(line, end='', flush=False)
    sys.stdout.flush()
