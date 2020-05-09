def leaf_form(lastChild):
    return "└- " if lastChild else "├- "


def branch_form(last_child):
    return "\t" if last_child else "│\t"
