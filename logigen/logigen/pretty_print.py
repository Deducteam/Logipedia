def pretty_print(ppterm):
    return pretty_print_latex(ppterm)

def pretty_print_latex(ppterm):
    typ, body = ppterm
    if typ == "Binder":
        return _pp_binder(body)
    elif typ == "Const":
        return _pp_const(body)
    elif typ == "Var":
        return __pp_var(body)
    else:
        raise ValueError("term should be a Binder, Const, or Var")

def _pp_args(args):
    return "\\ ".join([pretty_print_latex(arg) for arg in args])

def _pp_annot(annot):
    return f": {pretty_print_latex(annot)}" if annot else ""

def _pp_const(const):
    c_args, c_symb = [const[k] for k in ('c_args', 'c_symb')]
    if not c_args:
        return c_symb
    else:
        return f"\\left({c_symb} {_pp_args(c_args)} \\right)"

def __pp_var(var):
    v_args, v_symb = [var[k] for k in ('v_args', 'v_symb')]
    if not v_args:
        return v_symb
    else:
        return f"\\left({v_symb} {_pp_args(v_args)} \\right)"

def _pp_binder(binder):
    annotation, b_args, b_symb, body, bound = [binder[k] for k in ('annotation', 'b_args', 'b_symb', 'body', 'bound')]
    if not b_args:
        return f"\\left({b_symb} {bound} {_pp_annot(annotation)} , {pretty_print_latex(body)} \\right)"
    else:
        return f"\\left(\\left({b_symb} {bound} {_pp_annot(annotation)} , {pretty_print_latex(body)} \\right) {_pp_args(b_args)} \\right)"
    return str(binder)

