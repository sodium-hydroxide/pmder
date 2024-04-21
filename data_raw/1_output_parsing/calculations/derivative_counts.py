from sympy import (
    symbols,
    Symbol,
    sqrt,
    integrate,
    pretty_print,
    init_printing,
    latex,
    simplify,
    limit,
    diff,
    solve
)
init_printing(use_latex = True)

yperp = Symbol("y_{\perp}", positive = True)
beta = Symbol("\\hat{\\beta}_2", negative = True)
y = Symbol("y")
count_rate_derivative = diff(sqrt((y ** 2) + (yperp ** 2)) ** (beta) ,y)

y_crit = solve(count_rate_derivative, y)