from sympy import (
    symbols,
    Symbol,
    sqrt,
    integrate,
    pretty_print,
    init_printing,
    latex,
    simplify,
    limit
)
init_printing(use_latex = True)

yperp = Symbol("y_{\perp}", positive = True)
beta = Symbol("\\hat{\\beta}_2", negative = True)
x1 = Symbol("x_1")
x2 = Symbol("x_2")
z = Symbol("z")

integrand = sqrt((z ** 2) + (yperp ** 2)) ** (beta)
factor = sqrt((x1 ** 2) + (yperp ** 2)) ** (-beta)

integrated = integrate(
    integrand,
    (z, x2, x1)
)

gx1x2 = simplify(integrated * factor)
# $$
# G\left(
#     x_1, x_2
# \right)
# =
# \frac{
#     y_{\perp}^{\hat{\beta}_2}
# }{
#     \left(x_{1}^{2} + y_{\perp}^{2}\right)^{\frac{\hat{\beta}_2}{2}}
# }
# \left[
#     x_{1}\cdot {}_{2}F_{1}\left(
#         \begin{matrix}
#             \frac{1}{2},
#             -\frac{\hat{\beta}_2}{2} \\
#             \frac{3}{2}
#         \end{matrix} 
#         \middle|
#         -\frac{x_{1}^{2}}{y_{\perp}^{2}} 
#     \right) - 
#     x_{2}\cdot {}_{2}F_{1}\left(
#         \begin{matrix}
#             \frac{1}{2},
#             -\frac{\hat{\beta}_2}{2} \\
#             \frac{3}{2}
#         \end{matrix} 
#         \middle|
#         -\frac{x_{2}^{2}}{y_{\perp}^{2}}
#     \right)
# \right]
# $$