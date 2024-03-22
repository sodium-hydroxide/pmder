tbl <- function() {
    return(list(
        methods_materials_general =
            knitr::kable(
                data.frame(
                    material =
                        c(
                            "Air", "", "", "", "Concrete", "", "", "", "", "", "",
                            "Earth", "", "", ""
                        ),
                    density =
                        c(
                            "$1.205 \\times 10^{-3}$", "", "", "", "$1.032$", "",
                            "", "", "", "", "", "$1.520$", "", "", ""
                        ),
                    atom =
                        c(
                            "$\\mathrm{C}$", "$\\mathrm{N}$", "$\\mathrm{O}$",
                            "$\\mathrm{Ar}$", "$\\mathrm{H}$", "$\\mathrm{O}$",
                            "$\\mathrm{Na}$", "$\\mathrm{Al}$", "$\\mathrm{Si}$",
                            "$\\mathrm{Ca}$", "$\\mathrm{Fe}$", "$\\mathrm{H}$",
                            "$\\mathrm{O}$", "$\\mathrm{Al}$", "$\\mathrm{Si}$"
                        ),
                    mass_fraction =
                        c(
                            "$1.24 \\times 10^{-4}$", "$7.55 \\times 10^{-1}$",
                            "$2.32 \\times 10^{-1}$", "$1.28 \\times 10^{-2}$",
                            "$4.53 \\times 10^{-3}$", "$5.13 \\times 10^{-1}$",
                            "$1.53 \\times 10^{-2}$", "$3.56 \\times 10^{-2}$",
                            "$3.60 \\times 10^{-1}$", "$5.79 \\times 10^{-2}$",
                            "$1.38 \\times 10^{-2}$", "$2.38 \\times 10^{-2}$",
                            "$5.99 \\times 10^{-1}$", "$8.04 \\times 10^{-2}$",
                            "$2.97 \\times 10^{-1}$"
                        )
                ),
                col.names = c(
                    "Material",
                    "Mass Density ($\\mathrm{g}\\,\\mathrm{cm}^{-3}$)",
                    "Atom", "Mass Fraction"
                )
            )
    ))
}
tbl()$methods_materials_general


