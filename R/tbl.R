

tbl <- function(tbl_name) {

    if (tbl_name == "materials_general") {
        table <- data.frame(
            material = c(
                "Air", "", "", "", "Concrete", "", "", "", "", "", "",
                "Earth", "", "", ""
            ),
            density = c(
                "$1.205 \\times 10^{-3}$", "", "", "", "$1.032$", "",
                "", "", "", "", "", "$1.520$", "", "", ""
            ),
            atom = c(
                "$\\mathrm{C}$", "$\\mathrm{N}$", "$\\mathrm{O}$",
                "$\\mathrm{Ar}$", "$\\mathrm{H}$", "$\\mathrm{O}$",
                "$\\mathrm{Na}$", "$\\mathrm{Al}$", "$\\mathrm{Si}$",
                "$\\mathrm{Ca}$", "$\\mathrm{Fe}$", "$\\mathrm{H}$",
                "$\\mathrm{O}$", "$\\mathrm{Al}$", "$\\mathrm{Si}$"
            ),
            mass_fraction = c(
                "$1.24 \\times 10^{-4}$", "$7.55 \\times 10^{-1}$",
                "$2.32 \\times 10^{-1}$", "$1.28 \\times 10^{-2}$",
                "$4.53 \\times 10^{-3}$", "$5.13 \\times 10^{-1}$",
                "$1.53 \\times 10^{-2}$", "$3.56 \\times 10^{-2}$",
                "$3.60 \\times 10^{-1}$", "$5.79 \\times 10^{-2}$",
                "$1.38 \\times 10^{-2}$", "$2.38 \\times 10^{-2}$",
                "$5.99 \\times 10^{-1}$", "$8.04 \\times 10^{-2}$",
                "$2.97 \\times 10^{-1}$"
            )
        )

        col_names <- c(
            "Material", "Mass Density ($\\mathrm{g}\\,\\mathrm{cm}^{-3}$)",
            "Atom", "Mass Fraction"
        )
    }

    else if (tbl_name == "materials_portal_monitor") {

        table <- data.frame(
            material = c(
                "Polyvinyl", "Toluene", "Carbon Steel", "", "Lead"
            ),
            density = c(
                "$2.250$", "", "$7.820$", "", "$11.350$"
            ),
            atom = c(
                "$\\mathrm{H}$", "$\\mathrm{C}$", "$\\mathrm{C}$",
                "$\\mathrm{Fe}$ ", "$\\mathrm{Pb}$ "
            ),
            mass_fraction = c(
                "$8.50 \\times 10^{-2}$", "$9.15 \\times 10^{-1}$",
                "$5.00 \\times 10^{-3}$", "$9.95 \\times 10^{-1}$", "$1.00$"
            )
        )

        col_names <- c(
            "Material", "Mass Density ($\\mathrm{g}\\,\\mathrm{cm}^{-3}$)",
            "Atom", "Mass Fraction"
        )
    }

    else if (tbl_name == "materials_truck") {
        table <- data.frame(
            material = c(
                "Aluminium", "Foodstuff", "", "", "", "", "", "", "", "",
                "", "", "", "", "", "Scrap Metal", "", "", "", "", "", "",
                "", "", "", ""
            ),
            density = c(
                "$2.699$", "$5.006\\cdot 10^{-1}$", "", "", "", "", "", "",
                "", "", "", "", "", "", "", "$3.387\\times 10^{-1}$", "",
                "", "", "", "", "", "", "", "", ""

            ),
            atom = c(
                "$\\mathrm{Al}$", "$\\mathrm{H}$", "$\\mathrm{C}$",
                "$\\mathrm{N}$", "$\\mathrm{O}$", "$\\mathrm{Na}$",
                "$\\mathrm{Mg}$", "$\\mathrm{P}$", "$\\mathrm{S}$",
                "$\\mathrm{Cl}$", "$\\mathrm{Ar}$", "$\\mathrm{K}$",
                "$\\mathrm{Ca}$", "$\\mathrm{Fe}$", "$\\mathrm{Zn}$",
                "$\\mathrm{C}$", "$\\mathrm{N}$", "$\\mathrm{O}$",
                "$\\mathrm{Si}$", "$\\mathrm{P}$", "$\\mathrm{S}$",
                "$\\mathrm{Ar}$", "$\\mathrm{Cr}$", "$\\mathrm{Mn}$",
                "$\\mathrm{Fe}$", "$\\mathrm{Ni}$"
            ),
            mass_fraction = c(
                "$1.00$", "$1.04 \\times 10^{-1}$",
                "$2.31 \\times 10^{-1}$", "$2.57 \\times 10^{-2}$",
                "$6.30 \\times 10^{-1}$", "$1.13 \\times 10^{-3}$",
                "$1.30 \\times 10^{-4}$", "$1.33 \\times 10^{-3}$",
                "$1.99 \\times 10^{-3}$", "$1.34 \\times 10^{-3}$",
                "$1.54 \\times 10^{-5}$", "$1.99 \\times 10^{-3}$",
                "$2.30 \\times 10^{-4}$", "$4.99 \\times 10^{-5}$",
                "$3.00 \\times 10^{-5}$", "$7.48 \\times 10^{-4}$",
                "$3.82 \\times 10^{-2}$", "$7.89 \\times 10^{-4}$",
                "$4.98 \\times 10^{-3}$", "$2.99 \\times 10^{-3}$",
                "$1.49 \\times 10^{-4}$", "$4.67 \\times 10^{-5}$",
                "$1.79 \\times 10^{-1}$", "$8.72 \\times 10^{-2}$",
                "$6.73 \\times 10^{-1}$", "$4.98 \\times 10^{-2}$"
            )
        )

        col_names <- c(
            "Material", "Mass Density ($\\mathrm{g}\\,\\mathrm{cm}^{-3}$)",
            "Atom", "Mass Fraction"
        )
    }

    else

    else {
        stop(paste(
            "Potential tables are:",
            "materials_general",
            "materials_portal_monitor",
            sep = "\n"
        ))
    }

    return(list(table = table, col_names = col_names))
}

