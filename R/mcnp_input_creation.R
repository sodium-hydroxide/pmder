#' Create MCNP inputs for portal monitors
#'
#' Given a list of source and truck configurations, the MCNP decks will be written along with a bash script to execute the runs.
#'
#' @param energy_list_kev List of photon source energies in kiloelectron-volts
#' @param truck_position_list_cm List of truck positions in centimeters
#' @param content_list `c("f", "m")` Contents of interior
#'
#' @export
#'
mcnp_input_creation <- function(
        energy_list_kev,
        truck_position_list_cm,
        content_list
) {

    # Subroutines ----

    history_number <- function(
            energy_keV,# Float64
            y_cm, # Float64
            composition) {
        if (abs(y_cm) < 300) {
            if ((energy_keV < 500) || (energy_keV > 1300)) {
                histories_required <- 1e8
            }
            else {histories_required <- 4e8}
        }
        else if (abs(y_cm) < 800) {
            if (energy_keV < 600) {histories_required <- 1e9}
            else {histories_required <- 1e8}
        }
        else if(abs(y_cm) < 1300) {
            if (energy_keV < 600) {histories_required <- 3e9}
            else if (energy_keV < 1200) {histories_required <- 3e8}
            else {histories_required <- 1e8}
        }
        else {
            if (energy < 1000) {histories_required <- 2.5e9}
            else {histories_required <- 3e9}
        }

        return(bit64::as.integer64(histories_required))

    }

    parameters_file_name <- function(
            energy_keV, # Float64
            y_cm, #Float64
            composition) {
        return(paste(
            composition, "_",
            gsub("-", "m", as.character(y_cm)), "_",
            gsub("\\.", "dot", as.character(energy_keV)),
            sep = ""
        ))
    }



    single_input <- function(
            energy_keV,
            y_cm,
            composition) {

        # Get the number of particle histories ----
        histories <- history_number(energy_keV,y_cm,composition)

        # Get the file_name which will be used ----
        file_name <- parameters_file_name(energy_keV,y_cm,composition)

        # Get the truck cell card ----
        if (
            composition == "f"
        ) {
            truck_cell_card <-
                "45   8 -0.5006025 -42    IMP:N=0 IMP:P=2 $ Truck interior"
        } else {
            truck_cell_card <-
                "45   9 -0.3387145  -42    IMP:N=0 IMP:P=2 $ Truck interior"
        }

        # Get the truck y min and max values for interior and exterior ----
        exterior_min <- -426.720 + y_cm
        exterior_max <- 426.720 + y_cm
        interior_min <- -426.598 + y_cm
        interior_max <- 426.598 + y_cm

        # Create MCNP Input Deck ----
        deck <- c(
            file_name,
            "c",
            "c cccccccccccccccccccccccccccccc",
            "c                              c",
            "c  CELL CARD BLOCK             c",
            "c                              c",
            "c cccccccccccccccccccccccccccccc",
            "c",
            "c  Define the void and global geometry",
            "1    0             1 IMP:N=0 IMP:P=0          $ Void",
            "10   4 -1.520000 -11 IMP:N=0 IMP:P=1          $ Ground",
            "20   3 -1.032000 -21 IMP:N=0 IMP:P=2          $ Concrete roadway",
            "30   1 -0.001205 -31 41 51 61 IMP:N=0 IMP:P=2 $ Air",
            "c  Truck cell cards",
            "40   7 -2.698900  -41 42 IMP:N=0 IMP:P=2 $ Truck shell",
            truck_cell_card,
            "c  Detector cell cards",
            "50   5 -7.820000  -51 52 IMP:N=0 IMP:P=2 $ PVT shell",
            "51   1 -0.001205  -52 53 IMP:N=0 IMP:P=2 $ PVT Air Gap",
            "52   6 -11.350000 -53 54 IMP:N=0 IMP:P=2 $ PVT Lead Shield",
            "53   2 -2.250000  -54 IMP:N=0 IMP:P=2    $ PVT Scintillator",
            "c  Detector cell cards",
            "60   5 -7.820000  -61 62 IMP:N=0 IMP:P=2 $ PVT Steel shell",
            "61   1 -0.001205  -62 63 IMP:N=0 IMP:P=2 $ PVT Air Gap",
            "62   6 -11.350000 -63 64 IMP:N=0 IMP:P=2 $ PVT Lead Shield",
            "63   2 -2.250000  -64 IMP:N=0 IMP:P=2    $ PVT Scintillator",
            "c",
            "c  END OF CELL CARDS LEAVE FOLLOWING LINE BLANK AS DELIMINATER",
            "",
            "c cccccccccccccccccccccccccccccc",
            "c                              c",
            "c  SURFACE CARD BLOCK          c",
            "c                              c",
            "c cccccccccccccccccccccccccccccc",
            "c",
            "c  World for Simulation",
            "1  RPP -750 750 -2500 2500 -20 600",
            "c",
            "c  The ground is the lower half of the simulation space",
            "11   RPP -750 750 -2500 2500 -20 -10",
            "c",
            "c  The concrete will be a layer in the ground",
            "21   RPP -750 750 -2500 2500 -10 0",
            "c  Air",
            "c      width =  cm (X)",
            "c     length =  cm (Y)",
            "c     height =  cm (Z)",
            "31   RPP -750 750 -2500 2500 0 600",
            "c", paste(
                "41   RPP -130.000 130.000 ", exterior_min, " ", exterior_max,
                " 130.5 411.5000 $ Outer truck layer",
                sep = ""
            ), paste(
                "42   RPP -129.878 129.878 ", interior_min, " ", interior_max,
                " 134.0 411.398 $ Inner truck layer",
                sep = ""
            ),
            "c  These cards will form the geometry of the left RPM",
            "c    xmin  xmax  ymin   ymax  zmin zmax",
            "51   RPP -204.54 -179.54 -86.64 86.64 60.00 239.28 $ PVT shell exterior",
            "52   RPP -201.54 -179.54 -83.64 83.64 63.00 236.28 $ PVT shell interior",
            "53   RPP -201.54 -192.10 -78.64 78.64 78.00 231.28 $ PVT shield exterior",
            "54   RPP -200.90 -192.10 -78.00 78.00 78.64 230.64 $ PVT shield exterior",
            "c  These cards will form the geometry of the right RPM",
            "c    xmin  xmax  ymin   ymax  zmin zmax",
            "61   RPP 179.54 204.54 -86.64 86.64 60.00 239.28 $ PVT shell exterior",
            "62   RPP 179.54 201.54 -83.64 83.64 63.00 236.28 $ PVT shell interior",
            "63   RPP 192.10 201.54 -78.64 78.64 78.00 231.28 $ PVT shield exterior",
            "64   RPP 192.10 200.90 -78.00 78.00 78.64 230.64 $ PVT shield exterior",
            "c",
            "c  END OF SURFACE CARDS LEAVE FOLLOWING LINE BLANK AS DELIMINATER",
            "",
            "c cccccccccccccccccccccccccccccc",
            "c                              c",
            "c  DATA CARD BLOCK             c",
            "c                              c",
            "c cccccccccccccccccccccccccccccc",
            "c",
            "c cccccccccccccccccccccccccccccc",
            "c  MATERIALS CARD BLOCK        c",
            "c cccccccccccccccccccccccccccccc",
            "c",
            "c  Air (Dry, Near Sea Level)",
            "c    Mass Density (g/cm3): = 0.001205",
            "M1   006000 -0.000124 $ $ Atomic carbon",
            "     007000 -0.755268 $ Atomic nitrogen",
            "     008000 -0.231781 $ Atomic oxygen",
            "     018000 -0.012827 $ Atomic argon",
            "c",
            "c  Polyvinyl Toluene",
            "c    Mass Density (g/cm3): = 2.250000",
            "M2   001000 -0.085000 $",
            "     006000 -0.915000 $",
            "c",
            "c  Concrete, Los Alamos (MCNP)",
            "c    Mass Density (g/cm3): = 1.032000",
            "M3   001000 -0.004530 $ -nat",
            "     008000 -0.512600 $ -nat",
            "     011000 -0.015270 $ -nat",
            "     013000 -0.035550 $ -nat",
            "     014000 -0.360360 $ -nat",
            "     020000 -0.057910 $ -nat",
            "     026000 -0.013780 $ -nat",
            "c",
            "c  Earth, Typical Western U.S.",
            "c    Mass Density (g/cm3): = 1.520000",
            "M4   001000 -0.023834 $",
            "     008000 -0.598898 $",
            "     013000 -0.080446 $",
            "     014000 -0.296821 $",
            "c",
            "c  PVT Holder material",
            "c  Steel, Carbon",
            "c     Mass Density (g/cm3): = 7.820000",
            "M5   006000 -0.005000 $",
            "     026000 -0.995000 $",
            "c",
            "c  Lead",
            "c     Mass Density (g/cm3): = 11.350000",
            "M6   082000 -1.000000 $",
            "c",
            "c  Truck Shell",
            "c  Steel, Stainless 202",
            "c     Mass Density (g/cm3): = 2.698900",
            "M7   013000 -1.000000 $ Aluminum-nat",
            "c",
            "c  Truck Interior, Food Stuffs",
            "c     Mass Density (g/cm3): = 0.5006025",
            "M8   001000 -0.104346262753382    $ Hydrogen-nat",
            "     006000 -0.231910697030079    $ Carbon-nat",
            "     007000 -0.0257590582747789   $ Nitrogen-nat",
            "     008000 -0.629758437188188    $ Oxygen-nat",
            "     011000 -0.00112863998881348  $ Sodium-nat",
            "     012000 -0.000129843538536064 $ Magnesium-nat",
            "     015000 -0.00132839927886896  $ Phosphorus-nat",
            "     016000 -0.00198760493605206  $ Sulfur-nat",
            "     017000 -0.00133838724337174  $ Chlorine-nat",
            "     018000 -1.54379322915886e-05 $ Argon-nat",
            "     019000 -0.00198760493605206  $ Potassium-nat",
            "     020000 -0.000229723183563806 $ Calcium-nat",
            "     026000 -4.99398225138708e-05 $ Iron-nat",
            "     030000 -2.99638935083225e-05 $ Zinc-nat",
            "c",
            "c  Truck Interior, Metals",
            "c  Steel, Stainless 202",
            "c  and Dry Air",
            "c     Mass Density (g/cm3): = 0.3387145",
            "M9   006000 -0.000747868606137262 $ C-nat",
            "     007000 -0.00381726731245058  $ N-nat",
            "     008000 -0.000789163899200162 $ O-nat",
            "     014000 -0.00498297608735832  $ Si-nat",
            "     015000 -0.000298978565241499 $ P-nat",
            "     016000 -0.00014948928262075  $ S-nat",
            "     018000 -4.36731454909612e-05 $ Ar-nat",
            "     024000 -0.1793871391449      $ Cr-nat",
            "     025000 -0.0872020815287706   $ Mn-nat",
            "     026000 -0.672751601554247    $ Fe-nat",
            "     028000 -0.0498297608735832   $ Ni-nat",
            "c",
            "c cccccccccccccccccccccccccccccc",
            "c  CROSS-SECTION CARD BLOCK    c",
            "c cccccccccccccccccccccccccccccc",
            "c",
            "c cccccccccccccccccccccccccccccc",
            "c  SOURCE CARD BLOCK           c",
            "c cccccccccccccccccccccccccccccc",
            "c", paste(
                "SDEF POS 0 ", y_cm, " 272.699 ERG=", energy_keV / 1000," PAR=2",
                sep = ""
            ),
            "c",
            "c cccccccccccccccccccccccccccccc",
            "c  TALLY CARD BLOCK            c",
            "c cccccccccccccccccccccccccccccc",
            "c",
            "c Left PVT",
            "c Surface Current",
            "F111:P 53.1",
            "E111  0 199i 2",
            "*F121:P 53.1",
            "E121  0 199i 2",
            "c Surface Fluence",
            "F112:P 53.1",
            "E112  0 199i 2",
            "*F122:P 53.1",
            "E122  0 199i 2",
            "c Cell Fluence",
            "F114:P 53",
            "E114  0 199i 2",
            "*F124:P 53",
            "E124  0 199i 2",
            "c Cell Pulse",
            "F118:P 53",
            "E118  0 199i 2",
            "*F128:P 53",
            "E128  0 199i 2",
            "c",
            "c",
            "c Right PVT",
            "c Surface Current",
            "F211:P 63.2",
            "E211  0 199i 2",
            "*F221:P 63.2",
            "E221  0 199i 2",
            "c Surface Fluence",
            "F212:P 63.2",
            "E212  0 199i 2",
            "*F222:P 63.2",
            "E222  0 199i 2",
            "c Cell Fluence",
            "F214:P 63",
            "E214  0 199i 2",
            "*F224:P 63",
            "E224  0 199i 2",
            "c Cell Pulse",
            "F218:P 63",
            "E218  0 199i 2",
            "*F228:P 63",
            "E228  0 199i 2",
            "c",
            "c cccccccccccccccccccccccccccccc",
            "c  MODE AND NPS BLOCK          c",
            "c cccccccccccccccccccccccccccccc",
            "c",
            "MODE P",
            paste("NPS ", histories, sep = ""),
            "c",
            "c cccccccccccccccccccccccccccccc",
            "c  OTHER DATA CARDS BLOCK      c",
            "c cccccccccccccccccccccccccccccc",
            "c",
            "c  END OF DATA CARDS LEAVE FOLLOWING LINE BLANK AS DELIMINATER"
        )

        # Terminal Command ----
        terminal_command <- paste(
            "mpirun --use-hwthread-cpus /home/dbo/MY_MCNP/MCNP_truckPtSource/MCNP620/bin/mcnp6.mpi i=in_",
            file_name,
            ".mcnp o=out_",
            file_name,
            ".mcnpout r=run_",
            file_name,
            " xsdir=/home/dbo/MY_MCNP/MCNP_DATA/xsdir_mcnp6.2 > term_",
            file_name,
            ".txt",
            sep = ""
        )

        # Return everything ----
        return(list(
            deck = deck,
            terminal = terminal_command,
            file = file_name
        ))

    }


    # Basic Initialization ----
    directory <- paste("run", Sys.Date(), sep = "")
    dir.create(directory)

    number_of_runs <- prod(
        length(energy_list_kev),
        length(truck_position_list_cm),
        length(content_list)
    )

    bash_script <- c()
    input_list <- c()

    i <- 1

    # Create input file and shell script for each file ----
    for (energy_keV in energy_list_kev) {
        for (y_cm in truck_position_list_cm) {
            for (contents in content_list) {
                current_run <- single_input(
                    energy_keV,
                    y_cm,
                    contents
                )

                bash_script <- c(bash_script,current_run$terminal,paste(
                    "echo \" Run ",
                    i,
                    " of ",
                    number_of_runs,
                    " complete\"",
                    sep = ""
                ))
                input_list <- c(input_list,current_run$file)

                writeLines(
                    current_run$deck,
                    paste(directory,"/in_",current_run$file,".mcnp",sep = "")
                )

                i <- i+1
            }
        }
    }

    writeLines(bash_script,paste(directory, "/batch_run.sh", sep = ""))
    writeLines(input_list,paste(directory, "/list_of_inputs.txt", sep = ""))
}
