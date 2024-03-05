
efficiency_test_input <- function(source_energy_keV) {
  # Save energy in MeV
  source_energy_MeV <- source_energy_keV * 1.e-3
  # Create file name for input ----
  file_name <- paste(
    "effTest-",
    gsub("\\.", "dot", as.character(source_energy_keV)),
    "-keV",
    sep = ""
  )
  
  deck <- c(
    file_name,
    "c cccccccccccccccccccccccccccccc",
    "c                              c",
    "c    CELL CARD BLOCK           c",
    "c                              c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    "1    0           11     IMP:N=0 IMP:P=0 $ Void",
    "10   1 -0.000001 -11 21 IMP:N=0 IMP:P=1 $ Air surrounding PVT",
    "20   2 -2.250000 -21    IMP:N=0 IMP:P=1 $ PVT Scintillator",
    "c    END OF CELL CARDS LEAVE FOLLOWING LINE BLANK AS DELIMINATER",
    "",
    "c cccccccccccccccccccccccccccccc",
    "c                              c",
    "c    SURFACE CARD BLOCK        c",
    "c                              c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    "11   RPP -210.00   10.00  -100.00 100.00 000.00 300.00 $ world boundary",
    "21   RPP -200.90 -192.10 -078.00 078.00 078.64 230.64 $ PVT boundary",
    "c    END OF SURFACE CARDS LEAVE FOLLOWING LINE BLANK AS DELIMINATER",
    "",
    "c cccccccccccccccccccccccccccccc",
    "c                              c",
    "c    DATA CARD BLOCK           c",
    "c                              c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    "c cccccccccccccccccccccccccccccc",
    "c    MATERIALS CARD BLOCK      c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    "c  Air (Dry, Near Sea Level)",
    "c    Mass Density (g/cm3): = 0.000000001",
    "M1   006000 -0.000124 $ Atomic carbon",
    "     007000 -0.755268 $ Atomic nitrogen",
    "     008000 -0.231781 $ Atomic oxygen",
    "     018000 -0.012827 $ Atomic argon",
    "c",
    "c  Polyvinyl Toluene",
    "c    Mass Density (g/cm3): = 2.250000",
    "M2   001000 -0.085000 $ Atomic hydrogen",
    "     006000 -0.915000 $ Atomic carbon",
    "c",
    "c cccccccccccccccccccccccccccccc",
    "c    CROSS-SECTION CARD BLOCK  c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    "c cccccccccccccccccccccccccccccc",
    "c    SOURCE CARD BLOCK         c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    paste("SDEF POS 0 0 154.64 ERG=", source_energy_MeV, " PAR=2", sep = ""),
    "c",
    "c cccccccccccccccccccccccccccccc",
    "c     TALLY CARD BLOCK         c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    "F11:P 21.1   $ Flux through front surface",
    "E11 0 199i 2 $ Bin from 0 eV to 2 MeV in 10 keV increments",
    "F18:P 20     $ Deposition events in detector",
    "E18 0 199i 2 $ Bin from 0 eV to 2 MeV in 10 keV increments",
    "c",
    "c cccccccccccccccccccccccccccccc",
    "c    MODE AND NPS BLOCK        c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    "MODE P",
    "NPS 5000000000",
    "c",
    "c cccccccccccccccccccccccccccccc",
    "c    OTHER DATA CARDS BLOCK    c",
    "c cccccccccccccccccccccccccccccc",
    "c",
    "c    END OF DATA CARDS LEAVE FOLLOWING LINE BLANK AS DELIMINATER"
  )
  
  # Terminal Command ----
  term_cmd <- paste(
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
    terminal = term_cmd,
    file = file_name
  ))
}

batch_eff_test <- function(energy_list) {
  directory <- "batchRun"

  R.utils::mkdirs(directory)

  num_runs <- length(energy_list)

  term_out <- c(paste(
    "echo \" Total of ", num_runs, " Decks to Run\"", sep = ""
  ))

  file_list <- c("")

  for (i in 1:num_runs) {
    current_set <- efficiency_test_input(energy_list[i])

    current_file <- current_set$file
    current_deck <- current_set$deck
    current_term <- current_set$terminal

    writeLines(current_deck, paste(
      directory, "/in_", current_file, ".mcnp", sep = ""
    ))

    term_out <- c(term_out, current_term, paste(
      "echo \" Run ", i, " of ", num_runs, " complete\"", sep = ""
    ))

    file_list <- c(file_list, current_file)
  }

  term_out <- c(term_out, "echo \"All runs complete\"")

  writeLines(term_out, paste(
    directory, "/batchRun.sh", sep = ""
  ))

  writeLines(file_list, paste(
    directory, "/deckList.txt", sep = ""
  ))
}

energies_keV <- pracma::linspace(10,2000,n = 200)

energies_keV <- c(511, 662, energies_keV)

batch_eff_test(energies_keV)
