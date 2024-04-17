devtools::load_all()
wd <- paste(
    getwd(),
    "/",
    "data_raw/1_output_parsing/",
    sep = ""
)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

data_dir <- paste(wd, "truckPtSource/", sep = "")
input_list <- readLines(paste(data_dir, "listOfInputs.txt", sep = ""))
input_list <- paste("out_", input_list, sep = "")

results_original <- mcnparse(input_list, directory = data_dir)

data_dir <- paste(wd, "additional_position/", sep = "")
input_list <- readLines(paste(data_dir, "list_of_inputs.txt", sep = ""))
input_list <- paste("out_", input_list, sep = "")

results_additional_energy <- mcnparse(input_list, directory = data_dir)

data_dir <- paste(wd, "additional_energy/", sep = "")
input_list <- readLines(paste(data_dir, "list_of_inputs.txt", sep = ""))
input_list <- paste("out_", input_list, sep = "")

results_additional_position <- mcnparse(input_list, directory = data_dir)

statistical_checks <-
    mutate(
        rbind(
            mutate(results_original$check, batch = "original"),
            mutate(results_additional_energy$check, batch = "new_energy"),
            mutate(results_additional_position$check, batch = "new_position")
        ),
        batch = as.factor(batch)
    )

raw_data <-
    mutate(
        rbind(
            mutate(results_original$data, batch = "original"),
            mutate(results_additional_energy$data, batch = "new_energy"),
            mutate(results_additional_position$data, batch = "new_position")
        ),
        batch = as.factor(batch)
    )


raw_data <-
    raw_data |>
    mutate(se = value * re) |>
    relocate(se, .after = re) |>
    rename(
        urel_value = re,
        u_value = se
    )

raw_data[
    (raw_data$bin_mev == 0) & (raw_data$tally %in% c(118, 128, 218, 228)),
]$value <- 0


run_id_contents <- Vectorize(function(id) {
    return(str_split_1(id, "_")[2])
})

run_id_y_m <- Vectorize(function(id) {
    return(
        gsub("m", "-", str_split_1(id, "_")[3]) |>
            as.numeric() /
            100
    )
})

run_id_Es_keV <- Vectorize(function(id) {
    return(
        gsub("dot", "\\.", str_split_1(id, "_")[4]) |>
            as.numeric() *
            1000
    )
})

tally_rename <- Vectorize(function(tally_val) {
    tally_val <- str_split_1(tally_val, "")

    new_name <- ""

    if (tally_val[1] == "1") {
        new_name <- paste(new_name, "L", sep = "")
    }
    else {
        new_name <- paste(new_name, "R", sep = "")
    }
    if (tally_val[2] == "1") {
        new_name <- paste(new_name, "F", sep = "")
    }
    else {
        new_name <- paste(new_name, "sF", sep = "")
    }

    new_name <- paste(new_name, tally_val[3], sep = "")

    return(new_name)
})

raw_data <-
    raw_data |>
    mutate(
        contents = as.factor(run_id_contents(run_id)),
        y_m = run_id_y_m(run_id),
        Es_keV = run_id_Es_keV(run_id),
        Ed_keV = bin_mev * 1000,
        tally_name = tally,
        tally = tally_rename(as.character(tally))
    ) |>
    as.data.frame()

# I accidentally changed naming conventions for the new data
raw_data <-
    rbind(
        mutate(
            filter(raw_data, batch != "original"),
            Es_keV = Es_keV / 1000
        ),
        filter(raw_data, batch == "original")
    )

# Accidentally ran 200 keV twice
raw_data <-
    rbind(
        filter(raw_data, Es_keV != 200),
        filter(raw_data, Es_keV == 200 & batch == "original")
    )

# floating point error for Cs-137
raw_data[raw_data$Es_keV > 650 & raw_data$Es_keV < 700,]$Es_keV <- 661.6

write_csv(
    statistical_checks,
    file = paste(wd, "statistical_checks.csv", sep = "")
)
write_csv(
    raw_data,
    file = paste(wd, "raw_data.csv", sep = "")
)





