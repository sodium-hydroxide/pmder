devtools::load_all()
wd <- paste(
    getwd(),
    "/",
    "data_raw/1_output_parsing/",
    sep = ""
)
library(readr)
library(dplyr)

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

write_csv(
    statistical_checks,
    file = paste(wd, "statistical_checks.csv", sep = "")
)
write_csv(
    raw_data,
    file = paste(wd, "raw_data.csv", sep = "")
)





