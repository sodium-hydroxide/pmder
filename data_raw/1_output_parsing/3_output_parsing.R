wd <- paste(
    getwd(),
    "/",
    "data_raw/1_output_parsing/",
    sep = ""
)
data_dir <- paste(wd, "truckPtSource/", sep = "")
library(btools)
package_load(
    "readr",
    "dplyr"
)

input_list <- readLines(paste(data_dir, "listOfInputs.txt", sep = ""))

input_list <- paste("out_", input_list, sep = "")

results <- mcnparse(input_list, directory = data_dir)

statistical_checks <- results$check
raw_data <- results$data

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





