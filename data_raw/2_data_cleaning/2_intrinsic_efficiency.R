devtools::load_all()
library(dplyr)
library(stringr)
library(tidyr)
data("spectral_data")

data_dir <- paste(
    getwd(),
    "/data_raw/1_output_parsing/bareSource/isotropic/outputs/",
    sep = ""
)
input_list <- readLines(paste(data_dir, "deckList.txt", sep = ""))
input_list <- paste("out_", input_list[2:length(input_list)], sep = "")

isotropic_data <-
    mcnparse(input_list, directory = data_dir)$data |>
    mutate(se = value * re) |>
    relocate(se, .after = re) |>
    rename(
        urel_value = re,
        u_value = se
    ) |>
    mutate(
        Es_keV = unname(Vectorize(function(data_run_id) {
            as.numeric(str_split_1(data_run_id, pattern = "-")[2])
        })(run_id)),
        Ed_keV = 1000 * bin_mev,
        tally = unname(Vectorize(function(data_tally) {
            paste(
                "F",
                str_split_1(data_tally, pattern = "")[2],
                sep = ""
            )
        })(tally))
    )

isotropic_data[isotropic_data$bin_mev == 0 &
                   isotropic_data$tally == "F8",]$value <- 0

isotropic_data <-
    isotropic_data |>
    select(-bin_mev, -urel_value) |>
    pivot_wider(
        names_from = "tally",
        values_from = c("value", "u_value"),
        id_cols = c("run_id", "Es_keV", "Ed_keV")
    ) |>
    select(-run_id) |>
    rename(
        F1 = value_F1,
        F8 = value_F8,
        u_F1 = u_value_F1,
        u_F8 = u_value_F8
    )

truck_intrinsic <- intrinsic_efficiency(spectral_data)

intrinsic_data <-
    rbind(
        mutate(
            intrinsic_efficiency(spectral_data),
            data = "truck"
        ),
        mutate(
            intrinsic_efficiency(isotropic_data),
            data = "bare_isotropic"
        )
    ) |>
    mutate(data = as.factor(data))

ggplot2::ggplot(intrinsic_data,ggplot2::aes(x=E_keV,y=int_efficiency, color=data)) + ggplot2::geom_point()

