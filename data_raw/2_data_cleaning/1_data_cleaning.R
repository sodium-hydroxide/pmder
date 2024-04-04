library(btools)
package_load(
    "readr",
    "dplyr",
    "stringr",
    "tidyr"
)

wd <- paste(
    getwd(),
    "/",
    "data_raw/2_data_cleaning/",
    sep = ""
)


raw_data <- read_csv(paste(
    getwd(),
    "/",
    "data_raw/1_output_parsing/raw_data.csv",
    sep = ""
))

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


raw_data[
    (raw_data$bin_mev == 0) & (raw_data$tally %in% c(118, 128, 218, 228)),
]$value <- 0

spectral_parity_data <-
    raw_data |>
    mutate(
        contents = as.factor(run_id_contents(run_id)),
        y_m = run_id_y_m(run_id),
        Es_keV = run_id_Es_keV(run_id),
        Ed_keV = bin_mev * 1000,
        tally = tally_rename(as.character(tally))
    ) |>
    as.data.frame() |>
    select(
        "contents", "Es_keV", "Ed_keV", "y_m", "value", "u_value", "tally"
    ) |>
    as_tibble() |>
    pivot_wider(
        names_from = "tally",
        values_from = c("value", "u_value"),
        id_cols = c("contents", "Es_keV", "Ed_keV", "y_m")
    ) |>
    rename(
        LF1 = value_LF1,
        LF2 = value_LF2,
        LF4 = value_LF4,
        LF8 = value_LF8,
        LsF1 = value_LsF1,
        LsF2 = value_LsF2,
        LsF4 = value_LsF4,
        LsF8 = value_LsF8,
        RF1 = value_RF1,
        RF2 = value_RF2,
        RF4 = value_RF4,
        RF8 = value_RF8,
        RsF1 = value_RsF1,
        RsF2 = value_RsF2,
        RsF4 = value_RsF4,
        RsF8 = value_RsF8,
        u_LF1 = u_value_LF1,
        u_LF2 = u_value_LF2,
        u_LF4 = u_value_LF4,
        u_LF8 = u_value_LF8,
        u_LsF1 = u_value_LsF1,
        u_LsF2 = u_value_LsF2,
        u_LsF4 = u_value_LsF4,
        u_LsF8 = u_value_LsF8,
        u_RF1 = u_value_RF1,
        u_RF2 = u_value_RF2,
        u_RF4 = u_value_RF4,
        u_RF8 = u_value_RF8,
        u_RsF1 = u_value_RsF1,
        u_RsF2 = u_value_RsF2,
        u_RsF4 = u_value_RsF4,
        u_RsF8 = u_value_RsF8
    )

spectral_data <-
    spectral_parity_data |>
    mutate(
        F1 = 0.5 * (LF1 + RF1),
        F2 = 0.5 * (LF2 + RF2),
        F4 = 0.5 * (LF4 + RF4),
        F8 = 0.5 * (LF8 + RF8),
        u_F1 = 0.5 * sqrt((u_LF1 ^ 2) + (u_RF1 ^ 2)),
        u_F2 = 0.5 * sqrt((u_LF2 ^ 2) + (u_RF2 ^ 2)),
        u_F4 = 0.5 * sqrt((u_LF4 ^ 2) + (u_RF4 ^ 2)),
        u_F8 = 0.5 * sqrt((u_LF8 ^ 2) + (u_RF8 ^ 2)),
        PrReach = F1,
        uPrReach = u_F1,
        PrDet = F8,
        uPrDet = u_F8
    ) |>
    select(
        "contents", "Es_keV", "Ed_keV", "y_m", "F1", "F2", "F4", "F8", "u_F1",
        "u_F2", "u_F4", "u_F8", "PrReach", "uPrReach", "PrDet", "uPrDet"
    )

summary_data <-
    spectral_data |>
    group_by(
        contents, Es_keV, y_m
    ) |>
    summarise(
        PrReach = sum(PrReach),
        uPrReach = btools::rss(uPrReach),
        PrDet = sum(PrDet),
        uPrDet = btools::rss(uPrDet)
    ) |>
    as.data.frame()


write_csv(
    spectral_parity_data,
    file = paste(wd, "spectral_parity_data.csv", sep = "")
)

write_csv(
    spectral_data,
    file = paste(wd, "spectral_data.csv", sep = "")
)

write_csv(
    summary_data,
    file = paste(wd, "summary_data.csv", sep = "")
)

save(
    summary_data,
    file = paste(
        getwd(),
        "/data/summary_data.rda",
        sep = ""
    )
)

save(
    spectral_data,
    file = paste(
        getwd(),
        "/data/spectral_data.rda",
        sep = ""
    )
)
