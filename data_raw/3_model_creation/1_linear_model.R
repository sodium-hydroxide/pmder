library(btools)
package_load(
    "readr",
    "broom",
    "dplyr"
)
wd <- paste(
    getwd(),
    "/",
    "data_raw/3_model_creation/",
    sep = ""
)

load(paste(
    getwd(),
    "/",
    "data/summary_data.rda",
    sep = ""
))

linear_model <-
    summary_data |>
    mutate(
        r_m = (sqrt(y_m^2 + 1.795^2)),
        PrReach = NULL,
        uPrReach = NULL,
        weight = uPrDet ^ (-2)
    ) |>
    lm(
        log(PrDet) ~ log(r_m) + log(Es_keV) + contents,
        weights = weight
    ) |>
    lm_clean()

save(linear_model, file = paste(
    getwd(),
    "/data/linear_model.rda",
    sep = ""
))

