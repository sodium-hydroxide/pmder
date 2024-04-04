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
    "data_raw/2_data_cleaning/",
    "summary_data.rda",
    sep = ""
))

linear_model <-
    summary_data |>
    mutate(
        r_m = (sqrt(y_m^2 + 1.795^2)),
        PrReach = NULL,
        uPrReach = NULL,
        weight = uPrDet ^ (-2),
    ) |>
    lm(
        log(PrDet) ~ log(r_m) + log(Es_keV) + contents,
        data = _,
        weights = weight
    )

linear_model_tidy <-
    linear_model |>
    tidy() |>
    mutate(r.squared = summary(linear_model)$r.squared)

linear_model_aug <- augment(linear_model)

linear_model <- list(
    model = linear_model,
    aug = linear_model_aug,
    tidy = linear_model_tidy
)

save(linear_model, file = paste(
    getwd(),
    "/data/linear_model.rda",
    sep = ""
))
