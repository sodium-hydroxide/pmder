devtools::load_all()
library(readr)
library(dplyr)
wd <- paste(
    getwd(),
    "/",
    "data_raw/3_model_creation/",
    sep = ""
)

lm_clean <- function(linear_model) {
    # Clearing Global Variables ----
    std.error <- NULL
    p.value <- NULL
    std_unc <- NULL
    # Function ----
    linear_model_aug <- tibble(data.frame(broom::augment(linear_model)))

    covariance <- summary(linear_model)$cov.unscaled
    stdev <- diag(covariance)
    correlation <- covariance / sqrt(stdev %*% t(stdev))
    colnames(correlation) <- paste("cor_", colnames(correlation), sep = "")
    rownames(correlation) <- NULL

    linear_model_tidy <-
        linear_model |>
        broom::tidy() |>
        dplyr::mutate(r_sqr = summary(linear_model)$r.squared) |>
        rename(std_unc = std.error, p_value = p.value) |>
        cbind(correlation)

    linear_model_tidy <- dplyr::relocate(
        linear_model_tidy,
        grep("cor", names(linear_model_tidy)),
        .after = std_unc
    )

    return(list(
        model = linear_model,
        tidy = linear_model_tidy,
        aug = linear_model_aug
    ))
}

linear_model <-
    summary_data |>
    filter(PrDet > 0 & Es_keV > 300 & y_m <= 5) |>
    mutate(
        r_m = (sqrt(y_m^2 + 1.795^2)),
        PrReach = NULL,
        uPrReach = NULL,
        weight = uPrDet ^ (-2)
    ) |>
    lm(
        log(PrDet) ~ log(r_m) + log(Es_keV) + contents,
        #weights = weight,
        data = _
    ) |>
    lm_clean()


save(linear_model, file = paste(
    getwd(),
    "/data/linear_model.rda",
    sep = ""
))
