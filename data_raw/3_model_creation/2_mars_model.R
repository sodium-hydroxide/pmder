#----


devtools::load_all()
library(readr)
library(dplyr)
library(earth)
wd <- paste(
    getwd(),
    "/",
    "data_raw/3_model_creation/",
    sep = ""
)

mars_model_aug <-
    tibble(
        log.PrDet. = log(summary_data$PrDet),
        log.r_m. = log(sqrt(summary_data$y_m^2 + 1.795^2)),
        log.Es_keV. = log(summary_data$Es_keV),
        contents = summary_data$contents,
        unc = summary_data$uPrDet
    ) |>
    filter(is.finite(log.PrDet.)) |>
    mutate(unc = unc / exp(log.PrDet.)) |>
    mutate(`(weights)` = unc ^ (-2), unc = NULL)

mars_model <-
    earth(
        formula = (
            log(PrDet)
            ~ log(sqrt(y_m^2 + 1.795^2))
            + log(Es_keV)
            + contents
            + log(Es_keV):contents
            + log(Es_keV):log(sqrt(y_m^2 + 1.795^2))
            + log(sqrt(y_m^2 + 1.795^2)):contents
            #+ (log(Es_keV) ^ 2)
            #+ (log(sqrt(y_m^2 + 1.795^2)) ^ 2)
        ),
        data = filter(summary_data, PrDet > 0),
        varmod.method = "earth",
        #weights = (PrDet / uPrDet)^2,
        nfold = 2,
        ncross = 30
    )

mars_model_aug <- mutate(mars_model_aug,
    .fitted = unlist(unname(mars_model$fitted.values)),
    .resid = unlist(unname(mars_model$residuals)),
    .hat = hatvalues(mars_model),
    .sigma = unlist(unname(mars_model$varmod$model.var)),
    .coodsd = NULL,
    .std.resid = .resid / .sigma
)

mars_model_tidy <- tibble(
    term = rownames(summary(mars_model)$coefficients),
    estimate = unlist(unname(summary(mars_model)$coefficients)),
    r.squared = mars_model$rsq
)

mars_model <-
    list(model = mars_model, aug = mars_model_aug, tidy = mars_model_tidy)

save(
    mars_model,
    file = paste(
        getwd(),
        "/data/mars_model.rda",
        sep = ""
    )
)
