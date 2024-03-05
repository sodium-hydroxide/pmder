library(tidyverse)
library(earth)
library(btools)
library(ggpubr)
# Construction of EARTH model ----
earth_model_aug <- mutate(
    summary_data,
    PrReach = NULL,
    uPrReach = NULL,
    trans_actual = log(PrDet),
    se_trans = uPrDet / PrDet,
    weight_trans_actual = se_trans ^ -2,
    PrDet = NULL,
    uPrDet = NULL)

earth_model <- earth(
    formula = (
        trans_actual 
        ~ log(sqrt(as.numeric(y_cm)^2 + 179.5^2)) 
        + Es_keV + contents
        + Es_keV:contents
        + Es_keV:y_cm
        + log(sqrt(as.numeric(y_cm)^2 + 179.5^2)):contents),
    data = earth_model_aug,
    weights = weight_trans_actual,
    varmod.method = "earth",
    nfold = 2,
    ncross = 30)

earth_model_aug <- mutate(
    earth_model_aug,
    .fitted = earth_model$fitted.values,
    .resid = earth_model$residuals,
    .se.fitted = pull(
                mutate(
                    predict(earth_model,
                            interval = "cint",
                            level = 2 * pnorm(1) - 1),
                    se = 0.5 * (upr - lwr)),
                se),
    .std.resid = .resid / sqrt(.se.fitted ^ 2 + se_trans ^ 2)
)

save(earth_model, file = "data/earth_model.Rda")
save(earth_model_aug, file = "data/earth_model_aug.Rda")