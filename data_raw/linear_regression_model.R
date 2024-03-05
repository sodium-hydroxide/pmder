library(tidyverse)

# Construction of linear model ----
linear_model <- summary_data |> 
    mutate(
        cm = sqrt(y_cm^2 + 179.5 ^ 2),
        kev = Es_keV,
        y_cm = NULL,
        Es_keV = NULL,
        PrReach = NULL,
        uPrReach = NULL,
        actual = PrDet,
        PrDet = NULL,
        se_actual = uPrDet,
        uPrDet = NULL,
        trans_cm = log(cm),
        trans_kev = log(kev),
        trans_actual = log(actual),
        trans_se_actual = se_actual / actual,
        weight_trans = trans_se_actual ^ -2) |> 
    lm(
        trans_actual ~ trans_cm + trans_kev + contents,
        data = _,
        weights = weight_trans)

lm_val <- linear_model$coefficients[c(1,4,2,3)]
lm_cov <- vcov(linear_model)[c(1,4,2,3),c(1,4,2,3)]
lm_se <- sqrt(diag(lm_cov))
lm_cor <- lm_cov / lm_se %*% t(lm_se)

lm_df <- data.frame(cbind(
    lm_val, lm_se, lm_cor
)) |>
    mutate(type = c(
        "$\\hat{\\beta}_0$",
        "$\\hat{\\beta}_1$",
        "$\\hat{\\beta}_2$",
        "$\\hat{\\beta}_3$"
    )) |> relocate(type)

linear_model_beta <- list(
    val = lm_val,
    se = lm_se,
    cov = lm_cov,
    cor = lm_cor,
    dataframe = lm_df
)

# Save data ----
save(linear_model,file = "data/linear_model.rda")
save(linear_model_beta, file = "data/linear_model_beta.rda")
save(count_rate_data_continuous, file = "data/count_rate_data_continuous.R")