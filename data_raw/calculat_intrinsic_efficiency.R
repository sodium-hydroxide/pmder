
# Intrinsic efficiency for closestest approach and for all distances ----

source("R/intrinsic_efficiency.R")
load("data/spectral_data.Rda")
load("data/isotropic.rda")
library(tidyverse)
library(pracma)

correction <- 10

intrinsic_data <- rbind(
    mutate(
        intrinsic_efficiency(
            spectral_data,
            F8_correction_threshold = correction),
        Type = "all"),
    mutate(
        intrinsic_efficiency(
            filter(spectral_data, y_cm == 0),
            F8_correction_threshold = correction),
        Type = "closest"
    ),
    mutate(
        intrinsic_efficiency(
            isotropic,
            F8_correction_threshold = correction),
        Type = "isotropic"
    ))

save(intrinsic_data,file = "intrinsic_data.rda")
