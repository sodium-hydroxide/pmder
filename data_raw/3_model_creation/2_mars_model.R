#----
library(btools)
package_load(
    "readr",
    "broom",
    "dplyr",
    "earth"
)
library(ggplot2)
load("data/summary_data.rda")

summary_data |>
    mutate(
        eff = PrDet / PrReach,
        u_eff = sqrt((uPrDet * PrReach) ^ 2 + (PrDet * uPrReach / (PrReach ^ 2))^2)
    ) |>
    ggplot2::ggplot(ggplot2::aes(
        x = Es_keV, y = eff, color = as.factor(y_m),
        shape = as.factor(contents)
    )) +
    ggplot2::geom_point() +
    theme_bw()

#----
load("data/spectral_data.rda")

# shape <- function(x) {
#     return(c(nrow(x), ncol(x)))
# }

library(btools)
package_load(
    "dplyr",
    "pracma",
    "tidyr",
    "rray"
)

intrinsic_data <- spectral_data


# Rows correspond to bin energy
# Columns correpsond to src energy














int_eff <- intrinsic_efficiency(spectral_data)

ggplot(int_eff, aes(x = E_keV, y = int_efficiency, ymin = int_efficiency - u_int_efficiency, ymax = int_efficiency + u_int_efficiency)) + geom_point() + geom_errorbar()


# flux_t_flux <- flux %*% t(flux)
# u_flux_t_flux <- matrix_uncertainty(flux, t(flux), u_flux, t(u_flux))
#
# flux_t_flux_inv <- pinv(flux_t_flux)
# u_flux_t_flux_inv <- pmax(
#     pinv(flux_t_flux + u_flux_t_flux),
#     pinv(flux_t_flux - u_flux_t_flux)
# )
#
# flux_inverse <- t(flux) %*% flux_t_flux_inv
# u_flux_inverse <- matrix_uncertainty(t(flux), flux_t_flux_inv,
#                                      t(u_flux), u_flux_t_flux_inv)
#
# response <- pulse %*% flux_inverse
# u_response <- matrix_uncertainty(pulse, flux_inverse,
#                                  u_pulse, u_flux_inverse)
#
# efficiency <- apply(response, 1, sum)
# u_efficiency <- apply(u_response, 1, rss)
#
#
# int_eff <- data.frame(
#     E_keV = sort(unique(intrinsic_data$Ed_keV)),
#     value = efficiency,
#     uncert = u_efficiency
# )
