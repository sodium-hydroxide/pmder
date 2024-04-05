#' Predict absolute efficiency
#'
#' @param input_data Dataframe containing columns Es_keV, y_m, contents.
#' These are respectively, the source energy in keV, the perpendicular
#' distance in cm, and the contents ("m" for scrap metal and "f" for
#' foodstuff).
#' @param method `c("earth", "lm", "interpolate")` String, defaults to earth. Choose which model to use when predicting the intrinsic efficiency.
#'
#' @return The original dataframe with additional columns efficiency and
#' ugeom_efficiency, which are the absolute efficiency of detection and
#' the geometric standard uncertainty.
#' @export
#' @importFrom utils data
#' @importFrom stats pnorm
#' @importFrom stats predict
#'
predict_efficiency <- function(
        input_data,
        method = "earth") {

    # Global Variables ----
    y_m <- NULL
    Es_keV <- NULL

    # Function ----

    if (F %in% (c("Es_keV", "y_m", "contents") %in% names(input_data))) {
        return("Error! Dataframe must contain columns: Es_keV, d_cm, contents")
    }

    if (method == "lm") {
        # Parameters used in regression model, and their covariance matrix
        beta_hat <- list(
            e0 = +7.084329e+00, # intercept term
            e1 = +1.002321e+00, # contents term
            e2 = -4.031355e+00, # distance term
            e3 = +1.154673e+00) # energy term
        cov_beta_hat <- list(
            e00 = +2.736296e-02, # variance of intercept term
            e01 = -6.007654e-05, # covariance between intercept and contents
            e02 = -3.076598e-03, # covariance between intercept and position
            e03 = -1.592928e-03, # covariance between intercept and energy
            e11 = +1.607563e-04, # variance of contents term
            e12 = -2.070185e-05, # covariance between contents and position
            e13 = +7.392290e-06, # covariance between contents and energy
            e22 = +6.094552e-04, # variance of position term
            e23 = -1.618680e-05, # covariance between position and energy
            e33 = +2.398123e-04) # variance of energy term

        # Save predictor variables
        contents <- rep(0, nrow(input_data))
        contents[input_data$contents == "m"] <- 1
        log_distances <- log(sqrt(input_data$y_m^2 + 1.795^2))
        log_energy <- log(input_data$Es_keV)

        input_data$efficiency <- exp(
            beta_hat$e0
            + (beta_hat$e1 * contents)
            + (beta_hat$e2 * log_distances)
            + (beta_hat$e3 * log_energy)
        )

        input_data$ugeom_efficiency <- sqrt(
            (
                cov_beta_hat$e00
                + (cov_beta_hat$e01 * contents)
                + (cov_beta_hat$e02 * log_distances)
                + (cov_beta_hat$e03 * log_energy)
            )
            + contents * (
                cov_beta_hat$e01
                + (cov_beta_hat$e11 * contents)
                + (cov_beta_hat$e12 * log_distances)
                + (cov_beta_hat$e13 * log_energy)
            )
            + log_distances * (
                cov_beta_hat$e02
                + (cov_beta_hat$e12 * contents)
                + (cov_beta_hat$e22 * log_distances)
                + (cov_beta_hat$e23 * log_energy)
            )
            + log_energy * (
                cov_beta_hat$e03
                + (cov_beta_hat$e13 * contents)
                + (cov_beta_hat$e23 * log_distances)
                + (cov_beta_hat$e33 * log_energy)
            )
        )

    }

    else if (method == "interpolate") {
        data("spectral_data", envir = environment())

        all_energies <- unique(summary_data$Es_keV)

        input_data$efficiency <- 0
        input_data$u_efficiency <- 0

        for (i in 1:nrow(input_data)) {

            current_subset <- subset(
                summary_data,
                y_m == input_data$y_m[i]
                & contents == input_data$contents[i])

            current_energy <- input_data$Es_keV[i]

            if (current_energy %in% all_energies) {

                input_data$efficiency[i] <- (
                    subset(current_subset, Es_keV == current_energy)$PrDet[1])
                input_data$u_efficiency[i] <- (
                    subset(current_subset, Es_keV == current_energy)$uPrDet[1])

            }

            else {

                upper_energy <- min(
                    all_energies[all_energies > current_energy])
                lower_energy <- max(
                    all_energies[all_energies < current_energy])

                upper_efficiency <- subset(
                    current_subset, Es_keV == upper_energy)
                upper_u_efficiency <- upper_efficiency$uPrDet[1]
                upper_efficiency <- upper_efficiency$PrDet[1]

                lower_efficiency <- subset(
                    current_subset, Es_keV == lower_energy)
                lower_u_efficiency <- lower_efficiency$uPrDet[1]
                lower_efficiency <- lower_efficiency$PrDet[1]

                energy_slope <- (
                    (current_energy - lower_energy)
                    / (upper_energy - lower_energy)
                )

                input_data$efficiency[i] <- (
                    lower_efficiency
                    + (energy_slope * (upper_efficiency - lower_efficiency)))

                input_data$u_efficiency[i] <- sqrt(
                    (((1.0 - energy_slope) * lower_u_efficiency) ^ 2) +
                        ((energy_slope * upper_u_efficiency) ^ 2))

            }
        }
    }

    else if (method == "earth") {
        data("earth_model", envir = environment())

        input_data$pos <- input_data$y_m
        input_data$y_m <- abs(input_data$y_m)

        earth_output <- predict(
            earth_model,
            input_data,
            interval = "pint",
            level = 2 * pnorm(1) - 1)

        input_data$y_m <- input_data$pos
        input_data$pos <- NULL

        input_data$efficiency <- exp(earth_output$fit)

        input_data$ugeom_efficiency <- (
            0.5 * (earth_output$upr - earth_output$lwr)
        )
    }

    else {stop(paste(
        "Error!",
        "Must have valid model to predict efficiency.",
        sep = "\n"
    ))}

    return(input_data)
}
