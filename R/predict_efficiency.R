#' Predict absolute efficiency
#'
#' @param input_data Dataframe containing columns Es_keV, y_m, contents.
#' These are respectively, the source energy in keV, the perpendicular
#' distance in cm, and the contents ("m" for scrap metal and "f" for
#' foodstuff).
#' @param method `c("mars", "lm", "interpolate")` String, defaults to earth. Choose which model to use when predicting the intrinsic efficiency.
#'
#' @return The original dataframe with additional columns efficiency and
#' ugeom_efficiency, which are the absolute efficiency of detection and
#' the geometric standard uncertainty.
#' @export
#'
predict_efficiency <- function(
        input_data,
        method = "mars") {

    # Global Variables ----
    y_m <- NULL
    Es_keV <- NULL

    # Function ----

    if (F %in% (c("Es_keV", "y_m", "contents") %in% names(input_data))) {
        stop("Error! Dataframe must contain columns: Es_keV, y_m, contents")
    }

    if (method == "lm") {

        # Parameters used in regression model, and their covariance matrix
        beta_hat <- list(
            e0 = linear_model$tidy$estimate[1], # intercept term
            e1 = linear_model$tidy$estimate[4], # contents term
            e2 = linear_model$tidy$estimate[2], # distance term
            e3 = linear_model$tidy$estimate[3] # energy term
        )

        cov_beta_hat <- unname(as.matrix(linear_model$tidy[,4:7]))
        stdev <- linear_model$tidy$std_unc

        for (i in 1:4) {for (j in 1:4) {
            cov_beta_hat[i,j] <-
                cov_beta_hat[i,j] * stdev[i] * stdev[j]
        }}

        cov_beta_hat <- list(
            e00 = cov_beta_hat[1,1],# variance of intercept term
            e01 = cov_beta_hat[1,4],# covariance between intercept and contents
            e02 = cov_beta_hat[1,2],# covariance between intercept and position
            e03 = cov_beta_hat[1,3],# covariance between intercept and energy
            e11 = cov_beta_hat[4,4],# variance of contents term
            e12 = cov_beta_hat[4,2],# covariance between contents and position
            e13 = cov_beta_hat[4,3],# covariance between contents and energy
            e22 = cov_beta_hat[2,2],# variance of position term
            e23 = cov_beta_hat[2,3],# covariance between position and energy
            e33 = cov_beta_hat[3,3])# variance of energy term

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

    else if (method == "mars") {

        input_data$pos <- input_data$y_m
        input_data$y_m <- abs(input_data$y_m)

        mars_output <- stats::predict(
            mars_model,
            input_data,
            interval = "pint",
            level = 2 * stats::pnorm(1) - 1)

        input_data$y_m <- input_data$pos
        input_data$pos <- NULL

        input_data$efficiency <- exp(mars_output$fit)

        input_data$ugeom_efficiency <- (
            0.5 * (mars_output$upr - mars_output$lwr)
        )
    }

    else {stop(paste(
        "Error!",
        "Must have valid model to predict efficiency.",
        sep = "\n"
    ))}

    return(input_data)
}
