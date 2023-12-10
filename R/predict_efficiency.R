#' Predict absolute efficiency
#'
#' @param input_data Dataframe containing columns Es_keV, y_cm, contents.
#' These are respectively, the source energy in keV, the perpendicular 
#' distance in cm, and the contents ("m" for scrap metal and "f" for
#' foodstuff).
#'
#' @return The original dataframe with additional columns efficiency and
#' urel_efficiency, which are the absolute efficiency of detection and
#' the relative standard uncertainty.
#' @export
#'
predict_efficiency <- function(input_data) {

    if (F %in% (c("Es_keV", "y_cm", "contents") %in% names(input_data))) {
        return("Error! Dataframe must contain columns: Es_keV, d_cm, contents")
    }

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
    log_distances <- log(sqrt(input_data$y_cm^2 + 179.5^2))
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

    return(input_data)
}
