#' Predict count rate!
#'
#' @param input_data Dataframe, containing truck positions labeled as `y_m` and contents labeled as `contents`
#' @param photon_energy_keV Vector of float, source emission energies in keV
#' @param yield Vector of float, must be same length as photon_energy_keV. For discrete sources, this be the fraction of the time a photon with the corresponding energy is emitted following radioactive decay. For continuous sources, these two vectors should be used to generate a discrete approximation.
#' @param activity_Bq Activity of radioactive source in Bq. Defaults to 1 Bq.
#' @param method `c("mars", "lm", "interpolate")` String, defaults to earth. Choose which model to use when predicting the count rate.
#' @param cpm Boolean, defaults to FALSE. The results for the count rate and count rate derivative are given in cps and cps / s respectively. When true, the units are cpm and cpm / s.
#' @param net_counts_rolling If the linear model is used and `net_counts_rolling = TRUE`, the net counts from the previous amount of time will be calculated. This requires the `collection_time_s` and `speed_m_s` to be specified.
#' @param net_counts_cumulative If the linear model is used and `net_counts_cumulative = TRUE`, the cummulative net counts will be calculated. This requires the `speed_m_s` to be specified.
#' @param count_rate_derivative If the linear model is used and `count_rate_derivative`, the time derivative of the count rate will be calculated. This requires the `speed_m_s` to be specified.
#' @param collection_time_s Float, parameter to be used if `net_counts_rolling = TRUE`. The time in seconds of collection.
#' @param speed_m_s Float, parameter to be used if calculating net counts or count rate derivative. The speed of the truck in cm / s.
#'
#' @return Dataframe, input data frame with the count rate, its uncertainty, any other net counts / count rate derivative parameters specified.
#' @export
#'
predict_count_rate <- function(
        input_data,
        photon_energy_keV,
        yield,
        activity_Bq = 1,
        method = "mars",
        cpm = FALSE,
        net_counts_rolling = FALSE,
        net_counts_cumulative = FALSE,
        count_rate_derivative = FALSE,
        collection_time_s = NULL,
        speed_m_s = NULL) {

    # Checks that sufficient information is provided ----
    if (method %in% c("mars", "interpolate") & (
        net_counts_rolling
        | net_counts_cumulative
        | count_rate_derivative
        | !is.null(speed_m_s)
        | !is.null(collection_time_s))) {
        warning(paste(
            "Warning!\n",
            "The interpolation and earth methods are currently unable to",
            "predict anything besides the net count rate\n",
            "The net counts and count rate derivative will not be computed",
            sep = ""))

        net_counts_rolling <- FALSE
        net_counts_cumulative <- FALSE
        count_rate_derivative <- FALSE
    }

    number_energies <- length(photon_energy_keV)

    if (number_energies != length(yield)){
        stop(paste(
            "Error!",
            "Energy and yield vectors must have same length",
            sep = "\n"))
    }

    if (count_rate_derivative & is.null(speed_m_s)) {stop(paste(
        "Error!",
        "Derivative of count rate requires truck speed",
        sep = "\n"))}

    if (net_counts_cumulative & is.null(speed_m_s)) {stop(paste(
        "Error!",
        "Cumulative net counts requires truck speed",
        sep = "\n"))
    }

    if (
        net_counts_rolling & (
            is.null(collection_time_s) | is.null(speed_m_s))
    ) {stop(paste(
        "Error!",
        "Rolling net counts requiress truck speed and collection time",
        sep = "\n"))
    }

    # Subroutine for calculating integrated counts ----
    integrated_counts_factor <- function(
        # Final position in cm
        x1,
        # Initial position in cm
        x2) {

        beta <- linear_model$tidy$estimate[2]
        yperp <- 1.795

        coefficient <- (
            (yperp ^ beta)
            / ((x1 ^ 2) + (yperp ^ 2)) ^ (beta / 2)
        )

        geometric1 <- pracma::Real(hypergeo::hypergeo(
            A = 0.5,
            B = -0.5 * beta,
            C = 1.5,
            z = -1 * ((x1 ^ 2) / (yperp ^ 2))
        ))

        geometric2 <- pracma::Real(hypergeo::hypergeo(
            A = 0.5,
            B = -0.5 * beta,
            C = 1.5,
            z = -1 * ((x2 ^ 2) / (yperp ^ 2))
        ))

        return(coefficient * ((x1 * geometric1) - (x2 * geometric2)))
    }

    # Obtaining efficiency for each energy ----
    data_length <- nrow(input_data)
    output_data <- input_data

    input_data <- input_data[
        rep(seq_len(nrow(input_data)), number_energies),]
    input_data$Es_keV <- rep(photon_energy_keV, each = data_length)
    input_data$yield <- rep(yield, each = data_length)

    input_data <- predict_efficiency(input_data, method)

    output_data$count_rate <- 0
    output_data$ugeom_count_rate <- 0

    # Interpolated method has standard efficiency, not geometric
    if (method == "interpolate") {
        input_data$ugeom_efficiency <- input_data$u_efficiency
    }

    for (i in 1:data_length) {
        subset_i <- input_data[
            output_data$y_m[i] == input_data$y_m
            & output_data$contents[i] == input_data$contents, ]

        output_data$count_rate[i] <- activity_Bq * sum(
            subset_i$efficiency * subset_i$yield)

        output_data$ugeom_count_rate[i] <- sqrt(sum(
            (subset_i$ugeom_efficiency * subset_i$yield) ^ 2))
    }

    # Net counts on whole path ----
    if (net_counts_cumulative) {
        output_data$net_counts_cumulative <- (
            (output_data$count_rate / speed_m_s)
            * integrated_counts_factor(
                output_data$y_m,
                min(output_data$y_m)
            )
        )
    }

    # Net counts between two points ----
    if (net_counts_rolling) {
        output_data$net_counts_rolling <- (
            (output_data$count_rate / speed_m_s)
            * integrated_counts_factor(
                output_data$y_m,
                output_data$y_m - (speed_m_s * collection_time_s)
            )
        )
    }

    if (cpm) {
        output_data$count_rate <- output_data$count_rate * 60
    }

    # Interpolated method has standard efficiency, not geometric
    if (method == "interpolate") {
        output_data$u_count_rate <- output_data$ugeom_count_rate * activity_Bq
        output_data$ugeom_count_rate <- NULL

        if (cpm) {output_data$u_count_rate <- output_data$u_count_rate * 60}
    }

    if (count_rate_derivative) {
        output_data$count_rate_derivative <- (
            linear_model$tidy$estimate[2]
            * output_data$y_m
            * speed_m_s
            * output_data$count_rate
            / (
                (output_data$y_m ^ 2)
                + (1.795 ^ 2)
            )
        )
    }

    return(output_data)

}
