
#' Title
#'
#' @param input_data
#' @param photon_energy_keV
#' @param yield
#' @param activity_Bq
#' @param method
#' @param earth_data
#' @param cpm
#' @param net_counts_rolling
#' @param net_counts_cumulative
#' @param count_rate_derivative
#' @param collection_time_s
#' @param speed_cm_s
#'
#' @export
#'
predict_count_rate <- function(
        input_data,
        photon_energy_keV,
        yield,
        activity_Bq = 1,
        method = "earth",
        earth_data = earth_model,
        cpm = FALSE,
        net_counts_rolling = FALSE,
        net_counts_cumulative = FALSE,
        count_rate_derivative = FALSE,
        collection_time_s = NULL,
        speed_cm_s = NULL) {

    if (method == "earth") {
        return(predict_count_rate_earth(
            input_data,
            photon_energy_keV,
            yield,
            activity_Bq = activity_Bq,
            earth_data = earth_data,
            cpm = cpm,
            net_counts_rolling = net_counts_rolling,
            net_counts_cumulative = net_counts_cumulative,
            count_rate_derivative = count_rate_derivative,
            collection_time_s = collection_time_s,
            speed_cm_s = speed_cm_s
        ))
    } else if(method == "lm") {
        return(predict_count_rate_lm(
            input_data,
            photon_energy_keV,
            yield,
            activity_Bq = activity_Bq,
            cpm = cpm,
            net_counts_rolling = net_counts_rolling,
            net_counts_cumulative = net_counts_cumulative,
            count_rate_derivative = count_rate_derivative,
            collection_time_s = collection_time_s,
            speed_cm_s = speed_cm_s
        ))
    }
}


predict_count_rate_earth <- function(
        input_data,
        photon_energy_keV,
        yield,
        activity_Bq = 1,
        earth_data = earth_model,
        cpm = FALSE,
        net_counts_rolling = FALSE,
        net_counts_cumulative = FALSE,
        count_rate_derivative = FALSE,
        collection_time_s = NULL,
        speed_cm_s = NULL) {

    if (
        net_counts_rolling
        | net_counts_cumulative
        | count_rate_derivative
        | !is.null(speed_cm_s)
        | !is.null(collection_time_s)) {
        warning(paste(
            "Warning!",
            "The earth method is currently unable to predict anything besides the net count rate",
            "The net counts and count rate derivative will not be computed",
            sep = "\n"
        ))
    }

    number_energies <- length(photon_energy_keV)

    if (number_energies != length(yield)){
        stop(paste(
            "Error!",
            "Energy and yield vectors must have same length",
            sep = "\n"
        ))
    }

    # Calculation of net count rate ----
    data_length <- nrow(input_data)

    output_data <- input_data

    input_data <- input_data[
        rep(seq_len(nrow(input_data)), number_energies),]
    input_data$Es_keV <- rep(photon_energy_keV, each = data_length)
    input_data$yield <- rep(yield, each = data_length)

    input_data <- predict_efficiency(input_data, method = "earth")

    output_data$count_rate <- 0
    output_data$ugeom_count_rate <- 0

    for (i in 1:data_length) {
        subset_i <- input_data[
            output_data$y_cm[i] == input_data$y_cm
            & output_data$contents[i] == input_data$contents, ]

        output_data$count_rate[i] <- activity_Bq * sum(
            subset_i$efficiency * subset_i$yield)

        output_data$ugeom_count_rate <- sqrt(sum(
            (subset_i$ugeom_efficiency * subset_i$yield) ^ 2))
    }

    return(output_data)
}


predict_count_rate_lm <- function(
        input_data,
        photon_energy_keV,
        yield,
        activity_Bq = 1,
        cpm = FALSE,
        net_counts_rolling = FALSE,
        net_counts_cumulative = FALSE,
        count_rate_derivative = FALSE,
        collection_time_s = NULL,
        speed_cm_s = NULL) {

    number_energies <- length(photon_energy_keV)

    if (number_energies != length(yield)){
        stop(paste(
            "Error!",
            "Energy and yield vectors must have same length",
            sep = "\n"
        ))
    }
    if (count_rate_derivative & is.null(speed_cm_s)) {
        stop(paste(
            "Error!",
            "Derivative of count rate requires truck speed",
            sep = "\n"
        ))
    }
    if (net_counts_cumulative & is.null(speed_cm_s)) {
        stop(paste(
            "Error!",
            "Cumulative net counts requires truck speed",
            sep = "\n"
        ))
    }
    if (
        net_counts_rolling & (
            is.null(collection_time_s) | is.null(speed_cm_s))
    ) {
        stop(paste(
            "Error!",
            "Rolling net counts requiress truck speed and collection time",
            sep = "\n"
        ))
    }


    # Calculation of net count rate ----
    data_length <- nrow(input_data)

    output_data <- input_data

    input_data <- input_data[
        rep(seq_len(nrow(input_data)), number_energies),]
    input_data$Es_keV <- rep(photon_energy_keV, each = data_length)
    input_data$yield <- rep(yield, each = data_length)

    input_data <- predict_efficiency(input_data, method = "lm")

    output_data$count_rate <- 0
    output_data$ugeom_count_rate <- 0

    for (i in 1:data_length) {
        subset_i <- input_data[
            output_data$y_cm[i] == input_data$y_cm
            & output_data$contents[i] == input_data$contents, ]

        output_data$count_rate[i] <- activity_Bq * sum(
            subset_i$efficiency * subset_i$yield)

        output_data$ugeom_count_rate <- sqrt(sum(
            (subset_i$ugeom_efficiency * subset_i$yield) ^ 2))
    }

    # Calculation of cumulative integrated counts ----
    if (net_counts_cumulative) {
        output_data$net_counts_cumulative <- (
            (output_data$count_rate / speed_cm_s)
            * integrated_counts_factor(
                output_data$y_cm,
                min(output_data$y_cm)
            )
        )
    }

    # Calculation of rolling integrated counts ----
    if (net_counts_rolling) {
        output_data$net_counts_rolling <- (
            (output_data$count_rate / speed_cm_s)
            * integrated_counts_factor(
                output_data$y_cm,
                output_data$y_cm - (speed_cm_s * collection_time_s)
            )
        )
    }

    # Calculation of count rate derivative ----
    if (count_rate_derivative) {
        output_data$count_rate_derivative <- (
            -4.031354552443670
            * output_data$y_cm
            * speed_cm_s
            * output_data$count_rate
            / (
                (output_data$y_cm ^ 2)
                + (179.5 ^ 2)
            )
        )
    }

    if (cpm) {
        print("cpm")
    }

    # Return the output data ----
    return(output_data)

}
