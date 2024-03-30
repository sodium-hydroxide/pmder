#' PMDER Data Sets
#'
#' @param data_set string, data_set to return from PMDER
#'
#' @export
#'
data_set <- function(data_set) {
    if (data_set == "earth_model") {return(earth_model)}
    else if (data_set == "linear_model") {return(linear_model)}
    else if (data_set == "spectral_data") {return(spectral_data)}
    else if (data_set == "summary_data") {return(summary_data)}
    else {stop(paste(
        "",
        "Error!",
        "Must choose between:",
        "earth_model - MARS Regression Model",
        "linear_model - Linear Regression Model",
        "stat_checks - MCNP Statistical Checks",
        "spectral_data - Raw MCNP Data",
        "summary_data - Summary Data for MCNP",
        sep = "\n"
    ))}
}
