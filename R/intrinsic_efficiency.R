#' Calculate intrinsic efficiency of data
#'
#' @param intrinsic_data a
#'
#' @return a
#' @export
#'
intrinsic_efficiency <- function(intrinsic_data) {
    # Check that intrinsic_data contains correct columns
    if (
        FALSE %in% (
            c("Es_keV", "Ed_keV", "F1", "F8", "u_F8", "u_F1") %in%
            names(intrinsic_data)
        )) {
        stop(paste(
            "Error!",
            "Input dataframe must contain:",
            " - Source energies in column \"Es_keV\"",
            " - Binned energies in column \"Ed_keV\"",
            " - F1 values in column \"F1\"",
            " - F8 values in column \"F8\"",
            " - F1 uncertainty in column \"u_F1\"",
            " - F8 uncertainty in column \"u_F8\"",
            sep = "\n"
        ))
    }
    # Clear Global Variables ----
    Ed_keV <- NULL
    int_efficiency <- NULL
    # Subroutines ----
    make_matrix <- function(column, fun, names = FALSE) {
        out_matrix <-
            intrinsic_data |>
            dplyr::select(
                dplyr::all_of("contents"),
                dplyr::all_of("Es_keV"),
                dplyr::all_of("Ed_keV"),
                dplyr::all_of("y_m"),
                dplyr::all_of(column)
            ) |>
            tidyr::pivot_wider(
                names_from = "Es_keV",
                values_from = column,
                id_cols = c("contents", "Ed_keV", "y_m"),
                names_sort = TRUE
            )

        data_cols <- names(out_matrix)
        data_cols <- data_cols[
            !data_cols %in% c("contents", "Ed_keV", "y_m")
        ]

        out_matrix <-
            out_matrix |>
            dplyr::group_by(Ed_keV) |>
            dplyr::summarise(dplyr::across(dplyr::all_of(data_cols), fun)) |>
            dplyr::arrange(Ed_keV) |>
            dplyr::select(-Ed_keV) |>
            as.matrix() |>
            unname()

        return(out_matrix)
    }
    # Function ----
    # Get matrices containing flux and pulse height data
    pulse <- make_matrix("F8", mean)
    u_pulse <- make_matrix("u_F8", btools::rss)
    flux <- make_matrix("F1", mean)
    u_flux <- make_matrix("u_F1", btools::rss)

    # Calculate response function
    response <- pulse %*% (t(flux) %*% pracma::pinv(flux %*% t(flux)))
    response_upper <-
        (pulse + u_pulse) %*%
        (t(flux + u_flux) %*% pracma::pinv(
            (flux + u_flux) %*% t(flux + u_flux))
        )
    response_lower <-
        (pulse - u_pulse) %*%
        (t(flux - u_flux) %*% pracma::pinv(
            (flux - u_flux) %*% t(flux - u_flux))
        )

    return(dplyr::filter(
        data.frame(
            E_keV = sort(unique(intrinsic_data$Ed_keV)),
            int_efficiency = apply(response, 1, sum),
            u_int_efficiency = pmax(
                apply(response_upper, 1, btools::rss),
                apply(response_lower, 1, btools::rss)
            )
        ),
        int_efficiency <= 1
    ))
}
