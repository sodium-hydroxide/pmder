intrinsic_efficiency <- function(
        input_data_set,
        F8_correction_threshold = 1e-5) {

    if (
        FALSE %in% (
            c("Es_keV", "Ed_keV", "F1", "F8") %in% names(input_data_set)
        )) {
        stop(paste(
            "Error!",
            "Input dataframe must contain:",
            " - Source energies in column \"Es_keV\"",
            " - Binned energies in column \"Ed_keV\"",
            " - F1 values in column \"F1\"",
            " - F8 values in column \"F8\"",
            sep = "\n"
        ))
    }

    input_data_set <- input_data_set[
        input_data_set$Ed_keV > F8_correction_threshold
        & input_data_set$Es_keV > F8_correction_threshold,]

    source_energy_keV <- unique(input_data_set$Es_keV)
    num_source <- length(source_energy_keV)
    bin_energy_keV <- unique(input_data_set$Ed_keV)
    num_bin <- length(bin_energy_keV)

    F1_mat <- pracma::zeros(n = num_bin, m = num_source)
    F8_mat <- pracma::zeros(n = num_bin, m = num_source)
    u_F1_mat <- pracma::zeros(n = num_bin, m = num_source)
    u_F8_mat <- pracma::zeros(n = num_bin, m = num_source)

    for (i in 1:num_bin){for (j in 1:num_source) {

        subset_ij <- subset(
            input_data_set,
            Ed_keV == bin_energy_keV[i] & Es_keV == source_energy_keV[j])

        F1_mat[i, j] <- sum(subset_ij$F1)
        F8_mat[i, j] <- sum(subset_ij$F8)

    }}

    F8_tF1 <- F8_mat %*% t(F1_mat)
    F1_tF1 <- F1_mat %*% t(F1_mat)

    response_matrix <- F8_tF1 %*% pracma::pinv(F1_tF1)

    intrinsic_efficiency <- data.frame(
        E_keV = bin_energy_keV,
        value = rep(0, num_bin))

    for (i in 1:num_bin) {
        intrinsic_efficiency$value[i] <- sum(response_matrix[i,])
    }

    return(intrinsic_efficiency)
}
