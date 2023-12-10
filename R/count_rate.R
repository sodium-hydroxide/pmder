
library(dplyr)

# This function linearly interpolates the probability data to find the
# probability of a photon with a certain energy being detected, and the
# uncertainty in said probability.
# This function shouldn't be called on its own, but should be used when called
# from the countRate function
probDet <- function(
        photon_energy_keV_j,
        energy_list. = energy_list,
        prob_data. = prob_data_i
){
    if (photon_energy_keV_j %in% energy_list.) {
        prob_data_ij <- prob_data. |> filter(Es_keV == photon_energy_keV_j)
        
        PrDet_ij <- prob_data_ij$PrDet[1]
        uPrDet_ij <- prob_data_ij$uPrDet[1]
    } 
    else {
        
        # Calculate the upper and lower bounds of energy ----
        upperEnergy_ij <- min(energy_list.[energy_list. > photon_energy_keV_j])
        lowerEnergy_ij <- max(energy_list.[energy_list. < photon_energy_keV_j])
        
        # Perform linear interpolation for the probability of detection ----
        upperPrDet_ij <- filter(
            prob_data.,
            Es_keV == upperEnergy_ij)$PrDet[1]
        upperUPrDet_ij <- filter(
            prob_data.,
            Es_keV == upperEnergy_ij)$uPrDet[1]
        lowerPrDet_ij <- filter(
            prob_data.,
            Es_keV == lowerEnergy_ij)$PrDet[1]
        lowerUPrDet_ij <- filter(
          prob_data.,
          Es_keV == lowerEnergy_ij)$uPrDet[1]
        
        energySlope_ij <- `/`(
            (photon_energy_keV_j - lowerEnergy_ij),
            (upperEnergy_ij - lowerEnergy_ij)
        )
        
        PrDet_ij <- lowerPrDet_ij + energySlope_ij * (
            upperPrDet_ij - lowerPrDet_ij
        )
        
        uPrDet_ij <- sqrt(
            (((1.0 - energySlope_ij) * lowerUPrDet_ij) ^ 2) + 
                ((energySlope_ij * upperUPrDet_ij) ^ 2)
        )
        
    }
    
    return(list(pr = PrDet_ij, uncert = uPrDet_ij))
}



count_rate <- function(
        photon_energy_keV,
        yield,
        prob_data. = summary_data,
        activity_Bq = 1,
        cpm = FALSE
) {
    # Check if the yield and photonEnergies have the same length
    if(length(photon_energy_keV) != length(yield)){
      stop(
        "Error. yield and photon_energy_keV vectors must have same length"
    )}
    
    # Create list of energies that data exists for
    energy_list <- unique(prob_data.$Es_keV)
    
    # Initialize a dataframe with the count rate ----
    count_rate_data <- rbind(
        data.frame(y_cm = unique(prob_data.$y_cm)) |> mutate(contents = "m"),
        data.frame(y_cm = unique(prob_data.$y_cm)) |> mutate(contents = "f")
    ) |> mutate(
        contents = as.factor(contents),
        count_rate = 0,
        u_count_rate = 0
    )
    
    # Loop over different positions and truck contents ----
    for (i in 1:nrow(count_rate_data)) {
        prob_data_i <- prob_data. |> 
            filter(y_cm == count_rate_data$y_cm[i]) |> 
            filter(contents == count_rate_data$contents[i])

        count_rate_i <- c()
        u_count_rate_i <- c()
  
        # Loop over source energies ----
        for (j in 1:length(photon_energy_keV)) {
            yield_j <- yield[j]
            photon_energy_keV_j <- photon_energy_keV[j]
            
            PrDet_ij <- probDet(
                photon_energy_keV_j,
                energy_list. = energy_list,
                prob_data. = prob_data_i
            )

            count_rate_i <- c(
                count_rate_i,
                PrDet_ij$pr * yield_j)
            u_count_rate_j <- c(
                u_count_rate_i,
                PrDet_ij$uncert * yield_j)

        }# end loop over source energies

        count_rate_data$count_rate[i] <- sum(count_rate_i)
        count_rate_data$u_count_rate <- sqrt(sum(u_count_rate_i ^ 2))
        
    }# end loop over positions and contents
    
    count_rate_data <- rbind(
        count_rate_data,
        count_rate_data |> filter(y_cm < 0) |> mutate(y_cm = abs(y_cm))
    )
    
    if (cpm) {
        count_rate_data$count_rate <- count_rate_data$count_rate * 60
        count_rate_data$u_count_rate <- count_rate_data$u_count_rate * 60
    }
    
    return(count_rate_data)
}
