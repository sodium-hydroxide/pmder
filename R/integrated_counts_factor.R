integrated_counts_factor <- function(x1, x2) {
    require(hypergeo)
    require(pracma)

    beta <- -4.031354552443670
    yperp <- 179.5

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
