library(ggplot2)

history_number <-
    data.frame(
        energy = rep(10:2000,times = 4),
        position = rep(c(0,500, 1000, 1500), each = 1991),
        type = as.factor(rep(
            c(
                "0 m \u2264 |y| < 3 m",
                "3 m \u2264 |y| < 8 m",
                "8 m \u2264 |y| < 13 m",
                "13 m \u2264 |y| < 20 m"
            ),
            each = 1991
        ))
    ) |>
    dplyr::mutate(
        number = (
            Vectorize(function(
                    energy,
                    truckPosition) {

                if (abs(truckPosition) < 300) {
                    if ((energy < 0.5) | (energy > 1.3)) {
                        historiesRequired <- 1e8
                    }
                    else {
                        historiesRequired <- 4e8
                    }
                }
                else if (abs(truckPosition) < 800) {
                    if (energy < 0.6) {
                        historiesRequired <- 1e9
                    }
                    else {
                        historiesRequired <- 1e8
                    }
                }
                else if(abs(truckPosition) < 1300) {
                    if (energy < 0.6) {
                        historiesRequired <- 3e9
                    }
                    else if (energy < 1.2) {
                        historiesRequired <- 3e8
                    }
                    else {
                        historiesRequired <- 1e8
                    }
                }
                else {
                    if (energy < 1.0) {
                        historiesRequired <- 2.5e9
                    }
                    else {
                        historiesRequired <- 3e9
                    }
                }
                return(historiesRequired)
            })
        )(energy / 1000, position)
    ) |>
    ggplot() +
    geom_line(
        mapping = aes(
            x = energy,
            y = number
        )
    ) +
    facet_wrap(
        ~ type, nrow = 2
    ) +
    scale_x_continuous(
        name = "Energy (keV)",
        limits = c(0,2090),
        expand = c(0,0)
    ) +
    scale_y_continuous(
        name = "Histories Needed",
        transform = "log10",
        limits = c(5e7, 5e9),
        expand = c(0,0),
        n.breaks = 5,
        labels = btools::scientific_label
    ) +
    ggtitle("Particle Histories Needed for MCNP") +
    theme_bw() +
    theme(
        strip.background = element_rect(
            fill = "white"
        )
    )

# Save Images ----

file_name <- paste(
    getwd(),
    "/data_raw/4_figure_generation/gg/fig-methods-history_number",
    sep=""
)

ggsave(
    paste(file_name, ".eps", sep=""),
    plot=history_number,
    device="eps",
    width=5,
    height=5,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".png", sep=""),
    plot=history_number,
    device="png",
    width=5,
    height=5,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".tiff", sep=""),
    plot=history_number,
    device="tiff",
    width=5,
    height= 5,
    units="in",
    dpi=300,
    bg='transparent'
)
