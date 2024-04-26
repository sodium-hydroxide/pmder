devtools::load_all()
library(dplyr)
library(ggplot2)


mars_contour <-
    rbind(
        data.frame(
            y_m = rep(pracma::linspace(-5, 5, n = 101), times = 151),
            Es_keV = rep(pracma::linspace(20, 2000, n = 151), each = 101),
            contents = "m"
        ),
        data.frame(
            y_m = rep(pracma::linspace(-5, 5, n = 101), times = 151),
            Es_keV = rep(pracma::linspace(20, 2000, n = 151), each = 101),
            contents = "f"
        )
    ) |>
    predict_efficiency(method = "mars") |>
    mutate(contents = as.factor(contents)) |>
    ggplot(
        aes(x = Es_keV, y = y_m, z = 100*(efficiency))
    ) +
    facet_grid(
        rows = vars(contents),
        labeller = as_labeller(c(
            "m"="Scrap Metal",
            "f"="Foodstuff"
        ))
    ) +
    geom_contour_filled(bins = 10) +
    scale_fill_grey(
        name = "Efficiency (%)",
        start = 1,
        end = 0
    ) +
    scale_x_continuous(
        name = "Source Energy (keV)",
        expand = c(0,0)
    ) +
    scale_y_continuous(
        name = "Truck Position (m)",
        expand = c(0.,0.)
    ) +
    ggtitle(
        "Detection Efficiency for Different\nSource Energies and Positions") +
    theme_bw() +
    theme(
        legend.position = "right",
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(1, "lines")
    )

# Save Images ----

file_name <- paste(
    getwd(),
    "/data_raw/4_figure_generation/gg/fig-results-mars-mars_contour",
    sep=""
)

ggsave(
    paste(file_name, ".eps", sep=""),
    plot=mars_contour,
    device="eps",
    width=6,
    height=8,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".png", sep=""),
    plot=mars_contour,
    device="png",
    width=6,
    height=8,
    units="in",
    dpi=300,
    bg='transparent'
)
ggsave(
    paste(file_name, ".tiff", sep=""),
    plot=mars_contour,
    device="tiff",
    width=6,
    height= 8,
    units="in",
    dpi=300,
    bg='transparent'
)
